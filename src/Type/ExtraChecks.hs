{-# OPTIONS_GHC -Wall #-}

{-| This module contains checks to be run *after* type inference has completed
successfully. At that point we still need to do occurs checks and ensure that
`main` has an acceptable type.
-}
module Type.ExtraChecks (mainType, occurs, portTypes) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Traversable as Traverse
import qualified Data.UnionFind.IO as UF
import Text.PrettyPrint as P

import qualified AST.Annotation as A
import qualified AST.Expression.Canonical as Canonical
import qualified AST.PrettyPrint as PP
import qualified AST.Type as ST
import qualified AST.Variable as Var
import qualified Transform.Expression as Expr
import qualified Type.Hint as Hint
import qualified Type.Type as TT
import qualified Type.State as TS


throw :: [Doc] -> Either [Doc] a
throw err =
  Left [ P.vcat err ]


-- MAIN TYPE

mainType :: TS.Env -> ErrorT [P.Doc] IO (Map.Map String ST.CanonicalType)
mainType environment =
  do  environment' <- liftIO $ Traverse.traverse TT.toSrcType environment
      mainCheck environment'


mainCheck
    :: (Monad m)
    => Map.Map String ST.CanonicalType
    -> ErrorT [P.Doc] m (Map.Map String ST.CanonicalType)
mainCheck env =
  case Map.lookup "main" env of
    Nothing ->
        return env

    Just typeOfMain ->
        let tipe = ST.dealias typeOfMain
        in
            if tipe `elem` validMainTypes
              then return env
              else throwError [ badMainMessage typeOfMain ]


validMainTypes :: [ST.CanonicalType]
validMainTypes =
    [ element
    , html
    , varying element
    , varying html
    ]
  where
    fromModule :: [String] -> String -> ST.CanonicalType
    fromModule home name =
      ST.Type (Var.fromModule home name)

    html =
        fromModule ["VirtualDom"] "Node"

    varying tipe =
        ST.App (fromModule ["Signal"] "Varying") [ tipe ]

    element =
      let builtin name =
            ST.Type (Var.builtin name)

          maybe tipe =
            ST.App (fromModule ["Maybe"] "Maybe") [ tipe ]
      in
        ST.Record
          [ ("element", fromModule ["Graphics","Element"] "ElementPrim")
          , ("props",
              ST.Record
                [ ("click"  , builtin "_Tuple0")
                , ("color"  , maybe (fromModule ["Color"] "Color"))
                , ("height" , builtin "Int")
                , ("hover"  , builtin "_Tuple0")
                , ("href"   , builtin "String")
                , ("id"     , builtin "Int")
                , ("opacity", builtin "Float")
                , ("tag"    , builtin "String")
                , ("width"  , builtin "Int")
                ]
                Nothing
            )
          ]
          Nothing


badMainMessage :: ST.CanonicalType -> P.Doc
badMainMessage typeOfMain =
  P.vcat
    [ P.text "Type Error: 'main' must have one of the following types:"
    , P.text " "
    , P.text "    Element, Html, Varying Element, Varying Html"
    , P.text " "
    , P.text "Instead 'main' has type:\n"
    , P.nest 4 (PP.pretty typeOfMain)
    , P.text " "
    ]


-- PORT TYPES

data Direction = In | Out


portTypes :: (Monad m) => Canonical.Expr -> ErrorT [P.Doc] m ()
portTypes expr =
  case Expr.checkPorts (check In) (check Out) expr of
    Left err -> throwError err
    Right _  -> return ()
  where
    check = isValid True False False
    isValid isTopLevel seenFunc seenSignal direction name tipe =
        case tipe of
          ST.Aliased _ t -> valid t

          ST.Type v ->
              case any ($ v) [ Var.isJson, Var.isPrimitive, Var.isTuple ] of
                True -> return ()
                False -> err "an unsupported type"

          ST.App t [] -> valid t

          ST.App (ST.Type v) [t]
              | Var.isSignal v -> handleSignal t
              | Var.isMaybe  v -> valid t
              | Var.isArray  v -> valid t
              | Var.isList   v -> valid t

          ST.App (ST.Type v) [_, _]
              | Var.isPromise v ->
                  case direction of
                    In -> err "promises"
                    Out -> return ()

          ST.App (ST.Type v) ts
              | Var.isTuple v -> mapM_ valid ts

          ST.App _ _ -> err "an unsupported type"

          ST.Var _ -> err "free type variables"

          ST.Lambda _ _ ->
              case direction of
                In -> err "functions"
                Out | seenFunc   -> err "higher-order functions"
                    | seenSignal -> err "signals that contain functions"
                    | otherwise  ->
                        forM_ (ST.collectLambdas tipe)
                              (isValid' True seenSignal direction name)

          ST.Record _ (Just _) -> err "extended records with free type variables"

          ST.Record fields Nothing ->
              mapM_ (\(k,v) -> (,) k <$> valid v) fields

        where
          isValid' = isValid False
          valid = isValid' seenFunc seenSignal direction name

          handleSignal t
              | seenFunc   = err "functions that involve signals"
              | seenSignal = err "signals-of-signals"
              | isTopLevel = isValid' seenFunc True direction name t
              | otherwise  = err "a signal within a data stucture"

          dir inMsg outMsg = case direction of { In -> inMsg ; Out -> outMsg }
          txt = P.text . concat

          err kind =
              throw $
              [ txt [ "Type Error: the value ", dir "coming in" "sent out"
                    , " through port '", name, "' is invalid." ]
              , txt [ "It contains ", kind, ":\n" ]
              , P.nest 4 (PP.pretty tipe) <> P.text "\n"
              , txt [ "Acceptable values for ", dir "incoming" "outgoing", " ports include:" ]
              , txt [ "    Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, Tuples, unit values," ]
              , txt [ "    Json.Values, ", dir "" "first-order functions, promises, ", "and concrete records." ]
              ]


occurs :: (String, TT.Variable) -> StateT TS.SolverState IO ()
occurs (name, variable) =
  do  vars <- liftIO $ infiniteVars [] variable
      case vars of
        [] ->
          return ()

        var : _ ->
          do  desc <- liftIO $ UF.descriptor var
              case TT.structure desc of
                Nothing ->
                  TS.addHint (P.text msg)

                Just _ ->
                  do  liftIO $ UF.setDescriptor var (desc { TT.structure = Nothing })
                      var' <- liftIO $ UF.fresh desc
                      hint <- liftIO $ Hint.create (A.None (P.text name)) (Just msg) var var'
                      TS.addHint hint
  where
    msg = "Infinite types are not allowed"

    infiniteVars :: [TT.Variable] -> TT.Variable -> IO [TT.Variable]
    infiniteVars seen var =
        let go = infiniteVars (var:seen) in
        if var `elem` seen
        then return [var]
        else do
          desc <- UF.descriptor var
          case TT.structure desc of
            Nothing -> return []
            Just struct ->
                case struct of
                  TT.App1 a b -> (++) <$> go a <*> go b
                  TT.Fun1 a b -> (++) <$> go a <*> go b
                  TT.Var1 a   -> go a
                  TT.EmptyRecord1 -> return []
                  TT.Record1 fields ext -> concat <$> mapM go (ext : concat (Map.elems fields))
