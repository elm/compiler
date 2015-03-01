{-# OPTIONS_GHC -Wall #-}

{-| This module contains checks to be run *after* type inference has completed
successfully. At that point we still need to do occurs checks and ensure that
`main` has an acceptable type.
-}
module Type.ExtraChecks (mainType, occurs, wireTypes) where

import Prelude hiding (maybe)
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
        let tipe = ST.deepDealias typeOfMain
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


wireTypes :: (Monad m) => Canonical.Expr -> ErrorT [P.Doc] m ()
wireTypes expr =
  case Expr.checkWires (checkWires In) (checkWires Out) checkLoobacks expr of
    Left err -> throwError err
    Right _  -> return ()


checkWires :: Direction -> String -> ST.CanonicalType -> Either [P.Doc] ()
checkWires direction name tipe =
  checkWiresHelp direction name tipe tipe


checkWiresHelp
    :: Direction
    -> String
    -> ST.CanonicalType
    -> ST.CanonicalType
    -> Either [P.Doc] ()
checkWiresHelp direction name rootType tipe =
  case tipe of
    ST.Aliased _ args t ->
        checkWiresHelp direction name rootType (ST.dealias args t)

    ST.App (ST.Type signal) [t]
        | Var.isStream signal ->
            validWireType False (Just Stream) direction name rootType t

        | Var.isVarying signal ->
            validWireType False (Just Varying) direction name rootType t

    _ ->
        validWireType False Nothing direction name rootType tipe


data Signal = Stream | Varying


validWireType
    :: Bool
    -> Maybe Signal
    -> Direction
    -> String
    -> ST.CanonicalType
    -> ST.CanonicalType
    -> Either [P.Doc] ()
validWireType seenFunc seenSignal direction name rootType tipe =
    let valid localType =
            validWireType seenFunc seenSignal direction name rootType localType

        err kind =
            throw (wireError name direction rootType tipe kind)
    in
    case tipe of
      ST.Aliased _ args t ->
          valid (ST.dealias args t)

      ST.Type v ->
          case any ($ v) [ Var.isJson, Var.isPrimitive, Var.isTuple ] of
            True -> return ()
            False -> err "It contains an unsupported type"

      ST.App t [] ->
          valid t

      ST.App (ST.Type v) [t]
          | Var.isMaybe v -> valid t
          | Var.isArray v -> valid t
          | Var.isList  v -> valid t

      ST.App (ST.Type v) ts
          | Var.isTuple v -> mapM_ valid ts

      ST.App _ _ ->
          err "It contains an unsupported type"

      ST.Var _ ->
          err "It contains a free type variable"

      ST.Lambda _ _ ->
          case direction of
            In -> err "It contains functions"
            Out ->
              if seenFunc
                then err "It contains higher-order functions"
                else
                  case seenSignal of
                    Just Stream ->
                        err "It is a streams that contains a function"

                    Just Varying ->
                        err "It is a varying value that contains a function"

                    Nothing ->
                        forM_ (ST.collectLambdas tipe)
                              (validWireType True seenSignal direction name rootType)

      ST.Record _ (Just _) ->
          err "It contains extended records with free type variables"

      ST.Record fields Nothing ->
          mapM_ (\(k,v) -> (,) k <$> valid v) fields


wireError
    :: String
    -> Direction
    -> ST.CanonicalType
    -> ST.CanonicalType
    -> String
    -> [P.Doc]
wireError name direction rootType localType problemMessage =
    [ P.text (dir "Input" "Output" ++ " Error:")
    , P.nest 4 $
        P.vcat
          [ txt [ "The ", wire, " named '", name, "' has an invalid type.\n" ]
          , P.nest 4 (PP.pretty rootType) <> P.text "\n"
          , txt [ problemMessage, ":\n" ]
          , P.nest 4 (PP.pretty localType) <> P.text "\n"
          , txt [ "Acceptable values for ", wire, "s include:" ]
          , txt [ "  Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, Tuples, unit values," ]
          , txt [ "  Json.Values, ", dir "" "first-order functions, promises, ", "and concrete records." ]
          ]
    ]
  where
    dir inMsg outMsg =
        case direction of
          In -> inMsg
          Out -> outMsg

    txt = P.text . concat

    wire =
        dir "input" "output"


checkLoobacks :: String -> ST.CanonicalType -> Bool -> Either [P.Doc] ()
checkLoobacks name tipe hasExpr =
  checkLoobacksHelp name tipe tipe hasExpr


checkLoobacksHelp :: String -> ST.CanonicalType -> ST.CanonicalType -> Bool -> Either [P.Doc] ()
checkLoobacksHelp name rootType tipe hasExpr =
  case ST.deepDealias tipe of
    ST.Record
      [ ("mailbox", ST.App (ST.Type mailbox) [a])
      , ("stream", ST.App (ST.Type stream) [b])
      ]
      Nothing
        | not hasExpr
          && Var.isStream stream
          && Var.isMailbox mailbox
          && a == b
          ->
            return ()

    ST.App (ST.Type signal) [ ST.App (ST.Type result) [_,_] ]
        | hasExpr && Var.isStream signal && Var.isResult result ->
            return ()

    _ ->
        throw $
          [ P.text "Loopback Error:"
          , P.nest 4 $
              P.vcat
                [ P.text ("The loopback named '" ++ name ++ "' has an invalid type.\n")
                , P.nest 4 (PP.pretty rootType) <> P.text "\n"
                ]
          ]


-- INFINITE TYPES

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
                  TT.App1 a b ->
                      (++) <$> go a <*> go b

                  TT.Fun1 a b ->
                      (++) <$> go a <*> go b

                  TT.Var1 a ->
                      go a

                  TT.EmptyRecord1 ->
                      return []

                  TT.Record1 fields ext ->
                      concat <$> mapM go (ext : concat (Map.elems fields))
