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
import qualified AST.Variable as V
import qualified Transform.Expression as Expr
import qualified Type.Type as TT
import qualified Type.State as TS

throw :: [Doc] -> Either [Doc] a
throw err = Left [ P.vcat err ]

mainType :: TS.Env -> ErrorT [P.Doc] IO (Map.Map String ST.CanonicalType)
mainType environment =
  do environment' <- liftIO $ Traverse.traverse TT.toSrcType environment
     mainCheck environment'
  where
    mainCheck :: (Monad m) => Map.Map String ST.CanonicalType
              -> ErrorT [P.Doc] m (Map.Map String ST.CanonicalType)
    mainCheck env =
      case Map.lookup "main" env of
        Nothing -> return env
        Just typeOfMain
            | tipe `elem` acceptable -> return env
            | otherwise              -> throwError err
            where
              acceptable = [ "Graphics.Element.Element"
                           , "Signal.Signal Graphics.Element.Element" ]

              tipe = PP.renderPretty typeOfMain
              err = [ P.text "Type Error: 'main' must have type Element or (Signal Element)."
                    , P.text "Instead 'main' has type:\n"
                    , P.nest 4 (PP.pretty typeOfMain)
                    , P.text " " ]

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
              case any ($ v) [ V.isJson, V.isPrimitive, V.isTuple ] of
                True -> return ()
                False -> err "an unsupported type"

          ST.App t [] -> valid t

          ST.App (ST.Type v) [t]
              | V.isSignal v -> handleSignal t
              | V.isMaybe  v -> valid t
              | V.isArray  v -> valid t
              | V.isList   v -> valid t

          ST.App (ST.Type v) ts
              | V.isTuple v -> mapM_ valid ts
                    
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
              , txt [ "    Json.Values, ", dir "" "first-order functions, ", "and concrete records." ]
              ]

occurs :: (String, TT.Variable) -> StateT TS.SolverState IO ()
occurs (name, variable) =
  do vars <- liftIO $ infiniteVars [] variable
     case vars of
       [] -> return ()
       var:_ -> do
         desc <- liftIO $ UF.descriptor var
         case TT.structure desc of
           Nothing ->
               modify $ \s -> s { TS.sErrors = P.text msg : TS.sErrors s }
           Just _ ->
               do liftIO $ UF.setDescriptor var (desc { TT.structure = Nothing })
                  var' <- liftIO $ UF.fresh desc
                  TS.addError (A.None (P.text name)) (Just msg) var var'
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
