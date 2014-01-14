{-# OPTIONS_GHC -W #-}
module Type.ExtraChecks (mainType, occurs, portTypes) where
-- This module contains checks to be run *after* type inference has
-- completed successfully. At that point we still need to do occurs
-- checks and ensure that `main` has an acceptable type.

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.Type ( Variable, structure, Term1(..), toSrcType )
import qualified Type.State as TS
import qualified Type.Alias as Alias
import Text.PrettyPrint as P
import SourceSyntax.PrettyPrint (pretty)
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Location as L
import qualified Transform.Expression as Expr
import qualified Data.Traversable as Traverse

throw err = Left [ P.vcat err ]

mainType :: Alias.Rules -> TS.Env -> IO (Either [P.Doc] (Map.Map String T.Type))
mainType rules env = mainCheck rules <$> Traverse.traverse toSrcType env
  where
    mainCheck :: Alias.Rules -> Map.Map String T.Type -> Either [P.Doc] (Map.Map String T.Type)
    mainCheck rules env =
      case Map.lookup "main" env of
        Nothing -> Right env
        Just mainType
            | tipe `elem` acceptable -> Right env
            | otherwise              -> throw err
            where
              acceptable = [ "Graphics.Element.Element"
                           , "Signal.Signal Graphics.Element.Element" ]

              tipe = P.render . pretty $ Alias.canonicalRealias (fst rules) mainType
              err = [ P.text "Type Error: 'main' must have type Element or (Signal Element)."
                    , P.text "Instead 'main' has type:\n"
                    , P.nest 4 . pretty $ Alias.realias rules mainType
                    , P.text " " ]

data Direction = In | Out

portTypes :: Alias.Rules -> E.LExpr -> Either [P.Doc] ()
portTypes rules expr =
  const () <$> Expr.checkPorts (check In) (check Out) expr
  where
    check = isValid False False
    isValid seenFunc seenSignal direction name tipe =
        let valid = isValid seenFunc seenSignal direction name in
        case tipe of
          T.Data ctor ts
              | isJs ctor || isElm ctor -> mapM_ valid ts
              | ctor == "Signal.Signal" -> handleSignal ts
              | otherwise               -> err "Algebraic Data Types"

          T.Var _ -> err "free type variables"

          T.Lambda _ _ ->
              case direction of
                In -> err "functions"
                Out | seenFunc   -> err "higher-order functions"
                    | seenSignal -> err "signals that contain functions"
                    | otherwise  ->
                        mapM_ (isValid True seenSignal direction name) (T.collectLambdas tipe)

          T.Record _ (Just _) -> err "extended records with free type variables"

          T.Record fields Nothing ->
              mapM_ (\(k,v) -> (,) k <$> valid v) fields

        where
          isJs ctor =
              List.isPrefixOf "JavaScript." ctor
              && length (filter (=='.') ctor) == 1

          isElm ctor =
              ctor `elem` ["Int","Float","String","Bool","Maybe.Maybe","_List"]
              || Help.isTuple ctor

          handleSignal ts
              | seenFunc   = err "functions that involve signals"
              | seenSignal = err "signals-of-signals"
              | otherwise  = mapM_ (isValid seenFunc True direction name) ts

          dir inMsg outMsg = case direction of { In -> inMsg ; Out -> outMsg }
          txt = P.text . concat

          err kind =
              throw $
              [ txt [ "Type Error: the value ", dir "coming in" "sent out"
                    , " through port '", name, "' is invalid." ]
              , txt [ "Acceptable values for ", dir "incoming" "outgoing"
                    , " ports include JavaScript values and the following Elm values:" ]
              , txt [ "Ints, Floats, Bools, Strings, Maybes, Lists, Tuples, "
                    , dir "" "first-order functions, ", "and concrete records." ]
              , txt [ "The values sent through this port contain ", kind, ":\n" ]
              , (P.nest 4 . pretty $ Alias.realias rules tipe) <> P.text "\n"
              ]

occurs :: (String, Variable) -> StateT TS.SolverState IO ()
occurs (name, variable) =
  do vars <- liftIO $ infiniteVars [] variable
     case vars of
       [] -> return ()
       var:_ -> do
         desc <- liftIO $ UF.descriptor var
         case structure desc of
           Nothing ->
               modify $ \state -> state { TS.sErrors = fallback : TS.sErrors state }
           Just _ ->
               do liftIO $ UF.setDescriptor var (desc { structure = Nothing })
                  var' <- liftIO $ UF.fresh desc
                  TS.addError (L.NoSpan name) (Just msg) var var'
  where
    msg = "Infinite types are not allowed"
    fallback _ = return $ P.text msg

    infiniteVars :: [Variable] -> Variable -> IO [Variable]
    infiniteVars seen var =
        let go = infiniteVars (var:seen) in
        if var `elem` seen
        then return [var]
        else do
          desc <- UF.descriptor var
          case structure desc of
            Nothing -> return []
            Just struct ->
                case struct of
                  App1 a b -> (++) <$> go a <*> go b
                  Fun1 a b -> (++) <$> go a <*> go b
                  Var1 a   -> go a
                  EmptyRecord1 -> return []
                  Record1 fields ext -> concat <$> mapM go (ext : concat (Map.elems fields))
