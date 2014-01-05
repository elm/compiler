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
import qualified SourceSyntax.Type as T
import qualified SourceSyntax.Expression as E
import qualified SourceSyntax.Location as L
import qualified Transform.Expression as Expr
import qualified Data.Traversable as Traverse
import System.IO.Unsafe

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

portTypes :: Alias.Rules -> E.LExpr -> Either [P.Doc] ()
portTypes rules expr =
  const () <$> Expr.checkPorts checkIn checkOut expr
  where
    showType tipe = (P.nest 4 . pretty $ Alias.realias rules tipe) <> P.text "\n"

    checkIn name st tt =
        do t <- getSignal name st
           case unsafePerformIO (toSrcType tt) of
             T.Lambda a b ->
                 do isJsType "coming in" name a
                    if b `elem` okayTypes then return () else throw msg
                 where
                   okayTypes = [ t
                               , T.Data "Maybe.Maybe" [t]
                               , T.Data "Either.Either" [T.Data "String.String" [], t]
                               ]
                   msg = [ P.text $ "Type Error: the handler for port '" ++ name ++
                                    "' returns values of type:\n"
                         , showType b
                         , P.text $ "but the port's type annotation requires that the handler returns values"
                         , P.text $ "with one of the following types:\n"
                         ] ++ map showType okayTypes ++
                         [ P.text "If the handler returns a Maybe or Either, invalid values will cause an"
                         , P.text "error to be thrown to the JS error handler associated with this port." ]

             tipe ->
                 throw [ P.text $ "Type Error: the handler for port '" ++ name ++
                                  "' must be a function!"
                       , P.text "Instead it was given a value of type:\n"
                       , showType tipe
                       ]

    checkOut name st =
        do t <- getSignal name st
           isJsType "sent out" name t

    getSignal name tipe =
        case tipe of
          T.Data "Signal.Signal" [t] -> return t
          _ -> throw [ P.text $ "Type Error: port '" ++ name ++ "' must be a Signal,"
                     , P.text $ "but the type annotation says it has non-Signal type:\n"
                     , showType tipe
                     , P.text $ "Maybe instead you want the following type?\n"
                     , showType (T.Data "Signal.Signal" [tipe])
                     ]

    isJsType msg name tipe =
        case tipe of
          T.Data ctor ts | okay ctor -> mapM_ (isJsType msg name) ts
                         | otherwise -> throw $ err msg "Elm values"
          T.Var _       -> throw $ err msg "type variables"
          T.Lambda _ _  -> throw $ err msg "Elm functions"
          T.EmptyRecord -> throw $ err msg "Elm records"
          T.Record _ _  -> throw $ err msg "Elm records"
        where
          okay ctor = and [ List.isPrefixOf "JavaScript." ctor
                          , 1 == length (filter (=='.') ctor) ]

          err msg kind =
              [ P.text $ "Type Error: the values " ++ msg ++ " through port '" ++ name ++
                         "' must be JavaScript values."
              , P.text $ "The values sent through this port contain " ++ kind ++ " with type:\n"
              , showType tipe
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
