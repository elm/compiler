-- This module contains checks to be run *after* type inference has
-- completed successfully. At that point we still need to do occurs
-- checks and ensure that `main` has an acceptable type.
module Type.ExtraChecks (extraChecks, occursCheck) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.Type ( Variable, structure, Term1(..), toSrcType )
import qualified Type.State as TS
import qualified Type.Alias as Alias
import Text.PrettyPrint as P
import SourceSyntax.PrettyPrint (pretty)
import SourceSyntax.Type (Type)
import qualified SourceSyntax.Location as Location
import qualified SourceSyntax.Expression as Expr
import qualified Data.Traversable as Traverse

extraChecks :: Alias.Rules -> TS.Env -> IO (Either [P.Doc] (Map.Map String Type))
extraChecks rules env =
    mainCheck rules <$> Traverse.traverse toSrcType env

mainCheck :: Alias.Rules -> (Map.Map String Type) -> Either [P.Doc] (Map.Map String Type)
mainCheck rules env =
    let acceptable = ["Graphics.Element.Element","Signal.Signal Graphics.Element.Element"] in
    case Map.lookup "main" env of
      Nothing -> Right env
      Just tipe
        | P.render (pretty (Alias.canonicalRealias (fst rules) tipe)) `elem` acceptable ->
            Right env
        | otherwise ->
            Left [ P.vcat [ P.text "Type Error:"
                          , P.text "Bad type for 'main'. It must have type Element or a (Signal Element)"
                          , P.text "Instead 'main' has type:\n"
                          , P.nest 4 . pretty $ Alias.realias rules tipe
                          , P.text " " ]
                 ]

occursCheck :: (String, Variable) -> StateT TS.SolverState IO ()
occursCheck (name, variable) =
  do vars <- liftIO $ infiniteVars [] variable
     case vars of
       [] -> return ()
       var:_ -> do
         desc <- liftIO $ UF.descriptor var
         case structure desc of
           Nothing ->
               modify $ \state -> state { TS.sErrors = fallback : TS.sErrors state }
           Just struct ->
               do liftIO $ UF.setDescriptor var (desc { structure = Nothing })
                  var' <- liftIO $ UF.fresh desc
                  TS.addError (Location.NoSpan name) (Just msg) var var'
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
