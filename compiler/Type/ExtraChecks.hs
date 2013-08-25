-- This module contains checks to be run *after* type inference has
-- completed successfully. At that point we still need to do occurs
-- checks and ensure that `main` has an acceptable type.
module Type.ExtraChecks (extraChecks) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.Type ( Variable, structure, Term1(..), toSrcType )
import Type.State (Env)
import qualified Type.Alias as Alias
import Text.PrettyPrint as P
import SourceSyntax.PrettyPrint (pretty)
import SourceSyntax.Type (Type)
import qualified Data.Traversable as Traverse

extraChecks :: Alias.Rules -> Env -> IO (Either [P.Doc] (Map.Map String Type))
extraChecks rules env = do
  eitherEnv <- occursCheck env
  case eitherEnv of
    Left errs -> return $ Left errs
    Right env' ->
        mainCheck rules <$> Traverse.traverse toSrcType env'
           

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
                          , P.nest 4 . pretty $ Alias.canonicalRealias (fst rules) tipe
                          , P.text " " ]
                 ]

occursCheck :: Env -> IO (Either [P.Doc] Env)
occursCheck env = do
  errors <- concat <$> mapM isFinite (Map.toList env)
  return $ if null errors then Right env else Left errors

isFinite :: (String, Variable) -> IO [P.Doc]
isFinite (name, var) =
  do varIsFinite <- go [] var
     return $
       case varIsFinite of
         True -> []
         False -> [ P.vcat [ P.text "Type Error:"
                           , P.text $ "Cannot construct infinite type for '" ++ name ++ "'\n"
                           ]
                  ]
  where
    go :: [Variable] -> Variable -> IO Bool
    go seen var =
        let check = go (var:seen) in
        case var `elem` seen of
          True -> return False
          False -> do
            desc <- UF.descriptor var
            case structure desc of
              Nothing -> return True
              Just struct ->
                  case struct of
                    App1 a b -> (&&) <$> check a <*> check b
                    Fun1 a b -> (&&) <$> check a <*> check b
                    Var1 a   -> check a
                    EmptyRecord1 -> return True
                    Record1 fields ext -> and <$> mapM check (ext : concat (Map.elems fields))
