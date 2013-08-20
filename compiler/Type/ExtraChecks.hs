-- This module contains checks to be run *after* type inference has
-- completed successfully. At that point we still need to do occurs
-- checks and ensure that `main` has an acceptable type.
module Type.ExtraChecks (extraChecks) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import Type.Type ( Variable, structure, Term1(..) )
import Type.State (Env)
import Type.PrettyPrint ( pretty, ParensWhen(Never) )
import Text.PrettyPrint as P

extraChecks :: Env -> IO (Either [P.Doc] Env)
extraChecks env = 
    case mainCheck env of
      Left errs -> return $ Left errs
      Right env -> occursCheck env

mainCheck :: Env -> Either [P.Doc] Env
mainCheck env =
    case Map.lookup "main" env of
      Nothing -> Right env
      Just var
        | P.render (pretty Never var) `elem` ["Element","Signal Element"] -> Right env
        | otherwise ->
            Left [ P.vcat [ P.text "Type Error:"
                          , P.text "'main' must be an Element or a (Signal Element)\n" ] ]

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
