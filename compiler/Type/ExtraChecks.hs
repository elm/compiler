-- This module contains checks to be run *after* type inference has
-- completed successfully. At that point we still need to do occurs
-- checks and ensure that `main` has an acceptable type.
module Type.ExtraChecks (extraChecks) where

import qualified Data.Map as Map
import Type.State (Env)
import Type.PrettyPrint ( pretty, ParensWhen(Never) )
import Text.PrettyPrint as P

extraChecks :: Env -> Either [P.Doc] Env
extraChecks env = occursCheck =<< mainCheck env

mainCheck :: Env -> Either [P.Doc] Env
mainCheck env =
    case Map.lookup "main" env of
      Nothing -> Right env
      Just var
        | P.render (pretty Never var) `elem` ["Element","Signal Element"] -> Right env
        | otherwise ->
            Left [ P.vcat [ P.text "Type Error:"
                          , P.text "'main' must be an Element or a (Signal Element)" ] ]

occursCheck :: Env -> Either [P.Doc] Env
occursCheck env = return env