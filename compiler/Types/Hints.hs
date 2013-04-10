module Types.Hints (hints) where

import Control.Arrow (first)
import qualified Data.Map as Map
import Guid
import qualified Libraries as Libs
import Parse.Library (iParse)
import Parse.Types
import qualified Types.Substitutions as Subs
import Types.Types

hints :: GuidCounter [(String, Scheme)]
hints = mapM toScheme values
 where
  values :: [(String, String)]
  values = addPrefixes (Map.toList (Map.map Map.toList Libs.libraries))

  addPrefixes :: [(String,[(String, String)])] -> [(String, String)]
  addPrefixes = concatMap (\(m,vs) -> map (first (\n -> m ++ "." ++ n)) vs)

  toScheme :: (String, String) -> GuidCounter (String, Scheme)
  toScheme (name, tipeString) =
   case iParse (fmap toType typeExpr) "parsing types in docs.json" tipeString of
     Left err   -> error (show err)
     Right tipe -> do scheme <- Subs.generalize [] =<< Subs.superize name tipe
                      return (name, scheme)
