module Types.Hints (hints) where

import Control.Arrow (first)
import qualified Data.Map as Map
import Guid
import Libraries
import Parse.Library (iParse)
import Parse.Types
import Types.Substitutions
import Types.Types

hints :: GuidCounter [(String, Scheme)]
hints = mapM toScheme values
 where
  addPrefixes = concatMap (\(m,vs) -> map (first (\n -> m ++ "." ++ n)) vs)
  values = addPrefixes (Map.toList (Map.map Map.toList libraries))
  toScheme (name, tipeString) =
   case iParse (fmap toType typeExpr) "parsing types in docs.json" tipeString of
     Left err   -> error (show err)
     Right tipe -> do scheme <- generalize [] =<< superize name tipe
                      return (name, scheme)