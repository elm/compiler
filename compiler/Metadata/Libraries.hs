module Metadata.Libraries (libraries, addPrelude) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import Data.List (inits)
import Text.JSON
import Metadata.Load as Load
import SourceSyntax.Module

addPrelude :: Module t v -> Module t v
addPrelude (Module name exs ims stmts) = Module name exs (customIms ++ ims) stmts
    where customIms = concatMap addModule prelude

          addModule (n, method) = case lookup n ims of
                                    Nothing     -> [(n, method)]
                                    Just (As m) -> [(n, method)]
                                    Just _      -> []

prelude = text : map (\n -> (n, Hiding [])) modules
  where
    text = ("Text", Hiding ["link", "color", "height"])
    modules = [ "Prelude", "Signal", "List", "Maybe", "Time"
              , "Graphics.Element", "Color", "Graphics.Collage" ]

libraries :: Map.Map String (Map.Map String String)
libraries = Map.empty