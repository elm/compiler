module Reporting.Error.CheckMatch where

import qualified Data.Aeson as Json

import qualified AST.Variable as Var
import qualified Reporting.Region as R

data Error = ConstructorTypeNotFound Var.Canonical

-- TO STRING

toString :: R.Region -> Error -> String
toString region err =
  case err of
    _ -> error "CheckMatch.toString" region


-- TO JSON

toJson :: Error -> Json.Value
toJson err =
  case err of
    _ -> error "CheckMatch.toJson"

