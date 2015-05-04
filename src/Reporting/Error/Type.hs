{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

--import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import qualified AST.Type as T
import qualified Reporting.Region as R


data Error
    = Mismatch Mismatch
    | InfiniteType R.Region
    | BadMain T.Canonical
    | ProblemWithInferenceAlgorithm String


data Mismatch = MismatchInfo
    { hint :: Maybe String
    , leftType :: T.Canonical
    , rightType :: T.Canonical
    }


-- TO STRING

toString :: R.Region -> Error -> String
toString region err =
  case err of
    _ -> error "Type.toString" region


-- TO JSON

toJson :: Error -> Json.Value
toJson err =
  case err of
    _ -> error "Type.toJson"

