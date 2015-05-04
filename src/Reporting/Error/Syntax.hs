{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Syntax where

import qualified Data.Aeson as Json
import qualified Text.Parsec.Error as Parsec

import qualified Reporting.Region as R


data Error
    = Parse [Parsec.Message]
    | InfixDuplicate String
    | TypeWithoutDefinition String
    | PortWithoutAnnotation String
    | DuplicateValueDeclaration String
    | DuplicateTypeDeclaration String
    | DuplicateDefinition String
    | UnboundTypeVar String String


-- TO STRING

toString :: R.Region -> Error -> String
toString region err =
  case err of
    _ -> error "Syntax.toString" region


-- TO JSON

toJson :: Error -> Json.Value
toJson err =
  case err of
    _ -> error "Syntax.toJson"

