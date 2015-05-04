{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Region as R


-- ALL POSSIBLE ERRORS

data Error
    = Syntax Syntax.Error
    | Canonicalize Canonicalize.Error
    | Type Type.Error


-- TO STRING

toString :: R.Region -> Error -> String
toString region err =
  case err of
    Syntax syntaxError ->
        Syntax.toString region syntaxError

    Canonicalize canonicalizeError ->
        Canonicalize.toString region canonicalizeError

    Type typeError ->
        Type.toString region typeError


-- JSON

toJson :: R.Region -> Error -> Json.Value
toJson region err =
  let json =
        case err of
          Syntax syntaxError ->
              Syntax.toJson syntaxError

          Canonicalize canonicalizeError ->
              Canonicalize.toJson canonicalizeError

          Type typeError ->
              Type.toJson typeError
  in
      Json.object
        [ "tag" .= ("error" :: String)
        , "region" .= region
        , "error" .= json
        ]
