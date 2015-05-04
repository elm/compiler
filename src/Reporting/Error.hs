{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Error.CheckMatch as CheckMatch


-- ALL POSSIBLE ERRORS

data Error
    = Syntax Syntax.Error
    | Canonicalize Canonicalize.Error
    | Type Type.Error
    | CheckMatch CheckMatch.Error


-- TO STRING

toString :: A.Located Error -> String
toString (A.A region err) =
  case err of
    Syntax syntaxError ->
        Syntax.toString region syntaxError

    Canonicalize canonicalizeError ->
        Canonicalize.toString region canonicalizeError

    Type typeError ->
        Type.toString region typeError

    CheckMatch matchError ->
        CheckMatch.toString region matchError

-- JSON

toJson :: A.Located Error -> String
toJson (A.A region err) =
  let json =
        case err of
          Syntax syntaxError ->
              Syntax.toJson syntaxError

          Canonicalize canonicalizeError ->
              Canonicalize.toJson canonicalizeError

          Type typeError ->
              Type.toJson typeError

          CheckMatch matchError ->
              CheckMatch.toJson matchError
  in
      BS.unpack $ Json.encode $
        Json.object
          [ "tag" .= ("error" :: String)
          , "region" .= region
          , "error" .= json
          ]
