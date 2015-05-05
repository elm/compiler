{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Reporting.Annotation as A
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

toString :: String -> String -> A.Located Error -> String
toString location source (A.A region err) =
  let
    (tag, hint) =
        case err of
          Syntax syntaxError ->
              ( "SYNTAX ERROR"
              , Syntax.toHint syntaxError
              )

          Canonicalize canonicalizeError ->
              ( "NAMING ERROR"
              , Canonicalize.toHint canonicalizeError
              )

          Type typeError ->
              ( "TYPE ERROR"
              , Type.toHint typeError
              )

    usedSpace =
        4 + length tag + 1 + length location

    messageBar =
        "-- " ++ tag ++ " "
        ++ replicate (max 1 (80 - usedSpace)) '-'
        ++ " " ++ location
  in
      messageBar ++ "\n\n"
      ++ R.select region source ++ "\n"
      ++ hint ++ "\n\n\n"


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
  in
      BS.unpack $ Json.encode $
        Json.object
          [ "tag" .= ("error" :: String)
          , "region" .= region
          , "error" .= json
          ]
