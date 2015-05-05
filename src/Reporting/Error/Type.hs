{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Type where

--import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Text.PrettyPrint as P

import qualified AST.Type as T
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


data Error
    = Mismatch Mismatch
    | InfiniteType
    | BadMain T.Canonical


data Mismatch = MismatchInfo
    { _hint :: Maybe String
    , _leftType :: T.Canonical
    , _rightType :: T.Canonical
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


-- HINTS

toHint :: Error -> String
toHint err =
  case err of
    Mismatch (MismatchInfo maybeHint leftType rightType) ->
        maybe "This expression is triggering a type mismatch." id maybeHint
        ++ "\n\n"
        ++ "I am trying to infer the type, but I am getting conflicting information.\n"
        ++ "Some evidence suggests in has the type:\n\n"
        ++ P.render (P.nest 4 (P.pretty False leftType))
        ++ "\n\nBut other evidence suggests:\n\n"
        ++ P.render (P.nest 4 (P.pretty False rightType))

    InfiniteType ->
        error "TODO"

    BadMain tipe ->
        "The 'main' value has an unsupported type:\n\n"
        ++ P.render (P.nest 4 (P.pretty False tipe))
        ++ "\n\n"
        ++ "Please give me an Element, Html, (Signal Element), or (Signal Html) so\n"
        ++ "that I can render it on screen!"
