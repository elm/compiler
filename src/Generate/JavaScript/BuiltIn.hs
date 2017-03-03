{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.BuiltIn
  ( character
  , list, cons
  , recordUpdate
  , eq, cmp
  , effect, outgoingPort, incomingPort, taskPort
  , crash
  )
  where

import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified Generate.JavaScript.Builder as JS
import Generate.JavaScript.Helpers ((<|), (==>))
import qualified Generate.JavaScript.Variable as Var
import qualified Reporting.Region as R



utils :: Text -> [JS.Expr] -> JS.Expr
utils func args =
  JS.Call (Var.coreNative "Utils" func) args



-- LITERALS


character :: Text -> JS.Expr
character char =
  utils "chr" [ JS.String char ]



-- LISTS


list :: [JS.Expr] -> JS.Expr
list elements =
  case elements of
    [] ->
      JS.Object [ "ctor" ==> JS.String "[]" ]

    front : back ->
      cons front (list back)


cons :: JS.Expr -> JS.Expr -> JS.Expr
cons front back =
  JS.Object
    [ "ctor" ==> JS.String "::"
    , "_0" ==> front
    , "_1" ==> back
    ]



-- RECORDS


recordUpdate :: JS.Expr -> [(Text, JS.Expr)] -> JS.Expr
recordUpdate record fields =
  utils "update"
    [ record
    , JS.Object (map (uncurry (==>)) fields)
    ]



-- COMPARISIONS


eq :: JS.Expr -> JS.Expr -> JS.Expr
eq left right =
  utils "eq" [ left, right ]


cmp :: JS.Expr -> JS.Expr -> JS.Expr
cmp left right =
  utils "cmp" [ left, right ]



-- EFFECTS


effect :: ModuleName.Canonical -> JS.Expr
effect effectName =
  Var.coreNative "Platform" "leaf" <|
    JS.String (ModuleName.canonicalToText effectName)


outgoingPort :: Text -> JS.Expr -> JS.Expr
outgoingPort name converter =
  JS.Call
    (Var.coreNative "Platform" "outgoingPort")
    [ JS.String name, converter ]


incomingPort :: Text -> JS.Expr -> JS.Expr
incomingPort name converter =
  JS.Call
    (Var.coreNative "Platform" "incomingPort")
    [ JS.String name, converter ]

taskPort :: Text -> JS.Expr
taskPort name =
  JS.Call
  (Var.coreNative "Platform" "taskPort")
  [ JS.String name ]

-- CRASH


crash :: ModuleName.Canonical -> R.Region -> Maybe (JS.Expr) -> JS.Expr
crash home region maybeCaseCrashValue =
  let
    homeString =
      JS.String (ModuleName.canonicalToText home)
  in
  case maybeCaseCrashValue of
    Nothing ->
        utils "crash" [ homeString, regionToJs region ]

    Just crashValue ->
        utils "crashCase" [ homeString, regionToJs region, crashValue ]


regionToJs :: R.Region -> JS.Expr
regionToJs (R.Region start end) =
    JS.Object
      [ "start" ==> positionToJs start
      , "end"   ==> positionToJs end
      ]


positionToJs :: R.Position -> JS.Expr
positionToJs (R.Position line column) =
    JS.Object
      [ "line"   ==> JS.Int line
      , "column" ==> JS.Int column
      ]
