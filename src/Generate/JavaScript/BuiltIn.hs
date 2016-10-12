module Generate.JavaScript.BuiltIn
  ( character, string
  , list, cons
  , recordUpdate
  , eq, cmp
  , effect, outgoingPort, incomingPort
  , crash
  )
  where

import qualified Language.ECMAScript3.Syntax as JS

import qualified AST.Module.Name as ModuleName
import Generate.JavaScript.Helpers ((<|), (==>), call)
import qualified Generate.JavaScript.Variable as Var
import qualified Reporting.Region as R



utils :: String -> [JS.Expression ()] -> JS.Expression ()
utils func args =
  Var.coreNative "Utils" func `call` args



-- LITERALS


character :: Char -> JS.Expression ()
character char =
  utils "chr" [ JS.StringLit () [char] ]


string :: String -> JS.Expression ()
string str =
  JS.StringLit () str



-- LISTS


list :: [JS.Expression ()] -> JS.Expression ()
list elements =
  case elements of
    [] ->
      JS.ObjectLit () [ "ctor" ==> JS.StringLit () "[]" ]

    front : back ->
      cons front (list back)


cons :: JS.Expression () -> JS.Expression () -> JS.Expression ()
cons front back =
  JS.ObjectLit ()
    [ "ctor" ==> JS.StringLit () "::"
    , "_0" ==> front
    , "_1" ==> back
    ]



-- RECORDS


recordUpdate :: JS.Expression () -> [(String, JS.Expression ())] -> JS.Expression ()
recordUpdate record fields =
  utils "update"
    [ record
    , JS.ObjectLit () (map (uncurry (==>)) fields)
    ]



-- COMPARISIONS


eq :: JS.Expression () -> JS.Expression () -> JS.Expression ()
eq left right =
  utils "eq" [ left, right ]


cmp :: JS.Expression () -> JS.Expression () -> JS.Expression ()
cmp left right =
  utils "cmp" [ left, right ]



-- EFFECTS


effect :: ModuleName.Canonical -> JS.Expression ()
effect effectName =
  Var.coreNative "Platform" "leaf" <|
    JS.StringLit () (ModuleName.canonicalToString effectName)


outgoingPort :: String -> JS.Expression () -> JS.Expression ()
outgoingPort name converter =
  Var.coreNative "Platform" "outgoingPort" `call` [ JS.StringLit () name, converter ]


incomingPort :: String -> JS.Expression () -> JS.Expression ()
incomingPort name converter =
  Var.coreNative "Platform" "incomingPort" `call` [ JS.StringLit () name, converter ]



-- CRASH


crash :: ModuleName.Canonical -> R.Region -> Maybe (JS.Expression ()) -> JS.Expression ()
crash home region maybeCaseCrashValue =
  let
    homeString =
      JS.StringLit () (ModuleName.canonicalToString home)
  in
  case maybeCaseCrashValue of
    Nothing ->
        utils "crash" [ homeString, regionToJs region ]

    Just crashValue ->
        utils "crashCase" [ homeString, regionToJs region, crashValue ]


regionToJs :: R.Region -> JS.Expression ()
regionToJs (R.Region start end) =
    JS.ObjectLit ()
      [ "start" ==> positionToJs start
      , "end"   ==> positionToJs end
      ]


positionToJs :: R.Position -> JS.Expression ()
positionToJs (R.Position line column) =
    JS.ObjectLit ()
      [ "line"   ==> JS.IntLit () line
      , "column" ==> JS.IntLit () column
      ]
