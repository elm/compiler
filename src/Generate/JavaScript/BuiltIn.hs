module Generate.JavaScript.BuiltIn
  ( character, string
  , list, range
  , recordUpdate
  , eq, cmp
  , effect
  , crash
  )
  where

import Control.Arrow (first)
import qualified Language.ECMAScript3.Syntax as JS

import qualified AST.Module.Name as ModuleName
import Generate.JavaScript.Helpers
import qualified Reporting.Region as R



utils :: String -> [JS.Expression ()] -> JS.Expression ()
utils func args =
  obj ["_elm_lang$core$Native_Utils", func] `call` args


nativeList :: String -> [JS.Expression ()] -> JS.Expression ()
nativeList func args =
  obj ["_elm_lang$core$Native_List", func] `call` args



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
  nativeList "fromArray" [ JS.ArrayLit () elements ]


range :: JS.Expression () -> JS.Expression () -> JS.Expression ()
range low high =
  nativeList "range" [ low, high ]



-- RECORDS


recordUpdate :: JS.Expression () -> [(String, JS.Expression ())] -> JS.Expression ()
recordUpdate record fields =
  utils "update"
    [ record
    , JS.ObjectLit () (map (first prop) fields)
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
  obj ["_elm_lang$core$Native_Platform", "leaf"] <|
    JS.StringLit () (ModuleName.canonicalToString effectName)



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
      [ ( prop "start", positionToJs start )
      , ( prop "end", positionToJs end )
      ]


positionToJs :: R.Position -> JS.Expression ()
positionToJs (R.Position line column) =
    JS.ObjectLit ()
      [ ( prop "line", JS.IntLit () line )
      , ( prop "column", JS.IntLit () column )
      ]