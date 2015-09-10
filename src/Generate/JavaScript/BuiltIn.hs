module Generate.JavaScript.BuiltIn where

import qualified Language.ECMAScript3.Syntax as JS

import Generate.JavaScript.Helpers
import qualified Reporting.Region as R


utils :: String -> [JS.Expression ()] -> JS.Expression ()
utils func args =
  obj ["_utils", func] `call` args


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
  utils "list" [ JS.ArrayLit () elements ]


range :: JS.Expression () -> JS.Expression () -> JS.Expression ()
range low high =
  utils "range" [ low, high ]


-- RECORDS

recordUpdate :: JS.Expression () -> [(String, JS.Expression ())] -> JS.Expression ()
recordUpdate record fields =
  utils "update" [ record, JS.ArrayLit () (map toKeyValue fields) ]


toKeyValue :: (String, JS.Expression ()) -> JS.Expression ()
toKeyValue (key, value) =
  JS.ArrayLit () [ JS.StringLit () key, value ]


-- COMPARISIONS

eq :: JS.Expression () -> JS.Expression () -> JS.Expression ()
eq left right =
  utils "eq" [ left, right ]


compare :: JS.Expression () -> JS.Expression () -> JS.Expression ()
compare left right =
  utils "compare" [ left, right ]


-- CRASH

crash :: R.Region -> Maybe String -> JS.Expression ()
crash region maybeCaseCrashValue =
  case maybeCaseCrashValue of
    Nothing ->
        utils "crash" [ regionToJs region ]

    Just crashValue ->
        utils "caseCrash" [ regionToJs region, ref crashValue ]


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