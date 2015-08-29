module Generate.JavaScript.BuiltIn where

import qualified Language.ECMAScript3.Syntax as JS


-- LITERALS

character :: Char -> JS.Expression ()
character char =
  error "TODO - utils.chr" JS.StringLit () [char]


string :: String -> JS.Expression ()
string str =
  JS.StringLit () str


-- LISTS

list :: [JS.Expression ()] -> JS.Expression ()
list elements =
  error "TODO - utils.list" (JS.ArrayLit () elements)


range :: JS.Expression () -> JS.Expression () -> JS.Expression ()
range low high =
  error "TODO - utils.range" low high


-- RECORDS

recordUpdate :: JS.Expression () -> [(String, JS.Expression ())] -> JS.Expression ()
recordUpdate record fields =
  error "TODO - utils.update" record fields


-- COMPARISIONS

eq :: JS.Expression () -> JS.Expression () -> JS.Expression ()
eq left right =
  error "TODO - utils.eq" left right


compare :: JS.Expression () -> JS.Expression () -> JS.Expression ()
compare left right =
  error "TODO - utils.compare" left right
