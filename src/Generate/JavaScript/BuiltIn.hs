{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.BuiltIn
  ( char
  , list
  , cons
  , recordUpdate
  , eq
  , cmp
  , effectManagers
  , effect
  , outgoingPort
  , incomingPort
  , staticProgram
  , crash
  )
  where


import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Generate.JavaScript.Builder as JS
import Generate.JavaScript.Helpers ((<|), (==>))
import qualified Generate.JavaScript.Variable as Var
import Generate.JavaScript.Variable (Generator)
import qualified Reporting.Region as R



-- LITERALS


char :: Text -> Generator JS.Expr
char c =
  utils "chr" [ JS.String c ]


utils :: Text -> [JS.Expr] -> Generator JS.Expr
utils name args =
  do  func <- core "Elm.Kernel.Utils" name
      return $ JS.Call func args


core :: ModuleName.Raw -> Text -> Generator JS.Expr
core home name =
  Var.global (Var.Global (ModuleName.inCore home) name)



-- LISTS


list :: [JS.Expr] -> Generator JS.Expr
list elements =
  do  fromArray <- core "Elm.Kernel.List" "fromArray"
      return $ fromArray <| JS.Array elements


cons :: JS.Expr -> JS.Expr -> Generator JS.Expr
cons first rest =
  do  func <- core "Elm.Kernel.List" "Cons"
      return $ JS.Call func [first, rest]



-- RECORDS


recordUpdate :: JS.Expr -> [(Text, JS.Expr)] -> Generator JS.Expr
recordUpdate record fields =
  utils "update"
    [ record
    , JS.Object (map (uncurry (==>)) fields)
    ]



-- COMPARISIONS


eq :: JS.Expr -> JS.Expr -> Generator JS.Expr
eq left right =
  utils "eq" [ left, right ]


cmp :: JS.Expr -> JS.Expr -> Generator JS.Expr
cmp left right =
  utils "cmp" [ left, right ]



-- EFFECTS


effectManagers :: Generator JS.Expr
effectManagers =
  core "Elm.Kernel.Platform" "effectManagers"


effect :: ModuleName.Canonical -> Generator JS.Expr
effect effectName =
  do  leaf <- core "Elm.Kernel.Platform" "leaf"
      return $ leaf <| JS.String (ModuleName.canonicalToText effectName)


outgoingPort :: Text -> JS.Expr -> Generator JS.Expr
outgoingPort name converter =
  do  outPort <- core "Elm.Kernel.Platform" "outgoingPort"
      return $ JS.Call outPort [ JS.String name, converter ]


incomingPort :: Text -> JS.Expr -> Generator JS.Expr
incomingPort name converter =
  do  inPort <- core "Elm.Kernel.Platform" "incomingPort"
      return $ JS.Call inPort [ JS.String name, converter ]



-- VIRTUAL DOM


staticProgram :: Generator JS.Expr
staticProgram =
  Var.global (Var.Global (ModuleName.inBrowser "Browser") "staticPage")



-- CRASH


crash :: ModuleName.Canonical -> R.Region -> Maybe (JS.Expr) -> Generator JS.Expr
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
