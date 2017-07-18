{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Variable
  ( Generator
  , run
  , fresh
  , local
  , safe
  , global
  , globalName
  , globalToName
  )
  where


import qualified Control.Monad.State.Strict as State
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Helpers as JS



-- GENERATOR


type Generator = State.State Int


run :: Generator a -> a
run generator =
  State.evalState generator 0



-- FRESH NAMES


fresh :: Generator Text
fresh =
  do  uid <- State.get
      State.put (uid + 1)
      return ("_v" <> Text.pack (show uid))



-- LOCAL NAMES


local :: Text -> JS.Expr
local name =
  JS.ref (safe name)


safe :: Text -> Text
safe name =
  if Set.member name reservedWords then
    "_" <> name
  else
    name


reservedWords :: Set.Set Text
reservedWords =
  Set.fromList
    -- reserved by JavaScript
    [ "null", "undefined", "NaN", "Infinity", "true", "false", "eval"
    , "arguments", "int", "byte", "char", "goto", "long", "final", "float"
    , "short", "double", "native", "throws", "boolean", "abstract", "volatile"
    , "transient", "synchronized", "function", "break", "case", "catch"
    , "continue", "debugger", "default", "delete", "do", "else", "finally"
    , "for", "function", "if", "in", "instanceof", "new", "return", "switch"
    , "this", "throw", "try", "typeof", "var", "void", "while", "with", "class"
    , "const", "enum", "export", "extends", "import", "super", "implements"
    , "interface", "let", "package", "private", "protected", "public"
    , "static", "yield"
    -- reserved by Elm
    , "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    ]



-- GLOBAL NAMES


global :: Var.Global -> Generator JS.Expr
global var =
  JS.ref <$> globalName var


globalName :: Var.Global -> Generator Text
globalName (Var.Global home name) =
  return (globalToName home name)


globalToName :: ModuleName.Canonical -> Text -> Text
globalToName (ModuleName.Canonical (Pkg.Name user project) moduleName) name =
  if ModuleName.isKernel moduleName then
    "_" <> ModuleName.getKernel moduleName <> "_" <> name

  else
    let
      revisedName =
        if Help.isOp name then
          "_op_" <> Help.desymbol name
        else
          name
    in
      Text.replace "-" "_" user
      <> "$" <> Text.replace "-" "_" project
      <> "$" <> Text.replace "." "_" moduleName
      <> "$" <> revisedName

