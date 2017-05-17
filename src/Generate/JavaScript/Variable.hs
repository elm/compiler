{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Variable
  ( Generator
  , run
  , Table
  , emptyTable
  , fresh
  , local
  , safe
  , global
  , globalName
  )
  where


import qualified Control.Monad.State as State
import Data.Monoid ((<>))
import qualified Data.Map as Map
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


type Generator = State.State Table


run :: Table -> Generator a -> ( a, Table )
run table generator =
  State.runState generator table



-- STATE


data Table =
  Table
    { _uid :: !Int
    , _names :: Map.Map Var.Global Text
    }


emptyTable :: Table
emptyTable =
  Table 0 Map.empty



-- FRESH NAMES


fresh :: Generator Text
fresh =
  do  (Table uid names) <- State.get
      State.put (Table (uid + 1) names)
      return (Text.pack ("_v" ++ show uid))



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
    [ "null", "undefined", "Nan", "Infinity", "true", "false", "eval"
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
    , "ctor"
    ]



-- GLOBAL NAMES


global :: Var.Global -> Generator JS.Expr
global var =
  JS.ref <$> globalName var


globalName :: Var.Global -> Generator Text
globalName var =
  do  table <- State.get
      globalNameHelp var table


globalNameHelp :: Var.Global -> Table -> Generator Text
globalNameHelp var@(Var.Global home name) (Table uid names) =
  case Help.isOp name of
    False ->
      return $ globalToName home name

    True ->
      case Map.lookup var names of
        Just jsName ->
          return jsName

        Nothing ->
          do  let jsName = globalToName home ("_op" <> Text.pack (show uid))
              State.put (Table (uid + 1) (Map.insert var jsName names))
              return jsName


globalToName :: ModuleName.Canonical -> Text -> Text
globalToName (ModuleName.Canonical (Pkg.Name user project) moduleName) name =
  if ModuleName.isKernel moduleName then
    "_" <> ModuleName.getKernel moduleName <> "_" <> name

  else
    Text.replace "-" "_" user
    <> "$" <> Text.replace "-" "_" project
    <> "$" <> Text.replace "." "_" moduleName
    <> "$" <> name

