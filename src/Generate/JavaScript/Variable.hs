{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Variable
  ( Generator
  , run
  , Table
  , init
  , fresh
  , local
  , safe
  , global
  , getGlobalName
  )
  where


import Prelude hiding (init)
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Word (Word16, Word32)

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Elm.Compiler.Objects.Internal as Obj
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
    , _symbols :: Obj.SymbolTable
    }


init :: Obj.SymbolTable -> Table
init symbols =
  Table 0 Map.empty symbols



-- FRESH NAMES


fresh :: Generator Text
fresh =
  do  (Table uid names symbols) <- State.get
      State.put (Table (uid + 1) names symbols)
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
  do  table@(Table _ _ symbols) <- State.get
      case Obj.lookup var symbols of
        Nothing ->
          JS.ref <$> getGlobalNameHelp var table

        Just (Obj.Symbol symHome symName) ->
          return $
            JS.DotRef
              (JS.ref (symbolHomeToText symHome))
              (JS.Id (symbolNameToText symName))


getGlobalName :: Var.Global -> Generator Text
getGlobalName var =
  do  table <- State.get
      getGlobalNameHelp var table


getGlobalNameHelp :: Var.Global -> Table -> Generator Text
getGlobalNameHelp var@(Var.Global home name) (Table uid names symbols) =
  case Map.lookup var names of
    Just jsName ->
      return jsName

    Nothing ->
      case Help.isOp name of
        False ->
          do  let jsName = globalToName home name
              State.put (Table uid (Map.insert var jsName names) symbols)
              return jsName

        True ->
          do  let jsName = globalToName home ("_op" <> Text.pack (show uid))
              State.put (Table (uid + 1) (Map.insert var jsName names) symbols)
              return jsName


globalToName :: ModuleName.Canonical -> Text -> Text
globalToName (ModuleName.Canonical (Pkg.Name user project) moduleName) name =
  Text.replace "-" "_" user
  <> "$" <> Text.replace "-" "_" project
  <> "$" <> Text.replace "." "_" moduleName
  <> "$" <> name



-- SYMBOLS


symbolHomeToText :: Word16 -> Text
symbolHomeToText word =
  Text.pack ('$' : show word)


symbolNameToText :: Word32 -> Text
symbolNameToText word =
  Text.pack (makeName word "")


makeName :: Word32 -> String -> String
makeName word str =
  if word < 26 then
    makeChar 97 word : str

  else if word < 52 then
    makeChar 65 (word - 26) : str

  else
    let
      (next, leftover) =
        quotRem word 52
    in
      makeName next (makeInnerChar leftover : str)


makeInnerChar :: Word32 -> Char
makeInnerChar word =
  if word < 26 then
    makeChar 97 word
  else
    makeChar 65 (word - 26)


{-# INLINE makeChar #-}
makeChar :: Word32 -> Word32 -> Char
makeChar root offset =
  Char.chr (fromIntegral (root + offset))
