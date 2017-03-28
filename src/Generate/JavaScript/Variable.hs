{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Variable
    ( fresh
    , local
    , global
    , qualified
    , native
    , coreNative
    , staticProgram
    , defineGlobal
    , safe
    )
    where

import qualified Control.Monad.State as State
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Helpers as JS



-- FRESH NAMES


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack ("_v" ++ show n))



-- VARIABLES


local :: Text -> JS.Expr
local name =
  JS.ref (safe name)


global :: ModuleName.Canonical -> Text -> JS.Expr
global home name =
  if Help.isOp name then
    JS.BracketRef (JS.ref (opsDict home)) (JS.String name)
  else
    JS.ref (qualified home name)



-- MISC


native :: ModuleName.Canonical -> Text -> JS.Expr
native moduleName name =
  JS.obj [ moduleToText moduleName, name ]


coreNative :: Text -> Text -> JS.Expr
coreNative moduleName name =
  native (ModuleName.inCore ("Native." <> moduleName)) name


staticProgram :: JS.Expr
staticProgram =
  native (ModuleName.inVirtualDom "Native.VirtualDom") "staticProgram"



-- DEFINE GLOBALS


defineGlobal :: ModuleName.Canonical -> Text -> JS.Expr -> [JS.Stmt]
defineGlobal home name body =
  case Help.isOp name of
    False ->
      [ JS.VarDeclStmt [ JS.varDecl (qualified home name) body ]
      ]

    True ->
      let
        dict = opsDict home
        lvalue = JS.LBracket (JS.ref dict) (JS.String name)
      in
        [ JS.VarDeclStmt [ JS.varDecl dict (JS.refOrObject dict) ]
        , JS.ExprStmt (JS.Assign lvalue body)
        ]



-- GLOBAL NAMES


qualified :: ModuleName.Canonical -> Text -> Text
qualified home name =
  moduleToText home <> "$" <> name


opsDict :: ModuleName.Canonical -> Text
opsDict home =
  moduleToText home <> "_ops"


moduleToText :: ModuleName.Canonical -> Text
moduleToText (ModuleName.Canonical (Pkg.Name user project) moduleName) =
  let
    safeUser =
      Text.replace "-" "_" user

    safeProject =
      Text.replace "-" "_" project

    safeModuleName =
      Text.replace "." "_" moduleName
  in
    "_" <> safeUser <> "$" <> safeProject <> "$" <> safeModuleName



-- SAFE NAMES


safe :: Text -> Text
safe name =
  if Set.member name jsReserveds then "_" <> name else name


jsReserveds :: Set.Set Text
jsReserveds =
  Set.fromList
    -- JS reserved words
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
    -- reserved by the Elm runtime system
    , "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    , "ctor"
    ]
