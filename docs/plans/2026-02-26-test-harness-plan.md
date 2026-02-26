# Test Harness Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Re-introduce a two-layer test harness (parse tests + compile tests) for the Elm 0.19.1 compiler, using tasty.

**Architecture:** Parse tests call `Parse.Module.fromByteString` directly via shared source dirs. Compile tests shell out to the built `elm` binary via `System.Process`. Tests discover `.elm` files in `tests/` subdirectories automatically.

**Tech Stack:** Haskell, tasty + tasty-hunit, cabal test-suite, `Parse.Module` API, `System.Process`

**Design doc:** `docs/plans/2026-02-26-test-harness-design.md`

---

### Task 1: Add the test-suite stanza to elm.cabal

**Files:**
- Modify: `elm.cabal` (append after line 235)

**Step 1: Add the test-suite stanza**

Append this after the last line of `elm.cabal`:

```cabal

Test-Suite elm-tests
    Type:
        exitcode-stdio-1.0

    Hs-Source-Dirs:
        tests
        compiler/src

    Main-Is:
        Test.hs

    other-modules:
        Test.Parse
        Test.Compile
        -- compiler modules needed for parse tests
        AST.Canonical
        AST.Optimized
        AST.Source
        AST.Utils.Binop
        AST.Utils.Shader
        AST.Utils.Type
        Canonicalize.Effects
        Canonicalize.Environment
        Canonicalize.Environment.Dups
        Canonicalize.Environment.Foreign
        Canonicalize.Environment.Local
        Canonicalize.Expression
        Canonicalize.Module
        Canonicalize.Pattern
        Canonicalize.Type
        Compile
        Data.Bag
        Data.Index
        Data.Map.Utils
        Data.Name
        Data.NonEmptyList
        Data.OneOrMore
        Data.Utf8
        Elm.Compiler.Imports
        Elm.Compiler.Type
        Elm.Compiler.Type.Extract
        Elm.Constraint
        Elm.Docs
        Elm.Float
        Elm.Interface
        Elm.Kernel
        Elm.Licenses
        Elm.Magnitude
        Elm.ModuleName
        Elm.Package
        Elm.String
        Elm.Version
        Generate.Html
        Generate.JavaScript
        Generate.JavaScript.Builder
        Generate.JavaScript.Expression
        Generate.JavaScript.Functions
        Generate.JavaScript.Name
        Generate.Mode
        Json.Decode
        Json.Encode
        Json.String
        Nitpick.Debug
        Nitpick.PatternMatches
        Optimize.Case
        Optimize.DecisionTree
        Optimize.Expression
        Optimize.Module
        Optimize.Names
        Optimize.Port
        Parse.Declaration
        Parse.Expression
        Parse.Keyword
        Parse.Module
        Parse.Number
        Parse.Pattern
        Parse.Primitives
        Parse.Shader
        Parse.Space
        Parse.String
        Parse.Symbol
        Parse.Type
        Parse.Variable
        Reporting.Annotation
        Reporting.Doc
        Reporting.Error
        Reporting.Error.Canonicalize
        Reporting.Error.Docs
        Reporting.Error.Import
        Reporting.Error.Json
        Reporting.Error.Main
        Reporting.Error.Pattern
        Reporting.Error.Syntax
        Reporting.Error.Type
        Reporting.Render.Code
        Reporting.Render.Type
        Reporting.Render.Type.Localizer
        Reporting.Report
        Reporting.Result
        Reporting.Suggest
        Reporting.Warning
        Type.Constrain.Expression
        Type.Constrain.Module
        Type.Constrain.Pattern
        Type.Error
        Type.Instantiate
        Type.Occurs
        Type.Solve
        Type.Type
        Type.Unify
        Type.UnionFind

    ghc-options:
        -O0 -Wall

    Build-depends:
        base,
        tasty >= 1.4,
        tasty-hunit >= 0.10,
        bytestring,
        containers,
        directory,
        filepath,
        process,
        -- shared compiler deps
        ansi-terminal,
        ansi-wl-pprint < 1,
        binary,
        edit-distance,
        ghc-prim,
        language-glsl,
        mtl,
        parsec,
        raw-strings-qq,
        scientific,
        template-haskell,
        unordered-containers,
        utf8-string,
        vector
```

**Step 2: Verify the stanza is syntactically valid**

Run: `cd /Users/dokkora/fun/compiler && cabal check 2>&1 | head -20`

This may show warnings (that's OK), but should not show parse errors for the cabal file.

**Step 3: Commit**

```bash
git add elm.cabal
git commit -m "Add test-suite stanza to elm.cabal"
```

---

### Task 2: Create the test runner (Test.hs)

**Files:**
- Create: `tests/Test.hs`

**Step 1: Write the main test runner**

Create `tests/Test.hs`:

```haskell
module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Parse as Parse
import qualified Test.Compile as Compile


main :: IO ()
main =
  do  parseTests   <- Parse.tests
      compileTests <- Compile.tests
      defaultMain $
        testGroup "Elm Compiler Tests"
          [ parseTests
          , compileTests
          ]
```

**Step 2: Commit**

```bash
git add tests/Test.hs
git commit -m "Add main test runner"
```

---

### Task 3: Create the parse test module (Test/Parse.hs)

**Files:**
- Create: `tests/Test/Parse.hs`

**Step 1: Write the parse test discovery and execution module**

Create `tests/Test/Parse.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Test.Parse (tests) where

import qualified Data.ByteString as BS

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)

import qualified Parse.Module as Parse
import qualified Elm.Package as Pkg


tests :: IO TestTree
tests =
  do  goods <- getElmFiles "tests/parse-good"
      bads  <- getElmFiles "tests/parse-bad"
      goodTests <- traverse mkGoodTest goods
      badTests  <- traverse mkBadTest bads
      return $
        testGroup "Parse Tests"
          [ testGroup "Good (should parse)" goodTests
          , testGroup "Bad (should fail)" badTests
          ]


getElmFiles :: FilePath -> IO [FilePath]
getElmFiles dir =
  do  entries <- listDirectory dir
      let elmFiles = filter (\f -> takeExtension f == ".elm") entries
      return (map (dir </>) elmFiles)


mkGoodTest :: FilePath -> IO TestTree
mkGoodTest path =
  return $ testCase path $
    do  source <- BS.readFile path
        case Parse.fromByteString Parse.Application source of
          Right _modul ->
            assertBool "" True

          Left _err ->
            assertFailure ("Parse failed for: " ++ path)


mkBadTest :: FilePath -> IO TestTree
mkBadTest path =
  return $ testCase path $
    do  source <- BS.readFile path
        case Parse.fromByteString Parse.Application source of
          Right _modul ->
            assertFailure ("Parse succeeded but should have failed for: " ++ path)

          Left _err ->
            assertBool "" True
```

**Step 2: Commit**

```bash
git add tests/Test/Parse.hs
git commit -m "Add parse test module"
```

---

### Task 4: Create the compile test module (Test/Compile.hs)

**Files:**
- Create: `tests/Test/Compile.hs`

**Step 1: Write the compile test module**

Create `tests/Test/Compile.hs`:

```haskell
module Test.Compile (tests) where

import System.Directory (listDirectory, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.Process (readProcessWithExitCode)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)


tests :: IO TestTree
tests =
  do  elmExe <- findElm
      goods  <- getElmFiles "tests/compile-good/src"
      bads   <- getElmFiles "tests/compile-bad/src"
      goodTests <- traverse (mkGoodTest elmExe "tests/compile-good") goods
      badTests  <- traverse (mkBadTest elmExe "tests/compile-bad") bads
      return $
        testGroup "Compile Tests"
          [ testGroup "Good (should compile)" goodTests
          , testGroup "Bad (should fail)" badTests
          ]


findElm :: IO FilePath
findElm =
  do  result <- findExecutable "elm"
      case result of
        Just path -> return path
        Nothing   -> fail "Could not find 'elm' executable on PATH. Build it first with: cabal build elm"


getElmFiles :: FilePath -> IO [FilePath]
getElmFiles dir =
  do  entries <- listDirectory dir
      let elmFiles = filter (\f -> takeExtension f == ".elm") entries
      return (map (dir </>) elmFiles)


mkGoodTest :: FilePath -> FilePath -> FilePath -> IO TestTree
mkGoodTest elmExe projectDir path =
  return $ testCase path $
    do  (exitCode, _stdout, stderr) <-
          readProcessWithExitCode elmExe
            ["make", path, "--output=/dev/null"]
            ""
        case exitCode of
          ExitSuccess ->
            assertBool "" True

          ExitFailure _ ->
            assertFailure ("Compilation failed for: " ++ path ++ "\n" ++ stderr)


mkBadTest :: FilePath -> FilePath -> FilePath -> IO TestTree
mkBadTest elmExe projectDir path =
  return $ testCase path $
    do  (exitCode, _stdout, _stderr) <-
          readProcessWithExitCode elmExe
            ["make", path, "--output=/dev/null"]
            ""
        case exitCode of
          ExitSuccess ->
            assertFailure ("Compilation succeeded but should have failed for: " ++ path)

          ExitFailure _ ->
            assertBool "" True
```

Note: `elm make` must be run from the project directory (where `elm.json` lives). We pass absolute paths to the `.elm` files and set the working directory in the process call if needed. This will be refined during testing.

**Step 2: Commit**

```bash
git add tests/Test/Compile.hs
git commit -m "Add compile test module"
```

---

### Task 5: Create parse-good test files

**Files:**
- Create: `tests/parse-good/Literals.elm`
- Create: `tests/parse-good/Records.elm`
- Create: `tests/parse-good/PatternMatch.elm`
- Create: `tests/parse-good/Functions.elm`
- Create: `tests/parse-good/Types.elm`
- Create: `tests/parse-good/Comments.elm`

**Step 1: Create all parse-good test files**

`tests/parse-good/Literals.elm`:
```elm
module Literals exposing (..)

anInt = 42
aNegInt = -7
aHex = 0xFF
aFloat = 3.14
aNegFloat = -0.5
aChar = 'a'
aString = "hello"
aMultiline = """
  multi
  line
  string
"""
```

`tests/parse-good/Records.elm`:
```elm
module Records exposing (..)

point = { x = 1, y = 2 }

getX record = record.x

updateX record = { record | x = 10 }

type alias Point = { x : Int, y : Int }
```

`tests/parse-good/PatternMatch.elm`:
```elm
module PatternMatch exposing (..)

type Color = Red | Green | Blue

describe color =
  case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"

first tuple =
  case tuple of
    (a, _) -> a

headOf list =
  case list of
    [] -> Nothing
    x :: _ -> Just x
```

`tests/parse-good/Functions.elm`:
```elm
module Functions exposing (..)

identity x = x

add a b = a + b

apply f x = f x

withLet x =
  let
    y = x + 1
  in
  y * 2

lambda = \x -> x + 1

annotated : Int -> Int
annotated x = x + 1
```

`tests/parse-good/Types.elm`:
```elm
module Types exposing (..)

type alias Name = String

type Maybe a
  = Just a
  | Nothing

type Result error value
  = Ok value
  | Err error

type Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
```

`tests/parse-good/Comments.elm`:
```elm
module Comments exposing (..)

-- line comment

{- block comment -}

{- nested {- block -} comment -}

{-| doc comment -}
foo = 42
```

**Step 2: Commit**

```bash
git add tests/parse-good/
git commit -m "Add parse-good test files"
```

---

### Task 6: Create parse-bad test files

**Files:**
- Create: `tests/parse-bad/UnclosedString.elm`
- Create: `tests/parse-bad/BadIndentation.elm`
- Create: `tests/parse-bad/MissingModuleHeader.elm`
- Create: `tests/parse-bad/InvalidOperator.elm`

**Step 1: Create all parse-bad test files**

`tests/parse-bad/UnclosedString.elm`:
```elm
module UnclosedString exposing (..)

broken = "this string never ends
```

`tests/parse-bad/BadIndentation.elm`:
```elm
module BadIndentation exposing (..)

foo =
let
x = 1
in
x
```

`tests/parse-bad/MissingModuleHeader.elm` (parsed as a Package, not Application — packages require module headers):

NOTE: This file intentionally has NO module header. The test must parse it as `Parse.Package Pkg.core` (or any package) so the missing header is an error. Update `Test.Parse` to handle this special case, or rename to clarify. For simplicity, we'll use a different parse-bad file instead.

Actually, let's replace this with a simpler case. Since `Application` mode allows missing headers, we need files that genuinely fail to parse in Application mode.

`tests/parse-bad/GarbageTokens.elm`:
```elm
module GarbageTokens exposing (..)

foo = @#$%
```

`tests/parse-bad/InvalidOperator.elm`:
```elm
module InvalidOperator exposing (..)

(+++) a b = a
```

**Step 2: Commit**

```bash
git add tests/parse-bad/
git commit -m "Add parse-bad test files"
```

---

### Task 7: Create compile-good test project

**Files:**
- Create: `tests/compile-good/elm.json`
- Create: `tests/compile-good/src/BasicExpressions.elm`
- Create: `tests/compile-good/src/PatternMatching.elm`
- Create: `tests/compile-good/src/TypeAnnotations.elm`

**Step 1: Create the elm.json**

`tests/compile-good/elm.json`:
```json
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5"
        },
        "indirect": {
            "elm/json": "1.1.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
```

**Step 2: Create test elm files**

`tests/compile-good/src/BasicExpressions.elm`:
```elm
module BasicExpressions exposing (..)

add : Int -> Int -> Int
add a b = a + b

greet : String -> String
greet name = "Hello, " ++ name

isEven : Int -> Bool
isEven n = modBy 2 n == 0

double : Float -> Float
double x = x * 2.0
```

`tests/compile-good/src/PatternMatching.elm`:
```elm
module PatternMatching exposing (..)

type Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float Float

describe : Shape -> String
describe shape =
  case shape of
    Circle r ->
      "circle with radius " ++ String.fromFloat r

    Rectangle w h ->
      "rectangle " ++ String.fromFloat w ++ "x" ++ String.fromFloat h

    Triangle a b c ->
      "triangle"

safeHead : List a -> Maybe a
safeHead list =
  case list of
    [] -> Nothing
    x :: _ -> Just x
```

`tests/compile-good/src/TypeAnnotations.elm`:
```elm
module TypeAnnotations exposing (..)

identity : a -> a
identity x = x

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

type alias Point =
  { x : Float
  , y : Float
  }

origin : Point
origin = { x = 0, y = 0 }

moveX : Float -> Point -> Point
moveX dx point = { point | x = point.x + dx }
```

**Step 3: Install elm/core in the test project**

Run: `cd /Users/dokkora/fun/compiler/tests/compile-good && elm make --docs=/dev/null 2>&1 || true`

This will fetch elm/core and populate `elm-stuff/`. Add `elm-stuff/` to `.gitignore` in the test directory.

**Step 4: Commit**

```bash
echo "elm-stuff/" > tests/compile-good/.gitignore
git add tests/compile-good/
git commit -m "Add compile-good test project"
```

---

### Task 8: Create compile-bad test project

**Files:**
- Create: `tests/compile-bad/elm.json`
- Create: `tests/compile-bad/src/InfiniteType.elm`
- Create: `tests/compile-bad/src/IncompletePattern.elm`
- Create: `tests/compile-bad/src/TypeMismatch.elm`

**Step 1: Create the elm.json** (same as compile-good)

`tests/compile-bad/elm.json`:
```json
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5"
        },
        "indirect": {
            "elm/json": "1.1.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
```

**Step 2: Create test elm files**

`tests/compile-bad/src/InfiniteType.elm`:
```elm
module InfiniteType exposing (..)

f = f 42
```

`tests/compile-bad/src/IncompletePattern.elm`:
```elm
module IncompletePattern exposing (..)

type Fruit = Apple | Banana | Cherry

name : Fruit -> String
name fruit =
  case fruit of
    Apple -> "apple"
    Banana -> "banana"
```

`tests/compile-bad/src/TypeMismatch.elm`:
```elm
module TypeMismatch exposing (..)

bad : Int
bad = 1 + "hello"
```

**Step 3: Install deps and commit**

```bash
cd /Users/dokkora/fun/compiler/tests/compile-bad && elm make --docs=/dev/null 2>&1 || true
echo "elm-stuff/" > tests/compile-bad/.gitignore
git add tests/compile-bad/
git commit -m "Add compile-bad test project"
```

---

### Task 9: Run the parse tests and fix issues

**Step 1: Build the test suite**

Run: `cd /Users/dokkora/fun/compiler && cabal build elm-tests 2>&1`

Expect: Possible compilation errors from missing modules or dependency issues. Fix any cabal issues (missing deps, wrong module names).

**Step 2: Run just the parse tests**

Run: `cd /Users/dokkora/fun/compiler && cabal test elm-tests --test-option='-p "Parse Tests"' 2>&1`

Expect: All parse-good tests pass, all parse-bad tests pass. If any fail, inspect and fix the test file or test logic.

**Step 3: Commit fixes if any**

```bash
git add -A
git commit -m "Fix parse test issues"
```

---

### Task 10: Run the compile tests and fix issues

**Step 1: Make sure the elm binary is built and on PATH**

Run: `cd /Users/dokkora/fun/compiler && cabal build elm 2>&1`

Then: `export PATH=$(dirname $(cabal list-bin elm)):$PATH`

**Step 2: Run the compile tests**

Run: `cd /Users/dokkora/fun/compiler && cabal test elm-tests --test-option='-p "Compile Tests"' 2>&1`

Expect: All compile-good tests pass, all compile-bad tests pass. The `elm make` subprocess needs to run from the correct directory. If working directory is wrong, update `Test.Compile` to use `createProcess` with `cwd` instead of `readProcessWithExitCode`.

**Step 3: Commit fixes if any**

```bash
git add -A
git commit -m "Fix compile test issues"
```

---

### Task 11: Run the full test suite end-to-end

**Step 1: Run all tests**

Run: `cd /Users/dokkora/fun/compiler && cabal test elm-tests 2>&1`

Expect: All tests pass. Output should look like:

```
Elm Compiler Tests
  Parse Tests
    Good (should parse)
      tests/parse-good/Literals.elm:      OK
      tests/parse-good/Records.elm:       OK
      ...
    Bad (should fail)
      tests/parse-bad/UnclosedString.elm: OK
      ...
  Compile Tests
    Good (should compile)
      tests/compile-good/src/BasicExpressions.elm: OK
      ...
    Bad (should fail)
      tests/compile-bad/src/InfiniteType.elm: OK
      ...
```

**Step 2: Final commit**

```bash
git add -A
git commit -m "Test harness fully working"
```
