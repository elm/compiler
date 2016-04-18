{-# OPTIONS_GHC -W #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Compiler (compilerTests) where

import Control.Exception (try, IOException)

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Map as Map
import Data.Maybe (isJust)

import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), dropFileName, joinPath, splitDirectories)
import System.FilePath.Find (find, (==?), extension)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool, assertEqual)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Package
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Variable
import qualified AST.Type as Type


writeExpectedJsEnvVarName :: String
writeExpectedJsEnvVarName =
  "ELM_WRITE_NEW_EXPECTED"


compilerTests :: Test
compilerTests =
  buildTest $
    do  goods <- testIf isSuccess =<< getElms "good"
        bads  <- testIf isFailure =<< getElms "bad"
        expectedJsTests <- testMatchesExpected "good" "good-expected-js"
        return $
            testGroup "Compile Tests"
              [ testGroup "Good Tests" goods
              , testGroup "Bad Tests"  bads
              , testGroup "Expected JS Tests" expectedJsTests
              ]



-- GATHER FILES


getElms :: FilePath -> IO [FilePath]
getElms filePath =
    find
      (return True)
      (extension ==? ".elm")
      (testsDir </> filePath)


testsDir :: FilePath
testsDir =
    "tests" </> "test-files"

convertToExpectedJsFilePath :: String -> FilePath -> FilePath
convertToExpectedJsFilePath expectedJsDir filePath =
    testsDir
      </> expectedJsDir
      </> joinPath (drop 3 (splitDirectories filePath))
      <.> ".js"



-- TEST HELPERS


essentialInterfaces :: Module.Interfaces
essentialInterfaces =
  Map.fromList
    [
      (ModuleName.inCore ["Debug"],
        buildCoreInterface
          [
            ("crash", Type.Lambda (Type.Var "String") (Type.Var "a"))
          ]
      )
    ]


buildCoreInterface :: [(String, Type.Canonical)] -> Module.Interface
buildCoreInterface members =
  let
    exports = map (Variable.Value . fst) members
    imports = []
    types = Map.fromList members
    adts = Map.empty
    aliases = Map.empty
    fixities = []
  in
    Module.Interface Compiler.version Package.core exports imports types adts aliases fixities


compileString :: FilePath -> String -> Either String Compiler.Result
compileString filePath source =
  let dependentModuleNames =
        Map.keys essentialInterfaces
      context =
        Compiler.Context Package.core False dependentModuleNames
      (dealiaser, _warnings, result) =
        Compiler.compile context source essentialInterfaces
      formatErrors errors =
        concatMap (Compiler.errorToString dealiaser filePath source) errors
  in
      either (Left . formatErrors) Right result


readFileOrErrorStr :: FilePath -> IO String
readFileOrErrorStr filePath =
  do  strOrExc <- try $ readFile filePath
      case strOrExc of
        Right s ->
          return s

        Left (e :: IOException) ->
          return (show e)



-- RUN TESTS


testIf
    :: (Either String Compiler.Result -> Assertion)
    -> [FilePath]
    -> IO [Test]
testIf handleResult filePaths =
  let doTest filePath =
        do  source <- readFile filePath
            let formattedResult = compileString filePath source
            return $ testCase filePath (handleResult formattedResult)
  in
      traverse doTest filePaths


doMatchesExpectedTest :: String -> FilePath -> IO Test
doMatchesExpectedTest expectedJsDir filePath =
  do  source <- readFile filePath
      expectedJs <- readFileOrErrorStr (convertToExpectedJsFilePath expectedJsDir filePath)

      let formattedResult = compileString filePath source
      let assertion = matchesExpected expectedJs formattedResult

      return $ testCase filePath assertion


doWriteNewExpectedTest :: String -> FilePath -> IO Test
doWriteNewExpectedTest expectedJsDir filePath =
  do  source <- readFile filePath
      let formattedResult = compileString filePath source
      let expectedFilePath = convertToExpectedJsFilePath expectedJsDir filePath
      let assertion =
            case formattedResult of
                Right (Compiler.Result _ _ js) ->
                  do  createDirectoryIfMissing True (dropFileName expectedFilePath)
                      -- Force the evaluation of `js` before `writeFile`
                      --  so that if an error is raised we do not unintentionally write an empty file.
                      --  NB: `js` can be an empty string in the case of NoExpressions.elm
                      assertBool "" (Text.length js >= 0)
                      Text.writeFile expectedFilePath js
                      assertFailure ("Wrote new expected js: " ++ expectedFilePath)

                _ ->
                  assertFailure "Compile failed. Could not write new expected js."
      return $ testCase filePath assertion


testMatchesExpected :: String -> String -> IO [Test]
testMatchesExpected elmDir expectedJsDir =
  do  elmFiles <- getElms elmDir
      envVar <- lookupEnv writeExpectedJsEnvVarName
      if isJust envVar
        then
          traverse (doWriteNewExpectedTest expectedJsDir) elmFiles
        else
          traverse (doMatchesExpectedTest expectedJsDir) elmFiles



-- CHECK RESULTS


isSuccess :: Either String a -> Assertion
isSuccess result =
    case result of
      Right _ ->
          assertBool "" True

      Left errorMessages ->
          assertFailure errorMessages


isFailure :: Either a b -> Assertion
isFailure result =
    case result of
      Right _ ->
          assertFailure "Compilation succeeded but should have failed"

      Left _ ->
          assertBool "" True


matchesExpected :: String -> Either String Compiler.Result -> Assertion
matchesExpected expectedJs result =
    case result of
      Right (Compiler.Result _ _ js) ->
          assertEqual matchFailureMessage (Text.pack expectedJs) js

      Left errorMessages ->
          assertFailure $ "Compile failed:\n" ++ errorMessages


matchFailureMessage :: String
matchFailureMessage =
  "Compiled JS did not match expected JS." ++
  "\n  If the change is intentional, rerun " ++
  "the tests with environment variable: " ++ writeExpectedJsEnvVarName ++ "=1"
