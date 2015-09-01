{-# OPTIONS_GHC -W #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Compiler (compilerTests) where

import Control.Exception (try, IOException)

import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Traversable (traverse)

import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), dropFileName, joinPath, splitDirectories)
import System.FilePath.Find (find, (==?), extension)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool, assertEqual)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Package

writeExpectedJsEnvVarName :: String
writeExpectedJsEnvVarName =
  "ELM_WRITE_NEW_EXPECTED"


compilerTests :: Test
compilerTests =
  buildTest $ do
    goods <- testIf isSuccess =<< getElms "good"
    bads  <- testIf isFailure =<< getElms "bad"
    expectedJsTests <- testMatchesExpected "good" "good-expected-js"
    return $
        testGroup "Compile Tests"
          [ testGroup "Good Tests" goods
          , testGroup "Bad Tests"  bads
          , testGroup "Expected JS Tests" expectedJsTests
          ]


-- GATHER ELM FILES

getElms :: FilePath -> IO [FilePath]
getElms filePath =
    find
      (return True)
      (extension ==? ".elm")
      (testsDir </> filePath)


testsDir :: FilePath
testsDir =
    "tests" </> "test-files"


-- RUN COMPILER

compileString :: FilePath -> String -> Either String Compiler.Result
compileString filePath source =
  let context =
        Compiler.Context Package.coreName True False []
      (dealiaser, _warnings, result) =
        Compiler.compile context source Map.empty
      formatErrors errors =
        concatMap (Compiler.errorToString dealiaser filePath source) errors
  in
      either (Left . formatErrors) Right result


testIf
    :: (Either String Compiler.Result -> Assertion)
    -> [FilePath]
    -> IO [Test]
testIf handleResult filePaths =
    traverse setupTest filePaths
  where
    setupTest filePath = do
      source <- readFile filePath
      let formattedResult = compileString filePath source

      return $ testCase filePath (handleResult formattedResult)


testMatchesExpected :: String -> String -> IO [Test]
testMatchesExpected elmDir expectedJsDir = do
  elmFiles <- getElms elmDir
  envVar <- lookupEnv writeExpectedJsEnvVarName
  if isJust envVar
    then
      traverse writeNewExpected elmFiles
    else
      traverse doTest elmFiles
  where
    getExpectedFilePath filePath =
      testsDir
      </> expectedJsDir
      </> joinPath (drop 3 (splitDirectories filePath))
      <.> ".js"

    readFileOrErrorStr filePath = do
      strOrExc <- try $ readFile filePath
      case strOrExc of
        Right s ->
          return s

        Left (e :: IOException) ->
          return (show e)

    doTest filePath = do
      source <- readFile filePath
      expectedJs <- readFileOrErrorStr (getExpectedFilePath filePath)

      let formattedResult = compileString filePath source
          assertion = matchesExpected expectedJs formattedResult

      return $ testCase filePath assertion

    writeNewExpected filePath = do
      source <- readFile filePath
      let formattedResult = compileString filePath source
          expectedFilePath = getExpectedFilePath filePath
          assertion =
            case formattedResult of
                Right (Compiler.Result _ _ js) -> do
                  createDirectoryIfMissing True (dropFileName expectedFilePath)
                  writeFile expectedFilePath js
                  assertFailure ("Wrote new expected js: " ++ expectedFilePath)

                _ ->
                  assertFailure "Compile failed. Could not write new expected js."
      return $ testCase filePath assertion


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
          assertEqual matchFailureMessage expectedJs js

      Left errorMessages ->
          assertFailure $ "Compile failed:\n" ++ errorMessages


matchFailureMessage :: String
matchFailureMessage =
  "Compiled JS did not match expected JS." ++
  "\n  If the change is intentional, rerun " ++
  "the tests with environment variable: " ++ writeExpectedJsEnvVarName ++ "=1"
