{-# OPTIONS_GHC -W #-}
module Test.Compiler (compilerTests) where

import qualified Data.Map as Map
import Data.Traversable (traverse)
import Data.Bifunctor (first, second)

import System.FilePath ((</>))
import System.FilePath.Find (find, (==?), extension)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module

type ErrorMessage = String

compilerTests :: Test
compilerTests =
  buildTest $ do
    goods <- testIf isSuccess =<< getElms "good"
    bads  <- testIf isFailure =<< getElms "bad"
    return $
        testGroup "Compile Tests"
          [ testGroup "Good Tests" goods
          , testGroup "Bad Tests"  bads
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

testIf
    :: (([Compiler.Warning], Either [ErrorMessage] (Module.Interface, String)) -> Assertion)
    -> [FilePath]
    -> IO [Test]
testIf handleResult filePaths =
    traverse setupTest filePaths
  where
    setupTest filePath = do  
      source <- readFile filePath
      
      let result = Compiler.compile "elm-lang" "core" True source Map.empty
      let errorsToStrings = second (first $ map (Compiler.errorToString filePath source))

      return $ testCase filePath (handleResult .  errorsToStrings $ result)

-- CHECK RESULTS

isSuccess :: ([Compiler.Warning], Either [ErrorMessage] a) -> Assertion
isSuccess (_, result) =
    case result of
      Right _     -> assertBool "" True
      Left errorMessages -> assertFailure $ concat errorMessages


isFailure :: ([Compiler.Warning], Either a b) -> Assertion
isFailure (_, result) =
    case result of
      Right _ -> assertFailure "Compilation succeeded but should have failed"
      Left _  -> assertBool "" True
