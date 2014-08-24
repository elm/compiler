{-# OPTIONS_GHC -W #-}
module Tests.Compiler (compilerTests) where

import Data.Traversable (traverse)

import System.Exit
import System.FilePath ((</>))
import System.FilePath.Find (find, (==?), extension)
import System.Process (readProcessWithExitCode)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool)

compilerTests :: Test
compilerTests =
  buildTest $ do
    goods <- testIf isSuccess =<< getElms "good"
    bads  <- testIf isFailure =<< getElms "bad"
    return $ testGroup "Compile Tests"
             [ testGroup "Good Tests" goods
             , testGroup "Bad Tests"  bads
             ]

-- COLLECT FILES

getElms :: FilePath -> IO [FilePath]
getElms filePath =
    find (return True) (extension ==? ".elm") (testsDir </> filePath)

testsDir :: FilePath
testsDir = "tests" </> "compiler" </> "test-files"


-- RUN COMPILER

testIf :: (ExitCode -> String -> Assertion) -> [FilePath] -> IO [Test]
testIf handler filePaths =
    traverse setupTest filePaths
  where
    setupTest filePath = do
      let args = [ "--make", "--src-dir=" ++ testsDir, filePath ]
      (exitCode, stdout, stderr) <- readProcessWithExitCode compiler args ""
      return (testCase filePath (handler exitCode (stdout ++ stderr)))

compiler :: FilePath
compiler = "dist" </> "build" </> "elm" </> "elm"


-- CHECK RESULTS

isSuccess :: ExitCode -> String -> Assertion
isSuccess exitCode stderr =
    case exitCode of
      ExitSuccess -> assertBool "" True
      ExitFailure _ -> assertFailure stderr

isFailure :: ExitCode -> String -> Assertion
isFailure exitCode _stderr =
    case exitCode of
      ExitSuccess -> assertFailure "Compilation succeeded but should have failed"
      ExitFailure _ -> assertBool "" True
