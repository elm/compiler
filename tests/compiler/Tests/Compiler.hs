module Tests.Compiler (compilerTests)
       where

import Data.Functor                   ((<$>))
import Data.Traversable               (traverse)
import System.FilePath                ((</>))
import System.FilePath.Find           (find, (==?), extension)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (Assertion, assertFailure, assertBool)
import Text.Parsec                    (ParseError)

import Elm.Internal.Utils as Elm

compilerTests :: Test
compilerTests = buildTest $ do
  goods <- mkTests goodCompile  =<< getElms "good"
  bads  <- mkTests badCompile   =<< getElms "bad"
  return $ testGroup "Compile Tests"
    [
      testGroup "Good Tests" goods
    , testGroup "Bad Tests"  bads
    ]

  where getElms :: FilePath -> IO [FilePath]
        getElms fname = find (return True) (extension ==? ".elm") (testsDir </> fname)

        mkTests :: (Either String String -> Assertion) -> [FilePath] -> IO [Test]
        mkTests h = traverse setupTest
          where setupTest f = testCase f . mkCompileTest h <$> readFile f

        testsDir = "tests" </> "test-files"

goodCompile :: Either String String -> Assertion
goodCompile (Left err) = assertFailure err
goodCompile (Right _)  = assertBool "" True

badCompile :: Either String String -> Assertion
badCompile (Left _)  = assertBool "" True
badCompile (Right _) = assertFailure "Compilation succeeded but should have failed"
  
mkCompileTest :: ((Either String String) -> Assertion) -- ^ Handler
                 -> String -- ^ File Contents
                 -> Assertion
mkCompileTest handle = handle . Elm.compile
