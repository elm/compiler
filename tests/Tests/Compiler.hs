module Tests.Compiler (compilerTests)
       where

import Data.Functor                   ((<$>))
import Data.Traversable               (traverse)
import System.FilePath.Find           (find, (==?), extension)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     ((@=?), Assertion)

import Elm.Internal.Utils as Elm

compilerTests :: Test
compilerTests = buildTest $ do
  goods <- getElms "tests/data/good" >>= mkTests True
  bads  <- getElms "tests/data/bad"  >>= mkTests False

  return $ testGroup "Compile Tests"
    [
      testGroup "Good Tests" goods
    , testGroup "Bad Tests"  bads
    ]

  where getElms :: FilePath -> IO [FilePath]
        getElms = find (return True) (extension ==? ".elm")

        mkTests :: Bool -> [FilePath] -> IO [Test]
        mkTests b = traverse setupTest
          where setupTest f = testCase f . mkCompileTest b <$> readFile f

mkCompileTest :: Bool      -- ^ Expect success?
                 -> String -- ^ File Contents
                 -> Assertion
mkCompileTest succ modul = noCompileErr @=? succ
  where noCompileErr = either (const False) (const True) . Elm.compile $ modul
        expectation = "Compile " ++ if succ then "Success" else "Error"
