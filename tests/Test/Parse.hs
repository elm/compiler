{-# LANGUAGE OverloadedStrings #-}
module Test.Parse (tests) where

import qualified Data.ByteString as BS
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Parse.Module as Parse


tests :: TestTree
tests =
  testGroup "Parse Tests"
    [ testGroup "parse-good (should succeed)" (unsafeTests "tests/parse-good" True)
    , testGroup "parse-bad (should fail)" (unsafeTests "tests/parse-bad" False)
    ]


-- Build a list of test cases from .elm files in a directory.
-- 'expectSuccess' = True means we expect parsing to succeed.
{-# NOINLINE unsafeTests #-}
unsafeTests :: FilePath -> Bool -> [TestTree]
unsafeTests dir expectSuccess =
  unsafePerformIO $ do
    entries <- listDirectory dir
    let elmFiles = [ e | e <- entries, takeExtension e == ".elm" ]
    return $ map (makeTest dir expectSuccess) elmFiles


makeTest :: FilePath -> Bool -> FilePath -> TestTree
makeTest dir expectSuccess file =
  testCase file $ do
    src <- BS.readFile (dir </> file)
    let result = Parse.fromByteString Parse.Application src
    case (expectSuccess, result) of
      (True, Right _)  -> return ()
      (True, Left _)   -> assertBool (file ++ " should parse but failed") False
      (False, Left _)  -> return ()
      (False, Right _) -> assertBool (file ++ " should fail to parse but succeeded") False
