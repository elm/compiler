module Test.Compile (tests) where

import System.Directory (listDirectory, findExecutable, makeAbsolute)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)


tests :: TestTree
tests =
  testGroup "Compile Tests"
    [ testGroup "Good (should compile)" (unsafeCompileTests "tests/compile-good" True)
    , testGroup "Bad (should fail)" (unsafeCompileTests "tests/compile-bad" False)
    ]


{-# NOINLINE unsafeCompileTests #-}
unsafeCompileTests :: FilePath -> Bool -> [TestTree]
unsafeCompileTests projectDir expectSuccess =
  unsafePerformIO $ do
    elmExe <- findElm
    absProjectDir <- makeAbsolute projectDir
    let srcDir = projectDir </> "src"
    entries <- listDirectory srcDir
    let elmFiles = [ e | e <- entries, takeExtension e == ".elm" ]
    return $ map (makeCompileTest elmExe absProjectDir expectSuccess) elmFiles


findElm :: IO FilePath
findElm =
  do  result <- findExecutable "elm"
      case result of
        Just path -> return path
        Nothing   -> fail "Could not find 'elm' on PATH. Build with: cabal build elm"


makeCompileTest :: FilePath -> FilePath -> Bool -> FilePath -> TestTree
makeCompileTest elmExe projectDir expectSuccess file =
  testCase file $ do
    let srcFile = "src" </> file
    let p = (proc elmExe ["make", srcFile, "--output=/dev/null"])
              { cwd = Just projectDir }
    (exitCode, _stdout, stderr) <- readCreateProcessWithExitCode p ""
    case (expectSuccess, exitCode) of
      (True, ExitSuccess)   -> return ()
      (True, ExitFailure _) ->
        assertFailure ("Compilation failed for: " ++ file ++ "\n" ++ stderr)
      (False, ExitFailure _) -> return ()
      (False, ExitSuccess)  ->
        assertFailure ("Compilation succeeded but should have failed for: " ++ file)
