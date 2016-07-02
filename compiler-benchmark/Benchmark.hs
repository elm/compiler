module Main where

import Control.Monad (unless, forM_, filterM, void, when)
import System.Directory (doesDirectoryExist, copyFile, setCurrentDirectory, getCurrentDirectory, createDirectoryIfMissing, getDirectoryContents, removeDirectoryRecursive)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Process (rawSystem)

data Repo = Repo 
    { projectName :: String
    , path :: FilePath
    , branch :: String
    }

-- Version of the Elm compiler that is being used.
data Version
    = Gold
    | Test
    deriving Show

main :: IO ()
main = do
    root <- (</> "compiler-benchmark") <$> getCurrentDirectory
    inside root $ do
        -- Elm projects
        createDirectoryIfMissing True "benchmark-projects"
        inside "benchmark-projects" $ do
            todoMvcExists <- doesDirectoryExist "elm-todomvc"
            unless todoMvcExists . void $
                git ["clone", "https://github.com/evancz/elm-todomvc", "elm-todomvc"]
            
        -- Source 
        createDirectoryIfMissing True "benchmark-src"

        -- elm-compiler
        makeSrcRepo "elm-compiler" 
        inside "benchmark-src/elm-compiler" $ do
            git [ "checkout", "0.17", "--quiet" ]

        -- elm-package
        makeSrcRepo "elm-package" 
        inside "benchmark-src/elm-package" $ do
            git [ "checkout", "0.17", "--quiet" ]

        -- elm-make
        makeSrcRepo "elm-make" 
        inside "benchmark-src/elm-make" $ do
            git [ "checkout", "0.17", "--quiet" ]
            cabal ["sandbox", "init"]
            cabal ["sandbox", "add-source", root </> "benchmark-src" </> "elm-compiler"]
            cabal ["sandbox", "add-source", root </> "benchmark-src" </> "elm-package"]
            cabal ["install"]

        createDirectoryIfMissing True "benchmark-results"

        -- Compile each project with the golden versions of elm-compiler, elm-make, etc.
        projects <- listDirectory "benchmark-projects"
        forM_ projects (compile Gold)

        -- Use the development version of elm-compiler. By default, use the "master"
        -- branch of the repo in which benchmarking is being run.
        inside (root </> "benchmark-src/elm-compiler") $ do
            hasDevBranch <- successful <$> git ["remote", "show", "dev"]
            unless hasDevBranch (void $ git ["remote", "add", "dev", root </> ".."])
            git ["pull", "dev", "master"]

        forM_ projects (compile Test)

        reportResults

-- Compile an Elm project using the Elm compiler.
compile :: Version -> FilePath -> IO ()
compile version project = do
    root <- getCurrentDirectory
    setCurrentDirectory ("benchmark-projects" </> project) 
    
    -- Remove the build artifacts so that the full project is built each
    -- time we run benchmarking. This ensures that the results of subsequent
    -- benchmarking runs are comparable.
    elmStuffExists <- doesDirectoryExist "elm-stuff/build-artifacts"
    when elmStuffExists (removeDirectoryRecursive "elm-stuff/build-artifacts")

    -- The -p flag turns profiling on.
    let elmMake = rawSystem (root </> "benchmark-src/elm-make/.cabal-sandbox/bin/elm-make")
    elmMake ["+RTS", "-p"] 

    -- Profiling should produce a file called elm-make.prof.
    copyFile "elm-make.prof" (root </> "benchmark-results" </> (project ++ "." ++ show version ++ ".prof"))

    -- reset
    setCurrentDirectory root

reportResults :: IO ()
reportResults = putStrLn "results"

-- Fetch and prepare a repository containing some component of the Elm Platform.
makeSrcRepo :: String -> IO ()
makeSrcRepo projectName = do
    root <- getCurrentDirectory

    projectExists <- doesDirectoryExist $ "benchmark-src" </> projectName
    unless projectExists $ do
        git [ "clone", "https://github.com/elm-lang/" ++ projectName ++ ".git", "benchmark-src" </> projectName]
        inside ("benchmark-src" </> projectName) $ do
            copyFile (root </> "cabal.config.example") "cabal.config"

-- HELPER FUNCTIONS

git :: [String] -> IO ExitCode
git = rawSystem "git"

cabal :: [String] -> IO ExitCode
cabal = rawSystem "cabal" 

successful :: ExitCode -> Bool
successful = (== ExitSuccess)

listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = filter (\d -> d /= "." && d /= "..")
    <$> getDirectoryContents dir

-- inside changes into the specified directory, performs some IO action,
-- and then steps back out into the original directory once the action has been
-- performed.
inside :: FilePath -> IO a -> IO () 
inside dir action = do
    root <- getCurrentDirectory
    setCurrentDirectory dir >> action >> setCurrentDirectory root

