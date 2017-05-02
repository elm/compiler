{-| This script builds any version of the Elm Platform from source.
Before you use it, make sure you have the Haskell Platform with a recent
version of cabal.

To install a released version of Elm, you will run something like this:

    runhaskell BuildFromSource.hs 0.18

Before you do that, in some directory of your choosing, add
wherever/Elm-Platform/0.18/.cabal-sandbox/bin to your PATH.

Then, run the above. You will now actually have a new directory for the
Elm Platform, like this:

    Elm-Platform/0.18/
        elm-make/        -- git repo for the build tool, ready to edit
        elm-repl/        -- git repo for the REPL, ready to edit
        ...
        .cabal-sandbox/  -- various build files

All of the executables you need are in .cabal-sandbox/bin, which is on
your PATH and thus can be used from anywhere.

You can build many versions of the Elm Platform, so it is possible to have
Elm-Platform/0.18/ and Elm-Platform/0.17/ with no problems. It is up to you
to manage your PATH variable or symlinks though.

To get set up with the master branch of all Elm Platform projects, run this:

    runhaskell BuildFromSource.hs master

From there you can start developing on any of the projects, switching branches
and testing interactions between projects.
-}
module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode, exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (rawSystem, readProcess)
import System.Info (compilerVersion)
import qualified Data.Version as Vsn
import System.Directory  (findExecutable)
import Text.ParserCombinators.ReadP (readP_to_S)


(=:) = (,)


data Config =
  Config
    { elmVersion :: String
    , ghcVersion :: [Int]
    , dependencies :: [(String, String)]
    }


configs :: Map.Map String Config
configs =
  Map.fromList $ map (\config -> (elmVersion config, config)) $
    [
      Config "master" [7,10] $
        [ "elm-compiler" =: "mod"
        , "elm-package"  =: "master"
        , "elm-make"     =: "master"
        , "elm-reactor"  =: "master"
        , "elm-repl"     =: "master"
        ]
    ,
      Config "0.18" [7,10] $
        [ "elm-compiler" =: "mod"
        , "elm-package"  =: "0.18.0"
        , "elm-make"     =: "0.18.0"
        , "elm-reactor"  =: "0.18.0"
        , "elm-repl"     =: "0.18.0"
        ]
    ,
      Config "0.17.1" [7,10] $
        [ "elm-compiler" =: "0.17.1"
        , "elm-package"  =: "0.17.1"
        , "elm-make"     =: "0.17.1"
        , "elm-reactor"  =: "0.17.1"
        , "elm-repl"     =: "0.17.1"
        ]
    ,
      Config "0.17" [7,10] $
        [ "elm-compiler" =: "0.17"
        , "elm-package"  =: "0.17"
        , "elm-make"     =: "0.17"
        , "elm-reactor"  =: "0.17"
        , "elm-repl"     =: "0.17"
        ]
    ,
      Config "0.16" [7,10] $
        [ "elm-compiler" =: "0.16"
        , "elm-package"  =: "0.16"
        , "elm-make"     =: "0.16"
        , "elm-reactor"  =: "0.16"
        , "elm-repl"     =: "0.16"
        ]
    ,
      Config "0.15.1" [7,8] $
        [ "elm-compiler" =: "0.15.1"
        , "elm-package"  =: "0.5.1"
        , "elm-make"     =: "0.2"
        , "elm-reactor"  =: "0.3.2"
        , "elm-repl"     =: "0.4.2"
        ]
    ,
      Config "0.15" [7,8] $
        [ "elm-compiler" =: "0.15"
        , "elm-package"  =: "0.5"
        , "elm-make"     =: "0.1.2"
        , "elm-reactor"  =: "0.3.1"
        , "elm-repl"     =: "0.4.1"
        ]
    ,
      Config "0.14.1" [7,6] $
        [ "elm-compiler" =: "0.14.1"
        , "elm-package"  =: "0.4"
        , "elm-make"     =: "0.1.1"
        , "elm-reactor"  =: "0.3"
        , "elm-repl"     =: "0.4"
        ]
    ,
      Config "0.14" [7,6] $
        [ "elm-compiler" =: "0.14"
        , "elm-package"  =: "0.2"
        , "elm-make"     =: "0.1"
        , "elm-reactor"  =: "0.2"
        , "elm-repl"     =: "0.4"
        ]
    ,
      Config "0.13" [7,6] $
        [ "Elm"         =: "0.13"
        , "elm-reactor" =: "0.1"
        , "elm-repl"    =: "0.3"
        , "elm-get"     =: "0.1.3"
        ]
    ,
      Config "0.12.3" [7,6] $
        [ "Elm"        =: "0.12.3"
        , "elm-server" =: "0.11.0.1"
        , "elm-repl"   =: "0.2.2.1"
        , "elm-get"    =: "0.1.2"
        ]
    ]


main :: IO ()
main =
  do  args <- getArgs
      case args of
        [version] ->
          case Map.lookup version configs of
            Nothing ->
              failure $
                "You want me to build `" ++ version ++ "` but the only options are:\n    "
                ++ List.intercalate ", " (Map.keys configs)

            Just (Config _ expectedGhcVersion dependencies) ->
              do  checkGhc expectedGhcVersion
                  checkCabal
                  makeRepos ("Elm-Platform" </> version) version dependencies

        _ ->
          failure $
            "Expecting one of the following values as the only argument:\n    "
            ++ List.intercalate ", " (Map.keys configs)


failure :: String -> IO ()
failure msg =
  do  hPutStrLn stderr msg
      exitFailure


makeRepos :: FilePath -> String -> [(String, String)] -> IO ()
makeRepos artifactDirectory version repos =
  do  createDirectoryIfMissing True artifactDirectory
      setCurrentDirectory artifactDirectory
      writeFile "cabal.config" "split-objs: True"
      root <- getCurrentDirectory
      mapM_ (uncurry (makeRepo root)) repos

      cabal [ "update" ]

      -- create a sandbox for installation
      cabal [ "sandbox", "init" ]

      -- add each of the sub-directories as a sandbox source
      cabal ([ "sandbox", "add-source" ] ++ map fst repos)

      -- install all of the packages together in order to resolve transitive dependencies robustly
      -- (install the dependencies a bit more quietly than the elm packages)
      cabal ([ "install", "-j", "--only-dependencies", "--ghc-options=\"-w\"" ]
             ++ (if version <= "0.15.1" then [ "--constraint=fsnotify<0.2" ] else [])
             ++ map fst repos)
      cabal ([ "install", "-j" ]
             ++ (if version <= "0.15.1" then [ "--ghc-options=\"-XFlexibleContexts\"" ] else [])
             ++ filter (/= "elm-reactor") (map fst repos))

      -- elm-reactor needs to be installed last because of a post-build dependency on elm-make
      cabal [ "install", "-j", "elm-reactor" ]

      return ()


makeRepo :: FilePath -> String -> String -> IO ()
makeRepo root projectName version =
  let
    user =
      if projectName == "elm-compiler" then
        "elm-lang/"
      else
        "xarvh/"

  in
  do  -- get the right version of the repo
      git [ "clone", "https://github.com/" ++ user ++ projectName ++ ".git" ]
      setCurrentDirectory projectName
      git [ "checkout", version, "--quiet" ]

      -- move back into the root
      setCurrentDirectory root



-- CHECK HASKELL VERSIONS


checkGhc :: [Int] -> IO ()
checkGhc expected =
  if take 2 expected == take 2 (Vsn.versionBranch compilerVersion) then
    putStrLn $ "Using GHC " ++ Vsn.showVersion compilerVersion

  else
    failure $
      "You need GHC " ++ List.intercalate "." (map show expected)
      ++ " to build this version of Elm.\nYou are using GHC "
      ++ Vsn.showVersion compilerVersion ++ " instead."


checkCabal :: IO ()
checkCabal =
  do  path <- findExecutable "cabal"
      case path of
        Nothing ->
          failure $ "It looks like cabal is not installed. You need at least version 1.18 or higher."

        Just path ->
          do  versionString <- readProcess path [ "--numeric-version" ] ""
              let version = fst $ last $ readP_to_S Vsn.parseVersion versionString
              if [1,18] <= Vsn.versionBranch version
                then
                  putStrLn $ "Using Cabal " ++ versionString
                else
                  failure $ "You need cabal 1.18 or higher to build Elm. You are using cabal " ++ versionString



-- HELPER FUNCTIONS


cabal :: [String] -> IO ExitCode
cabal =
  rawSystem "cabal"


git :: [String] -> IO ExitCode
git =
  rawSystem "git"
