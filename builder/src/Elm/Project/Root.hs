{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Root
  ( get
  , unsafeGet
  , getWithReplFallback
  )
  where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Network.HTTP.Client as Client
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Compiler.Version as Elm
import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Licenses as Licenses
import Elm.Project.Json (Project(..), PkgInfo(..))
import Elm.Project.Summary (Summary)
import qualified Elm.PerUserCache as PerUserCache
import qualified File.IO as IO
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Task.Http as Http
import qualified Stuff.Verify as Verify
import qualified Json.Encode as Encode



-- GET


get :: Task.Task Summary
get =
  do  root <- moveToRoot
      project <- Project.read "elm.json"
      Verify.verify root project


unsafeGet :: Task.Task (FilePath, Project)
unsafeGet =
  (,) <$> moveToRoot <*> Project.read "elm.json"



-- MOVE TO ROOT


moveToRoot :: Task.Task FilePath
moveToRoot =
  do  maybeRoot <- liftIO $ IO.find "elm.json"
      case maybeRoot of
        Just root ->
          do  liftIO $ Dir.setCurrentDirectory root
              return root

        Nothing ->
          do  approved <- Task.getApproval $ D.vcat $
                [ "It looks like you are starting a new Elm project. Very exciting!"
                , ""
                , "I would like to create two files to help you get started:"
                , ""
                , "  1. elm.json" <> D.black " ------------ describes your dependencies"
                , "  2. src/Main.elm" <> D.black " ------ a basic Elm program to expand"
                , ""
                , "This is common to all Elm projects. Would you like me to create them? [Y/n]: "
                ]

              if approved
                then
                  do  liftIO $ Dir.createDirectoryIfMissing True "src"
                      fetch "elm.json"
                      fetch "src/Main.elm"
                      Task.report Progress.ElmJsonApproved
                      liftIO $ Dir.getCurrentDirectory
                else
                  do  Task.report Progress.ElmJsonRejected
                      Task.throw Exit.NoElmJson


fetch :: FilePath -> Task.Task ()
fetch path =
  Http.run $
    Http.anything
    ("https://experiment.elm-lang.org/" ++ Pkg.versionToString Elm.version ++ "/starter/" ++ path)
    (\request manager ->
        do  response <- Client.httpLbs request manager
            Right <$> LBS.writeFile path (Client.responseBody response)
    )



-- GET WITH FALLBACK


getWithReplFallback :: IO FilePath
getWithReplFallback =
  do  maybeRoot <- IO.find "elm.json"

      case maybeRoot of
        Just root ->
          do  Dir.setCurrentDirectory root
              return root

        Nothing ->
          do  cache <- PerUserCache.getReplRoot
              let root = cache </> "tmp"
              Dir.createDirectoryIfMissing True root
              Dir.setCurrentDirectory root
              IO.removeDir "elm-stuff"
              Encode.write "elm.json" (Project.encode (Pkg replInfo))
              return root


replInfo :: PkgInfo
replInfo =
  PkgInfo
    { _pkg_name = Pkg.dummyName
    , _pkg_summary = "dummy code for the REPL"
    , _pkg_license = Licenses.bsd3
    , _pkg_version = Pkg.dummyVersion
    , _pkg_exposed = Project.ExposedList []
    , _pkg_deps = Map.singleton Pkg.core Con.anything
    , _pkg_test_deps = Map.empty
    , _pkg_elm_version = Con.defaultElm
    }
