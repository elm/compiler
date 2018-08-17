{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Root
  ( get
  , unsafeGet
  , getWithReplFallback
  )
  where


import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Licenses as Licenses
import qualified Elm.Project.Summary as Summary
import qualified Elm.PerUserCache as PerUserCache
import qualified File.IO as IO
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff.Verify as Verify
import qualified Json.Encode as Encode



-- GET


get :: Task.Task Summary.Summary
get =
  do  root <- moveToRoot
      project <- Project.read "elm.json"
      Verify.verify root project


unsafeGet :: Task.Task (FilePath, Project.Project)
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
          Task.throw Exit.NoElmJson



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
              Encode.write "elm.json" (Project.encode (Project.Pkg replInfo))
              return root


replInfo :: Project.PkgInfo
replInfo =
  Project.PkgInfo
    { Project._pkg_name = Pkg.dummyName
    , Project._pkg_summary = "dummy code for the REPL"
    , Project._pkg_license = Licenses.bsd3
    , Project._pkg_version = Pkg.dummyVersion
    , Project._pkg_exposed = Project.ExposedList []
    , Project._pkg_deps = Map.singleton Pkg.core Con.anything
    , Project._pkg_test_deps = Map.empty
    , Project._pkg_elm_version = Con.defaultElm
    }
