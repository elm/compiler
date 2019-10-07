{-# LANGUAGE OverloadedStrings #-}
module Develop.StaticFiles.Build
  ( readAsset
  , buildReactorFrontEnd
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.NonEmptyList as NE
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified BackgroundWriter as BW
import qualified Build
import qualified Elm.Details as Details
import qualified Generate
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task



-- ASSETS


readAsset :: FilePath -> IO BS.ByteString
readAsset path =
  BS.readFile ("reactor" </> "assets" </> path)



-- BUILD REACTOR ELM


buildReactorFrontEnd :: IO BS.ByteString
buildReactorFrontEnd =
  BW.withScope $ \scope ->
  Dir.withCurrentDirectory "reactor" $
  do  root <- Dir.getCurrentDirectory
      runTaskUnsafe $
        do  details    <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts  <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths
            javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.prod root details artifacts
            return (LBS.toStrict (B.toLazyByteString javascript))


paths :: NE.List FilePath
paths =
  NE.List
    ("src" </> "NotFound.elm")
    [ "src" </> "Errors.elm"
    , "src" </> "Index.elm"
    ]


runTaskUnsafe :: Task.Task Exit.Reactor a -> IO a
runTaskUnsafe task =
  do  result <- Task.run task
      case result of
        Right a ->
          return a

        Left exit ->
          do  Exit.toStderr (Exit.reactorToReport exit)
              error
                "\n--------------------------------------------------------\
                \\nError in Develop.StaticFiles.Build.buildReactorFrontEnd\
                \\nCompile with `elm make` directly to figure it out faster\
                \\n--------------------------------------------------------\
                \\n"
