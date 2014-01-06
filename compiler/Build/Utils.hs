{-# OPTIONS_GHC -W #-}
module Build.Utils where

import System.FilePath ((</>), replaceExtension)
import qualified Build.Flags as Flag

buildPath :: Flag.Flags -> FilePath -> String -> FilePath
buildPath flags filePath ext =
    Flag.build_dir flags </> replaceExtension filePath ext

cachePath :: Flag.Flags -> FilePath -> String -> FilePath
cachePath flags filePath ext =
    Flag.cache_dir flags </> replaceExtension filePath ext

elmo :: Flag.Flags -> FilePath -> FilePath
elmo flags filePath =
    cachePath flags filePath "elmo"

elmi :: Flag.Flags -> FilePath -> FilePath
elmi flags filePath =
    cachePath flags filePath "elmi"
