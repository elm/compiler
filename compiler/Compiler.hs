{-# OPTIONS_GHC -W #-}
module Main where

import Control.Monad (foldM, when)
import qualified Data.Maybe as Maybe
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified System.Console.CmdArgs as CmdArgs
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath
import GHC.Conc

import Build.Dependencies (getBuildRecipe, Recipe(..))
import qualified Generate.Html as Html
import qualified Metadata.Prelude as Prelude
import qualified Build.Utils as Utils
import qualified Build.Flags as Flag
import qualified Build.File as File
import qualified Elm.Internal.Paths as Path

main :: IO ()
main = do setNumCapabilities =<< getNumProcessors
          compileArgs =<< CmdArgs.cmdArgs Flag.flags

compileArgs :: Flag.Flags -> IO ()
compileArgs flags =
  do when (Flag.get_runtime flags) $ do
       putStrLn Path.runtime
       exitSuccess
     case Flag.files flags of
       [] -> putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
       fs -> mapM_ (build flags) fs

build :: Flag.Flags -> FilePath -> IO ()
build flags rootFile =
    do let noPrelude = Flag.no_prelude flags
       builtIns <- Prelude.interfaces noPrelude

       (Recipe elmFiles jsFiles) <-
           if Flag.make flags
             then Utils.run (getBuildRecipe (Flag.src_dir flags) builtIns rootFile)
             else return (Recipe [rootFile] [])

       moduleName <- File.build flags builtIns elmFiles

       let elmos = map (Utils.elmo flags) elmFiles
       js <- foldM appendToOutput BS.empty (elmos ++ jsFiles)

       (extension, code) <-
           if Flag.only_js flags
           then do putStr "Generating JavaScript ... "
                   return ("js", js)
           else do putStr "Generating HTML ... "
                   return (makeHtml js moduleName)

       let targetFile = Utils.buildPath flags rootFile extension
       createDirectoryIfMissing True (takeDirectory targetFile)
       BS.writeFile targetFile code
       putStrLn "Done"

    where
      appendToOutput :: BS.ByteString -> FilePath -> IO BS.ByteString
      appendToOutput js filePath = do
        src <- BS.readFile filePath
        return (BS.append src js)

      sources js = map Html.Link (Flag.scripts flags) ++ [ Html.Source js ]

      makeHtml js moduleName = ("html", BS.pack $ renderHtml html)
          where
            rtsPath = Maybe.fromMaybe Path.runtime (Flag.set_runtime flags)
            html = Html.generate rtsPath (takeBaseName rootFile) (sources js) moduleName ""
