{-# LANGUAGE DeriveDataTypeable #-}
module Build.Flags where

import qualified Elm.Internal.Version as Version
import System.Console.CmdArgs

data Flags = Flags
    { make :: Bool
    , files :: [FilePath]
    , runtime :: Maybe FilePath
    , only_js :: Bool
    , print_types :: Bool
    , scripts :: [FilePath]
    , no_prelude :: Bool
    , cache_dir :: FilePath
    , build_dir :: FilePath
    , src_dir :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)
             
flags = Flags
  { files = def &= args &= typ "FILES"
  , make = False
           &= help "automatically compile dependencies."
  , only_js = False
              &= help "Compile only to JavaScript."
  , no_prelude = False
                 &= help "Do not import Prelude by default, used only when compiling standard libraries."
  , scripts = [] &= typFile
              &= help "Load JavaScript files in generated HTML. Files will be included in the given order."
  , runtime = Nothing &= typFile
              &= help "Specify a custom location for Elm's runtime system."
  , cache_dir = "cache" &= typFile
                &= help "Directory for files cached to make builds faster. Defaults to cache/ directory."
  , build_dir = "build" &= typFile
                &= help "Directory for generated HTML and JS files. Defaults to build/ directory."
  , src_dir = ["."] &= typFile
              &= help "Additional source directories besides project root. Searched when using --make"
  , print_types = False
                  &= help "Print out infered types of top-level definitions."
  } &= help "Compile Elm programs to HTML, CSS, and JavaScript."
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary (show Version.elmVersion)]
    &= summary ("The Elm Compiler " ++ show Version.elmVersion ++ ", (c) Evan Czaplicki 2011-2014")
