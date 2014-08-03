{-# OPTIONS_GHC -Wall #-}
module Build.Metadata where

import qualified Data.Maybe as Maybe
import qualified AST.Module as Module
import qualified Elm.Internal.Name as N


---- METADATA ----

type Located = Metadata ()
type WithDeps = Metadata [Located]

data Metadata deps = Metadata
    { _path :: FilePath
    , _name :: Module.Name
    , _pkg :: Maybe N.Name
    , _deps :: deps
    } deriving (Show, Eq, Ord)

addDeps :: [Located] -> Located -> WithDeps
addDeps deps (Metadata path name pkg ()) =
    Metadata path name pkg deps

moduleName :: Metadata deps -> String
moduleName metadata =
    Module.nameToString (_name metadata)

---- ROOTS ----

{-| Locations that need to be searched when finding modules. -}
data Root
    = SrcDir FilePath
    | Package N.Name FilePath
    deriving (Show, Eq, Ord)

rootDirectory :: Root -> FilePath
rootDirectory root =
    case root of
      SrcDir path -> path
      Package _ path -> path

packageName :: Root -> Maybe N.Name
packageName root =
    case root of
      Package name _ -> Just name
      SrcDir _ -> Nothing

-- For error reporting

packages :: [Root] -> [N.Name]
packages roots =
    Maybe.mapMaybe packageName roots

srcDirs :: [Root] -> [FilePath]
srcDirs roots =
    Maybe.mapMaybe srcDir roots
  where
    srcDir root =
        case root of
          SrcDir path -> Just path
          _ -> Nothing
