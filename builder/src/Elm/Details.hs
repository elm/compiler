{-# OPTIONS_GHC -Wall #-}
module Elm.Details
  ( Details(..)
  , ValidOutline(..)
  , Local(..)
  , Foreign(..)
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import qualified Deps.Registry as Registry
import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified File
import qualified Http
import qualified Json.Decode as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff



-- DETAILS


data Details =
  Details
    { _outlineTime :: File.Time
    , _outline :: ValidOutline
    , _locals :: Map.Map ModuleName.Raw Local
    , _foreigns :: Map.Map ModuleName.Raw Foreign
    }


data ValidOutline
  = ValidApp Outline.AppOutline
  | ValidPkg Outline.PkgOutline (Map.Map Pkg.Name V.Version)


data Local =
  Local
    { _path :: FilePath
    , _time :: File.Time
    , _deps :: [ModuleName.Raw]
    }


data Foreign =
  Foreign Pkg.Name [Pkg.Name]



-- BINARY


instance Binary Details where
  get = liftM4 Details get get get get
  put (Details a b c d) = put a >> put b >> put c >> put d


instance Binary ValidOutline where
  put outline =
    case outline of
      ValidApp a   -> putWord8 0 >> put a
      ValidPkg a b -> putWord8 1 >> put a >> put b

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  ValidApp get
          1 -> liftM2 ValidPkg get get
          _ -> error "binary encoding of ValidOutline was corrupted"


instance Binary Local where
  get = liftM3 Local get get get
  put (Local a b c) = put a >> put b >> put c


instance Binary Foreign where
  get = liftM2 Foreign get get
  put (Foreign a b) = put a >> put b


instance Binary Artifacts where
  get = liftM3 Artifacts get get get
  put (Artifacts a b c) = put a >> put b >> put c
