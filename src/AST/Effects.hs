{-# OPTIONS_GHC -Wall #-}
module AST.Effects
  ( Effects(..), Raw, Canonical
  , Info(..)
  , RawManagerType(..), ManagerType(..)
  , PortRaw(..), PortCanonical(..), Kind(..)
  )
  where

import Data.Binary
import Data.Text (Text)

import qualified AST.Type as Type
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EFFECTS


data Effects pkg ports
  = None
  | Manager pkg Info
  | Port ports


type Raw =
  Effects () [A.Commented PortRaw]


type Canonical =
  Effects Pkg.Name [A.Commented PortCanonical]



-- EFFECT MANAGERS


data Info =
  Info
    { _tag :: R.Region
    , _init :: R.Region
    , _onEffects :: R.Region
    , _onSelfMsg :: R.Region
    , _managerType :: RawManagerType
    }


data RawManagerType
  = CmdManager (A.Located Text)
  | SubManager (A.Located Text)
  | FxManager (A.Located Text) (A.Located Text)


data ManagerType = Cmds | Subs | Both



-- FOREIGN EFFECTS


data PortRaw =
  PortRaw
    { _rawName :: Text
    , _rawType :: Type.Raw
    }


data PortCanonical =
  PortCanonical
    { _name :: Text
    , _kind :: Kind
    , _type :: Type.Canonical
    }


data Kind
  = Outgoing Type.Canonical
  | Incoming Type.Canonical



-- BINARY


instance Binary ManagerType where
  put managerType =
    case managerType of
      Cmds -> putWord8 0
      Subs -> putWord8 1
      Both -> putWord8 2

  get =
    do  word <- getWord8
        case word of
          0 -> pure Cmds
          1 -> pure Subs
          2 -> pure Both
          _ -> error "problem getting Effects.ManagerType binary"
