module AST.Effects where

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
    , _managerType :: ManagerType
    }


data ManagerType
  = CmdManager (A.Located String)
  | SubManager (A.Located String)
  | FxManager (A.Located String) (A.Located String)



-- FOREIGN EFFECTS


data PortRaw =
  PortRaw
    { _rawName :: String
    , _rawType :: Type.Raw
    }


data PortCanonical =
  PortCanonical
    { _name :: String
    , _kind :: Kind
    , _type :: Type.Canonical
    }


data Kind
  = Outgoing Type.Canonical
  | Incoming Type.Canonical
