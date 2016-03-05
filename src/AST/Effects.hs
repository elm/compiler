module AST.Effects where

import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



data Effects
  = None
  | Effect Info
  | Foreign


data Info =
  Info
    { _tag :: R.Region
    , _init :: R.Region
    , _onEffects :: R.Region
    , _onSelfMsg :: R.Region
    , _type :: ManagerType
    }


data ManagerType
  = CmdManager (A.Located String)
  | SubManager (A.Located String)
  | FxManager (A.Located String) (A.Located String)