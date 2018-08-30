module Terminal.Env (configure) where


import Control.Monad (when)

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified System.Environment as Env



-- CONFIGURE


configure :: IO ()
configure =
  do configureTerminfo



-- TERMINFO


configureTerminfo :: IO ()
configureTerminfo =
  let
    variable = "TERMINFO_DIRS"
  in
  do terminfoDirs <- Env.lookupEnv variable
     when (Maybe.isNothing terminfoDirs) $
       Env.setEnv variable terminfoSearchPath


terminfoSearchPath :: String
terminfoSearchPath =
  List.intercalate ":"
    [ "/etc/terminfo"
    , "/lib/terminfo"
    , "/usr/share/terminfo"
    , "/usr/lib/terminfo"
    , "/usr/local/share/terminfo"
    , "/usr/local/lib/terminfo"
    ]
