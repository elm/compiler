{-# OPTIONS_GHC -Wall #-}
module CommandLine.Args
  ( chomp
  , ElmRoute(..)
  , DevFlags(..)
  , MakeFlags(..)
  , ReplFlags(..)
  )
  where


import CommandLine.Args.Internal as Args
import qualified Data.Text as Text
import qualified Elm.Diff as Diff
import qualified Elm.Package as Pkg
import qualified Elm.Project.Flags as Flags
import qualified System.FilePath as FP



-- ROUTE


data ElmRoute
  = Naked () ()
  | Version () ()
  | Install Pkg.Name ()
  | Publish () ()
  | Make [FilePath] MakeFlags
  | Bump () ()
  | Diff Diff.Args ()
  | Repl () ReplFlags
  | Dev () DevFlags


chomp :: IO ElmRoute
chomp =
  get
    (Route Naked noArgs noFlags)
    [ "version" ==> Route Version noArgs noFlags
    , "install" ==> Route Install (required package) noFlags
    , "publish" ==> Route Publish noArgs noFlags
    , "make" ==> Route Make (zeroOrMore file) makeFlags
    , "bump" ==> Route Bump noArgs noFlags
    , "diff" ==> Route Diff diffArgs noFlags
    , "repl" ==> Route Repl noArgs replFlags
    , "develop" ==> Route Dev noArgs devFlags
    ]


(==>) :: a -> b -> (a, b)
(==>) =
  (,)



-- MAKE


data MakeFlags =
  MakeFlags
    { _warn :: Bool
    , _debug :: Bool
    , _output :: Maybe Flags.Output
    , _server :: Bool
    }


makeFlags :: Flags MakeFlags
makeFlags =
  flags MakeFlags
    |-- onOff "warn" "Report warnings to improve code quality."
    |-- onOff "debug" "Generate programs in debug mode."
    |-- flag "output" output "Specify the name of the resulting JS file (use --output=/dev/null for no output)"
    |-- onOff "server" "Generate JS for server-side rendering."


output :: Parser Flags.Output
output =
  custom "PATH" $ \string ->
    if string == "/dev/null" then
      Just Flags.None

    else
      case FP.splitPath string of
        [] ->
          Nothing

        [name] ->
          outputCustom Nothing name

        segments ->
          outputCustom (Just (FP.joinPath (init segments))) (last segments)


outputCustom :: Maybe FilePath -> FilePath -> Maybe Flags.Output
outputCustom maybeDir fileName =
  if FP.takeExtension fileName == ".js" then
    Just (Flags.Custom maybeDir fileName)
  else
    Nothing



-- REPL


data ReplFlags =
  ReplFlags
    { _interpreter :: Maybe FilePath
    }


replFlags :: Flags ReplFlags
replFlags =
  flags ReplFlags
    |-- flag "interpreter" (anything "EXE") "Path to a alternate JS interpreter, like node or nodejs."



-- DEVELOP


data DevFlags =
  DevFlags
    { _port :: Maybe Int
    }


devFlags :: Flags DevFlags
devFlags =
  flags DevFlags
    |-- flag "port" (int "PORT") "The port of the server (default: 8000)"



-- DIFF


diffArgs :: Args Diff.Args
diffArgs =
  oneOf
    [ require0 Diff.CodeVsLatest
    , require1 Diff.CodeVsExactly version
    , require2 Diff.LocalInquiry version version
    , require3 Diff.GlobalInquiry package version version
    ]


package :: Parser Pkg.Name
package =
  custom "USER/PROJECT" $ \string ->
    case Pkg.fromText (Text.pack string) of
      Left _ ->
        Nothing

      Right a ->
        Just a


version :: Parser Pkg.Version
version =
  custom "VERSION" (Pkg.versionFromText . Text.pack)
