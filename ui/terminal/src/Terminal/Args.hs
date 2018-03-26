module Terminal.Args
  ( simple
  , Interface(..)
  , Summary(..)
  , Details(..)
  , complex
  , Flags, noFlags, flags, (|--)
  , Flag, flag, onOff
  , Parser(..), suggestFiles
  , Args, noArgs, required, optional, zeroOrMore, oneOrMore, oneOf
  , require0, require1, require2, require3, require4, require5
  , RequiredArgs, args, exactly, (!), (?), (...)
  )
  where


import Control.Monad (filterM)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.FilePath ((</>))
import System.IO (hPutStrLn, stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Terminal.Args.Internal
import qualified Terminal.Args.Chomp as Chomp
import qualified Terminal.Args.Error as Error



-- GET


simple :: String -> Args args -> Flags flags -> (args -> flags -> IO ()) -> IO ()
simple details args_ flags_ callback =
  do  argStrings <- Env.getArgs
      case argStrings of
        "<autocomplete>" : number : chunks ->
          attemptAutoComplete number $ \index ->
            fst $ Chomp.chomp (Just index) chunks args_ flags_

        chunks ->
          if elem "--help" chunks then
            Error.exitWithHelp Nothing details args_ flags_

          else
            case snd $ Chomp.chomp Nothing chunks args_ flags_ of
              Right (argsValue, flagValue) ->
                callback argsValue flagValue

              Left err ->
                Error.exitWithError Nothing err


complex :: P.Doc -> P.Doc -> [Interface] -> IO ()
complex intro outro interfaces =
  do  argStrings <- Env.getArgs
      case argStrings of
        [] ->
          Error.exitWithOverview intro outro interfaces

        "<autocomplete>" : number : chunks ->
          attemptAutoComplete number $ \index ->
            complexSuggest index chunks interfaces

        command : chunks ->
          case List.find (\iface -> toName iface == command) interfaces of
            Nothing ->
              Error.exitWithUnknown command (map toName interfaces)

            Just (Interface _ _ (Details details) args_ flags_ callback) ->
              if elem "--help" chunks then
                Error.exitWithHelp (Just command) details args_ flags_

              else
                case snd $ Chomp.chomp Nothing chunks args_ flags_ of
                  Right (argsValue, flagValue) ->
                    callback argsValue flagValue

                  Left err ->
                    Error.exitWithError (Just command) err



-- AUTO-COMPLETE


attemptAutoComplete :: String -> (Int -> IO [String]) -> IO a
attemptAutoComplete number getSuggestions =
  if all Char.isDigit number then
    do  suggestions <- getSuggestions (read number)
        hPutStrLn stdout (List.intercalate " " suggestions)
        Exit.exitSuccess
  else
    Exit.exitFailure


complexSuggest :: Int -> [String] -> [Interface] -> IO [String]
complexSuggest index strings interfaces =
  case strings of
    [] ->
      return (map toName interfaces)

    command : chunks ->
      case List.find (\iface -> toName iface == command) interfaces of
        Nothing ->
          return (map toName interfaces)

        Just (Interface _ _ _ args_ flags_ _) ->
          fst $ Chomp.chomp (Just (index-1)) chunks args_ flags_



-- FLAGS


{-|-}
noFlags :: Flags ()
noFlags =
  FDone ()


{-|-}
flags :: a -> Flags a
flags =
  FDone


{-|-}
(|--) :: Flags (a -> b) -> Flag a -> Flags b
(|--) =
  FMore



-- FLAG


{-|-}
flag :: String -> Parser a -> String -> Flag (Maybe a)
flag =
  Flag


{-|-}
onOff :: String -> String -> Flag Bool
onOff =
  OnOff



-- FANCY ARGS


{-|-}
args :: a -> RequiredArgs a
args =
  Done


{-|-}
exactly :: RequiredArgs a -> Args a
exactly requiredArgs =
  Args [Exactly requiredArgs]


{-|-}
(!) :: RequiredArgs (a -> b) -> Parser a -> RequiredArgs b
(!) =
  Required


{-|-}
(?) :: RequiredArgs (Maybe a -> b) -> Parser a -> Args b
(?) requiredArgs optionalArg =
  Args [Optional requiredArgs optionalArg]


{-|-}
(...) :: RequiredArgs ([a] -> b) -> Parser a -> Args b
(...) requiredArgs repeatedArg =
  Args [Multiple requiredArgs repeatedArg]


{-|-}
oneOf :: [Args a] -> Args a
oneOf listOfArgs =
  Args (concatMap (\(Args a) -> a) listOfArgs)



-- SIMPLE ARGS


{-|-}
noArgs :: Args ()
noArgs =
  exactly (args ())


{-|-}
required :: Parser a -> Args a
required parser =
  require1 id parser


{-|-}
optional :: Parser a -> Args (Maybe a)
optional parser =
  args id ? parser


{-|-}
zeroOrMore :: Parser a -> Args [a]
zeroOrMore parser =
  args id ... parser


{-|-}
oneOrMore :: Parser a -> Args (a, [a])
oneOrMore parser =
  args (,) ! parser ... parser


{-|-}
require0 :: args -> Args args
require0 value =
  exactly (args value)


{-|-}
require1 :: (a -> args) -> Parser a -> Args args
require1 func a =
  exactly (args func ! a)


{-|-}
require2 :: (a -> b -> args) -> Parser a -> Parser b -> Args args
require2 func a b =
  exactly (args func ! a ! b)


{-|-}
require3 :: (a -> b -> c -> args) -> Parser a -> Parser b -> Parser c -> Args args
require3 func a b c =
  exactly (args func ! a ! b ! c)


{-|-}
require4 :: (a -> b -> c -> d -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Args args
require4 func a b c d =
  exactly (args func ! a ! b ! c ! d)


{-|-}
require5 :: (a -> b -> c -> d -> e -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Args args
require5 func a b c d e =
  exactly (args func ! a ! b ! c ! d ! e)



-- SUGGEST FILES


{-| Helper for creating custom `Parser` values. It will suggest directories and
file names:

    suggestFiles []             -- suggests any file
    suggestFiles ["elm"]        -- suggests only .elm files
    suggestFiles ["js","html"]  -- suggests only .js and .html files

Notice that you can limit the suggestion by the file extension! If you need
something more elaborate, you can implement a function like this yourself that
does whatever you need!
-}
suggestFiles :: [String] -> String -> IO [String]
suggestFiles extensions string =
  let
    (dir, start) =
      FP.splitFileName string
  in
  do  content <- Dir.getDirectoryContents dir
      filterM (isPossibleSuggestion extensions start dir) content


isPossibleSuggestion :: [String] -> String -> FilePath -> FilePath -> IO Bool
isPossibleSuggestion extensions start dir path =
  if List.isPrefixOf start path then
    do  isDir <- Dir.doesDirectoryExist (dir </> path)
        return (isDir || isOkayExtension path extensions)
  else
    return False


isOkayExtension :: FilePath -> [String] -> Bool
isOkayExtension path extensions =
  null extensions || elem (FP.takeExtension path) extensions

