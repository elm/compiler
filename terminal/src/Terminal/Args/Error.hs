{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Terminal.Args.Error
  ( Error(..)
  , ArgError(..)
  , FlagError(..)
  , Expectation(..)
  , exitWithHelp
  , exitWithError
  , exitWithUnknown
  , exitWithOverview
  )
  where


import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC.IO.Handle (hIsTerminalDevice)
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO (hPutStrLn, stderr)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Reporting.Suggest as Suggest
import Terminal.Args.Internal



-- ERROR


data Error where
  BadArgs :: [(CompleteArgs a, ArgError)] -> Error
  BadFlag :: FlagError -> Error


data ArgError
  = ArgMissing Expectation
  | ArgBad String Expectation
  | ArgExtras [String]


data FlagError where
  FlagWithValue :: String -> String -> FlagError
  FlagWithBadValue :: String -> String -> Expectation -> FlagError
  FlagWithNoValue :: String -> Expectation -> FlagError
  FlagUnknown :: String -> Flags a -> FlagError


data Expectation =
  Expectation
    { _type :: String
    , _examples :: IO [String]
    }



-- EXIT


exitSuccess :: [P.Doc] -> IO a
exitSuccess =
  exitWith Exit.ExitSuccess


exitFailure :: [P.Doc] -> IO a
exitFailure =
  exitWith (Exit.ExitFailure 1)


exitWith :: Exit.ExitCode -> [P.Doc] -> IO a
exitWith code docs =
  do  isTerminal <- hIsTerminalDevice stderr
      let adjust = if isTerminal then id else P.plain
      P.displayIO stderr $ P.renderPretty 1 80 $
        adjust $ P.vcat $ concatMap (\d -> [d,""]) docs
      hPutStrLn stderr ""
      Exit.exitWith code


getExeName :: IO String
getExeName =
  FP.takeFileName <$> Env.getProgName


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string



-- HELP


exitWithHelp :: Maybe String -> String -> P.Doc -> Args args -> Flags flags -> IO a
exitWithHelp maybeCommand details example (Args args) flags =
  do  command <- toCommand maybeCommand
      exitSuccess $
        [ reflow details
        , P.indent 4 $ P.cyan $ P.vcat $ map (argsToDoc command) args
        , example
        ]
        ++
          case flagsToDocs flags [] of
            [] ->
              []

            docs@(_:_) ->
              [ "You can customize this command with the following flags:"
              , P.indent 4 $ stack docs
              ]


toCommand :: Maybe String -> IO String
toCommand maybeCommand =
  do  exeName <- getExeName
      return $
        case maybeCommand of
          Nothing ->
            exeName

          Just command ->
            exeName ++ " " ++ command


argsToDoc :: String -> CompleteArgs a -> P.Doc
argsToDoc command args =
  case args of
    Exactly required ->
      argsToDocHelp command required []

    Multiple required (Parser _ plural _ _ _) ->
      argsToDocHelp command required ["zero or more " ++ plural]

    Optional required (Parser singular _ _ _ _) ->
      argsToDocHelp command required ["optional " ++ singular]


argsToDocHelp :: String -> RequiredArgs a -> [String] -> P.Doc
argsToDocHelp command args names =
  case args of
    Done _ ->
      P.hang 4 $ P.hsep $ map P.text $
        command : map toToken names

    Required others (Parser singular _ _ _ _) ->
      argsToDocHelp command others (singular : names)


toToken :: String -> String
toToken string =
  "<" ++ map (\c -> if c == ' ' then '-' else c) string ++ ">"


flagsToDocs :: Flags flags -> [P.Doc] -> [P.Doc]
flagsToDocs flags docs =
  case flags of
    FDone _ ->
      docs

    FMore more flag ->
      let
        flagDoc =
          P.vcat $
            case flag of
              Flag name (Parser singular _ _ _ _) description ->
                [ P.dullcyan $ P.text $ "--" ++ name ++ "=" ++ toToken singular
                , P.indent 4 $ reflow description
                ]

              OnOff name description ->
                [ P.dullcyan $ P.text $ "--" ++ name
                , P.indent 4 $ reflow description
                ]
      in
      flagsToDocs more (flagDoc:docs)



-- OVERVIEW


exitWithOverview :: P.Doc -> P.Doc -> [Interface] -> IO a
exitWithOverview intro outro interfaces =
  do  exeName <- getExeName
      exitSuccess
        [ intro
        , "The most common commands are:"
        , P.indent 4 $ stack $ Maybe.mapMaybe (toSummary exeName) interfaces
        , "There are a bunch of other commands as well though. Here is a full list:"
        , P.indent 4 $ P.dullcyan $ toCommandList exeName interfaces
        , "Adding the --help flag gives a bunch of additional details about each one."
        , outro
        ]


toSummary :: String -> Interface -> Maybe P.Doc
toSummary exeName (Interface name summary _ _ (Args args) _ _) =
  case summary of
    Uncommon ->
      Nothing

    Common summaryString ->
      Just $
        P.vcat
          [ P.cyan $ argsToDoc (exeName ++ " " ++ name) (head args)
          , P.indent 4 $ reflow summaryString
          ]


toCommandList :: String -> [Interface] -> P.Doc
toCommandList exeName interfaces =
  let
    names = map toName interfaces
    width = maximum (map length names)

    toExample name =
      P.text $ exeName ++ " " ++ name ++ replicate (width - length name) ' ' ++ " --help"
  in
  P.vcat (map toExample names)



-- UNKNOWN


exitWithUnknown :: String -> [String] -> IO a
exitWithUnknown unknown knowns =
  let
    nearbyKnowns =
      takeWhile (\(r,_) -> r <= 3) (Suggest.rank unknown id knowns)

    suggestions =
      case map toGreen (map snd nearbyKnowns) of
        [] ->
          []

        [nearby] ->
          ["Try",nearby,"instead?"]

        [a,b] ->
          ["Try",a,"or",b,"instead?"]

        abcs@(_:_:_:_) ->
          ["Try"] ++ map (<> ",") (init abcs) ++ ["or",last abcs,"instead?"]
  in
  do  exeName <- getExeName
      exitFailure
        [ P.fillSep $ ["There","is","no",toRed unknown,"command."] ++ suggestions
        , reflow $ "Run `" ++ exeName ++ "` with no arguments to get more hints."
        ]



-- ERROR TO DOC


exitWithError :: Error -> IO a
exitWithError err =
  exitFailure =<<
    case err of
      BadFlag flagError ->
        flagErrorToDocs flagError

      BadArgs argErrors ->
        case argErrors of
          [] ->
            error "TODO no possible args"

          [(_args, argError)] ->
            argErrorToDocs argError

          _:_:_ ->
            error "TODO show possible arg configurations"


toGreen :: String -> P.Doc
toGreen str =
  P.green (P.text str)


toYellow :: String -> P.Doc
toYellow str =
  P.yellow (P.text str)


toRed :: String -> P.Doc
toRed str =
  P.red (P.text str)



-- ARG ERROR TO DOC


argErrorToDocs :: ArgError -> IO [P.Doc]
argErrorToDocs argError =
  case argError of
    ArgMissing (Expectation tipe makeExamples) ->
      do  examples <- makeExamples
          return
            [ P.fillSep
                ["The","arguments","you","have","are","fine,","but","in","addition,","I","was"
                ,"expecting","a",toYellow (toToken tipe),"value.","For","example:"
                ]
            , P.indent 4 $ P.green $ P.vcat $ map P.text examples
            ]

    ArgBad string (Expectation tipe makeExamples) ->
      do  examples <- makeExamples
          return
            [ "I am having trouble with this argument:"
            , P.indent 4 $ toRed string
            , P.fillSep
                ["It","is","supposed","to","be","a"
                ,toYellow (toToken tipe),"value,","like","this:"
                ]
            , P.indent 4 $ P.green $ P.vcat $ map P.text examples
            ]

    ArgExtras extras ->
      let
        (these, them) =
          case extras of
            [_] -> ("this argument", "it")
            _ -> ("these arguments", "them")
      in
      return
        [ reflow $ "I was not expecting " ++ these ++ ":"
        , P.indent 4 $ P.red $ P.vcat $ map P.text extras
        , reflow $ "Try removing " ++ them ++ "?"
        ]



-- FLAG ERROR TO DOC


flagErrorHelp :: String -> String -> [P.Doc] -> IO [P.Doc]
flagErrorHelp summary original explanation =
  return $
    [ reflow summary
    , P.indent 4 (toRed original)
    ]
    ++ explanation


flagErrorToDocs :: FlagError -> IO [P.Doc]
flagErrorToDocs flagError =
  case flagError of
    FlagWithValue flagName value ->
      flagErrorHelp
        "This on/off flag was given a value:"
        ("--" ++ flagName ++ "=" ++ value)
        [ "An on/off flag either exists or not. It cannot have an equals sign and value.\n\
          \Maybe you want this instead?"
        , P.indent 4 $ toGreen $ "--" ++ flagName
        ]

    FlagWithNoValue flagName (Expectation tipe makeExamples) ->
      do  examples <- makeExamples
          flagErrorHelp
            "This flag needs more information:"
            ("--" ++ flagName)
            [ P.fillSep ["It","needs","a",toYellow (toToken tipe),"like","this:"]
            , P.indent 4 $ P.vcat $ map toGreen $
                case take 4 examples of
                  [] ->
                    ["--" ++ flagName ++ "=" ++ toToken tipe]

                  _:_ ->
                    map (\example -> "--" ++ flagName ++ "=" ++ example) examples
            ]

    FlagWithBadValue flagName badValue (Expectation tipe makeExamples) ->
      do  examples <- makeExamples
          flagErrorHelp
            "This flag was given a bad value:"
            ("--" ++ flagName ++ "=" ++ badValue)
            [ P.fillSep $
                ["I","need","a","valid",toYellow (toToken tipe),"value.","For","example:"
                ]
            , P.indent 4 $ P.vcat $ map toGreen $
                case take 4 examples of
                  [] ->
                    ["--" ++ flagName ++ "=" ++ toToken tipe]

                  _:_ ->
                    map (\example -> "--" ++ flagName ++ "=" ++ example) examples
            ]

    FlagUnknown unknown flags ->
      flagErrorHelp
        "I do not recognize this flag:"
        unknown
        (
          let unknownName = takeWhile ('=' /=) (dropWhile ('-' ==) unknown) in
          case getNearbyFlags unknownName flags [] of
            [] ->
              []

            [thisOne] ->
              [ P.fillSep ["Maybe","you","want",P.green thisOne,"instead?"]
              ]

            suggestions ->
              [ P.fillSep ["Maybe","you","want","one","of","these","instead?"]
              , P.indent 4 $ P.green $ P.vcat suggestions
              ]
        )


getNearbyFlags :: String -> Flags a -> [(Int, String)] -> [P.Doc]
getNearbyFlags unknown flags unsortedFlags =
  case flags of
    FMore more flag ->
      getNearbyFlags unknown more (getNearbyFlagsHelp unknown flag : unsortedFlags)

    FDone _ ->
      map P.text $ map snd $ List.sortOn fst $
        case filter (\(d,_) -> d < 3) unsortedFlags of
          [] ->
            unsortedFlags

          nearbyUnsortedFlags ->
            nearbyUnsortedFlags


getNearbyFlagsHelp :: String -> Flag a -> (Int, String)
getNearbyFlagsHelp unknown flag =
  case flag of
    OnOff flagName _ ->
      ( Suggest.distance unknown flagName
      , "--" ++ flagName
      )

    Flag flagName (Parser singular _ _ _ _) _ ->
      ( Suggest.distance unknown flagName
      , "--" ++ flagName ++ "=" ++ toToken singular
      )
