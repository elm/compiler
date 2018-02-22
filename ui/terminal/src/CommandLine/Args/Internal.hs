{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module CommandLine.Args.Internal
  ( get
  , Flags, noFlags, flags, (|--)
  , Flag, flag, onOff
  , Parser, int, file, anything, custom
  , Args, noArgs, required, optional, zeroOrMore, oneOrMore, oneOf
  , require0, require1, require2, require3, require4, require5
  , RequiredArgs, args, exactly, (!), (?), (...)
  , Route(..)
  )
  where

import qualified Data.Char as Char
import qualified Data.List as List
import GHC.IO.Handle (hIsTerminalDevice)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.ANSI.Leijen
  ( Doc, (<>), comma, displayIO, fillSep, green
  , plain, red, renderPretty, text, vcat
  )

import Elm.Utils (nearbyNames)



-- ROUTE


data Route a where
  Route :: (args -> flags -> a) -> Args args -> Flags flags -> Route a



-- FLAGS


data Flags a where
  FDone :: a -> Flags a
  FMore :: Flags (a -> b) -> Flag a -> Flags b


noFlags :: Flags ()
noFlags =
  FDone ()


flags :: a -> Flags a
flags =
  FDone


(|--) :: Flags (a -> b) -> Flag a -> Flags b
(|--) =
  FMore



-- FLAG


data Flag a where
  Flag :: String -> Parser a -> String -> Flag (Maybe a)
  OnOff :: String -> String -> Flag Bool


flag :: String -> Parser a -> String -> Flag (Maybe a)
flag =
  Flag


onOff :: String -> String -> Flag Bool
onOff =
  OnOff



-- PARSERS


data Parser a where
  Anything :: String -> Parser String
  File :: Parser FilePath
  Int :: String -> Parser Int
  Custom :: String -> (String -> Maybe a) -> Parser a


file :: Parser FilePath
file =
  File


int :: String -> Parser Int
int =
  Int


anything :: String -> Parser String
anything =
  Anything


custom :: String -> (String -> Maybe a) -> Parser a
custom =
  Custom


data ParserInfo =
  ParserInfo
    { _type :: String
    }


getParserInfo :: Parser a -> ParserInfo
getParserInfo parser =
  case parser of
    Anything tipe ->
      ParserInfo tipe

    File ->
      ParserInfo "FILE"

    Int tipe ->
      ParserInfo tipe

    Custom tipe _ ->
      ParserInfo tipe


runParser :: Parser a -> String -> Either ParserError a
runParser parser str =
  case parser of
    Anything _ ->
      Right str

    File ->
      Right str

    Int tipe ->
      if all Char.isDigit str then
        Right (read str :: Int)
      else
        Left (Expecting tipe)

    Custom tipe toMaybe ->
      case toMaybe str of
        Nothing ->
          Left (Expecting tipe)

        Just a ->
          Right a


data ParserError = Expecting String



-- ARGS


noArgs :: Args ()
noArgs =
  exactly (args ())


required :: Parser a -> Args a
required parser =
  require1 id parser


optional :: Parser a -> Args (Maybe a)
optional parser =
  args id
    ? parser


zeroOrMore :: Parser a -> Args [a]
zeroOrMore parser =
  args id
    ... parser


oneOrMore :: Parser a -> Args (a, [a])
oneOrMore parser =
  args (,)
    ! parser
    ... parser


require0 :: args -> Args args
require0 value =
  exactly (args value)


require1 :: (a -> args) -> Parser a -> Args args
require1 func a =
  exactly (
    args func
      ! a
  )


require2 :: (a -> b -> args) -> Parser a -> Parser b -> Args args
require2 func a b =
  exactly (
    args func
      ! a
      ! b
  )


require3 :: (a -> b -> c -> args) -> Parser a -> Parser b -> Parser c -> Args args
require3 func a b c =
  exactly (
    args func
      ! a
      ! b
      ! c
  )


require4 :: (a -> b -> c -> d -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Args args
require4 func a b c d =
  exactly (
    args func
      ! a
      ! b
      ! c
      ! d
  )


require5 :: (a -> b -> c -> d -> e -> args) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Args args
require5 func a b c d e =
  exactly (
    args func
      ! a
      ! b
      ! c
      ! d
      ! e
  )


oneOf :: [Args a] -> Args a
oneOf =
  OneOf



-- FANCE ARGS


data RequiredArgs a where
  Done :: a -> RequiredArgs a
  Required :: RequiredArgs (a -> b) -> Parser a -> RequiredArgs b


data Args a where
  Exactly :: RequiredArgs a -> Args a
  Optional :: RequiredArgs (Maybe a -> b) -> Parser a -> Args b
  Multiple :: RequiredArgs ([a] -> b) -> Parser a -> Args b
  OneOf :: [Args a] -> Args a


args :: a -> RequiredArgs a
args =
  Done


exactly :: RequiredArgs a -> Args a
exactly =
  Exactly


(!) :: RequiredArgs (a -> b) -> Parser a -> RequiredArgs b
(!) =
  Required


(?) :: RequiredArgs (Maybe a -> b) -> Parser a -> Args b
(?) =
  Optional


(...) :: RequiredArgs ([a] -> b) -> Parser a -> Args b
(...) =
  Multiple



-- GET


get :: Route a -> [(String, Route a)] -> IO a
get route namedRoutes =
  do  chunks <- getArgs
      case parse route namedRoutes chunks of
        Right flagsAndArgs ->
          return flagsAndArgs

        Left err ->
          failure err



-- PARSE


data Error
  = AfterCommand String RouteError
  | BadCommand String [String] String
  | NoCommand RouteError


parse :: Route a -> [(String, Route a)] -> [String] -> Either Error a
parse route namedRoutes chunks =
  case parseHelp namedRoutes chunks [] of
    Right answer ->
      Right answer

    Left errors ->
      case parseRoute route chunks of
        Right answer ->
          Right answer

        Left routeError ->
          case errors of
            (name, specificRouteError) : _ ->
              Left (AfterCommand name specificRouteError)

            [] ->
              case chunks of
                first : _ | not (List.isPrefixOf "-" first) ->
                  case nearbyNames id first (map fst namedRoutes) of
                    [] ->
                      Left (NoCommand routeError)

                    suggestions ->
                      Left (BadCommand first (init suggestions) (last suggestions))

                _ ->
                  Left (NoCommand routeError)


parseHelp :: [(String, Route a)] -> [String] -> RouteErrors -> Either RouteErrors a
parseHelp namedRoutes chunks errors =
  case namedRoutes of
    [] ->
      Left errors

    (name, route) : rest ->
      case parseNamedRoute name route chunks of
        Left errs ->
          parseHelp rest chunks (errs ++ errors)

        Right answer ->
          Right answer



-- PARSE NAMED ROUTE


type RouteErrors = [(String, RouteError)]


parseNamedRoute :: String -> Route a -> [String] -> Either RouteErrors a
parseNamedRoute name route rawChunks =
  case rawChunks of
    first : chunks | name == first ->
      case parseRoute route chunks of
        Right answer ->
          Right answer

        Left err ->
          Left [(name, err)]

    _ ->
      Left []



-- PARSE ROUTE


data RouteError = BadFlags FlagError | BadArgs ArgError


parseRoute :: Route a -> [String] -> Either RouteError a
parseRoute (Route func args_ flags_) chunks =
  case parseFlags flags_ chunks of
    Left flagError ->
      Left (BadFlags flagError)

    Right (flagData, remainingChunks) ->
      case parseArgs args_ remainingChunks of
        Left argError ->
          Left (BadArgs argError)

        Right argData ->
          Right (func argData flagData)



-- PARSE ARGS


data ArgError
  = NoneOf [ArgError]
  | MissingArg ParserInfo
  | BadArg String ParserError
  | ExtraArgs [String]


parseArgs :: Args a -> [String] -> Either ArgError a
parseArgs args_ chunks =
  case args_ of
    OneOf argsList ->
      parseOneOf argsList chunks []

    Exactly requiredArgs ->
      do  (value, extras) <- parseRequiredArgs requiredArgs chunks
          case extras of
            [] ->
              Right value

            _ : _ ->
              Left (ExtraArgs extras)

    Optional requiredArgs optionalParser ->
      do  (func, subChunks) <- parseRequiredArgs requiredArgs chunks
          case subChunks of
            [] ->
              Right (func Nothing)

            [chunk] ->
              do  value <- runArgParser optionalParser chunk
                  Right (func (Just value))

            _ : extras ->
              Left (ExtraArgs extras)

    Multiple requiredArgs multiParser ->
      do  (func, extras) <- parseRequiredArgs requiredArgs chunks
          values <- traverse (runArgParser multiParser) extras
          Right (func values)


parseOneOf :: [Args a] -> [String] -> [ArgError] -> Either ArgError a
parseOneOf argsList chunks errors =
  case argsList of
    [] ->
      Left (NoneOf (reverse errors))

    args_ : rest ->
      case parseArgs args_ chunks of
        Left err ->
          parseOneOf rest chunks (err:errors)

        Right answer ->
          Right answer


parseRequiredArgs :: RequiredArgs a -> [String] -> Either ArgError (a, [String])
parseRequiredArgs args_ chunks =
  case args_ of
    Done value ->
      Right (value, chunks)

    Required funcArgs parser ->
      do  (func, subChunks) <- parseRequiredArgs funcArgs chunks
          case subChunks of
            [] ->
              Left (MissingArg (getParserInfo parser))

            chunk : rest ->
              do  value <- runArgParser parser chunk
                  Right ( func value, rest )


runArgParser :: Parser a -> String -> Either ArgError a
runArgParser parser chunk =
  case runParser parser chunk of
    Left parserError ->
      Left (BadArg chunk parserError)

    Right value ->
      Right value



-- PARSE FLAGS


type Result a =
  Either FlagError (a, [String])


data FlagError
  = BadOnOff String String
  | BadFlag String ParserInfo (Maybe (String, ParserError))
  | Unrecognized String [(String, String)]


parseFlags :: Flags a -> [String] -> Result a
parseFlags flags_ chunks =
  do  (a, leftoverChunks) <- parseFlagsHelp flags_ chunks
      case List.find (List.isPrefixOf "-") leftoverChunks of
        Nothing ->
          Right (a, leftoverChunks)

        Just badArg ->
          Left (Unrecognized badArg (collectFlagInfo flags_))


parseFlagsHelp :: Flags a -> [String] -> Result a
parseFlagsHelp flags_ chunks =
  case flags_ of
    FDone value ->
      Right (value, chunks)

    FMore makeFunc flag_ ->
      do  (func, remainingChunks) <- parseFlagsHelp makeFunc chunks
          (arg, leftoverChunks) <- parseFlag flag_ remainingChunks
          Right (func arg, leftoverChunks)


parseFlag :: Flag a -> [String] -> Result a
parseFlag flag_ chunks =
  case flag_ of
    Flag name parser _summary ->
      parseFlagFlag name parser chunks

    OnOff name _summary ->
      parseFlagOnOff name chunks


parseFlagFlag :: String -> Parser a -> [String] -> Result (Maybe a)
parseFlagFlag name parser chunks =
  case findDoubleDash [] ("--" ++ name) chunks of
    Nothing ->
      Right (Nothing, chunks)

    Just (ChunkZipper before value after) ->
      case value of
        Definitely str ->
          parseFlagFlagHelp name parser before str after

        Possibly str ->
          parseFlagFlagHelp name parser before str after

        DefNope ->
          Left (BadFlag name (getParserInfo parser) Nothing)


parseFlagFlagHelp :: String -> Parser a -> [String] -> String -> [String] -> Result (Maybe a)
parseFlagFlagHelp name parser before str after =
  case runParser parser str of
    Right value ->
      Right (Just value, before ++ after)

    Left parserError ->
      Left $ BadFlag name (getParserInfo parser) $
        Just (str, parserError)


parseFlagOnOff :: String -> [String] -> Result Bool
parseFlagOnOff name chunks =
  case findDoubleDash [] ("--" ++ name) chunks of
    Nothing ->
      Right (False, chunks)

    Just (ChunkZipper before value after) ->
      case value of
        DefNope ->
          Right (True, before ++ after)

        Possibly str ->
          Right (True, before ++ str : after)

        Definitely str ->
          Left (BadOnOff name str)



-- DOUBLE DASH


data ChunkZipper =
  ChunkZipper
    { _before :: [String]
    , _value :: Value
    , _after :: [String]
    }


data Value
  = Definitely String
  | Possibly String
  | DefNope


findDoubleDash :: [String] -> String -> [String] -> Maybe ChunkZipper
findDoubleDash revPrev prefix chunks =
  case chunks of
    flag_ : val : rest | flag_ == prefix ->
      if List.isPrefixOf "-" val then
        Just $ ChunkZipper (reverse revPrev) DefNope (val : rest)
      else
        Just $ ChunkZipper (reverse revPrev) (Possibly val) rest

    [flag_] | flag_ == prefix ->
      Just $ ChunkZipper (reverse revPrev) DefNope []

    flag_ : rest | List.isPrefixOf (prefix ++ "=") flag_ ->
      let
        value =
          drop (length prefix + 1) flag_
      in
        Just $ ChunkZipper (reverse revPrev) (Definitely value) rest

    chunk : rest ->
      findDoubleDash (chunk : revPrev) prefix rest

    [] ->
      Nothing



-- COLLECT FLAG NAMES


collectFlagInfo :: Flags a -> [(String, String)]
collectFlagInfo flags_ =
  collectFlagInfoHelp flags_ []


collectFlagInfoHelp :: Flags a -> [(String, String)] -> [(String, String)]
collectFlagInfoHelp flags_ info =
  case flags_ of
    FDone _ ->
      reverse info

    FMore otherFlags flag_ ->
      collectFlagInfoHelp otherFlags (getFlagInfo flag_ : info)


getFlagInfo :: Flag a -> (String, String)
getFlagInfo flag_ =
  case flag_ of
    OnOff name summary ->
      (name, summary)

    Flag name _ summary ->
      (name, summary)



-- FAILURE


failure :: Error -> IO a
failure err =
  do  let doc = toDoc err
      isTerminal <- hIsTerminalDevice stderr
      displayIO stderr $ renderPretty 1 80 $
        if isTerminal then doc else plain doc
      hPutStrLn stderr ""
      exitFailure


toDoc :: Error -> Doc
toDoc err =
  case err of
    BadCommand badName suggestions lastSuggestion ->
      fillSep $
        [ text "There is no"
        , code badName
        , text "command."
        , text "Did you mean"
        ]
        ++ oxfordify (map code suggestions) "or" (code lastSuggestion <> text "?")

    NoCommand (BadArgs argError) ->
      argErrorToDoc Nothing argError

    NoCommand (BadFlags flagError) ->
      flagErrorToDoc Nothing flagError

    AfterCommand name (BadArgs argError) ->
      argErrorToDoc (Just name) argError

    AfterCommand name (BadFlags flagError) ->
      flagErrorToDoc (Just name) flagError



-- ARG FAILURE


argErrorToDoc :: Maybe String -> ArgError -> Doc
argErrorToDoc maybeCommand argError =
  case argError of
    NoneOf argErrors ->
      text $ "TODO - " ++ show (map (argErrorToDoc maybeCommand) argErrors)

    MissingArg (ParserInfo tipe) ->
      text $ "TODO - " ++ tipe

    BadArg chunk _parserError ->
      text $ "TODO - " ++ chunk

    ExtraArgs extraChunks ->
      text $ "TODO - " ++ show extraChunks



-- FLAG FAILURE


flagErrorToDoc :: Maybe String -> FlagError -> Doc
flagErrorToDoc maybeCommand flagError =
  case flagError of
    BadOnOff name value ->
      let
        chunk =
          "--" ++ name ++ "=" ++ value
      in
        if null value then
          flagErrorToDocHelp "This on/off flag has a trailing equals sign:" chunk
            [ fillSep
                [ text "Just get rid of the equals sign, like this:"
                , green (text ("--" ++ name))
                ]
            ]

        else
          flagErrorToDocHelp "This on/off flag was given a value:" chunk
            [ text "An on/off flag either exists or not, no need for an equals sign and value."
            , text "Maybe you want this instead?"
            , text ""
            , text "    " <> green (text ("--" ++ name))
            , text ""
            ]

    BadFlag name (ParserInfo tipe) Nothing ->
      flagErrorToDocHelp
        "This flag cannot appear without a value:"
        ("--" ++ name)
        [ text $ "It needs a " ++ tipe ++ " value."
        ]

    BadFlag name (ParserInfo tipe) (Just (value, parserError)) ->
      flagErrorToDocHelp
        "This flag was given a bad value:"
        ("--" ++ name ++ "=" ++ value)
        [ fillSep $ map text $ words $
            "I need a " ++ tipe ++ " value, but "
            ++
              case parserError of
                Expecting expectedType ->
                  value ++ " is not a valid " ++ expectedType ++ "."
        ]

    Unrecognized chunk [] ->
      let
        explanation =
          case maybeCommand of
            Nothing ->
              "I am not expecting any flags."

            Just command ->
              "The `" ++ command ++ "` command does not expect any flags."
      in
        flagErrorToDocHelp "Get rid of this flag:" chunk $
          [ fillSep (map text (words explanation))
          ]

    Unrecognized chunk info ->
      case takeWhile ('=' /=) (dropWhile ('-' ==) chunk) of
        "" ->
          flagErrorToDocHelp "This is not a valid flag:" chunk
            [ text "Valid flags look like --output=FILE"
            ]

        name ->
          let
            suggestions =
              nearbyNames id name (map fst info)
          in
            flagErrorToDocHelp "I do not recognize this flag:" chunk $
              unrecognizedExplanation $
                if null suggestions then map fst info else suggestions


flagErrorToDocHelp :: String -> String -> [Doc] -> Doc
flagErrorToDocHelp summary original explanation =
  vcat $
    [ text summary
    , text ""
    , text "    " <> red (text original)
    , text ""
    ]
    ++ explanation


unrecognizedExplanation :: [String] -> [Doc]
unrecognizedExplanation suggestions =
  let
    toBullet name =
      text "    " <> green (text ("--" ++ name))
  in
    case suggestions of
      [] ->
        []

      [suggestion] ->
        [ text "Looks like a typo. Maybe you want:"
        , text ""
        , toBullet suggestion
        , text ""
        ]

      _ ->
        [ text "Maybe you want one of these?"
        , text ""
        ]
        ++ map toBullet suggestions
        ++ [ text "" ]


code :: String -> Doc
code str =
  red (text str)


oxfordify :: [Doc] -> String -> Doc -> [Doc]
oxfordify chunks conjuction lastChunk =
  case chunks of
    [] ->
      [ lastChunk ]

    [chunk] ->
      [ chunk, text conjuction, lastChunk ]

    _ ->
      map (<> comma) chunks ++ [ text conjuction, lastChunk ]
