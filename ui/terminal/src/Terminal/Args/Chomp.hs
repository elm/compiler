{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Terminal.Args.Chomp
  ( chomp
  )
  where


import qualified Data.List as List

import Terminal.Args.Error
import Terminal.Args.Internal



-- CHOMP INTERFACE


chomp :: Maybe Int -> [String] -> Args args -> Flags flags -> ( IO [String], Either Error (args, flags) )
chomp maybeIndex strings args flags =
  let
    (Chomper flagChomper) =
      chompFlags flags

    ok suggest chunks flagValue =
      fmap (flip (,) flagValue) <$> chompArgs suggest chunks args

    err suggest flagError =
      ( addSuggest (return []) suggest, Left (BadFlag flagError) )
  in
  flagChomper (toSuggest maybeIndex) (toChunks strings) ok err


toChunks :: [String] -> [Chunk]
toChunks strings =
  zipWith Chunk [ 1 .. length strings ] strings


toSuggest :: Maybe Int -> Suggest
toSuggest maybeIndex =
  case maybeIndex of
    Nothing ->
      NoSuggestion

    Just index ->
      Suggest index



-- CHOMPER


newtype Chomper x a =
  Chomper (
    forall result.
      Suggest
      -> [Chunk]
      -> (Suggest -> [Chunk] -> a -> result)
      -> (Suggest -> x -> result)
      -> result
  )


data Chunk =
  Chunk
    { _index :: Int
    , _chunk :: String
    }


data Suggest
  = NoSuggestion
  | Suggest Int
  | Suggestions (IO [String])


makeSuggestion :: Suggest -> (Int -> Maybe (IO [String])) -> Suggest
makeSuggestion suggest maybeUpdate =
  case suggest of
    NoSuggestion ->
      suggest

    Suggestions _ ->
      suggest

    Suggest index ->
      maybe suggest Suggestions (maybeUpdate index)



-- ARGS


chompArgs :: Suggest -> [Chunk] -> Args a -> (IO [String], Either Error a)
chompArgs suggest chunks (Args completeArgsList) =
  chompArgsHelp suggest chunks completeArgsList [] []


chompArgsHelp :: Suggest -> [Chunk] -> [CompleteArgs a] -> [Suggest] -> [(CompleteArgs a, ArgError)] -> (IO [String], Either Error a)
chompArgsHelp suggest chunks completeArgsList revSuggest revArgErrors =
  case completeArgsList of
    [] ->
      ( foldl addSuggest (return []) revSuggest
      , Left (BadArgs (reverse revArgErrors))
      )

    completeArgs : others ->
      case chompCompleteArgs suggest chunks completeArgs of
        (s1, Left argError) ->
          chompArgsHelp suggest chunks others (s1:revSuggest) ((completeArgs,argError):revArgErrors)

        (s1, Right value) ->
          ( addSuggest (return []) s1
          , Right value
          )


addSuggest :: IO [String] -> Suggest -> IO [String]
addSuggest everything suggest =
  case suggest of
    NoSuggestion ->
      everything

    Suggest _ ->
      everything

    Suggestions newStuff ->
      (++) <$> newStuff <*> everything



-- COMPLETE ARGS


chompCompleteArgs :: Suggest -> [Chunk] -> CompleteArgs a -> (Suggest, Either ArgError a)
chompCompleteArgs suggest chunks completeArgs =
  let
    numChunks = length chunks
  in
  case completeArgs of
    Exactly requiredArgs ->
      chompExactly suggest chunks (chompRequiredArgs numChunks requiredArgs)

    Optional requiredArgs parser ->
      chompOptional suggest chunks (chompRequiredArgs numChunks requiredArgs) parser

    Multiple requiredArgs parser ->
      chompMultiple suggest chunks (chompRequiredArgs numChunks requiredArgs) parser


chompExactly :: Suggest -> [Chunk] -> Chomper ArgError a -> (Suggest, Either ArgError a)
chompExactly suggest chunks (Chomper chomper) =
  let
    ok s cs value =
      case map _chunk cs of
        [] -> (s, Right value)
        es -> (s, Left (ArgExtras es))

    err s argError =
      (s, Left argError)
  in
  chomper suggest chunks ok err


chompOptional :: Suggest -> [Chunk] -> Chomper ArgError (Maybe a -> b) -> Parser a -> (Suggest, Either ArgError b)
chompOptional suggest chunks (Chomper chomper) parser =
  let
    ok s1 cs func =
      case cs of
        [] ->
          (s1, Right (func Nothing))

        Chunk index string : others ->
          case tryToParse s1 parser index string of
            (s2, Left expectation) ->
              (s2, Left (ArgBad string expectation))

            (s2, Right value) ->
              case map _chunk others of
                [] -> (s2, Right (func (Just value)))
                es -> (s2, Left (ArgExtras es))

    err s1 argError =
      (s1, Left argError)
  in
  chomper suggest chunks ok err


chompMultiple :: Suggest -> [Chunk] -> Chomper ArgError ([a] -> b) -> Parser a -> (Suggest, Either ArgError b)
chompMultiple suggest chunks (Chomper chomper) parser =
  let
    err s1 argError =
      (s1, Left argError)
  in
  chomper suggest chunks (chompMultipleHelp parser []) err


chompMultipleHelp :: Parser a -> [a] -> Suggest -> [Chunk] -> ([a] -> b) -> (Suggest, Either ArgError b)
chompMultipleHelp parser revArgs suggest chunks func =
  case chunks of
    [] ->
      (suggest, Right (func (reverse revArgs)))

    Chunk index string : otherChunks ->
      case tryToParse suggest parser index string of
        (s1, Left expectation) ->
          (s1, Left (ArgBad string expectation))

        (s1, Right arg) ->
          chompMultipleHelp parser (arg:revArgs) s1 otherChunks func



-- REQUIRED ARGS


chompRequiredArgs :: Int -> RequiredArgs a -> Chomper ArgError a
chompRequiredArgs numChunks args =
  case args of
    Done value ->
      return value

    Required funcArgs argParser ->
      do  func <- chompRequiredArgs numChunks funcArgs
          arg <- chompArg numChunks argParser
          return (func arg)


chompArg :: Int -> Parser a -> Chomper ArgError a
chompArg numChunks parser@(Parser singular _ _ _ toExamples) =
  Chomper $ \suggest chunks ok err ->
    case chunks of
      [] ->
        let
          newSuggest = makeSuggestion suggest (suggestArg parser numChunks)
          theError = ArgMissing (Expectation singular (toExamples ""))
        in
        err newSuggest theError

      Chunk index string : otherChunks ->
        case tryToParse suggest parser index string of
          (newSuggest, Left expectation) ->
            err newSuggest (ArgBad string expectation)

          (newSuggest, Right arg) ->
            ok newSuggest otherChunks arg


suggestArg :: Parser a -> Int -> Int -> Maybe (IO [String])
suggestArg (Parser _ _ _ toSuggestions _) numChunks targetIndex =
  if numChunks <= targetIndex then
    Just (toSuggestions "")
  else
    Nothing



-- PARSER


tryToParse :: Suggest -> Parser a -> Int -> String -> (Suggest, Either Expectation a)
tryToParse suggest (Parser singular _ parse toSuggestions toExamples) index string =
  let
    newSuggest =
      makeSuggestion suggest $ \targetIndex ->
        if index == targetIndex then Just (toSuggestions string) else Nothing

    outcome =
      case parse string of
        Nothing ->
          Left (Expectation singular (toExamples string))

        Just value ->
          Right value
  in
  (newSuggest, outcome)



-- FLAGS


chompFlags :: Flags a -> Chomper FlagError a
chompFlags flags =
  do  value <- chompFlagsHelp flags
      checkForUnknownFlags flags
      return value


chompFlagsHelp :: Flags a -> Chomper FlagError a
chompFlagsHelp flags =
  case flags of
    FDone value ->
      return value

    FMore funcFlags argFlag ->
      do  func <- chompFlagsHelp funcFlags
          arg <- chompFlag argFlag
          return (func arg)



-- FLAG


chompFlag :: Flag a -> Chomper FlagError a
chompFlag flag =
  case flag of
    OnOff flagName _ ->
      chompOnOffFlag flagName

    Flag flagName parser _ ->
      chompNormalFlag flagName parser


chompOnOffFlag :: String -> Chomper FlagError Bool
chompOnOffFlag flagName =
  Chomper $ \suggest chunks ok err ->
    case findFlag flagName chunks of
      Nothing ->
        ok suggest chunks False

      Just (FoundFlag before value after) ->
        case value of
          DefNope ->
            ok suggest (before ++ after) True

          Possibly chunk ->
            ok suggest (before ++ chunk : after) True

          Definitely _ string ->
            err suggest (FlagWithValue flagName string)


chompNormalFlag :: String -> Parser a -> Chomper FlagError (Maybe a)
chompNormalFlag flagName parser@(Parser singular _ _ _ toExamples) =
  Chomper $ \suggest chunks ok err ->
    case findFlag flagName chunks of
      Nothing ->
        ok suggest chunks Nothing

      Just (FoundFlag before value after) ->
        let
          attempt index string =
            case tryToParse suggest parser index string of
              (newSuggest, Left expectation) ->
                err newSuggest (FlagWithBadValue flagName string expectation)

              (newSuggest, Right flagValue) ->
                ok newSuggest (before ++ after) (Just flagValue)
        in
        case value of
          Definitely index string ->
            attempt index string

          Possibly (Chunk index string) ->
            attempt index string

          DefNope ->
            err suggest (FlagWithNoValue flagName (Expectation singular (toExamples "")))



-- FIND FLAG


data FoundFlag =
  FoundFlag
    { _before :: [Chunk]
    , _value :: Value
    , _after :: [Chunk]
    }


data Value
  = Definitely Int String
  | Possibly Chunk
  | DefNope


findFlag :: String -> [Chunk] -> Maybe FoundFlag
findFlag flagName chunks =
  findFlagHelp [] ("--" ++ flagName) ("--" ++ flagName ++ "=") chunks


findFlagHelp :: [Chunk] -> String -> String -> [Chunk] -> Maybe FoundFlag
findFlagHelp revPrev loneFlag flagPrefix chunks =
  let
    succeed value after =
      Just (FoundFlag (reverse revPrev) value after)

    deprefix string =
      drop (length flagPrefix) string
  in
  case chunks of
    [] ->
      Nothing

    chunk@(Chunk index string) : rest ->
      if List.isPrefixOf flagPrefix string then
        succeed (Definitely index (deprefix string)) rest

      else if string /= loneFlag then
        findFlagHelp (chunk:revPrev) loneFlag flagPrefix rest

      else
        case rest of
          [] ->
            succeed DefNope []

          argChunk@(Chunk _ potentialArg) : restOfRest ->
            if List.isPrefixOf "-" potentialArg then
              succeed DefNope rest
            else
              succeed (Possibly argChunk) restOfRest



-- CHECK FOR UNKNOWN FLAGS


checkForUnknownFlags :: Flags a -> Chomper FlagError ()
checkForUnknownFlags flags =
  Chomper $ \suggest chunks ok err ->
    case filter startsWithDash chunks of
      [] ->
        ok suggest chunks ()

      unknownFlags@(Chunk _ unknownFlag : _) ->
        err
          (makeSuggestion suggest (suggestFlag unknownFlags flags))
          (FlagUnknown unknownFlag flags)


suggestFlag :: [Chunk] -> Flags a -> Int -> Maybe (IO [String])
suggestFlag unknownFlags flags targetIndex =
  case unknownFlags of
    [] ->
      Nothing

    Chunk index string : otherUnknownFlags ->
      if index == targetIndex then
        Just (return (filter (List.isPrefixOf string) (getFlagNames flags [])))
      else
        suggestFlag otherUnknownFlags flags targetIndex


startsWithDash :: Chunk -> Bool
startsWithDash (Chunk _ string) =
  List.isPrefixOf "-" string


getFlagNames :: Flags a -> [String] -> [String]
getFlagNames flags names =
  case flags of
    FDone _ ->
      "--help" : names

    FMore subFlags flag ->
      getFlagNames subFlags (getFlagName flag : names)


getFlagName :: Flag a -> String
getFlagName flag =
  case flag of
    Flag name _ _ ->
      "--" ++ name

    OnOff name _ ->
      "--" ++ name



-- CHOMPER INSTANCES


instance Functor (Chomper x) where
  fmap func (Chomper chomper) =
    Chomper $ \i w ok err ->
      let
        ok1 s1 cs1 value =
          ok s1 cs1 (func value)
      in
      chomper i w ok1 err


instance Applicative (Chomper x) where
  pure value =
    Chomper $ \ss cs ok _ ->
      ok ss cs value

  (<*>) (Chomper funcChomper) (Chomper argChomper) =
    Chomper $ \s cs ok err ->
      let
        ok1 s1 cs1 func =
          let
            ok2 s2 cs2 value =
              ok s2 cs2 (func value)
          in
          argChomper s1 cs1 ok2 err
      in
      funcChomper s cs ok1 err


instance Monad (Chomper x) where
  return = pure

  (>>=) (Chomper aChomper) callback =
    Chomper $ \s cs ok err ->
      let
        ok1 s1 cs1 a =
          case callback a of
            Chomper bChomper -> bChomper s1 cs1 ok err
      in
      aChomper s cs ok1 err
