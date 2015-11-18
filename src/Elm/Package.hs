module Elm.Package where

import Data.Aeson
import Data.Binary
import qualified Data.Char as Char
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Text as T
import System.FilePath ((</>))


-- PACKGE NAMES

data Name = Name
    { user :: String
    , project :: String
    }
    deriving (Eq, Ord, Show)


type Package = (Name, Version)


dummyName :: Name
dummyName =
    Name "user" "project"


coreName :: Name
coreName =
  Name "elm-lang" "core"


toString :: Name -> String
toString name =
    user name ++ "/" ++ project name


toUrl :: Name -> String
toUrl name =
    user name ++ "/" ++ project name


toFilePath :: Name -> FilePath
toFilePath name =
    user name </> project name


fromString :: String -> Either String Name
fromString string =
    case break (=='/') string of
      ( user, '/' : project ) ->
          if null user then
              Left "You did not provide a user name (user/project)"

          else if null project then
              Left "You did not provide a project name (user/project)"

          else if all (/='/') project then
              Name user <$> validate project

          else
              Left "Expecting only one slash, separating the user and project name (user/project)"

      _ ->
          Left "There should be a slash separating the user and project name (user/project)"


whitelistedUppercaseName :: String -> Bool
whitelistedUppercaseName name =
  -- These packages were uploaded to package.elm-lang.org before version 0.16,
  -- when uppercase letters were disallowed in package names.
  -- They should be considered deprecated and removed from this list once users
  -- migrate to using the lowercase versions of the names.
  let whitelist =
          [ "Apanatshka/elm-list-ndet"
          , "Apanatshka/elm-signal-extra"
          , "Bogdanp/elm-combine"
          , "Dandandan/Easing"
          , "Dandandan/parser"
          , "JoeyEremondi/LoadAssets"
          , "JoeyEremondi/elm-MultiDimArray"
          , "JoeyEremondi/elm-SafeLists"
          , "JoeyEremondi/elm-typenats"
          , "JustusAdam/elm-path"
          , "NoRedInk/elm-check"
          , "NoRedInk/elm-lazy-list"
          , "NoRedInk/elm-rails"
          , "NoRedInk/elm-random-extra"
          , "NoRedInk/elm-shrink"
          , "NoRedInk/elm-string-extra"
          , "NoRedInk/elm-task-extra"
          , "TheSeamau5/GraphicsEngine"
          , "TheSeamau5/elm-check"
          , "TheSeamau5/elm-history"
          , "TheSeamau5/elm-html-decoder"
          , "TheSeamau5/elm-lazy-list"
          , "TheSeamau5/elm-material-icons"
          , "TheSeamau5/elm-quadtree"
          , "TheSeamau5/elm-random-extra"
          , "TheSeamau5/elm-rosetree"
          , "TheSeamau5/elm-router"
          , "TheSeamau5/elm-shrink"
          , "TheSeamau5/elm-spring"
          , "TheSeamau5/elm-task-extra"
          , "TheSeamau5/elm-undo-redo"
          , "TheSeamau5/flex-html"
          , "TheSeamau5/selection-list"
          , "TheSeamau5/typographic-scale"
          , "ThomasWeiser/elmfire"
          , "ThomasWeiser/elmfire-extra"
          , "adam-r-kowalski/Elm-Css"
          , "deadfoxygrandpa/Elm-Test"
          , "heyLu/elm-format-date"
          , "jterbraak/DateOp"
          , "mgold/Elm-Align-Distribute"
          , "mgold/Elm-Format-String"
          , "mgold/Elm-Multiset"
          , "mgold/Elm-Random-Sampling"
          , "thSoft/ElmCache"
          , "thSoft/ExternalStorage"
          , "uehaj/IntRange"
          ]
  in
      List.any (\x -> x == name) whitelist

validate :: String -> Either String String
validate str =
  if elem ('-','-') (zip str (tail str)) then
      Left "There is a double dash -- in your package name. It must be a single dash."

  else if elem '_' str then
      Left "Underscores are not allowed in package names."

  else if any Char.isUpper str && not (whitelistedUppercaseName str) then
      Left "Upper case characters are not allowed in package names."

  else if not (Char.isLetter (head str)) then
      Left "Package names must start with a letter."

  else
      Right str


instance Binary Name where
    get = Name <$> get <*> get
    put (Name user project) =
        do  put user
            put project


instance FromJSON Name where
    parseJSON (String text) =
        let
          string = T.unpack text
        in
          case fromString string of
            Left msg ->
                fail ("Ran into an invalid package name: " ++ string ++ "\n\n" ++ msg)

            Right name ->
                return name

    parseJSON _ =
        fail "Project name must be a string."


instance ToJSON Name where
    toJSON name =
        toJSON (toString name)


-- PACKAGE VERSIONS

data Version = Version
    { _major :: Int
    , _minor :: Int
    , _patch :: Int
    }
    deriving (Eq, Ord)


initialVersion :: Version
initialVersion =
    Version 1 0 0

dummyVersion :: Version
dummyVersion =
    Version 0 0 0


bumpPatch :: Version -> Version
bumpPatch (Version major minor patch) =
    Version major minor (patch + 1)

bumpMinor :: Version -> Version
bumpMinor (Version major minor _patch) =
    Version major (minor + 1) 0

bumpMajor :: Version -> Version
bumpMajor (Version major _minor _patch) =
    Version (major + 1) 0 0


-- FILTERING

filterLatest :: (Ord a) => (Version -> a) -> [Version] -> [Version]
filterLatest characteristic versions =
    map last (List.groupBy ((==) `on` characteristic) (List.sort versions))


majorAndMinor :: Version -> (Int,Int)
majorAndMinor (Version major minor _patch) =
    (major, minor)


-- CONVERSIONS

versionToString :: Version -> String
versionToString (Version major minor patch) =
    show major ++ "." ++ show minor ++ "." ++ show patch


versionFromString :: String -> Either String Version
versionFromString string =
    case splitNumbers string of
      Just [major, minor, patch] ->
          Right (Version major minor patch)
      _ ->
          Left "Must have format MAJOR.MINOR.PATCH (e.g. 1.0.2)"
  where
    splitNumbers :: String -> Maybe [Int]
    splitNumbers ns =
        case span Char.isDigit ns of
          ("", _) ->
              Nothing

          (numbers, []) ->
              Just [ read numbers ]

          (numbers, '.':rest) ->
              (read numbers :) <$> splitNumbers rest

          _ ->
              Nothing


instance Binary Version where
    get = Version <$> get <*> get <*> get
    put (Version major minor patch) =
        do put major
           put minor
           put patch


instance FromJSON Version where
    parseJSON (String text) =
        let string = T.unpack text in
        case versionFromString string of
          Right v ->
              return v

          Left problem ->
              fail $ unlines
                 [ "Ran into an invalid version number: " ++ string
                 , problem
                 ]

    parseJSON _ =
        fail "Version number must be stored as a string."


instance ToJSON Version where
    toJSON version =
        toJSON (versionToString version)

