{-# LANGUAGE OverloadedStrings #-}
module Terminal.Helpers
  ( version
  , elmFile
  , package
  )
  where


import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Utf8 as Utf8
import qualified System.FilePath as FP

import Terminal (Parser(..))
import qualified Deps.Registry as Registry
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Parse.Primitives as P
import qualified Stuff
import qualified Reporting.Suggest as Suggest



-- VERSION


version :: Parser V.Version
version =
  Parser
    { _singular = "version"
    , _plural = "versions"
    , _parser = parseVersion
    , _suggest = suggestVersion
    , _examples = return . exampleVersions
    }


parseVersion :: String -> Maybe V.Version
parseVersion chars =
  case P.fromByteString V.parser (,) (BS_UTF8.fromString chars) of
    Right vsn -> Just vsn
    Left _    -> Nothing


suggestVersion :: String -> IO [String]
suggestVersion _ =
  return []


exampleVersions :: String -> [String]
exampleVersions chars =
  let
    chunks = map Utf8.toChars (Utf8.split 0x2E {-.-} (Utf8.fromChars chars))
    isNumber cs = not (null cs) && all Char.isDigit cs
  in
  if all isNumber chunks then
    case chunks of
      [x]     -> [ x ++ ".0.0" ]
      [x,y]   -> [ x ++ "." ++ y ++ ".0" ]
      x:y:z:_ -> [ x ++ "." ++ y ++ "." ++ z ]
      _       -> ["1.0.0", "2.0.3"]

  else
    ["1.0.0", "2.0.3"]



-- ELM FILE


elmFile :: Parser FilePath
elmFile =
  Parser
    { _singular = "elm file"
    , _plural = "elm files"
    , _parser = parseElmFile
    , _suggest = \_ -> return []
    , _examples = exampleElmFiles
    }


parseElmFile :: String -> Maybe FilePath
parseElmFile chars =
  if FP.takeExtension chars == ".elm" then
    Just chars
  else
    Nothing


exampleElmFiles :: String -> IO [String]
exampleElmFiles _ =
  return ["Main.elm","src/Main.elm"]



-- PACKAGE


package :: Parser Pkg.Name
package =
  Parser
    { _singular = "package"
    , _plural = "packages"
    , _parser = parsePackage
    , _suggest = suggestPackages
    , _examples = examplePackages
    }


parsePackage :: String -> Maybe Pkg.Name
parsePackage chars =
  case P.fromByteString Pkg.parser (,) (BS_UTF8.fromString chars) of
    Right pkg -> Just pkg
    Left _    -> Nothing


suggestPackages :: String -> IO [String]
suggestPackages given =
  do  cache <- Stuff.getPackageCache
      maybeRegistry <- Registry.read cache
      return $
        case maybeRegistry of
          Nothing ->
            []

          Just (Registry.Registry _ versions) ->
            filter (List.isPrefixOf given) $
              map Pkg.toChars (Map.keys versions)


examplePackages :: String -> IO [String]
examplePackages given =
  do  cache <- Stuff.getPackageCache
      maybeRegistry <- Registry.read cache
      return $
        case maybeRegistry of
          Nothing ->
            [ "elm/json"
            , "elm/http"
            , "elm/random"
            ]

          Just (Registry.Registry _ versions) ->
            map Pkg.toChars $ take 4 $
              Suggest.sort given Pkg.toChars (Map.keys versions)
