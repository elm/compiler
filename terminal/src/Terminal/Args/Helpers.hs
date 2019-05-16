{-# LANGUAGE OverloadedStrings #-}
module Terminal.Args.Helpers
  ( version
  , elmFile
  , package
  )
  where


import qualified Data.Char as Char
import qualified Data.Utf8 as Utf8
import qualified System.FilePath as FP

import Terminal.Args (Parser(..))
import qualified Elm.Package as Pkg
import qualified Elm.Version as V



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
  V.fromString (Utf8.fromChars chars)


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
parsePackage string =
  either (const Nothing) Just $
    Pkg.fromString (Utf8.fromChars string)


suggestPackages :: String -> IO [String]
suggestPackages _ =
  return []


examplePackages :: String -> IO [String]
examplePackages string =
  case Pkg.fromString (Utf8.fromChars string) of
    Left _ ->
      return (error "TODO need to make suggestions")

    _ ->
      return
        [ "elm/http"
        , "elm/json"
        , "elm-community/random-extra"
        ]
