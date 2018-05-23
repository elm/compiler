{-# LANGUAGE OverloadedStrings #-}
module Terminal.Args.Helpers
  ( version
  , elmFile
  , package
  )
  where


import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified System.FilePath as FP

import Terminal.Args (Parser(..))
import qualified Elm.Package as Pkg



-- VERSION


version :: Parser Pkg.Version
version =
  Parser
    { _singular = "version"
    , _plural = "versions"
    , _parser = parseVersion
    , _suggest = suggestVersion
    , _examples = return . exampleVersions
    }


parseVersion :: String -> Maybe Pkg.Version
parseVersion str =
  Pkg.versionFromText (Text.pack str)


suggestVersion :: String -> IO [String]
suggestVersion _ =
  return []


exampleVersions :: String -> [String]
exampleVersions string =
  let
    chunks = map Text.unpack (Text.splitOn "." (Text.pack string))
    isNumber str = not (null str) && all Char.isDigit str
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
parseElmFile string =
  if FP.takeExtension string == ".elm" then
    Just string
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
    Pkg.fromText (Text.pack string)


suggestPackages :: String -> IO [String]
suggestPackages _ =
  return []


examplePackages :: String -> IO [String]
examplePackages string =
  case Pkg.fromText (Text.pack string) of
    Left (_, suggestions@(_:_)) ->
      return suggestions

    _ ->
      return
        [ "elm/http"
        , "elm/json"
        , "elm-community/random-extra"
        ]
