{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module where

import Data.Text (Text)

import qualified AST.Module as Module
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- MODULE HEADER


header :: Parser (Module.Header [Module.UserImport])
header =
  do  freshLine
      oneOf
        [ fullHeader
        , Module.defaultHeader <$> chompImports []
        ]



-- FULL HEADER


fullHeader :: Parser (Module.Header [Module.UserImport])
fullHeader =
  do  tag <- sourceTag
      spaces
      name <- qualifiedCapVar -- TODO <?> "the name of this module"
      spaces
      settings <- effectSettings
      keyword "exposing" -- TODO <?> "something like `exposing (..)` which replaced `where` in 0.17"
      spaces
      exports <- listing (addLocation listingValue)
      freshLine
      docs <- maybeDocComment
      imports <- chompImports []
      return (Module.Header tag name exports settings docs imports)



-- MODULE TAGS - module, effect module, port module


sourceTag :: Parser Module.SourceTag
sourceTag =
  oneOf
    [
      do  keyword "module"
          return Module.Normal
    ,
      do  start <- getPosition
          keyword "port"
          spaces
          keyword "module"
          end <- getPosition
          return (Module.Port (R.Region start end))
    ,
      do  start <- getPosition
          keyword "effect"
          spaces
          keyword "module"
          end <- getPosition
          return (Module.Effect (R.Region start end))
    ]



-- EFFECTS - where { command = MyCmd }


effectSettings :: Parser Module.SourceSettings
effectSettings =
  oneOf
    [ do  keyword "where"
          spaces
          start <- getPosition
          leftCurly
          spaces
          entry <- setting
          spaces
          effectSettingsHelp start [entry]
    , return Module.emptySettings
    ]


effectSettingsHelp :: R.Position -> [(A.Located Text, A.Located Text)] -> Parser Module.SourceSettings
effectSettingsHelp start entries =
  oneOf
    [ do  comma
          spaces
          entry <- setting
          spaces
          effectSettingsHelp start (entry:entries)
    , do  rightCurly
          end <- getPosition
          spaces
          return (A.at start end entries)
    ]


setting :: Parser (A.Located Text, A.Located Text)
setting =
  do  name <- addLocation lowVar
      spaces
      equals
      spaces
      tipe <- addLocation capVar
      return (name, tipe)



-- DOC COMMENTS


maybeDocComment :: Parser (Maybe (A.Located Text))
maybeDocComment =
  oneOf
    [ do  doc <- addLocation docComment
          freshLine
          return (Just doc)
    , return Nothing
    ]



-- IMPORTS


chompImports :: [Module.UserImport] -> Parser [Module.UserImport]
chompImports imports =
  oneOf
    [ do  start <- getPosition
          keyword "import"
          spaces
          name <- qualifiedCapVar
          end <- getPosition
          pos <- whitespace
          oneOf
            [ do  checkFreshline pos
                  let userImport = method start end name Nothing Var.closedListing
                  chompImports (userImport:imports)
            , do  checkSpace pos
                  oneOf
                    [ chompAs start name imports
                    , chompExposing start name Nothing imports
                    ]
            ]
    , return (reverse imports)
    ]


chompAs :: R.Position -> Text -> [Module.UserImport] -> Parser [Module.UserImport]
chompAs start name imports =
  do  keyword "as"
      spaces
      alias <- capVar
      end <- getPosition
      pos <- whitespace
      oneOf
        [ do  checkFreshline pos
              let userImport = method start end name (Just alias) Var.closedListing
              chompImports (userImport:imports)
        , do  checkSpace pos
              chompExposing start name (Just alias) imports
        ]


chompExposing :: R.Position -> Text -> Maybe Text -> [Module.UserImport] -> Parser [Module.UserImport]
chompExposing start name maybeAlias imports =
  do  keyword "exposing"
      spaces
      exposed <- listing listingValue
      end <- getPosition
      freshLine
      let userImport = method start end name maybeAlias exposed
      chompImports (userImport:imports)


method :: R.Position -> R.Position -> Text -> Maybe Text -> Var.Listing Var.Value -> Module.UserImport
method start end name maybeAlias exposed =
  A.at start end ( name, Module.ImportMethod maybeAlias exposed )



-- LISTING


listing :: Parser a -> Parser (Var.Listing a)
listing parser =
  expecting "a listing of values and types to expose, like (..)" $
  do  leftParen
      spaces
      oneOf
        [ do  dot
              dot
              spaces
              rightParen
              return (Var.Listing [] True)
        , do  value <- parser
              spaces
              listingHelp parser [value]
        ]


listingHelp :: Parser a -> [a] -> Parser (Var.Listing a)
listingHelp parser values =
  oneOf
    [ do  comma
          spaces
          value <- parser
          spaces
          listingHelp parser (value:values)
    , do  rightParen
          return (Var.Listing (reverse values) False)
    ]


listingValue :: Parser Var.Value
listingValue =
  oneOf
    [ Var.Value <$> lowVar
    , do  leftParen
          spaces
          op <- infixOp
          spaces
          rightParen
          return (Var.Value op)
    , do  name <- capVar
          spaces
          oneOf
            [ Var.Union name <$> listing capVar
            , return (Var.Alias name)
            ]
    ]



-- FRESH LINES


freshLine :: Parser ()
freshLine =
  do  pos <- whitespace
      checkFreshline pos

