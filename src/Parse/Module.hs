{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module where

import Data.Text (Text)

import qualified AST.Module as Module
import qualified AST.Variable as Var
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- MODULE HEADER


header :: Parser (Module.Header [Module.UserImport])
header =
  do  start <- getPosition
      freshLine E.ModuleDecl
      end <- getPosition
      oneOf
        [ fullHeader end
        , Module.defaultHeader start end <$> chompImports []
        ]



-- FULL HEADER


fullHeader :: R.Position -> Parser (Module.Header [Module.UserImport])
fullHeader start =
  do  pushContext start E.Module
      tag <- sourceTag
      spaces
      name <- qualifiedCapVar
      spaces
      settings <- effectSettings
      hint E.Exposing $ keyword "exposing"
      spaces
      exports <- listing (addLocation listingValue)
      popContext ()
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


maybeDocComment :: Parser (A.Located (Maybe Text))
maybeDocComment =
  do  oldEnd <- getPosition
      freshLine E.DocCommentDecl
      newStart <- getPosition
      oneOf
        [ do  doc <- docComment
              end <- getPosition
              freshLine E.ImportDecl
              return (A.at newStart end (Just doc))
        , return (A.at oldEnd newStart Nothing)
        ]



-- IMPORTS


chompImports :: [Module.UserImport] -> Parser [Module.UserImport]
chompImports imports =
  oneOf
    [ do  start <- getPosition
          keyword "import"
          pushContext start E.Import
          spaces
          name <- qualifiedCapVar
          end <- getPosition
          pos <- whitespace
          oneOf
            [ do  checkFreshLine E.ImportDecl pos
                  let userImport = method start end name Nothing Var.closedListing
                  popContext ()
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
        [ do  checkFreshLine E.ImportDecl pos
              let userImport = method start end name (Just alias) Var.closedListing
              popContext ()
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
      freshLine E.ImportDecl
      let userImport = method start end name maybeAlias exposed
      popContext ()
      chompImports (userImport:imports)


method :: R.Position -> R.Position -> Text -> Maybe Text -> Var.Listing Var.Value -> Module.UserImport
method start end name maybeAlias exposed =
  A.at start end ( name, Module.ImportMethod maybeAlias exposed )



-- LISTING


listing :: Parser a -> Parser (Var.Listing a)
listing parser =
  hint E.Listing $
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
          op <- infixOp
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


freshLine :: E.NextDecl -> Parser ()
freshLine nextDecl =
  do  pos <- whitespace
      checkFreshLine nextDecl pos

