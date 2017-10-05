{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module
  ( header
  , kernelHeader
  )
  where

import Data.Text (Text)

import qualified AST.Exposing as Exposing
import qualified AST.Module as Module
import Parse.Helpers
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- MODULE HEADER


header :: Parser (Module.Header [Module.UserImport])
header =
  do  freshLine
      Module.Header <$> maybeHeaderDecl <*> chompImports []


kernelHeader :: Parser [Module.UserImport]
kernelHeader =
  do  headerOpen
      freshLine
      imports <- chompImports []
      headerClose
      return imports


{-# NOINLINE headerOpen #-}
headerOpen :: Parser ()
headerOpen =
  symbol "/*"


{-# NOINLINE headerClose #-}
headerClose :: Parser ()
headerClose =
  symbol "*/"



-- HEADER DECL


maybeHeaderDecl :: Parser (Maybe Module.HeaderDecl)
maybeHeaderDecl =
  oneOf
    [ Just <$> headerDecl
    , return Nothing
    ]


headerDecl :: Parser Module.HeaderDecl
headerDecl =
  do  start <- getPosition
      pushContext start E.Module
      tag <- sourceTag
      spaces
      name <- qualifiedCapVar
      spaces
      settings <- effectSettings
      hint E.Exposing $ keyword "exposing"
      spaces
      exports <- exposing exposingEntry
      popContext ()
      docs <- maybeDocComment
      return (Module.HeaderDecl tag name exports settings docs)



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
      freshLine
      newStart <- getPosition
      oneOf
        [ do  doc <- docComment
              end <- getPosition
              freshLine
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
          name <- addLocation qualifiedCapVar
          end <- getPosition
          pos <- whitespace
          oneOf
            [ do  checkFreshLine pos
                  let userImport = method start end name Nothing Exposing.closed
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


chompAs :: R.Position -> A.Located Text -> [Module.UserImport] -> Parser [Module.UserImport]
chompAs start name imports =
  do  keyword "as"
      spaces
      alias <- capVar
      end <- getPosition
      pos <- whitespace
      oneOf
        [ do  checkFreshLine pos
              let userImport = method start end name (Just alias) (Exposing.Explicit [])
              popContext ()
              chompImports (userImport:imports)
        , do  checkSpace pos
              chompExposing start name (Just alias) imports
        ]


chompExposing :: R.Position -> A.Located Text -> Maybe Text -> [Module.UserImport] -> Parser [Module.UserImport]
chompExposing start name maybeAlias imports =
  do  keyword "exposing"
      spaces
      exposed <- exposing exposingEntry
      end <- getPosition
      freshLine
      let userImport = method start end name maybeAlias exposed
      popContext ()
      chompImports (userImport:imports)


method :: R.Position -> R.Position -> A.Located Text -> Maybe Text -> Exposing.Raw -> Module.UserImport
method start end name maybeAlias exposed =
  A.at start end ( name, Module.ImportMethod maybeAlias exposed )



-- LISTING


exposing :: Parser a -> Parser (Exposing.Exposing a)
exposing parser =
  hint E.Listing $
  do  leftParen
      spaces
      oneOf
        [ do  dot
              dot
              spaces
              rightParen
              return Exposing.Open
        , do  value <- addLocation parser
              spaces
              exposingHelp parser [value]
        ]


exposingHelp :: Parser a -> [A.Located a] -> Parser (Exposing.Exposing a)
exposingHelp parser values =
  oneOf
    [ do  comma
          spaces
          value <- addLocation parser
          spaces
          exposingHelp parser (value:values)
    , do  rightParen
          return (Exposing.Explicit (reverse values))
    ]


exposingEntry :: Parser Exposing.Entry
exposingEntry =
  oneOf
    [ Exposing.Lower <$> lowVar
    , do  leftParen
          op <- infixOp
          rightParen
          return (Exposing.Lower op)
    , do  name <- capVar
          spaces
          Exposing.Upper name <$>
            oneOf
              [ Just <$> exposing capVar
              , return Nothing
              ]
    ]



-- FRESH LINES


freshLine :: Parser ()
freshLine =
  checkFreshLine =<< whitespace

