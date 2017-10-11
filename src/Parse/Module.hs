{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module
  ( header
  , kernelHeader
  )
  where


import qualified Data.ByteString as B
import Data.Text (Text)

import qualified AST.Exposing as Exposing
import qualified AST.Module as Module
import Parse.Primitives (Parser, oneOf)
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Primitives.Whitespace as W
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
  do  Symbol.jsMultiCommentOpen
      freshLine
      imports <- chompImports []
      Symbol.jsMultiCommentClose
      return imports



-- HEADER DECL


maybeHeaderDecl :: Parser (Maybe Module.HeaderDecl)
maybeHeaderDecl =
  oneOf
    [ Just <$> headerDecl
    , return Nothing
    ]


headerDecl :: Parser Module.HeaderDecl
headerDecl =
  do  start <- P.getPosition
      P.pushContext start E.Module
      tag <- sourceTag
      P.spaces
      name <- Var.moduleName
      P.spaces
      settings <- effectSettings
      P.hint E.Exposing $ Keyword.exposing_
      P.spaces
      exports <- exposing exposingEntry
      P.popContext ()
      docs <- maybeDocComment
      return (Module.HeaderDecl tag name exports settings docs)



-- MODULE TAGS - module, effect module, port module


sourceTag :: Parser Module.SourceTag
sourceTag =
  oneOf
    [
      do  Keyword.module_
          return Module.Normal
    ,
      do  start <- P.getPosition
          Keyword.port_
          P.spaces
          Keyword.module_
          end <- P.getPosition
          return (Module.Port (R.Region start end))
    ,
      do  start <- P.getPosition
          Keyword.effect_
          P.spaces
          Keyword.module_
          end <- P.getPosition
          return (Module.Effect (R.Region start end))
    ]



-- EFFECTS - where { command = MyCmd }


effectSettings :: Parser Module.SourceSettings
effectSettings =
  oneOf
    [ do  Keyword.where_
          P.spaces
          start <- P.getPosition
          Symbol.leftCurly
          P.spaces
          entry <- setting
          P.spaces
          effectSettingsHelp start [entry]
    , return Module.emptySettings
    ]


effectSettingsHelp :: R.Position -> [(A.Located Text, A.Located Text)] -> Parser Module.SourceSettings
effectSettingsHelp start entries =
  oneOf
    [ do  Symbol.comma
          P.spaces
          entry <- setting
          P.spaces
          effectSettingsHelp start (entry:entries)
    , do  Symbol.rightCurly
          end <- P.getPosition
          P.spaces
          return (A.at start end entries)
    ]


setting :: Parser (A.Located Text, A.Located Text)
setting =
  do  name <- P.addLocation Var.lower
      P.spaces
      Symbol.equals
      P.spaces
      tipe <- P.addLocation Var.upper
      return (name, tipe)



-- DOC COMMENTS


maybeDocComment :: Parser (A.Located (Maybe B.ByteString))
maybeDocComment =
  do  oldEnd <- P.getPosition
      freshLine
      newStart <- P.getPosition
      oneOf
        [ do  doc <- W.docComment
              end <- P.getPosition
              freshLine
              return (A.at newStart end (Just doc))
        , return (A.at oldEnd newStart Nothing)
        ]



-- IMPORTS


chompImports :: [Module.UserImport] -> Parser [Module.UserImport]
chompImports imports =
  oneOf
    [ do  start <- P.getPosition
          Keyword.import_
          P.pushContext start E.Import
          P.spaces
          name <- P.addLocation Var.moduleName
          end <- P.getPosition
          pos <- W.whitespace
          oneOf
            [ do  P.checkFreshLine pos
                  let userImport = method start end name Nothing Exposing.closed
                  P.popContext ()
                  chompImports (userImport:imports)
            , do  P.checkSpace pos
                  oneOf
                    [ chompAs start name imports
                    , chompExposing start name Nothing imports
                    ]
            ]
    , return (reverse imports)
    ]


chompAs :: R.Position -> A.Located Text -> [Module.UserImport] -> Parser [Module.UserImport]
chompAs start name imports =
  do  Keyword.as_
      P.spaces
      alias <- Var.upper
      end <- P.getPosition
      pos <- W.whitespace
      oneOf
        [ do  P.checkFreshLine pos
              let userImport = method start end name (Just alias) (Exposing.Explicit [])
              P.popContext ()
              chompImports (userImport:imports)
        , do  P.checkSpace pos
              chompExposing start name (Just alias) imports
        ]


chompExposing :: R.Position -> A.Located Text -> Maybe Text -> [Module.UserImport] -> Parser [Module.UserImport]
chompExposing start name maybeAlias imports =
  do  Keyword.exposing_
      P.spaces
      exposed <- exposing exposingEntry
      end <- P.getPosition
      freshLine
      let userImport = method start end name maybeAlias exposed
      P.popContext ()
      chompImports (userImport:imports)


method :: R.Position -> R.Position -> A.Located Text -> Maybe Text -> Exposing.Raw -> Module.UserImport
method start end name maybeAlias exposed =
  A.at start end ( name, Module.ImportMethod maybeAlias exposed )



-- LISTING


exposing :: Parser a -> Parser (Exposing.Exposing a)
exposing parser =
  P.hint E.Listing $
  do  Symbol.leftParen
      P.spaces
      oneOf
        [ do  Symbol.dot
              Symbol.dot
              P.spaces
              Symbol.rightParen
              return Exposing.Open
        , do  value <- P.addLocation parser
              P.spaces
              exposingHelp parser [value]
        ]


exposingHelp :: Parser a -> [A.Located a] -> Parser (Exposing.Exposing a)
exposingHelp parser values =
  oneOf
    [ do  Symbol.comma
          P.spaces
          value <- P.addLocation parser
          P.spaces
          exposingHelp parser (value:values)
    , do  Symbol.rightParen
          return (Exposing.Explicit (reverse values))
    ]


exposingEntry :: Parser Exposing.Entry
exposingEntry =
  oneOf
    [ Exposing.Lower <$> Var.lower
    , do  Symbol.leftParen
          op <- Symbol.binop
          Symbol.rightParen
          return (Exposing.Lower op)
    , do  name <- Var.upper
          P.spaces
          Exposing.Upper name <$>
            oneOf
              [ Just <$> exposing Var.upper
              , return Nothing
              ]
    ]



-- FRESH LINES


freshLine :: Parser ()
freshLine =
  P.checkFreshLine =<< W.whitespace

