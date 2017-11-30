{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Repl
  ( Entry(..)
  , parseEntry
  )
  where


import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Source as Src
import Parse.Primitives
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Pattern as Pattern
import qualified Reporting.Annotation as A



-- ENTRY


data Entry
  = Import String Text
  | Type String Text
  | Def (Maybe String) Text
  | Other Text
  | Annotation
  | Port



-- PARSE


parseEntry :: String -> Entry
parseEntry rawEntry =
  let
    source =
      Text.pack rawEntry
  in
    case run (entryParser source) (Utf8.fromString rawEntry) of
      Right entry ->
        entry

      Left _ ->
        Other source


entryParser :: Text -> Parser Entry
entryParser source =
  oneOf
    [ do  Keyword.import_
          spaces
          name <- Var.moduleName
          return (Import (Text.unpack name) source)

    , do  Keyword.port_
          return Port

    , do  Keyword.type_
          spaces
          oneOf
            [ do  Keyword.alias_
                  spaces
            , return ()
            ]
          name <- Var.upper
          return (Type (Text.unpack name) source)

    , do  root <- Pattern.term
          spaces
          case A.toValue root of
            Src.PVar name ->
              oneOf
                [ do  Symbol.hasType
                      return Annotation
                , do  chompArgs
                      return (Def (Just (Text.unpack name)) source)
                ]

            _ ->
              do  Symbol.equals
                  return (Def Nothing source)
    ]


chompArgs :: Parser ()
chompArgs =
  oneOf
    [ do  Pattern.term
          spaces
          chompArgs
    , do  Symbol.equals
          return ()
    ]
