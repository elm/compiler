{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Repl
  ( Entry(..)
  , parseEntry
  )
  where


import qualified Data.Text as Text
import Data.Text (Text)

import Parse.Helpers
import qualified Parse.Pattern as Pattern

import qualified AST.Pattern as P
import qualified Reporting.Annotation as A



-- ENTRY


data Entry
  = Import String Text
  | Type String Text
  | Def (Maybe String) Text
  | Other Text
  | Annotation
  | Port
  | Infix



-- PARSE


parseEntry :: String -> Entry
parseEntry rawEntry =
  let
    source =
      Text.pack rawEntry
  in
    case run (entryParser source) source of
      Right entry ->
        entry

      Left _ ->
        Other source


entryParser :: Text -> Parser Entry
entryParser source =
  oneOf
    [ do  keyword "import"
          spaces
          name <- qualifiedCapVar
          return (Import (Text.unpack name) source)

    , do  oneOf (map keyword ["infix","infixl","infixr"])
          return Infix

    , do  keyword "port"
          return Port

    , do  keyword "type"
          spaces
          oneOf
            [ do  keyword "alias"
                  spaces
            , return ()
            ]
          name <- capVar
          return (Type (Text.unpack name) source)

    , do  root <- Pattern.term
          spaces
          case A.drop root of
            P.Var name ->
              oneOf
                [ do  hasType
                      return Annotation
                , do  chompArgs
                      return (Def (Just (Text.unpack name)) source)
                ]

            _ ->
              do  chompArgs
                  return (Def Nothing source)
    ]


chompArgs :: Parser ()
chompArgs =
  oneOf
    [ do  Pattern.term
          spaces
          chompArgs
    , do  equals
          return ()
    ]
