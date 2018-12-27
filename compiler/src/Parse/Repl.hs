{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Repl
  ( Entry(..)
  , parseEntry
  )
  where


import qualified Data.ByteString.UTF8 as BS_UTF8
import Data.Name (Name)

import qualified AST.Source as Src
import qualified Parse.Primitives as P
import Parse.Utils



-- ENTRY


data Entry
  = Import Name (Maybe Name) Src.Exposing String
  | Type Name String
  | Def (Maybe Name) String
  | Other String
  | Annotation
  | Port



-- PARSE


parseEntry :: String -> Entry
parseEntry source =
  case P.fromByteString parser (BS_UTF8.fromString source) of
    P.Ok entry _ ->
      entry

    P.Err _ _ _ _ ->
      Other source


parser :: Parser Entry
parser =
  error "TODO parse the REPL stuff"

{-
  oneOf
    [
      do  Keyword.import_
          spaces
          name <- Var.moduleName
          alias <- tryAlias
          exposing <-
            oneOf
              [ do  spaces
                    Keyword.exposing_
                    spaces
                    Module.exposing
              , return (Src.Explicit [])
              ]
          return (Import name alias exposing source)
    ,
      do  Keyword.port_
          return Port
    ,
      do  Keyword.type_
          spaces
          oneOf
            [ do  Keyword.alias_
                  spaces
            , return ()
            ]
          name <- Var.upper
          return (Type name source)
    ,
      do  root <- Pattern.term
          spaces
          case A.toValue root of
            Src.PVar name ->
              oneOf
                [ do  word1 0x3A {-:-} E.XXX
                      return Annotation
                , do  chompArgs
                      return (Def (Just name) source)
                ]

            _ ->
              do  word1 0x3D {-=-} E.XXX
                  return (Def Nothing source)
    ]


chompArgs :: Parser ()
chompArgs =
  oneOf
    [ do  Pattern.term
          spaces
          chompArgs
    , do  word1 0x3D {-=-} E.XXX
          return ()
    ]


tryAlias :: Parser (Maybe Name)
tryAlias =
  oneOf
    [ try $
        do  spaces
            Keyword.as_
            spaces
            Just <$> Var.upper
    , return Nothing
    ]
-}