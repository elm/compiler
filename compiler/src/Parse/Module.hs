{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module
  ( fromByteString
  , Header(..)
  , Effects(..)
  , chompHeader
  , chompImports
  , freshLine
  )
  where


import qualified Data.ByteString as B
import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Package as Pkg
import qualified Parse.Declaration as Decl
import qualified Parse.Keyword as Keyword
import qualified Parse.Symbol as Symbol
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives hiding (Parser, State, fromByteString)
import Parse.Utils
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Result as Result



-- FROM BYTE STRING


fromByteString :: Pkg.Name -> B.ByteString -> Result.Result i w E.Error Src.Module
fromByteString pkg source =
  let
    parser
      | pkg == Pkg.core  = core
      | Pkg.isKernel pkg = platform
      | otherwise        = normal
  in
  case P.fromByteString parser source of
    P.Ok outcome _ ->
      case outcome of
        Right modul -> Result.ok modul
        Left err -> Result.throw err

    P.Err row col ctx err ->
      Result.throw (E.ParseError row col ctx err)



-- MODULE


normal :: Parser (Either E.Error Src.Module)
normal =
  do  freshLine
      header <- chompHeader
      imports <- chompImports []
      decls <- chompDecls []
      endOfFile
      return (checkModule header (Imports.addDefaults imports) [] decls)


core :: Parser (Either E.Error Src.Module)
core =
  do  freshLine
      header <- chompHeader
      imports <- chompImports []
      binops <- chompBinops []
      decls <- chompDecls []
      endOfFile
      return (checkModule header imports binops decls)


platform :: Parser (Either E.Error Src.Module)
platform =
  do  freshLine
      header <- chompHeader
      imports <- chompImports []
      binops <- chompBinops []
      decls <- chompDecls []
      endOfFile
      return (checkModule header (Imports.addDefaults imports) binops decls)



-- CHECK MODULE


checkModule :: Maybe Header -> [Src.Import] -> [A.Located Src.Binop] -> [Decl.Decl] -> Either E.Error Src.Module
checkModule maybeHeader imports binops decls =
  let
    (Header name effects exports) = maybe (Header "Main" NoEffects (A.At A.one Src.Open)) id maybeHeader
    (values, unions, aliases, ports) = categorizeDecls [] [] [] [] decls
  in
  Src.Module name exports imports values unions aliases binops
    <$> checkEffects ports effects


checkEffects :: [Src.Port] -> Effects -> Either E.Error Src.Effects
checkEffects ports effects =
  case effects of
    NoEffects ->
      case ports of
        []  -> Right Src.NoEffects
        _:_ -> Left (error "TODO no ports")

    Ports region ->
      case ports of
        []  -> Left (error "TODO no ports in port module" region)
        _:_ -> Right (Src.Ports ports)

    Manager region manager ->
      case ports of
        []  -> Right (Src.Manager region manager)
        _:_ -> Left (error "TODO ports in effect manager")


categorizeDecls :: [A.Located Src.Value] -> [A.Located Src.Union] -> [A.Located Src.Alias] -> [Src.Port] -> [Decl.Decl] -> ( [A.Located Src.Value], [A.Located Src.Union], [A.Located Src.Alias], [Src.Port] )
categorizeDecls values unions aliases ports decls =
  case decls of
    [] ->
      (values, unions, aliases, ports)

    decl:otherDecls ->
      case decl of
        Decl.Value value -> categorizeDecls (value:values) unions aliases ports otherDecls
        Decl.Union union -> categorizeDecls values (union:unions) aliases ports otherDecls
        Decl.Alias alias -> categorizeDecls values unions (alias:aliases) ports otherDecls
        Decl.Port  port_ -> categorizeDecls values unions aliases (port_:ports) otherDecls



-- FRESH LINES


freshLine :: Parser ()
freshLine =
  checkFreshLine =<< whitespace


endOfFile :: Parser ()
endOfFile =
  P.Parser $ \state@(P.State pos end _ row col ctx) _ eok _ eerr ->
    if pos < end then
      eerr row col ctx E.EndOfFile
    else
      eok () state



-- CHOMP DECLARATIONS


chompDecls :: [Decl.Decl] -> Parser [Decl.Decl]
chompDecls decls =
  do  (decl, _, pos) <- Decl.declaration
      oneOf E.XXX
        [ do  checkFreshLine pos
              chompDecls (decl:decls)
        , return (reverse (decl:decls))
        ]


chompBinops :: [A.Located Src.Binop] -> Parser [A.Located Src.Binop]
chompBinops binops =
  oneOf E.XXX
    [ do  binop <- Decl.infix_
          chompBinops (binop:binops)
    , return binops
    ]



-- HEADER


data Header =
  Header Name.Name Effects (A.Located Src.Exposing)


data Effects
  = NoEffects
  | Ports A.Region
  | Manager A.Region Src.Manager


chompHeader :: Parser (Maybe Header)
chompHeader =
  do  start <- getPosition
      oneOf E.XXX
        [
          -- module MyThing exposing (..)
          do  Keyword.module_
              pushContext start E.Module
              spaces
              name <- Var.moduleName
              spaces
              Keyword.exposing_
              spaces
              exports <- addLocation exposing
              popContext ()
              freshLine
              return (Just (Header name NoEffects exports))
        ,
          -- port module MyThing exposing (..)
          do  Keyword.port_
              pushContext start E.Module
              spaces
              Keyword.module_
              end <- getPosition
              spaces
              name <- Var.moduleName
              spaces
              Keyword.exposing_
              spaces
              exports <- addLocation exposing
              popContext ()
              freshLine
              return (Just (Header name (Ports (A.Region start end)) exports))
        ,
          -- effect module MyThing where { command = MyCmd } exposing (..)
          do  Keyword.effect_
              pushContext start E.Module
              spaces
              Keyword.module_
              end <- getPosition
              spaces
              name <- Var.moduleName
              spaces
              Keyword.where_
              spaces
              manager <- chompManager
              spaces
              Keyword.exposing_
              spaces
              exports <- addLocation exposing
              popContext ()
              freshLine
              return (Just (Header name (Manager (A.Region start end) manager) exports))
        ,
          -- default header
          return Nothing
        ]


chompManager :: Parser Src.Manager
chompManager =
  do  word1 0x7B {- { -} E.XXX
      spaces
      oneOf E.XXX
        [ do  cmd <- chompCommand
              spaces
              oneOf E.XXX
                [ do  word1 0x7D {-}-} E.XXX
                      spaces
                      return (Src.Cmd cmd)
                , do  word1 0x2C {-,-} E.XXX
                      spaces
                      sub <- chompSubscription
                      spaces
                      word1 0x7D {-}-} E.XXX
                      spaces
                      return (Src.Fx cmd sub)
                ]
        , do  sub <- chompSubscription
              spaces
              oneOf E.XXX
                [ do  word1 0x7D {-}-} E.XXX
                      spaces
                      return (Src.Sub sub)
                , do  word1 0x2C {-,-} E.XXX
                      spaces
                      cmd <- chompCommand
                      spaces
                      word1 0x7D {-}-} E.XXX
                      spaces
                      return (Src.Fx cmd sub)
                ]
        ]


chompCommand :: Parser (A.Located Name.Name)
chompCommand =
  do  Keyword.command_
      spaces
      word1 0x3D {-=-} E.XXX
      spaces
      addLocation Var.upper


chompSubscription :: Parser (A.Located Name.Name)
chompSubscription =
  do  Keyword.subscription_
      spaces
      word1 0x3D {-=-} E.XXX
      spaces
      addLocation Var.upper



-- IMPORTS


chompImports :: [Src.Import] -> Parser [Src.Import]
chompImports imports =
  oneOf E.XXX
    [ do  start <- getPosition
          Keyword.import_
          pushContext start E.Import
          spaces
          name <- addLocation Var.moduleName
          pos <- whitespace
          oneOf E.XXX
            [ do  checkFreshLine pos
                  popContext ()
                  chompImports $
                    Src.Import name Nothing (Src.Explicit []) : imports
            , do  checkSpace pos
                  oneOf E.XXX
                    [ chompAs name imports
                    , chompExposing name Nothing imports
                    ]
            ]
    , return (reverse imports)
    ]


chompAs :: A.Located Name.Name -> [Src.Import] -> Parser [Src.Import]
chompAs name imports =
  do  Keyword.as_
      spaces
      alias <- Var.upper
      pos <- whitespace
      oneOf E.XXX
        [ do  checkFreshLine pos
              popContext ()
              chompImports $
                Src.Import name (Just alias) (Src.Explicit []) : imports
        , do  checkSpace pos
              chompExposing name (Just alias) imports
        ]


chompExposing :: A.Located Name.Name -> Maybe Name.Name -> [Src.Import] -> Parser [Src.Import]
chompExposing name maybeAlias imports =
  do  Keyword.exposing_
      spaces
      exposed <- exposing
      freshLine
      popContext ()
      chompImports $
        Src.Import name maybeAlias exposed : imports



-- LISTING


exposing :: Parser Src.Exposing
exposing =
  do  word1 0x28 {-(-} E.XXX
      spaces
      oneOf E.XXX
        [ do  word2 0x2E 0x2E {-..-} E.XXX
              spaces
              word1 0x29 {-)-} E.XXX
              return Src.Open
        , do  entry <- addLocation chompEntry
              spaces
              exposingHelp [entry]
        ]


exposingHelp :: [A.Located Src.Exposed] -> Parser Src.Exposing
exposingHelp revEntries =
  oneOf E.XXX
    [ do  word1 0x2C {-,-} E.XXX
          spaces
          entry <- addLocation chompEntry
          spaces
          exposingHelp (entry:revEntries)
    , do  word1 0x29 {-)-} E.XXX
          return (Src.Explicit (reverse revEntries))
    ]


chompEntry :: Parser Src.Exposed
chompEntry =
  oneOf E.XXX
    [ Src.Lower <$> Var.lower
    , do  word1 0x28 {-(-} E.XXX
          op <- Symbol.binop
          word1 0x29 {-)-} E.XXX
          return (Src.Operator op)
    , do  name <- Var.upper
          spaces
          Src.Upper name <$> privacy
    ]


privacy :: Parser Src.Privacy
privacy =
  oneOf E.XXX
    [ do  word1 0x28 {-(-} E.XXX
          spaces
          word2 0x2E 0x2E {-..-} E.XXX
          spaces
          word1 0x29 {-)-} E.XXX
          return Src.Public
    , return Src.Private
    ]
