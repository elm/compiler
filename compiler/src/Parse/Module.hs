{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module
  ( fromByteString
  , Header(..)
  , Effects(..)
  , chompImports
  )
  where


import qualified Data.ByteString as BS
import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Package as Pkg
import qualified Parse.Declaration as Decl
import qualified Parse.Keyword as Keyword
import qualified Parse.Space as Space
import qualified Parse.Symbol as Symbol
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives hiding (State, fromByteString)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- FROM BYTE STRING


fromByteString :: Pkg.Name -> BS.ByteString -> Either E.Error Src.Module
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
        Right modul -> Right modul
        Left err -> Left err

    P.Err err ->
      Left (E.ParseError err)



-- MODULE


normal :: Parser E.Module (Either E.Error Src.Module)
normal =
  do  freshLine E.FreshLineModuleStart
      header <- chompHeader
      comment <- chompModuleDocComment
      imports <- chompImports []
      decls <- specialize E.Declarations $ chompDecls []
      endOfFile
      return (checkModule header comment (Imports.addDefaults imports) [] decls)


core :: Parser E.Module (Either E.Error Src.Module)
core =
  do  freshLine E.FreshLineModuleStart
      header <- chompHeader
      comment <- chompModuleDocComment
      imports <- chompImports []
      binops <- chompBinops []
      decls <- specialize E.Declarations $ chompDecls []
      endOfFile
      return (checkModule header comment imports binops decls)


platform :: Parser E.Module (Either E.Error Src.Module)
platform =
  do  freshLine E.FreshLineModuleStart
      header <- chompHeader
      comment <- chompModuleDocComment
      imports <- chompImports []
      binops <- chompBinops []
      decls <- specialize E.Declarations $ chompDecls []
      endOfFile
      return (checkModule header comment (Imports.addDefaults imports) binops decls)



-- CHECK MODULE


checkModule :: Maybe Header -> Maybe Space.DocComment -> [Src.Import] -> [A.Located Src.Binop] -> [Decl.Decl] -> Either E.Error Src.Module
checkModule maybeHeader _maybeDocs imports binops decls =
  let
    (values, unions, aliases, ports) = categorizeDecls [] [] [] [] decls
  in
  case maybeHeader of
    Just (Header name effects exports) ->
      Src.Module (Just name) exports imports values unions aliases binops
        <$> checkEffects ports effects

    Nothing ->
      Right $
        Src.Module Nothing (A.At A.one Src.Open) imports values unions aliases binops $
          case ports of
            [] -> Src.NoEffects
            _:_ -> Src.Ports ports


checkEffects :: [Src.Port] -> Effects -> Either E.Error Src.Effects
checkEffects ports effects =
  case effects of
    NoEffects region ->
      case ports of
        []  -> Right Src.NoEffects
        _:_ -> Left (E.UnexpectedPort region)

    Ports region ->
      case ports of
        []  -> Left (E.NoPorts region)
        _:_ -> Right (Src.Ports ports)

    Manager region manager ->
      case ports of
        []  -> Right (Src.Manager region manager)
        _:_ -> Left (E.UnexpectedPort region)


categorizeDecls :: [A.Located Src.Value] -> [A.Located Src.Union] -> [A.Located Src.Alias] -> [Src.Port] -> [Decl.Decl] -> ( [A.Located Src.Value], [A.Located Src.Union], [A.Located Src.Alias], [Src.Port] )
categorizeDecls values unions aliases ports decls =
  case decls of
    [] ->
      (values, unions, aliases, ports)

    decl:otherDecls ->
      case decl of
        Decl.Value _ value -> categorizeDecls (value:values) unions aliases ports otherDecls
        Decl.Union _ union -> categorizeDecls values (union:unions) aliases ports otherDecls
        Decl.Alias _ alias -> categorizeDecls values unions (alias:aliases) ports otherDecls
        Decl.Port  _ port_ -> categorizeDecls values unions aliases (port_:ports) otherDecls



-- FRESH LINES


freshLine :: (Row -> Col -> E.Module) -> Parser E.Module ()
freshLine toFreshLineError =
  do  Space.chomp E.ModuleSpace
      Space.checkFreshLine toFreshLineError


endOfFile :: Parser E.Module ()
endOfFile =
  P.Parser $ \state@(P.State pos end _ row col) _ eok _ eerr ->
    if pos < end then
      eerr row col E.ModuleEndOfFile
    else
      eok () state



-- CHOMP DECLARATIONS


chompDecls :: [Decl.Decl] -> Parser E.Decl [Decl.Decl]
chompDecls decls =
  do  (decl, _) <- Decl.declaration
      oneOfWithFallback
        [ do  Space.checkFreshLine E.DeclFreshLineStart
              chompDecls (decl:decls)
        ]
        (reverse (decl:decls))


chompBinops :: [A.Located Src.Binop] -> Parser E.Module [A.Located Src.Binop]
chompBinops binops =
  oneOfWithFallback
    [ do  binop <- Decl.infix_
          chompBinops (binop:binops)
    ]
    binops



-- MODULE DOC COMMENT


chompModuleDocComment :: Parser E.Module (Maybe Space.DocComment)
chompModuleDocComment =
  oneOfWithFallback
    [
      do  docComment <- Space.docComment E.ImportStart E.ModuleSpace
          Space.chomp E.ModuleSpace
          Space.checkFreshLine E.FreshLineAfterDocComment
          return (Just docComment)
    ]
    Nothing



-- HEADER


data Header =
  Header Name.Name Effects (A.Located Src.Exposing)


data Effects
  = NoEffects A.Region
  | Ports A.Region
  | Manager A.Region Src.Manager


chompHeader :: Parser E.Module (Maybe Header)
chompHeader =
  do  start <- getPosition
      oneOfWithFallback
        [
          -- module MyThing exposing (..)
          do  Keyword.module_ E.Module
              end <- getPosition
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentName
              name <- Var.moduleName E.ModuleName
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentExposing
              Keyword.exposing_ E.ModuleExposing
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentExposingList
              exports <- addLocation (specialize E.ModuleExposingList exposing)
              freshLine E.FreshLineAfterModuleLine
              return (Just (Header name (NoEffects (A.Region start end)) exports))
        ,
          -- port module MyThing exposing (..)
          do  Keyword.port_ E.Module
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentPortModule
              Keyword.module_ E.ModulePortModule
              end <- getPosition
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentName
              name <- Var.moduleName E.ModuleName
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentExposing
              Keyword.exposing_ E.ModuleExposing
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentExposingList
              exports <- addLocation (specialize E.ModuleExposingList exposing)
              freshLine E.FreshLineAfterModuleLine
              return (Just (Header name (Ports (A.Region start end)) exports))
        ,
          -- effect module MyThing where { command = MyCmd } exposing (..)
          do  Keyword.effect_ E.Module
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleEffect
              Keyword.module_ E.ModuleEffect
              end <- getPosition
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleEffect
              name <- Var.moduleName E.ModuleName
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleEffect
              Keyword.where_ E.ModuleEffect
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleEffect
              manager <- chompManager
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentExposing
              Keyword.exposing_ E.ModuleExposing
              Space.chompAndCheckIndent E.ModuleSpace E.ModuleIndentExposingList
              exports <- addLocation (specialize E.ModuleExposingList exposing)
              freshLine E.FreshLineAfterModuleLine
              return (Just (Header name (Manager (A.Region start end) manager) exports))
        ]
        -- default header
        Nothing


chompManager :: Parser E.Module Src.Manager
chompManager =
  do  word1 0x7B {- { -} E.ModuleEffect
      spaces_em
      oneOf E.ModuleEffect
        [ do  cmd <- chompCommand
              spaces_em
              oneOf E.ModuleEffect
                [ do  word1 0x7D {-}-} E.ModuleEffect
                      spaces_em
                      return (Src.Cmd cmd)
                , do  word1 0x2C {-,-} E.ModuleEffect
                      spaces_em
                      sub <- chompSubscription
                      spaces_em
                      word1 0x7D {-}-} E.ModuleEffect
                      spaces_em
                      return (Src.Fx cmd sub)
                ]
        , do  sub <- chompSubscription
              spaces_em
              oneOf E.ModuleEffect
                [ do  word1 0x7D {-}-} E.ModuleEffect
                      spaces_em
                      return (Src.Sub sub)
                , do  word1 0x2C {-,-} E.ModuleEffect
                      spaces_em
                      cmd <- chompCommand
                      spaces_em
                      word1 0x7D {-}-} E.ModuleEffect
                      spaces_em
                      return (Src.Fx cmd sub)
                ]
        ]


chompCommand :: Parser E.Module (A.Located Name.Name)
chompCommand =
  do  Keyword.command_ E.ModuleEffect
      spaces_em
      word1 0x3D {-=-} E.ModuleEffect
      spaces_em
      addLocation (Var.upper E.ModuleEffect)


chompSubscription :: Parser E.Module (A.Located Name.Name)
chompSubscription =
  do  Keyword.subscription_ E.ModuleEffect
      spaces_em
      word1 0x3D {-=-} E.ModuleEffect
      spaces_em
      addLocation (Var.upper E.ModuleEffect)


spaces_em :: Parser E.Module ()
spaces_em =
  Space.chompAndCheckIndent E.ModuleSpace E.ModuleEffect



-- IMPORTS


chompImports :: [Src.Import] -> Parser E.Module [Src.Import]
chompImports imports =
  oneOfWithFallback
    [ do  Keyword.import_ E.ImportStart
          Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName
          name@(A.At (A.Region _ end) _) <- addLocation (Var.moduleName E.ImportName)
          Space.chomp E.ModuleSpace
          oneOf E.ImportEnd
            [ do  Space.checkFreshLine E.ImportEnd
                  chompImports $
                    Src.Import name Nothing (Src.Explicit []) : imports
            , do  Space.checkIndent end E.ImportIndentAs
                  oneOf E.ImportAs
                    [ chompAs name imports
                    , chompExposing name Nothing imports
                    ]
            ]
    ]
    (reverse imports)


chompAs :: A.Located Name.Name -> [Src.Import] -> Parser E.Module [Src.Import]
chompAs name imports =
  do  Keyword.as_ E.ImportAs
      Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias
      alias <- Var.upper E.ImportAlias
      end <- getPosition
      Space.chomp E.ModuleSpace
      oneOf E.ImportEnd
        [ do  Space.checkFreshLine E.ImportEnd
              chompImports $
                Src.Import name (Just alias) (Src.Explicit []) : imports
        , do  Space.checkIndent end E.ImportIndentExposing
              chompExposing name (Just alias) imports
        ]


chompExposing :: A.Located Name.Name -> Maybe Name.Name -> [Src.Import] -> Parser E.Module [Src.Import]
chompExposing name maybeAlias imports =
  do  Keyword.exposing_ E.ImportExposing
      Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList
      exposed <- specialize E.ImportExposingList exposing
      freshLine E.ImportEnd
      chompImports $
        Src.Import name maybeAlias exposed : imports



-- LISTING


exposing :: Parser E.Exposing Src.Exposing
exposing =
  do  word1 0x28 {-(-} E.ExposingStart
      Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
      oneOf E.ExposingIndentValue
        [ do  word2 0x2E 0x2E {-..-} E.ExposingIndentValue
              Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
              word1 0x29 {-)-} E.ExposingEnd
              return Src.Open
        , do  exposed <- addLocation chompExposed
              Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
              exposingHelp [exposed]
        ]


exposingHelp :: [A.Located Src.Exposed] -> Parser E.Exposing Src.Exposing
exposingHelp revExposed =
  oneOf E.ExposingEnd
    [ do  word1 0x2C {-,-} E.ExposingEnd
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
          exposed <- addLocation chompExposed
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValueEnd
          exposingHelp (exposed:revExposed)
    , do  word1 0x29 {-)-} E.ExposingEnd
          return (Src.Explicit (reverse revExposed))
    ]


chompExposed :: Parser E.Exposing Src.Exposed
chompExposed =
  oneOf E.ExposingValue
    [ Src.Lower <$> Var.lower E.ExposingValue
    , do  word1 0x28 {-(-} E.ExposingValue
          op <- Symbol.operator E.ExposingOperator E.ExposingOperatorReserved
          word1 0x29 {-)-} E.ExposingOperatorRightParen
          return (Src.Operator op)
    , do  name <- Var.upper E.ExposingValue
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentTypePrivacy
          Src.Upper name <$> privacy
    ]


privacy :: Parser E.Exposing Src.Privacy
privacy =
  oneOfWithFallback
    [ do  word1 0x28 {-(-} E.ExposingTypePrivacy
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentTypePrivacyDots
          word2 0x2E 0x2E {-..-} E.ExposingTypePrivacyDots
          Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentTypePrivacyEnd
          word1 0x29 {-)-} E.ExposingTypePrivacyEnd
          return Src.Public
    ]
    Src.Private
