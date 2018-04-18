{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Module
  ( module_
  , chompImports
  , freshLine
  , exposing
  )
  where


import qualified AST.Source as Src
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import Parse.Primitives (Parser, oneOf)
import qualified Parse.Primitives as P
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Primitives.Whitespace as W
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- MODULE


module_ :: Pkg.Name -> Parser decls -> Parser (Src.Module decls)
module_ pkgName chompDecls =
  do  freshLine
      header <- oneOf [ Just <$> chompHeader, return Nothing ]
      imports <- Imports.addDefaults pkgName <$> chompImports []
      decls <- chompDecls
      return (Src.Module header imports decls)


chompHeader :: Parser Src.Header
chompHeader =
  do  start <- P.getPosition
      P.pushContext start E.Module
      (name, effects) <- chompNameAndEffects
      P.hint E.Exposing $ Keyword.exposing_
      P.spaces
      exports <- P.addLocation exposing
      P.popContext ()
      docs <- chompDocComment
      return (Src.Header name effects exports docs)



-- MODULE NAME
--
-- module MyThing
-- port module MyThing
-- effect module MyThing where { command = MyCmd }
--
chompNameAndEffects :: Parser (N.Name, Src.Effects)
chompNameAndEffects =
  oneOf
    [
      do  Keyword.module_
          P.spaces
          name <- Var.moduleName
          P.spaces
          return ( name, Src.NoEffects )
    ,
      do  start <- P.getPosition
          Keyword.port_
          P.spaces
          Keyword.module_
          end <- P.getPosition
          P.spaces
          name <- Var.moduleName
          P.spaces
          return ( name, Src.Ports (R.Region start end) )
    ,
      do  start <- P.getPosition
          Keyword.effect_
          P.spaces
          Keyword.module_
          end <- P.getPosition
          P.spaces
          name <- Var.moduleName
          P.spaces
          manager <- chompManager
          return ( name, Src.Manager (R.Region start end) manager )
    ]



-- MANAGER
--
-- where { command = MyCmd }
-- where { subscription = MySub }
-- where { command = MyCmd, subscription = MySub }
--
chompManager :: Parser Src.Manager
chompManager =
  do  Keyword.where_
      P.spaces
      Symbol.leftCurly
      P.spaces
      oneOf
        [ do  cmd <- chompCommand
              P.spaces
              oneOf
                [ do  Symbol.rightCurly
                      P.spaces
                      return (Src.Cmd cmd)
                , do  Symbol.comma
                      P.spaces
                      sub <- chompSubscription
                      P.spaces
                      Symbol.rightCurly
                      P.spaces
                      return (Src.Fx cmd sub)
                ]
        , do  sub <- chompSubscription
              P.spaces
              oneOf
                [ do  Symbol.rightCurly
                      P.spaces
                      return (Src.Sub sub)
                , do  Symbol.comma
                      P.spaces
                      cmd <- chompCommand
                      P.spaces
                      Symbol.rightCurly
                      P.spaces
                      return (Src.Fx cmd sub)
                ]
        ]


chompCommand :: Parser (A.Located N.Name)
chompCommand =
  do  Keyword.command_
      P.spaces
      Symbol.equals
      P.spaces
      P.addLocation Var.upper


chompSubscription :: Parser (A.Located N.Name)
chompSubscription =
  do  Keyword.subscription_
      P.spaces
      Symbol.equals
      P.spaces
      P.addLocation Var.upper



-- DOC COMMENTS


chompDocComment :: Parser Src.Docs
chompDocComment =
  do  endOfExposing <- P.getPosition
      freshLine
      nextStart <- P.getPosition
      oneOf
        [ do  doc <- W.docComment
              docEnd <- P.getPosition
              freshLine
              return (Src.YesDocs (R.Region nextStart docEnd) doc)
        , return (Src.NoDocs (R.Region endOfExposing nextStart))
        ]



-- IMPORTS


chompImports :: [Src.Import] -> Parser [Src.Import]
chompImports imports =
  oneOf
    [ do  start <- P.getPosition
          Keyword.import_
          P.pushContext start E.Import
          P.spaces
          name <- P.addLocation Var.moduleName
          pos <- W.whitespace
          oneOf
            [ do  P.checkFreshLine pos
                  P.popContext ()
                  chompImports $
                    Src.Import name Nothing (Src.Explicit []) : imports
            , do  P.checkSpace pos
                  oneOf
                    [ chompAs name imports
                    , chompExposing name Nothing imports
                    ]
            ]
    , return (reverse imports)
    ]


chompAs :: A.Located N.Name -> [Src.Import] -> Parser [Src.Import]
chompAs name imports =
  do  Keyword.as_
      P.spaces
      alias <- Var.upper
      pos <- W.whitespace
      oneOf
        [ do  P.checkFreshLine pos
              P.popContext ()
              chompImports $
                Src.Import name (Just alias) (Src.Explicit []) : imports
        , do  P.checkSpace pos
              chompExposing name (Just alias) imports
        ]


chompExposing :: A.Located N.Name -> Maybe N.Name -> [Src.Import] -> Parser [Src.Import]
chompExposing name maybeAlias imports =
  do  Keyword.exposing_
      P.spaces
      exposed <- exposing
      freshLine
      P.popContext ()
      chompImports $
        Src.Import name maybeAlias exposed : imports



-- LISTING


exposing :: Parser Src.Exposing
exposing =
  P.hint E.Listing $
  do  Symbol.leftParen
      P.spaces
      oneOf
        [ do  Symbol.doubleDot
              P.spaces
              Symbol.rightParen
              return Src.Open
        , do  entry <- P.addLocation chompEntry
              P.spaces
              exposingHelp [entry]
        ]


exposingHelp :: [A.Located Src.Exposed] -> Parser Src.Exposing
exposingHelp revEntries =
  oneOf
    [ do  Symbol.comma
          P.spaces
          entry <- P.addLocation chompEntry
          P.spaces
          exposingHelp (entry:revEntries)
    , do  Symbol.rightParen
          return (Src.Explicit (reverse revEntries))
    ]


chompEntry :: Parser Src.Exposed
chompEntry =
  oneOf
    [ Src.Lower <$> Var.lower
    , do  Symbol.leftParen
          op <- Symbol.binop
          Symbol.rightParen
          return (Src.Operator op)
    , do  name <- Var.upper
          P.spaces
          Src.Upper name <$> privacy
    ]


privacy :: Parser Src.Privacy
privacy =
  oneOf
    [ do  Symbol.leftParen
          P.spaces
          Symbol.doubleDot
          P.spaces
          Symbol.rightParen
          return Src.Public
    , return Src.Private
    ]



-- FRESH LINES


freshLine :: Parser ()
freshLine =
  P.checkFreshLine =<< W.whitespace

