{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Declaration
  ( Decl(..)
  , declaration
  , infix_
  )
  where


import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified AST.Utils.Binop as Binop
import qualified Parse.Expression as Expr
import qualified Parse.Pattern as Pattern
import qualified Parse.Keyword as Keyword
import qualified Parse.Number as Number
import qualified Parse.Symbol as Symbol
import qualified Parse.Type as Type
import qualified Parse.Variable as Var
import Parse.Utils
import Parse.Primitives hiding (Parser, State)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- DECLARATION


data Decl
  = Value (A.Located Src.Value)
  | Union (A.Located Src.Union)
  | Alias (A.Located Src.Alias)
  | Port Src.Port


declaration :: SParser Decl
declaration =
  do  start <- getPosition
      oneOf E.XXX
        [ typeDecl start
        , portDecl
        , valueDecl start
        ]



-- DEFINITION and ANNOTATION


{-# INLINE valueDecl #-}
valueDecl :: A.Position -> SParser Decl
valueDecl start =
  do  name <- Var.lower
      end <- getPosition
      spaces
      oneOf E.XXX
        [
          do  word1 0x3A {-:-} E.XXX
              pushContext start (E.Annotation name)
              spaces
              (tipe, _, space) <- Type.expression
              popContext ()
              checkAligned space
              defName <- matchingName name
              let (A.At (A.Region defStart _) _) = defName
              pushContext defStart (E.Definition name)
              spaces
              decl <- valueHelp start defName (Just tipe) []
              popContext decl
        ,
          do  pushContext start (E.Definition name)
              decl <- valueHelp start (A.at start end name) Nothing []
              popContext decl
        ]


valueHelp :: A.Position -> A.Located Name.Name -> Maybe Src.Type -> [Src.Pattern] -> SParser Decl
valueHelp start name tipe revArgs =
  oneOf E.XXX
    [ do  arg <- Pattern.term
          spaces
          valueHelp start name tipe (arg : revArgs)
    , do  word1 0x3D {-=-} E.XXX
          spaces
          (body, end, space) <- Expr.expression
          let value = A.at start end (Src.Value name (reverse revArgs) body tipe)
          return ( Value value, end, space )
    ]


matchingName :: Name.Name -> Parser (A.Located Name.Name)
matchingName expectedName =
  let (P.Parser parserL) = Var.lower in
  P.Parser $ \state@(P.State _ _ _ sr sc ctx) cok eok cerr eerr ->
    let
      cokL name newState@(P.State _ _ _ er ec _) =
        if expectedName == name
        then cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else cerr sr sc ctx (E.MatchingName expectedName)

      eokL name newState@(P.State _ _ _ er ec _) =
        if expectedName == name
        then eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState
        else eerr sr sc ctx (E.MatchingName expectedName)
    in
    parserL state cokL eokL cerr eerr



-- TYPE ALIAS and UNION TYPES


{-# INLINE typeDecl #-}
typeDecl :: A.Position -> SParser Decl
typeDecl start =
  do  Keyword.type_
      spaces
      oneOf E.XXX
        [
          inContext E.TypeAlias Keyword.alias_ $
            do  spaces
                (name, args) <- nameArgsEquals
                (tipe, end, pos) <- Type.expression
                let alias = A.at start end (Src.Alias name args tipe)
                return (Alias alias, end, pos)
        ,
          do  pushContext start E.TypeUnion
              (name, args) <- nameArgsEquals
              (firstCtor, firstEnd, firstSpace) <- Type.unionConstructor
              (ctors, end, pos) <- chompConstructors [firstCtor] firstEnd firstSpace
              let union = A.at start end (Src.Union name args ctors)
              popContext (Union union, end, pos)
        ]


nameArgsEquals :: Parser (A.Located Name.Name, [A.Located Name.Name])
nameArgsEquals =
  do  name <- addLocation Var.upper
      spaces
      nameArgsEqualsHelp name []


nameArgsEqualsHelp :: A.Located Name.Name -> [A.Located Name.Name] -> Parser (A.Located Name.Name, [A.Located Name.Name])
nameArgsEqualsHelp name args =
  oneOf E.XXX
    [ do  arg <- addLocation Var.lower
          spaces
          nameArgsEqualsHelp name (arg:args)
    , do  word1 0x3D {-=-} E.XXX
          spaces
          return ( name, reverse args )
    ]


chompConstructors :: [(A.Located Name.Name, [Src.Type])] -> A.Position -> SPos -> SParser [(A.Located Name.Name, [Src.Type])]
chompConstructors ctors end pos =
  oneOf E.XXX
    [ do  checkSpace pos
          word1 0x7C {-|-} E.XXX
          spaces
          (ctor, newEnd, newSpace) <- Type.unionConstructor
          chompConstructors (ctor:ctors) newEnd newSpace
    , return ( reverse ctors, end, pos )
    ]



-- PORT


{-# INLINE portDecl #-}
portDecl :: SParser Decl
portDecl =
  inContext E.Port Keyword.port_ $
    do  spaces
        name <- addLocation Var.lower
        spaces
        word1 0x3A {-:-} E.XXX
        spaces
        (tipe, end, pos) <- Type.expression
        return ( Port (Src.Port name tipe), end, pos )



-- INFIX


-- INVARIANT: always chomps to a freshline
--
infix_ :: Parser (A.Located Src.Binop)
infix_ =
  do  start <- getPosition
      inContext E.Infix Keyword.infix_ $
        do  spaces
            associativity <-
              oneOf E.XXX
                [ Keyword.left_  >> return Binop.Left
                , Keyword.right_ >> return Binop.Right
                , Keyword.non_   >> return Binop.Non
                ]
            spaces
            precedence <- Number.precedence
            spaces
            word1 0x28 {-(-} E.XXX
            op <- Symbol.binop
            word1 0x29 {-)-} E.XXX
            spaces
            word1 0x3D {-=-} E.XXX
            spaces
            name <- Var.lower
            end <- getPosition
            checkFreshLine =<< whitespace
            return (A.at start end (Src.Binop op associativity precedence name))
