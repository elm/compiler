{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Declaration
  ( declaration
  , infix_
  )
  where


import qualified Data.Text.Encoding as Text

import qualified AST.Source as Src
import qualified AST.Utils.Binop as Binop
import qualified Elm.Name as N
import qualified Parse.Expression as Expr
import qualified Parse.Pattern as Pattern
import Parse.Primitives
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Number as Number
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Primitives.Whitespace as W
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- DECLARATION


declaration :: SParser Src.Decl
declaration =
  hint E.Decl $
  do  start <- getPosition
      oneOf
        [ doc_ start
        , type_ start
        , port_ start
        , def_ start
        ]



-- DOC COMMENTS


{-# INLINE doc_ #-}
doc_ :: R.Position -> SParser Src.Decl
doc_ start =
  do  doc <- W.docComment
      end <- getPosition
      pos <- W.whitespace
      return ( A.at start end (Src.Docs (Text.decodeUtf8 doc)), end, pos )



-- DEFINITION and ANNOTATION


{-# INLINE def_ #-}
def_ :: R.Position -> SParser Src.Decl
def_ start =
  do  name <- Var.lower
      nameEnd <- getPosition
      let locatedName = A.at start nameEnd name
      spaces
      oneOf
        [ do  Symbol.hasType
              inContext start (E.Annotation name) $
                do  spaces
                    (tipe, end, space) <- Type.expression
                    return ( A.at start end (Src.Annotation locatedName tipe), end, space )
        , inContext start (E.Definition name) $
            definitionHelp start locatedName []
        ]


definitionHelp :: R.Position -> A.Located N.Name -> [Src.Pattern] -> SParser Src.Decl
definitionHelp start name revArgs =
  oneOf
    [ do  arg <- hint E.Arg Pattern.term
          spaces
          definitionHelp start name (arg : revArgs)
    , do  Symbol.equals
          spaces
          (body, end, space) <- Expr.expression
          let def = A.at start end (Src.Definition name (reverse revArgs) body)
          return ( def, end, space )
    ]



-- TYPE ALIAS and UNION TYPES


{-# INLINE type_ #-}
type_ :: R.Position -> SParser Src.Decl
type_ start =
  do  Keyword.type_
      spaces
      oneOf
        [ do  Keyword.alias_
              inContext start E.TypeAlias $
                do  spaces
                    (name, args) <- nameArgsEquals
                    (tipe, end, pos) <- Type.expression
                    return ( A.at start end (Src.Alias name args tipe), end, pos )
        , inContext start E.TypeUnion $
            do  (name, args) <- nameArgsEquals
                (firstCtor, firstEnd, firstSpace) <- Type.unionConstructor
                (ctors, end, pos) <- chompConstructors [firstCtor] firstEnd firstSpace
                return ( A.at start end (Src.Union name args ctors), end, pos )
        ]


nameArgsEquals :: Parser (A.Located N.Name, [A.Located N.Name])
nameArgsEquals =
  do  name <- addLocation Var.upper
      spaces
      nameArgsEqualsHelp name []


nameArgsEqualsHelp :: A.Located N.Name -> [A.Located N.Name] -> Parser (A.Located N.Name, [A.Located N.Name])
nameArgsEqualsHelp name args =
  oneOf
    [ do  arg <- addLocation Var.lower
          spaces
          nameArgsEqualsHelp name (arg:args)
    , do  Symbol.equals
          spaces
          return ( name, reverse args )
    ]


chompConstructors :: [(A.Located N.Name, [Src.Type])] -> R.Position -> SPos -> SParser [(A.Located N.Name, [Src.Type])]
chompConstructors ctors end pos =
  oneOf
    [ do  checkSpace pos
          Symbol.pipe
          spaces
          (ctor, newEnd, newSpace) <- Type.unionConstructor
          chompConstructors (ctor:ctors) newEnd newSpace
    , return ( reverse ctors, end, pos )
    ]



-- PORT


{-# INLINE port_ #-}
port_ :: R.Position -> SParser Src.Decl
port_ start =
  do  Keyword.port_
      inContext start E.Port $
        do  spaces
            name <- addLocation Var.lower
            spaces
            Symbol.hasType
            spaces
            (tipe, end, pos) <- Type.expression
            return ( A.at start end (Src.Port name tipe), end, pos )



-- INFIX


-- INVARIANT: always chomps to a freshline
--
infix_ :: Parser Src.Decl
infix_ =
  do  start <- getPosition
      Keyword.infix_
      inContext start E.Infix $
        do  spaces
            associativity <-
              oneOf
                [ Keyword.left_  >> return Binop.Left
                , Keyword.right_ >> return Binop.Right
                , Keyword.non_   >> return Binop.Non
                ]
            spaces
            precedence <- Number.precedence
            spaces
            Symbol.leftParen
            op <- Symbol.binop
            Symbol.rightParen
            spaces
            Symbol.equals
            spaces
            name <- Var.lower
            end <- getPosition
            checkFreshLine =<< W.whitespace
            return (A.at start end (Src.Binop op associativity precedence name))
