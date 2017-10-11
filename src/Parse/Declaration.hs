{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Declaration where

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified AST.Declaration as Decl
import qualified AST.Type as Type
import qualified Parse.Expression as Expr
import Parse.Primitives
import qualified Parse.Primitives.Keyword as Keyword
import qualified Parse.Primitives.Symbol as Symbol
import qualified Parse.Primitives.Variable as Var
import qualified Parse.Primitives.Whitespace as W
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- DECLARATION


declaration :: SParser Decl.Source
declaration =
  hint E.Decl $
  do  start <- getPosition
      oneOf
        [ docDecl start
        , typeDecl start
        , infixDecl start
        , portDecl start
        , defDecl start
        ]



-- DOC COMMENTS


docDecl :: R.Position -> SParser Decl.Source
docDecl start =
  do  doc <- W.docComment
      end <- getPosition
      pos <- W.whitespace
      return ( Decl.Comment (A.at start end (Text.decodeUtf8 doc)), end, pos )


-- TYPE ANNOTATIONS and DEFINITIONS


defDecl :: R.Position -> SParser Decl.Source
defDecl start =
  do  (def, end, pos) <- Expr.definition
      let decl = A.at start end (Decl.Def def)
      return ( Decl.Whatever decl, end, pos )



-- TYPE ALIAS and UNION TYPES


typeDecl :: R.Position -> SParser Decl.Source
typeDecl start =
  do  Keyword.type_
      spaces
      oneOf
        [ do  Keyword.alias_
              inContext start E.TypeAlias $
                do  spaces
                    (name, args) <- nameArgsEquals
                    (tipe, end, pos) <- Type.expression
                    let decl = A.at start end (Decl.Alias (Decl.Type name args tipe))
                    return ( Decl.Whatever decl, end, pos )
        , inContext start E.TypeUnion $
            do  (name, args) <- nameArgsEquals
                (firstCtor, firstEnd, firstSpace) <- Type.unionConstructor
                (ctors, end, pos) <- chompConstructors [firstCtor] firstEnd firstSpace
                let decl = A.at start end (Decl.Union (Decl.Type name args ctors))
                return ( Decl.Whatever decl, end, pos )
        ]


nameArgsEquals :: Parser (Text, [Text])
nameArgsEquals =
  do  name <- Var.upper
      spaces
      nameArgsEqualsHelp name []


nameArgsEqualsHelp :: Text -> [Text] -> Parser (Text, [Text])
nameArgsEqualsHelp name args =
  oneOf
    [ do  arg <- Var.lower
          spaces
          nameArgsEqualsHelp name (arg:args)
    , do  Symbol.equals
          spaces
          return ( name, reverse args )
    ]


chompConstructors :: [(Text, [Type.Raw])] -> R.Position -> SPos -> SParser [(Text, [Type.Raw])]
chompConstructors ctors end pos =
  oneOf
    [ do  checkSpace pos
          Symbol.pipe
          spaces
          (ctor, newEnd, newSpace) <- Type.unionConstructor
          chompConstructors (ctor:ctors) newEnd newSpace
    , return ( reverse ctors, end, pos )
    ]



-- INFIX


infixDecl :: R.Position -> SParser Decl.Source
infixDecl start =
  oneOf
    [ do  Keyword.infixl_
          inContext start E.Infix $ infixDeclHelp start Decl.Left
    , do  Keyword.infixr_
          inContext start E.Infix $ infixDeclHelp start Decl.Right
    , do  Keyword.infix_
          inContext start E.Infix $ infixDeclHelp start Decl.Non
    ]


infixDeclHelp :: R.Position -> Decl.Assoc -> SParser Decl.Source
infixDeclHelp start assoc =
  do  spaces
      n <- digit
      spaces
      op <- Symbol.binop
      end <- getPosition
      pos <- W.whitespace
      let decl = A.at start end (Decl.Fixity (Decl.Infix op assoc n))
      return ( Decl.Whatever decl, end, pos )



-- FOREIGN


portDecl :: R.Position -> SParser Decl.Source
portDecl start =
  do  Keyword.port_
      inContext start E.Port $
        do  spaces
            name <- Var.lower
            spaces
            Symbol.hasType
            spaces
            (tipe, end, pos) <- Type.expression
            let decl = A.at start end (Decl.Port name tipe)
            return ( Decl.Whatever decl, end, pos )

