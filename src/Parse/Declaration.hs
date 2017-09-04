{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Declaration where

import Data.Text (Text)

import qualified AST.Declaration as Decl
import qualified AST.Type as Type
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
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
  do  comment <- docComment
      end <- getPosition
      pos <- whitespace
      return ( Decl.Comment (A.at start end comment), end, pos )


-- TYPE ANNOTATIONS and DEFINITIONS


defDecl :: R.Position -> SParser Decl.Source
defDecl start =
  do  (def, end, pos) <- Expr.topLevelDef
      let decl = A.at start end (Decl.Def def)
      return ( Decl.Whatever decl, end, pos )



-- TYPE ALIAS and UNION TYPES


typeDecl :: R.Position -> SParser Decl.Source
typeDecl start =
  do  keyword "type"
      spaces
      oneOf
        [ do  keyword "alias"
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
  do  name <- capVar
      spaces
      nameArgsEqualsHelp name []


nameArgsEqualsHelp :: Text -> [Text] -> Parser (Text, [Text])
nameArgsEqualsHelp name args =
  oneOf
    [ do  arg <- lowVar
          spaces
          nameArgsEqualsHelp name (arg:args)
    , do  equals
          spaces
          return ( name, reverse args )
    ]


chompConstructors :: [(Text, [Type.Raw])] -> R.Position -> SPos -> SParser [(Text, [Type.Raw])]
chompConstructors ctors end pos =
  oneOf
    [ do  checkSpace pos
          pipe
          spaces
          (ctor, newEnd, newSpace) <- Type.unionConstructor
          chompConstructors (ctor:ctors) newEnd newSpace
    , return ( reverse ctors, end, pos )
    ]



-- INFIX


infixDecl :: R.Position -> SParser Decl.Source
infixDecl start =
  oneOf
    [ do  keyword "infixl"
          inContext start E.Infix $ infixDeclHelp start Decl.Left
    , do  keyword "infixr"
          inContext start E.Infix $ infixDeclHelp start Decl.Right
    , do  keyword "infix"
          inContext start E.Infix $ infixDeclHelp start Decl.Non
    ]


infixDeclHelp :: R.Position -> Decl.Assoc -> SParser Decl.Source
infixDeclHelp start assoc =
  do  spaces
      n <- digit
      spaces
      op <- infixOp
      end <- getPosition
      pos <- whitespace
      let decl = A.at start end (Decl.Fixity (Decl.Infix op assoc n))
      return ( Decl.Whatever decl, end, pos )



-- FOREIGN


portDecl :: R.Position -> SParser Decl.Source
portDecl start =
  do  keyword "port"
      inContext start E.Port $
        do  spaces
            name <- lowVar
            spaces
            hasType
            spaces
            (tipe, end, pos) <- Type.expression
            let decl = A.at start end (Decl.Port name tipe)
            return ( Decl.Whatever decl, end, pos )

