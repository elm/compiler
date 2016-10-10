{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Text.Parsec ( (<?>), choice, digit, optionMaybe, string, try )

import qualified AST.Declaration as Decl
import qualified Parse.Binop as Binop
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type



declaration :: IParser Decl.Source
declaration =
  commentOr $ addLocation $
    choice
      [ typeDecl
      , infixDecl
      , portDecl
      , definition
      ]


commentOr :: IParser a -> IParser (Decl.CommentOr a)
commentOr parser =
  choice
    [ Decl.Comment <$> addLocation Help.docComment
    , Decl.Whatever <$> parser
    ]



-- TYPE ANNOTATIONS and DEFINITIONS


definition :: IParser Decl.Raw
definition =
  do  def <- Expr.def <?> "a definition or type annotation"
      return $ Decl.Def def



-- TYPE ALIAS and UNION TYPES


typeDecl :: IParser Decl.Raw
typeDecl =
  do  try (reserved "type") <?> "a type declaration"
      forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar
      args <- spacePrefix lowVar
      padded equals

      case isAlias of
        Just _ ->
            do  tipe <- Type.expr <?> "a type"
                return $ Decl.Alias (Decl.Type name args tipe)

        Nothing ->
            do  tcs <- pipeSep1 Type.constructor <?> "a constructor for a union type"
                return $ Decl.Union (Decl.Type name args tcs)



-- INFIX


infixDecl :: IParser Decl.Raw
infixDecl =
  expecting "an infix declaration" $
  do  assoc <-
          choice
            [ try (reserved "infixl") >> return Decl.L
            , try (reserved "infixr") >> return Decl.R
            , try (reserved "infix")  >> return Decl.N
            ]
      forcedWS
      n <- digit
      forcedWS
      op <- Binop.infixOp
      return $ Decl.Fixity (Decl.Infix op assoc (read [n]))



-- FOREIGN


portDecl :: IParser Decl.Raw
portDecl =
  expecting "a port declaration" $
  do  try (reserved "port")
      forcedWS
      name <- Help.lowVar
      padded hasType
      tipe <- Type.expr
      return $ Decl.Port name tipe

