{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Declaration where

import Text.Parsec ( (<?>), choice, digit, optionMaybe, string, try )
import qualified Text.Parsec.Indent as Indent

import qualified AST.Declaration as Decl
import qualified AST.Expression.Source as Source
import qualified Parse.Expression as Expr
import Parse.Helpers as Help
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A



declaration :: IParser Decl.Source
declaration =
  commentOr $ addLocation $
    choice
      [ Decl.Source <$> typeDecl
      , Decl.Source <$> infixDecl
      , defineDecl
      , Decl.Source <$> definition
      ]


commentOr :: IParser a -> IParser (Decl.CommentOr a)
commentOr parser =
  choice
    [ Decl.Comment <$> addLocation Help.docComment
    , Decl.Whatever <$> parser
    ]



-- TYPE ANNOTATIONS and DEFINITIONS


definition :: IParser Decl.Source'
definition =
  do  def <- Expr.def <?> "a value definition"
      return $ Decl.Def def



-- TYPE ALIAS and UNION TYPES


typeDecl :: IParser Decl.Source'
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
                return $ Decl.Alias name args tipe

        Nothing ->
            do  tcs <- pipeSep1 Type.constructor <?> "a constructor for a union type"
                return $ Decl.Union name args tcs



-- INFIX


infixDecl :: IParser Decl.Source'
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
      op <- anyOp
      return $ Decl.Fixity assoc (read [n]) op



-- DEFINE


defineDecl :: IParser Decl.SourceOrDefine
defineDecl =
  do  try (reserved "define")
      forcedWS

      effectType <-
        choice
          [ string "commands" >> return Decl.Cmd
          , string "subscriptions" >> return Decl.Sub
          , do  string "foreign"
                whitespace
                choice
                  [ string "commands" >> return Decl.ForeignCmd
                  , string "subscriptions" >> return Decl.ForeignSub
                  ]
          ]

      padded (reserved "as")

      effects <-
        Indent.block (commentOr effectDef <* whitespace)

      return (Decl.Define effectType effects)


effectDef :: IParser (A.Located Source.Effect)
effectDef =
  choice
    [ Expr.annotation Source.Type
    , Expr.definition Source.Def
    , addLocation $
      do  name <- lowVar
          padded (reserved "with")
          expr <- Expr.expr
          return (Source.With name expr)
    ]
