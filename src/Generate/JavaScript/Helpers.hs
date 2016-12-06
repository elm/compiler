module Generate.JavaScript.Helpers where

import Data.Text (Text)
import Generate.JavaScript.Builder



-- DECLARATIONS


varDecl :: Text -> Expr -> VarDecl
varDecl x expr =
  VarDecl (Id x) (Just expr)


refOrObject :: Text -> Expr
refOrObject name =
  Infix OpLOr (ref name) (Object [])



-- VARIABLES


ref :: Text -> Expr
ref name =
    VarRef (Id name)


(==>) :: Text -> Expr -> (Prop, Expr)
(==>) name expr =
  ( IdProp (Id name), expr )


obj :: [Text] -> Expr
obj vars =
  case vars of
    x:xs ->
      foldl DotRef (ref x) (map Id xs)

    [] ->
      error "dotSep must be called on a non-empty list of variables"



-- FUNCTION CALLS


(<|) :: Expr -> Expr -> Expr
(<|) f x =
  Call f [x]


function :: [Text] -> [Stmt] -> Expr
function args stmts =
  Function Nothing (map Id args) stmts
