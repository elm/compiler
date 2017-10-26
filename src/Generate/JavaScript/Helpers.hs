{-# OPTIONS_GHC -Wall #-}
module Generate.JavaScript.Helpers
  ( varDecl
  , refOrObject
  , ref
  , (==>)
  , obj
  , (<|)
  , function
  , toFieldName
  )
  where


import qualified Data.Char as Char
import qualified Data.Text as Text
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


(==>) :: Text -> Expr -> (Id, Expr)
(==>) name expr =
  ( Id name, expr )


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



-- FIELD NAMES


toFieldName :: Int -> Text
toFieldName word =
  Text.pack (toFieldNameHelp word [])


toFieldNameHelp :: Int -> [Char] -> [Char]
toFieldNameHelp word str =
  if word < 52 then
    toChar word : str

  else
    let
      (next, remainder) =
        quotRem word 52
    in
      toFieldNameHelp next (toChar remainder : str)


toChar :: Int -> Char
toChar word =
  Char.chr $ fromIntegral $ word + (if word < 26 then 97 else 39)
