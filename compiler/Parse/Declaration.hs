
module Parse.Declaration where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import Text.Parsec hiding (newline,spaces)
import Text.Parsec.Indent
import qualified Text.Pandoc as Pan

import Parse.Helpers
import qualified Parse.Expression as Expr
import qualified Parse.Type as Type

import SourceSyntax.Declaration (Declaration(..))

import Unique


definition :: IParser (Declaration t v)
definition = Definition <$> Expr.def

alias :: IParser [Declaration t v]
alias = do
  start <- getPosition
  reserved "type" <?> "type alias (type Point = {x:Int, y:Int})"
  forcedWS
  alias <- capVar
  args  <- spacePrefix lowVar
  whitespace ; string "=" ; whitespace
  tipe <- Type.expr
  end <- getPosition
  return [TypeAlias alias args tipe]
