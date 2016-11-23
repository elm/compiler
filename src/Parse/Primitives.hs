{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parse.Primitives
  ( IParser, parse
  , expecting, failure
  , withPos, block, checkIndent, indented
  )
  where

import Control.Monad.Identity (Identity, runIdentity)
import qualified Text.Parsec.Pos as Pos
import Text.Parsec
  ( ParsecT, runParserT, getState, putState
  , SourcePos, sourceColumn, sourceLine, setSourceLine, getPosition
  , getInput, setInput
  , ParseError, (<?>), parserFail, anyToken, many1
  )



-- PARSER


type IParser a =
  ParsecT String SourcePos Identity a


parse :: IParser a -> String -> Either ParseError a
parse parser source =
  runIdentity (runParserT parser (Pos.initialPos "") "" source)



-- ERROR HANDLING


expecting :: String -> IParser a -> IParser a
expecting =
  flip (<?>)


failure :: String -> IParser a
failure msg =
  do  input <- getInput
      setInput ('x' : input)
      anyToken
      fail msg



-- INDENTATION


withPos :: IParser a -> IParser a
withPos parser =
  do  old <- getState
      new <- getPosition
      putState new
      result <- parser
      putState old
      return result


block :: IParser a -> IParser [a]
block parser =
  withPos $
    many1 (checkIndent >> parser)


checkIndent :: IParser ()
checkIndent =
  do  old <- getState
      new <- getPosition
      if sourceColumn old == sourceColumn new
        then return ()
        else parserFail "indentation doesn't match"


indented :: IParser ()
indented =
  do  old <- getState
      new <- getPosition
      if sourceColumn new <= sourceColumn old
        then parserFail "not indented"
        else putState $ setSourceLine old (sourceLine new)
