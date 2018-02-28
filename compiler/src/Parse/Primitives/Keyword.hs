{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Parse.Primitives.Keyword
  ( type_, alias_, port_
  , if_, then_, else_
  , case_, of_
  , let_, in_
  , infix_, left_, right_, non_
  , module_, import_, exposing_, as_, where_, effect_
  , command_, subscription_
  , jsonTrue, jsonFalse, jsonNull
  )
  where


import Control.Exception (assert)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as Char8

import Parse.Primitives.Internals (Parser(..), State(..), expect, noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Variable as Var
import qualified Reporting.Error.Syntax as E



-- PRIVATE IMPLEMENTATION


{- We can some avoid allocation by declaring all available keywords here.
That means the `keyword` function should only be used within this file on
values tagged as NOINLINE.
-}
keyword :: B.ByteString -> Parser ()
keyword kwd@(B.PS kwdFp kwdOffset kwdLength) =
  let
    !theory =
      assert
        (I.isNonNewlineAscii kwdFp kwdOffset (kwdOffset + kwdLength))
        (E.Keyword (Char8.unpack kwd))
  in
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    if I.isSubstring kwdFp kwdOffset kwdLength fp offset terminal
      && Var.getInnerWidth fp (offset + kwdLength) terminal == 0
    then
      let
        !newState =
          State fp (offset + kwdLength) terminal indent row (col + kwdLength) ctx
      in
        cok () newState noError

    else
      eerr (expect row col ctx theory)



-- DECLARATIONS


{-# NOINLINE type_ #-}
type_ :: Parser ()
type_ =
  keyword "type"


{-# NOINLINE alias_ #-}
alias_ :: Parser ()
alias_ =
  keyword "alias"


{-# NOINLINE port_ #-}
port_ :: Parser ()
port_ =
  keyword "port"



-- IF EXPRESSIONS


{-# NOINLINE if_ #-}
if_ :: Parser ()
if_ =
  keyword "if"


{-# NOINLINE then_ #-}
then_ :: Parser ()
then_ =
  keyword "then"


{-# NOINLINE else_ #-}
else_ :: Parser ()
else_ =
  keyword "else"



-- CASE EXPRESSIONS


{-# NOINLINE case_ #-}
case_ :: Parser ()
case_ =
  keyword "case"


{-# NOINLINE of_ #-}
of_ :: Parser ()
of_ =
  keyword "of"



-- LET EXPRESSIONS


{-# NOINLINE let_ #-}
let_ :: Parser ()
let_ =
  keyword "let"


{-# NOINLINE in_ #-}
in_ :: Parser ()
in_ =
  keyword "in"



-- INFIXES


{-# NOINLINE infix_ #-}
infix_ :: Parser ()
infix_ =
  keyword "infix"


{-# NOINLINE left_ #-}
left_ :: Parser ()
left_ =
  keyword "left"


{-# NOINLINE right_ #-}
right_ :: Parser ()
right_ =
  keyword "right"


{-# NOINLINE non_ #-}
non_ :: Parser ()
non_ =
  keyword "non"



-- IMPORTS


{-# NOINLINE module_ #-}
module_ :: Parser ()
module_ =
  keyword "module"


{-# NOINLINE import_ #-}
import_ :: Parser ()
import_ =
  keyword "import"


{-# NOINLINE exposing_ #-}
exposing_ :: Parser ()
exposing_ =
  keyword "exposing"


{-# NOINLINE as_ #-}
as_ :: Parser ()
as_ =
  keyword "as"


{-# NOINLINE where_ #-}
where_ :: Parser ()
where_ =
  keyword "where"


{-# NOINLINE effect_ #-}
effect_ :: Parser ()
effect_ =
  keyword "effect"



-- EFFECTS


{-# NOINLINE command_ #-}
command_ :: Parser ()
command_ =
  keyword "command"


{-# NOINLINE subscription_ #-}
subscription_ :: Parser ()
subscription_ =
  keyword "subscription"



-- JSON


{-# NOINLINE jsonTrue #-}
jsonTrue :: Parser ()
jsonTrue =
  keyword "true"


{-# NOINLINE jsonFalse #-}
jsonFalse :: Parser ()
jsonFalse =
  keyword "false"


{-# NOINLINE jsonNull #-}
jsonNull :: Parser ()
jsonNull =
  keyword "null"
