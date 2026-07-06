{-# LANGUAGE QuasiQuotes #-}
module Parse.Keyword
  ( type_, alias_, port_
  , if_, then_, else_
  , case_, of_
  , let_, in_
  , infix_, left_, right_, non_
  , module_, import_, exposing_, as_
  , effect_, where_, command_, subscription_
  )
  where


import Parse.Primitives (Parser, Cursor)
import Parse.Keyword_TH (keyword)



-- KEYWORDS


type_         :: (Cursor -> x) -> Parser x (); type_         = [keyword|type|]
alias_        :: (Cursor -> x) -> Parser x (); alias_        = [keyword|alias|]
port_         :: (Cursor -> x) -> Parser x (); port_         = [keyword|port|]
if_           :: (Cursor -> x) -> Parser x (); if_           = [keyword|if|]
then_         :: (Cursor -> x) -> Parser x (); then_         = [keyword|then|]
else_         :: (Cursor -> x) -> Parser x (); else_         = [keyword|else|]
case_         :: (Cursor -> x) -> Parser x (); case_         = [keyword|case|]
of_           :: (Cursor -> x) -> Parser x (); of_           = [keyword|of|]
let_          :: (Cursor -> x) -> Parser x (); let_          = [keyword|let|]
in_           :: (Cursor -> x) -> Parser x (); in_           = [keyword|in|]
infix_        :: (Cursor -> x) -> Parser x (); infix_        = [keyword|infix|]
left_         :: (Cursor -> x) -> Parser x (); left_         = [keyword|left|]
right_        :: (Cursor -> x) -> Parser x (); right_        = [keyword|right|]
non_          :: (Cursor -> x) -> Parser x (); non_          = [keyword|non|]
module_       :: (Cursor -> x) -> Parser x (); module_       = [keyword|module|]
import_       :: (Cursor -> x) -> Parser x (); import_       = [keyword|import|]
exposing_     :: (Cursor -> x) -> Parser x (); exposing_     = [keyword|exposing|]
as_           :: (Cursor -> x) -> Parser x (); as_           = [keyword|as|]
effect_       :: (Cursor -> x) -> Parser x (); effect_       = [keyword|effect|]
where_        :: (Cursor -> x) -> Parser x (); where_        = [keyword|where|]
command_      :: (Cursor -> x) -> Parser x (); command_      = [keyword|command|]
subscription_ :: (Cursor -> x) -> Parser x (); subscription_ = [keyword|subscription|]


