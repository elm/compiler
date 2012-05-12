
module Hints (hints) where

import Control.Monad (liftM,mapM)
import Types
import Guid

element = ADT "Element" []
direction = ADT "Direction" []
text = ADT "Text" []
listOf t = ADT "List" [t]
string = listOf CharT

infixr ==>
t1 ==> t2 = LambdaT t1 t2

infix 8 -:
name -: tipe = (,) name tipe

hasType t = map (-: t)

--------  Text and Elements  --------

str2elem = hasType (string ==> element) [ "image","video","plainText" ]

textToText = ["header", "italic", "bold", "underline"
             , "overline", "strikeThrough", "monospace" ]

textAttrs = [ "toText" -: string ==> text
            , "link"   -: string ==> text ==> text
            , "height" -: IntT ==> text ==> text
            ] ++ hasType (text ==> text) textToText

elements = let iee = IntT ==> element ==> element in
           [ "flow"    -: direction ==> listOf element ==> element
           , "opacity" -: iee
           , "width"   -: iee
           , "height"  -: iee
           , "size"    -: IntT ==> iee
           , "box"     -: iee
           ]


--------  Math and Binops  --------

iii = IntT ==> IntT ==> IntT
xxb x = x ==> x ==> BoolT

math =
  hasType (IntT ==> iii) ["clamp"] ++
  hasType iii ["+", "-", "*", "/","rem","mod","logBase","max","min"] ++
  hasType (IntT ==> IntT) ["sin","cos","tan","asin","acos","atan","sqrt","abs"]

bool =
  [ "not" -: BoolT ==> BoolT ] ++
  hasType (xxb BoolT) ["&&","||"] ++
  hasType (xxb IntT)  ["==","/=","<",">","<=",">="]


--------  Polymorphic Functions  --------

var = VarT `liftM` guid
vars n = mapM (const var) [1..n]

infix 8 -::
name -:: tipe = return $ name -: tipe

funcs = sequence
    [ do a <- var          ; "id"   -:: a ==> a
    , do [a,b,c] <- vars 3 ; "flip" -:: (a ==> b ==> c) ==> (b ==> a ==> c)
    , do [a,b,c] <- vars 3 ; "."    -:: (b ==> c) ==> (a ==> b) ==> (a ==> c)
    , do [a,b] <- vars 2   ; "$"    -:: (a ==> b) ==> a ==> b
    ]

lists = sequence
    [ do a <- var ; ":"       -:: a ==> listOf a ==> listOf a
    , do a <- var ; "++"      -:: listOf a ==> listOf a ==> listOf a
    , do a <- var ; "head"    -:: listOf a ==> a
    , do a <- var ; "tail"    -:: listOf a ==> listOf a
    , do a <- var ; "length"  -:: listOf a ==> IntT
    , do a <- var ; "filter"  -:: (a ==> BoolT) ==> listOf a ==> listOf a
    , do a <- var ; "reverse" -:: listOf a ==> listOf a
    , do [a,b] <- vars 2 ; "map"   -:: (a ==> b) ==> listOf a ==> listOf b
    , do [a,b] <- vars 2 ; "foldr" -:: (a ==> b ==> b) ==> b ==> listOf a ==> b
    , do [a,b] <- vars 2 ; "foldl" -:: (a ==> b ==> b) ==> b ==> listOf a ==> b
    ]


--------  Everything  --------

hints = do fs <- funcs ; ls <- lists
           return $ fs ++ ls ++ str2elem ++ textAttrs ++ elements ++ math ++ bool
