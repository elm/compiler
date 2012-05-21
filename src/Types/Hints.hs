
module Hints (hints) where

import Control.Monad (liftM,mapM)
import Control.Arrow (first)
import Types
import Guid


--------  Text and Elements  --------

str2elem = hasType (string ==> element) [ "image","video","plainText" ]

textToText = [ "header", "italic", "bold", "underline"
             , "overline", "strikeThrough", "monospace" ]

textAttrs = [ "toText" -: string ==> text
            , "link"   -: string ==> text ==> text
            , "Text.height" -: IntT ==> text ==> text
            ] ++ hasType (text ==> text) textToText

elements = let iee = IntT ==> element ==> element in
           [ "flow"    -: direction ==> listOf element ==> element
           , "layers"  -: listOf element ==> element
           , "text"    -: text ==> element
           , "opacity" -: iee
           , "width"   -: iee
           , "height"  -: iee
           , "size"    -: IntT ==> iee
           , "box"     -: iee
           , "centeredText"  -: text ==> element
           , "justifiedText" -: text ==> element
           , "collage" -: IntT ==> IntT ==> listOf form ==> element
           ]

directions = hasType direction ["up","down","left","right","inward","outward"]
colors = [ "rgb"  -: IntT ==> IntT ==> IntT ==> color
         , "rgba" -: IntT ==> IntT ==> IntT ==> IntT ==> color
         ] ++ hasType color ["red","green","blue","black","white"]

lineTypes = [ "line"       -: listOf point ==> line
            , "customLine" -: listOf IntT ==> color ==> line ==> form
            ] ++ hasType (color ==> line ==> form) ["solid","dashed","dotted"]

shapes = [ "polygon"       -: listOf point ==> point ==> shape
         , "filled"        -: color ==> shape ==> form
         , "outlined"      -: color ==> shape ==> form
         , "customOutline" -: listOf IntT ==> color ==> shape ==> form
         ] ++ hasType (IntT ==> IntT ==> point ==> shape) ["ngon","rect","oval"]


--------  Signals  --------

sig ts = fn ts ==> fn (map signalOf ts)
    where fn = foldr1 (==>)

signals = sequence
    [ do ts <- vars 1 ; "constant" -:: sig ts
    , do ts <- vars 2 ; "lift"     -:: sig ts
    , do ts <- vars 3 ; "lift2"    -:: sig ts
    , do ts <- vars 4 ; "lift3"    -:: sig ts
    , do ts <- vars 5 ; "lift4"    -:: sig ts
    , do [a,b] <- vars 2 
         "foldp" -:: (a ==> b ==> b) ==> b ==> signalOf a ==> signalOf b
    ]

concreteSignals =
    [ "Mouse.position"    -: signalOf point
    , "Mouse.x"           -: signalOf IntT
    , "Mouse.y"           -: signalOf IntT
    , "Mouse.isDown"      -: signalOf BoolT
    , "Mouse.isClicked"   -: signalOf BoolT
    , "Window.dimensions" -: signalOf point
    , "Window.width"      -: signalOf IntT
    , "Window.height"     -: signalOf IntT
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
    , do a <- var ; ":"       -:: a ==> listOf a ==> listOf a
    , do a <- var ; "++"      -:: listOf a ==> listOf a ==> listOf a
    , do a <- var ; "Cons"    -:: a ==> listOf a ==> listOf a 
    , do a <- var ; "Nil"     -:: listOf a
    , do a <- var ; "Just"    -:: a ==> ADT "Maybe" [a]
    , do a <- var ; "Nothing" -:: ADT "Maybe" [a]
    , "elmRange" -:: IntT ==> IntT ==> listOf IntT
    ]

ints = map (-: (listOf IntT ==> IntT)) [ "sum","product","maximum","minimum" ]

lists = liftM (map (first ("List."++)) . (++ints)) . sequence $
    [ "and"  -:: listOf BoolT ==> BoolT
    , "or"   -:: listOf BoolT ==> BoolT
    , "sort" -:: listOf IntT ==> listOf IntT
    , do a <- var ; "head"    -:: listOf a ==> a
    , do a <- var ; "tail"    -:: listOf a ==> listOf a
    , do a <- var ; "length"  -:: listOf a ==> IntT
    , do a <- var ; "filter"  -:: (a ==> BoolT) ==> listOf a ==> listOf a
    , do a <- var ; "foldr1"  -:: (a ==> a ==> a) ==> listOf a ==> a
    , do a <- var ; "foldl1"  -:: (a ==> a ==> a) ==> listOf a ==> a
    , do a <- var ; "scanl1"  -:: (a ==> a ==> a) ==> listOf a ==> a
    , do a <- var ; "forall"  -:: (a ==> BoolT) ==> listOf a ==> BoolT
    , do a <- var ; "exists"  -:: (a ==> BoolT) ==> listOf a ==> BoolT
    , do a <- var ; "concat"  -:: listOf (listOf a) ==> listOf a
    , do a <- var ; "reverse" -:: listOf a ==> listOf a
    , do a <- var ; "intersperse"  -:: a ==> listOf a ==> listOf a
    , do a <- var ; "intercalate"  -:: listOf a ==> listOf(listOf a) ==> listOf a
    , do [a,b] <- vars 2 ; "zip"   -:: listOf a ==>listOf b ==>listOf(tupleOf [a,b])
    , do [a,b] <- vars 2 ; "map"   -:: (a ==> b) ==> listOf a ==> listOf b
    , do [a,b] <- vars 2 ; "foldr" -:: (a ==> b ==> b) ==> b ==> listOf a ==> b
    , do [a,b] <- vars 2 ; "foldl" -:: (a ==> b ==> b) ==> b ==> listOf a ==> b
    , do [a,b] <- vars 2 ; "scanl" -:: (a==>b==>b)==>b==>listOf a==>listOf b
    , do [a,b] <- vars 2 ; "concatMap" -:: (a==>listOf b)==>listOf a ==> listOf b
    , do [a,b,c] <- vars 3
         "zipWith" -:: (a ==> b ==> c) ==> listOf a ==> listOf b ==> listOf c
    ]


--------  Everything  --------

hints = do
  fs <- funcs ; ls <- lists ; ss <- signals
  return $ concat [ fs, ls, ss, math, bool, str2elem, textAttrs
                  , elements, directions, colors, lineTypes, shapes
                  , concreteSignals
                  ]
