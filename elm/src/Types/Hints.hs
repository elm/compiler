
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
            , numScheme (\t -> t ==> text ==> text) "Text.height"
            ] ++ hasType (text ==> text) textToText

elements = let iee = int ==> element ==> element in
           [ "flow"    -: direction ==> listOf element ==> element
           , "layers"  -: listOf element ==> element
           , "text"    -: text ==> element
           , "opacity" -: float ==> element ==> element
           , "width"   -: iee
           , "height"  -: iee
           , "size"    -: int ==> iee
           , "box"     -: iee
           , "centeredText"  -: text ==> element
           , "justifiedText" -: text ==> element
           , "collage" -: int ==> int ==> listOf form ==> element
           ]

directions = hasType direction ["up","down","left","right","inward","outward"]
colors = [ numScheme (\n -> n ==> n ==> n ==> color) "rgb"
         , numScheme (\n -> n ==> n ==> n ==> n ==> color) "rgba"
         ] ++ hasType color ["red","green","blue","black","white"
                            ,"yellow","cyan","magenta","grey","gray"]

lineTypes = [ numScheme (\n -> listOf (pairOf n) ==> line) "line"
            , "customLine" -: listOf int ==> color ==> line ==> form
            ] ++ hasType (color ==> line ==> form) ["solid","dashed","dotted"]

shapes = [ twoNums (\n m -> listOf (pairOf n) ==> pairOf m ==> shape) "polygon"
         , "filled"        -: color ==> shape ==> form
         , "outlined"      -: color ==> shape ==> form
         , "customOutline" -: listOf int ==> color ==> shape ==> form
         ] ++ map (twoNums (\n m -> n ==> n ==> pairOf m ==> shape)) [ "ngon"
                                                                     , "rect"
                                                                     , "oval" ]


--------  Foreign  --------

casts =
  [ "castJSBoolToBool"       -: jsBool ==> bool
  , "castBoolToJSBool"       -: bool ==> jsBool
  , "castJSNumberToInt"      -: jsNumber ==> int
  , "castIntToJSNumber"      -: int ==> jsNumber
  , "castJSElementToElement" -: int ==> int ==> jsElement ==> element
  , "castElementToJSElement" -: element ==> jsElement
  , "castJSStringToString"   -: jsString ==> string
  , "castStringToJSString"   -: string ==> jsString
  , "castJSNumberToFloat"    -: jsNumber ==> float 
  , "castFloatToJSNumber"    -: float ==> jsNumber
  ]

castToTuple n = (,) name $ Forall [1..n] [] (jsTuple vs ==> tupleOf vs)
    where vs = map VarT [1..n]
          name = "castJSTupleToTuple" ++ show n
castToJSTuple n = (,) name $ Forall [1..n] [] (tupleOf vs ==> jsTuple vs)
    where vs = map VarT [1..n]
          name = "castTupleToJSTuple" ++ show n

polyCasts =
  map castToTuple [2..5] ++ map castToJSTuple [2..5] ++
  [ "castJSArrayToList"   -:: jsArray a ==> listOf a
  , "castListToJSArray"   -:: listOf a ==> jsArray a
  ]

json =
  [ "JsonString" -: string ==> jsonValue
  , "JsonBool"   -: bool   ==> jsonValue
  , "JsonNull"   -: jsonValue
  , "JsonArray"  -: listOf jsonValue ==> jsonValue
  , "JsonObject" -: jsonObject ==> jsonValue
  , numScheme (\n -> n ==> jsonValue) "JsonNumber"
  , "toPrettyString" -: string ==> jsonObject ==> string
  , "toPrettyJSString" -: string ==> jsonObject ==> jsString
  , "toJSString" -: jsonObject ==> jsString
  ]


--------  Signals  --------

sig n name = (,) name $ Forall [1..n] [] (fn ts ==> fn (map signalOf ts))
    where fn = foldr1 (==>)
          ts = map VarT [1..n]

signals =
    [ sig 1 "constant"
    , sig 2 "lift" 
    , sig 3 "lift2"
    , sig 4 "lift3"
    , sig 5 "lift4"
    , "foldp" -:: (a ==> b ==> b) ==> b ==> signalOf a ==> signalOf b
    , "randomize" -:: int ==> int ==> signalOf a ==> signalOf int
    , "count"     -:: signalOf a ==> signalOf int
    , "keepIf"    -:: (a==>bool) ==> a ==> signalOf a ==> signalOf a
    , "dropIf"    -:: (a==>bool) ==> a ==> signalOf a ==> signalOf a
    , "keepWhen"  -:: signalOf bool ==>a==> signalOf a ==> signalOf a
    , "dropWhen"  -:: signalOf bool ==>a==> signalOf a ==> signalOf a
    , "dropRepeats" -:: signalOf a ==> signalOf a
    , "sampleOn" -:: signalOf a ==> signalOf b ==> signalOf b
    ]

concreteSignals = 
  [ "keysDown"    -: signalOf (listOf int)
  , "charPressed" -: signalOf (maybeOf int)
  , "inRange"     -: int ==> int ==> signalOf int
  , timeScheme "every"  (\t -> t ==> signalOf t)
  , timeScheme "before" (\t -> t ==> signalOf bool)
  , timeScheme "after"  (\t -> t ==> signalOf bool)
  , "dimensions"  -: signalOf point
  , "position"    -: signalOf point
  , "x"           -: signalOf int
  , "y"           -: signalOf int
  , "isDown"      -: signalOf bool
  , "isClicked"   -: signalOf bool
  , "textField"   -: string ==> tupleOf [element, signalOf string]
  , "password"    -: string ==> tupleOf [element, signalOf string]
  , "textArea"    -: int ==> int ==> tupleOf [element, signalOf string]
  , "checkBox"    -: bool ==> tupleOf [element, signalOf bool]
  , "button"      -: string ==> tupleOf [element, signalOf bool]
  , "stringDropDown" -: listOf string ==> tupleOf [element, signalOf string]
  ]

--------  Math and Binops  --------

binop t = t ==> t ==> t
scheme1 super t name = (name, Forall [0] [VarT 0 :<: super] (t (VarT 0)))
numScheme t name = scheme1 number t name
timeScheme name t = scheme1 time t name
twoNums f name =
    (,) name . Forall [0,1] [ VarT 0 :<: number, VarT 1 :<: number ] $
        f (VarT 0) (VarT 1)

math =
  map (numScheme (\t -> t ==> binop t)) ["clamp"] ++
  map (numScheme (\t -> binop t)) ["+","-","*","max","min"] ++
  [ numScheme (\t -> t ==> t) "abs" ] ++
  hasType (binop float) [ "/", "logBase" ] ++
  hasType (binop int) ["rem","div","mod"] ++
  hasType (float ==> float) ["sin","cos","tan","asin","acos","atan","sqrt"] ++
  hasType float ["pi","e"] ++
  hasType (int ==> float) ["toFloat","castIntToFloat"] ++
  hasType (float ==> int) ["round","floor","ceiling","truncate"]

bools =
  [ "not" -: bool ==> bool ] ++
  hasType (binop bool) ["&&","||"] ++
  map (scheme1 comparable (\t -> t ==> t ==> bool))  ["<",">","<=",">="] ++
  [ ( "compare"
    , Forall [0,1] [ VarT 0 :<: comparable ] (VarT 0 ==> VarT 0 ==> VarT 1) )
  ]

--------  Polymorphic Functions  --------

var = VarT `liftM` guid
vars n = mapM (const var) [1..n]

infix 8 -::
name -:: tipe = (name, Forall [1,2,3] [] tipe)

[a,b,c] = map VarT [1,2,3]

funcs =
    [ "id"   -:: a ==> a
    , "=="   -:: a ==> a ==> bool
    , "/="   -:: a ==> a ==> bool
    , "flip" -:: (a ==> b ==> c) ==> (b ==> a ==> c)
    , "."    -:: (b ==> c) ==> (a ==> b) ==> (a ==> c)
    , "$"    -:: (a ==> b) ==> a ==> b
    , ":"       -:: a ==> listOf a ==> listOf a
    , (,) "++" . Forall [0,1] [ VarT 0 :<: appendable (VarT 1) ] $ VarT 0 ==> VarT 0 ==> VarT 0
    , "Cons"    -:: a ==> listOf a ==> listOf a 
    , "Nil"     -:: listOf a
    , "Just"    -:: a ==> maybeOf a
    , "Nothing" -:: maybeOf a
    , "elmRange" -:: int ==> int ==> listOf int
    ]

lists =
  [ "and"  -:: listOf bool ==> bool
  , "or"   -:: listOf bool ==> bool
  , numScheme (\n -> listOf n ==> listOf n) "sort"
  , "head"    -:: listOf a ==> a
  , "tail"    -:: listOf a ==> listOf a
  , "length"  -:: listOf a ==> int
  , "filter"  -:: (a ==> bool) ==> listOf a ==> listOf a
  , "foldr1"  -:: (a ==> a ==> a) ==> listOf a ==> a
  , "foldl1"  -:: (a ==> a ==> a) ==> listOf a ==> a
  , "scanl1"  -:: (a ==> a ==> a) ==> listOf a ==> a
  , "forall"  -:: (a ==> bool) ==> listOf a ==> bool
  , "exists"  -:: (a ==> bool) ==> listOf a ==> bool
  , "reverse" -:: listOf a ==> listOf a
  , "take"    -:: int ==> listOf a ==> listOf a
  , "drop"    -:: int ==> listOf a ==> listOf a
  , "partition"    -:: (a ==> bool) ==> listOf a ==> tupleOf [listOf a,listOf a]
  , "intersperse"  -:: a ==> listOf a ==> listOf a
  , "zip"   -:: listOf a ==>listOf b ==>listOf(tupleOf [a,b])
  , "map"   -:: (a ==> b) ==> listOf a ==> listOf b
  , "foldr" -:: (a ==> b ==> b) ==> b ==> listOf a ==> b
  , "foldl" -:: (a ==> b ==> b) ==> b ==> listOf a ==> b
  , "scanl" -:: (a ==> b ==> b) ==> b ==> listOf a ==> listOf b
  , (,) "concat"      . Forall [0,1]   [ VarT 0 :<: appendable (VarT 1) ] $
        listOf (VarT 0) ==> VarT 0
  , (,) "concatMap"   . Forall [0,1,2] [ VarT 0 :<: appendable (VarT 1) ] $
        (VarT 2 ==> VarT 0) ==> listOf (VarT 2) ==> VarT 0
  , (,) "intercalate" . Forall [0,1]   [ VarT 0 :<: appendable (VarT 1) ] $
        VarT 0 ==> listOf (VarT 0) ==> VarT 0
  , "zipWith" -:: (a ==> b ==> c) ==> listOf a ==> listOf b ==> listOf c
  ] ++ map (numScheme (\n -> listOf n ==> n)) [ "sum", "product"
                                              , "maximum", "minimum" ]


--------  Everything  --------

hints =
  concat [ funcs, lists, signals, math, bools, str2elem, textAttrs
         , elements, directions, colors, lineTypes, shapes
         , concreteSignals, casts, polyCasts, json
         ]
