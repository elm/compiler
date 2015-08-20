module RecordBench where

type alias BigRecord = 
  { myLargeNamedField1 : Float 
  , myLargeNamedField2 : Float
  , myLargeNamedField3 : Float
  , myLargeNamedField4 : Float
  , myLargeNamedField5 : Float
  , myLargeNamedField6 : Float
  , myLargeNamedField7 : Float
  , myLargeNamedField8 : Float
  , myLargeNamedField9 : Float
  , myLargeNamedField10 : Float
  , myLargeNamedField11 : Float
  , myLargeNamedField12 : Float
  , myLargeNamedField13 : Float
  , myLargeNamedField14 : Float
  , myLargeNamedField15 : Float
  , myLargeNamedField16 : Float
  , myLargeNamedField17 : Float
  , myLargeNamedField18 : Float
  , myLargeNamedField19 : Float
  , myLargeNamedField20 : Float
  }
  
defaultRec = 
  BigRecord 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0
    11.0 12.0 13.0 14.0 15.0 16.0 17.0 18.0 19.0 20.0
  
getField i = 
  case (i % 20) of
    0 -> 
      defaultRec.myLargeNamedField20
    1 -> 
      defaultRec.myLargeNamedField1
    2 -> 
      defaultRec.myLargeNamedField2
    3 -> 
      defaultRec.myLargeNamedField3
    4 -> 
      defaultRec.myLargeNamedField4
    5 -> 
      defaultRec.myLargeNamedField5
    6 -> 
      defaultRec.myLargeNamedField6
    7 -> 
      defaultRec.myLargeNamedField7
    8 -> 
      defaultRec.myLargeNamedField8
    9 -> 
      defaultRec.myLargeNamedField9
    10 -> 
      defaultRec.myLargeNamedField10
    11 -> 
      defaultRec.myLargeNamedField11
    12 -> 
      defaultRec.myLargeNamedField12
    13 -> 
      defaultRec.myLargeNamedField13
    14 -> 
      defaultRec.myLargeNamedField14
    15 -> 
      defaultRec.myLargeNamedField15
    16 -> 
      defaultRec.myLargeNamedField16
    17 -> 
      defaultRec.myLargeNamedField17
    18 -> 
      defaultRec.myLargeNamedField18
    19 -> 
      defaultRec.myLargeNamedField19
      

sumRandomElems : List Int -> Float
sumRandomElems randomInts = 
  List.foldr (\i sumSoFar -> (getField i) + sumSoFar ) 0.0 randomInts