module SmallRecordBench where

type alias SmallRecord = 
  { myLargeNamedField1 : Float 
  , myLargeNamedField2 : Float
  , myLargeNamedField3 : Float
  , myLargeNamedField4 : Float
  , myLargeNamedField5 : Float
  , myLargeNamedField6 : Float
  , myLargeNamedField7 : Float
  , myLargeNamedField8 : Float
  , myLargeNamedField9 : Float
  }
  
defaultRec = 
  SmallRecord 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 
  
getField i = 
  case (i % 9) of
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
    0 -> 
      defaultRec.myLargeNamedField9
      

sumRandomElems : List Int -> Float
sumRandomElems randomInts = 
  List.foldr (\i sumSoFar -> (getField i) + sumSoFar ) 0.0 randomInts