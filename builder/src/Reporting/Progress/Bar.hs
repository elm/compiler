{-# OPTIONS_GHC -Wall #-}
module Reporting.Progress.Bar
  ( render
  , clear
  )
  where



-- PROGRESS BAR


width :: Float
width =
  50.0


render :: Int -> Int -> Int -> String
render successes failures total =
  let
    numDone =
      successes + failures

    fraction =
      fromIntegral numDone / fromIntegral total

    unitsDone =
      truncate (fraction * width)

    unitsLeft =
      truncate width - unitsDone
  in
    "\r["
    ++ replicate unitsDone '='
    ++ replicate unitsLeft ' '
    ++ "] - "
    ++ show numDone ++ " / " ++ show total


clear :: String
clear =
  '\r' : replicate (length (render 49999 50000 99999)) ' ' ++ "\r"

