import Graphics.Element exposing (show)
import Signal
import Benchmark
import Task exposing (Task, andThen)
import Text
import Array

import LargeDictionary
import ArrayBench
import ListBench
import Random
import Time
{-| Compile this with --output=Benchmarks.html to generate
    a webpage, which runs the benchmarks and shows the results. 
-}


mySuite =
  Benchmark.Suite "My Suite" 
  [ Benchmark.bench1 "Make and sum dict, 10000 elems:   " LargeDictionary.addNToDictAndSum 10000
    , Benchmark.bench1 "Make and sum dict, 50 elems:   " LargeDictionary.addNToDictAndSum 50

    , Benchmark.bench1 "Array native foldr, 500 elems:   " ArrayBench.foldrMeanStddev array500
    , Benchmark.bench1 "Array native foldl, 500 elems:   " ArrayBench.foldlMeanStddev array500
    , Benchmark.bench1 "Array custom foldr, 500 elems:   " ArrayBench.customFoldrMeanStddev array500
    , Benchmark.bench1 "Array custom foldl, 500 elems:   " ArrayBench.customFoldrMeanStddev array500

    , Benchmark.bench1 "List native foldr, 500 elems:   " ListBench.foldrMeanStddev list500
    , Benchmark.bench1 "List native foldl, 500 elems:   " ListBench.foldlMeanStddev list500
    , Benchmark.bench1 "List custom foldr, 500 elems:   " ListBench.customFoldrMeanStddev list500
    , Benchmark.bench1 "List custom foldl, 500 elems:   " ListBench.customFoldrMeanStddev list500

    , Benchmark.bench2 "200 lookups in array of 500 elems:   " ArrayBench.randomLookups indices200 (Array.fromList [1 .. 500] )
    , Benchmark.bench2 "200 lookups in list of 500 elems:   " ListBench.randomLookups indices200 ([1 .. 500] )
  ]


--Display of results:

--Mailbox updated with benchmark run progress
results : Signal.Mailbox String
results =
  Signal.mailbox "Benchmark loading"


--The actual task which runs the benchmarks
port benchResults : (Task Benchmark.Never ())
port benchResults =
  Benchmark.runWithProgress (Just results) mySuite
  `andThen` \_ -> Task.sleep 0


--Our main page:
main =
 Signal.map2 (\s1 s2 -> Graphics.Element.leftAligned <| Text.fromString (s1 ++ " " ++ (toString s2) )  ) results.signal (Time.every Time.second)



{-
Random generation and structures
-}


--Maximum size for  random numbers
maxSize = 1000000


--Random generator for floating point numbers
floatGen =
  Random.float (-1 * maxSize) (maxSize)


--Generate random indices for lists out of 500 elements
indexGen =
  Random.int (0) (499)


--Random seeds: they're fixed, so it's not truly random
--But we get seemingly random data, that we can compare fairly
--across multiple runs of the benchmark

--seed for our index generation
indexSeed = Random.initialSeed 56


--Seed for our float generation
seed = Random.initialSeed 23


--Pseudo-random array of 500 floating point numbers
array500 = (ArrayBench.randomArray floatGen 500 seed)


--Same contents as array500, but in list form
list500 = (ListBench.randomList floatGen 500 seed)


--Pseudo random list of 200 indices between 0 and 499
indices200 = (ListBench.randomList indexGen 200 indexSeed)

