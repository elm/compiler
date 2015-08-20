import Graphics.Element exposing (show)
import Signal
import Benchmark
import Task exposing (Task, andThen)
import Text
import Array
import Random
import Time

import LargeDictionary
import ArrayBench
import ListBench
import AnimateBench
import RecordBench
import SmallRecordBench
import TupleBench
import SmallTupleBench
import CtorBench
import SmallCtorBench
import PatternMatchBench

{-| Compile this with --output=Benchmarks.html to generate
    a webpage, which runs the benchmarks and shows the results. 
-}


mySuite =
  Benchmark.Suite "My Suite" 
  [ Benchmark.bench1 "Access 20-field record 200 times:   " RecordBench.sumRandomElems indices200
  , Benchmark.bench1 "Access 9-field record 200 times:   " SmallRecordBench.sumRandomElems indices200
  
  , Benchmark.bench1 "Access 9-field tuple 200 times:   " TupleBench.sumRandomElems indices200
  , Benchmark.bench1 "Access 3-field tuple 200 times:   " SmallTupleBench.sumRandomElems indices200
  
  , Benchmark.bench1 "Access 20-constructor Type 200 times:   " CtorBench.sumFieldsList ctorData200
  , Benchmark.bench1 "Access 10-constructor Type 200 times:   " SmallCtorBench.sumFieldsList smallCtorData200
  
  , Benchmark.bench1 "Pattern match 18-levels deep, use all values, 200 times " PatternMatchBench.deepMatchSum nestedData200
  , Benchmark.bench1 "Pattern match 18-levels deep, 16 wildcards, 200 times " PatternMatchBench.wildcardMatchSum nestedData200
  , Benchmark.bench1 "Pattern match 4-levels deep, use all values, 200 times " PatternMatchBench.shallowMatchSum nestedData200
  
  , Benchmark.bench1 "Make and sum dict, 10000 elems:   " LargeDictionary.addNToDictAndSum 10000
  , Benchmark.bench1 "Make and sum dict, 50 elems:   " LargeDictionary.addNToDictAndSum 50

  , Benchmark.bench1 "Array native foldr, 500 elems:   " ArrayBench.foldrMeanStddev array500
  , Benchmark.bench1 "Array native foldl, 500 elems:   " ArrayBench.foldlMeanStddev array500
  , Benchmark.bench1 "Array custom foldr, 500 elems:   " ArrayBench.customFoldrMeanStddev array500
  , Benchmark.bench1 "Array custom foldl, 500 elems:   " ArrayBench.customFoldrMeanStddev array500
  , Benchmark.bench3 "Array square nth elem, 500 elems:   " ArrayBench.updateNRandomIndices (\x -> x*x) indices200 array500

  , Benchmark.bench1 "List native foldr, 500 elems:   " ListBench.foldrMeanStddev list500
  , Benchmark.bench1 "List native foldl, 500 elems:   " ListBench.foldlMeanStddev list500
  , Benchmark.bench1 "List custom foldr, 500 elems:   " ListBench.customFoldrMeanStddev list500
  , Benchmark.bench1 "List custom foldl, 500 elems:   " ListBench.customFoldrMeanStddev list500
  , Benchmark.bench3 "List square nth elem, 500 elems:   " ListBench.updateNRandomIndices (\x -> x*x) indices200 list500

  , Benchmark.bench2 "200 lookups in array of 500 elems:   " ArrayBench.randomLookups indices200 (Array.fromList [1 .. 500] )
  , Benchmark.bench2 "200 lookups in list of 500 elems:   " ListBench.randomLookups indices200 ([1 .. 500] )
    
  , Benchmark.bench1 "30s simulated animation @ 30fps:   " AnimateBench.runAnimation timeSteps30s        
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
 Signal.map (Graphics.Element.leftAligned << Text.fromString) results.signal



-- Random generation and structures

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
indexSeed = 
  Random.initialSeed 56


--Seed for our float generation
seed = 
  Random.initialSeed 23


--Pseudo-random array of 500 floating point numbers
array500 = 
  (ArrayBench.randomArray floatGen 500 seed)


--Same contents as array500, but in list form
list500 = 
  (ListBench.randomList floatGen 500 seed)


--Pseudo random list of 200 indices between 0 and 499
indices200 = 
  (ListBench.randomList indexGen 200 indexSeed)


--List of timesteps simulating 30 seconds of animation at 30fps
timeSteps30s = 
  List.map (\x -> (1.0 / 30.0) * (toFloat x * Time.second) ) [1 .. 900]

ctorData200 = 
  List.map CtorBench.makeRandomData indices200
  
  
smallCtorData200 = 
  List.map SmallCtorBench.makeRandomData indices200
  
  
nestedData200 = 
  List.map PatternMatchBench.intToNest indices200