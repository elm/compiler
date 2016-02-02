module Syntax.Records (benchmark) where

import Benchmark exposing (Benchmark, test, suite)
import String


benchmark : Benchmark.Benchmark
benchmark =
  suite "records"
    [ suite "update"
        [ test "1 of 2 fields" (update1of2 record2)
        , test "2 of 2 fields" (update2of2 record2)
        , test "2 of 2 new" (new2of2 record2)
        , test "3 of 5 fields" (update3of5 record5)
        , test "5 of 5 fields" (update5of5 record5)
        , test "5 of 5 new" (new5of5 record5)
        ]
    , suite "access"
        [ test "2 of 2" (access2of2 record2)
        , test "4 of 10" (access4of10 record10)
        ]
    ]


-- RECORDS WITH 2 FIELDS

record2 =
  { x = 3
  , y = 4
  }


update1of2 r _ =
  { r | x = r.x + 42 }


update2of2 r _ =
  { r
    | x = r.x + 1
    , y = r.y + 1
  }


new2of2 r _ =
  { x = r.x + 1
  , y = r.y + 1
  }


access2of2 r _ =
  r.x + r.y


-- RECORDS WITH 5 FIELDS

record5 =
  { product = "iPhone 6s"
  , storage = 16
  , weight = 143
  , ppi = 326
  , chip = "A9"
  }


update3of5 r _ =
  { r
    | product = "iPhone 6s Plus"
    , weight = 192
    , ppi = 401
  }


update5of5 r _ =
  { r
    | product = "iPhone 6"
    , storage = r.storage
    , weight = 129
    , ppi = r.ppi
    , chip = "A8"
  }


new5of5 r _ =
  { product = "iPhone 6"
  , storage = r.storage
  , weight = 129
  , ppi = r.ppi
  , chip = "A8"
  }


-- RECORDS WITH 10 FIELDS

record10 =
  { a = 1
  , b = 2
  , c = 3
  , d = 4
  , e = True
  , f = 'a'
  , g = "abc"
  , h = record2
  , i = record5
  , j = [1,2,3]
  }


access4of10 r _ =
  r.a + r.b + r.c + r.d
