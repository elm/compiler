#!/bin/bash

cd /home/USER/compiler/worker/
./dist-newstyle/build/x86_64-linux/ghc-8.6.3/elm-0.19.1/x/worker/build/worker/worker &
echo $! > worker.pid
