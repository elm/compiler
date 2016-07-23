Benchmarking the Elm compiler
=============================

This script can help measure the performance of the Elm compiler. To benchmark
the compiler, type `cabal run compiler-benchmark` in the directory containing
`elm-compiler.cabal`. By default, this will download stable versions of
elm-compiler, elm-make, and elm-package from GitHub, and it will compile a set
of Elm projects using the stable version of the compiler, and the version of
the Elm compiler in the enclosing repository.

To update the git branches that should be considered "stable", edit the
`sources` list at the top of `Benchmark.hs`.

To add a new Elm project to the benchmarking script, edit the `projects` list
at the top of `Benchmark.hs`.
