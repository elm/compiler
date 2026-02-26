# Test Harness Design

## Context

The Elm compiler's test suite was removed in commit `e2008b50` (Feb 2018) with the
message "Get rid of testing stuff. Needs to be redone anyway for 0.19". This design
re-introduces a test harness updated for the 0.19 compiler API.

## Architecture: Two-Layer Testing

### Layer 1: Parse Tests (fast, no dependencies)

Call `Parse.Module.fromByteString` directly from Haskell. No interfaces needed.

- `tests/parse-good/` — files that should parse successfully
- `tests/parse-bad/` — files that should produce parse errors
- Most valuable layer for adding language features (syntax changes start at the parser)

### Layer 2: Compile Tests (full pipeline, subprocess)

Shell out to the built `elm` binary via `System.Process`.

- `tests/compile-good/` — files that should compile (real `elm.json` project)
- `tests/compile-bad/` — files that should fail type-checking/canonicalization
- Runs `elm make --output=/dev/null`, checks exit code

### Why Two Layers?

The 0.19 `Compile.compile` requires `Map ModuleName.Raw I.Interface` for all default
imports (Basics, List, Maybe, etc.). These interfaces can only be created by compiling
real Elm source — they can't be hand-built. `Parse.Module.fromByteString` works
standalone with no dependencies.

## File Structure

```
tests/
├── Test.hs                    -- Main test runner (tasty)
├── Test/
│   ├── Parse.hs               -- Parse test discovery + execution
│   └── Compile.hs             -- Compilation test discovery + execution
├── parse-good/                -- .elm files that should parse OK
│   ├── Literals.elm
│   ├── Records.elm
│   ├── PatternMatch.elm
│   ├── Functions.elm
│   ├── Types.elm
│   └── Comments.elm
├── parse-bad/                 -- .elm files with syntax errors
│   ├── UnclosedString.elm
│   ├── BadIndentation.elm
│   ├── MissingModuleHeader.elm
│   └── InvalidOperator.elm
├── compile-good/              -- elm project: files that compile
│   ├── elm.json
│   └── src/
│       ├── BasicExpressions.elm
│       ├── PatternMatching.elm
│       └── TypeAnnotations.elm
└── compile-bad/               -- elm project: files that fail
    ├── elm.json
    └── src/
        ├── InfiniteType.elm
        ├── IncompletePattern.elm
        └── TypeMismatch.elm
```

## Cabal Test Suite

Framework: `tasty` with `tasty-hunit`.

```cabal
Test-Suite elm-tests
    Type: exitcode-stdio-1.0
    Hs-Source-Dirs: tests, compiler/src
    Main-Is: Test.hs
    ghc-options: -O0 -Wall
    Build-depends:
        tasty, tasty-hunit,
        base, bytestring, directory, filepath, process,
        ... (shared compiler deps for parse tests)
```

- `-O0` for fast test compilation
- Shares `compiler/src` so parse tests call internal APIs directly
- Compile tests shell out to the `elm` binary

## Test File Updates for 0.19

All test files written fresh for 0.19 syntax:

| 0.18 Pattern | 0.19 Status |
|---|---|
| `Debug.crash` | Replaced by `Debug.todo` |
| Custom operators | Removed entirely |
| Variable shadowing | Now a compile error |
| `toString` | `String.fromInt` / `String.fromFloat` |
| Tuples > 3 elements | Max 3 elements |
| Partial constructor exposure | `MyType(..)` or `MyType` only |

## Compile Test Project

Each compile test directory needs a valid `elm.json`:

```json
{
    "type": "application",
    "source-directories": ["src"],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": { "elm/core": "1.0.5" },
        "indirect": { "elm/json": "1.1.3" }
    },
    "test-dependencies": { "direct": {}, "indirect": {} }
}
```

Requires `elm/core` installed. First test run fetches it, or run `elm install` manually.
