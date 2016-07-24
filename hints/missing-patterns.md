# Hints for Missing Patterns

Elm checks to make sure that all possible inputs to a function or `case` are handled. This gives us the guarantee that no Elm code is ever going to crash because data had an unexpected shape.

There are a couple techniques for making this work for you in every scenario.


## The danger of wildcard patterns

A common scenario is that you want to add a tag to a union type that is used in a bunch of places. For example, maybe you are working with some widgets:

```elm
type Widget
    = ScatterPlot ...
    | Table ...


isGraph : Widget -> Bool
isGraph widget =
  case widget of
    ScatterPlot _ ->
        True

    _ ->
        False
```

Notice the wildcard pattern in `isGraph`. This will hurt us! When you add `BarChart` as a new kind of `Widget` you are introducing a bug that the compiler cannot catch. A `BarChart` is a graph, but the code deciding that is just lumping a bunch of stuff together.

It is better to explicitly list all possible patterns, like this:

```elm
isGraph : Widget -> Bool
isGraph widget =
  case widget of
    ScatterPlot _ ->
        True

    Table _ ->
        False
```

So now when you want to add `BarChart`, the compiler will say "hey, what should `isGraph` do when it sees a bar chart?" This is a tiny bit of extra work, but it is very worth it!


## I want to go fast!

Okay, so we are still editing that `Widget` code. We are not using any dangerous wildcards anymore, but there are 20 or 30 functions all pattern matching on `Widget` and doing something.

```elm
type Widget
    = ScatterPlot ...
    | Table ...


isGraph : Widget -> Bool
isGraph widget =
  case widget of
    ScatterPlot _ ->
        True

    Table _ ->
        False


-- and maybe a bunch of other things
```

So now you add a new tag `BarChart` to `Widget`. This will trigger errors in all the functions that pattern match on widgets. In a big project, maybe this is a ton of functions.

To get things running quickly, it can be helpful to use [`Debug.crash`](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash) temporarily. So you might update the code to this:

```elm
type Widget
    = ScatterPlot ...
    | Table ...
    | BarChart ...


isGraph : Widget -> Bool
isGraph widget =
  case widget of
    ScatterPlot _ ->
        True

    Table _ ->
        False

    BarChart _ ->
        Debug.crash "TODO"


-- and maybe a bunch of other things
```

Rather than figure out the exact details, just put in `Debug.crash "TODO"` for now. The Elm compiler is actually aware of `Debug.crash` so when it sees it in a `case` like this, it will augment the crash message with a bunch of helpful information. It will tell you:

  1. The name of the module that contains the offending code.
  2. The line numbers of the `case` containing the crash.
  3. The particular value that led to this crashing branch.

From that information you have a pretty good idea of what went wrong and can go fix it.

I tend to use `TODO` as the message when my goal is to go quick because it makes it easy to go and find all remaining todos in my code before a release.

> And in 99.999% of cases, a `Debug.crash` should never make it to a release. It is nice for debugging (like it says in the name) but it is not really a good idea to use it in other ways. When you hit a `Debug.crash` the program really does crash. It is just done working.


## A list that definitely is not empty

This can come up from time to time, but Elm **will not** let you write code like this:

```elm
last : List a -> a
last list =
  case list of
    [x] ->
        x

    _ :: rest ->
        last rest
```

This is no good. It does not handle the empty list. The recommended definition would be:

```elm
last : List a -> Maybe a
last list =
  case list of
    [] ->
        Nothing

    [x] ->
        Just x

    _ :: rest ->
        last rest
```

This is nice because it lets users know that there might be a failure, so they can recover from it however they want.
