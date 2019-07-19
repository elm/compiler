
# Variable Shadowing

Variable shadowing is when you define the same variable name twice in an ambiguous way. Here is a pretty reasonable use of shadowing:

```elm
viewName : Maybe String -> Html msg
viewName name =
  case name of
    Nothing ->
      ...

    Just name ->
      ...
```

I define a `name` with type `Maybe String` and then in that second branch, I define a `name` that is a `String`. Now that there are two `name` values, it is not 100% obvious which one you want in that second branch.

Most linters produce warnings on variable shadowing, so Elm makes “best practices” the default. Just rename the first one to `maybeName` and move on.

This choice is relatively uncommon in programming languages though, so I want to provide the reasoning behind it.


## The Cost of Shadowing

The code snippet from above is the best case scenario for variable shadowing. It is pretty clear really. But that is because it is a fake example. It does not even compile.

In a large module that is evolving over time, this is going to cause bugs in a very predictable way. You will have two definitions, separated by hundreds of lines. For example:

```elm
name : String
name =
  "Tom"

-- hundreds of lines

viewName : String -> Html msg
viewName name =
  ... name ... name ... name ...
```

Okay, so the `viewName` function has an argument `name` and it uses it three times. Maybe the `viewName` function is 50 lines long in total, so those uses are not totally easy to see. This is fine so far, but say your colleague comes along five months later and wants to support first and last names. They refactor the code like this:

```elm
viewName : String -> String -> Html msg
viewName firstName lastName =
  ... name ... name ... name ...
```

The code compiles, but it does not work as intended. They forgot to change all the uses of `name`, and because it shadows the top-level `name` value, it always shows up as `"Tom"`. It is a simple mistake, but it is always the last thing I think of.

> Is the data being fetched properly? Let me log all of the JSON requests. Maybe the JSON decoders are messed up? Hmm. Maybe someone is transforming the name in a bad way at some point? Let me check my `update` code.

Basically, a bunch of time gets wasted on something that could easily be detected by the compiler. But this bug is rare, right?


## Aggregate Cost

Thinking of a unique and helpful name takes some extra time. Maybe 30 seconds. But it means that:

  1. Your code is easier to read and understand later on. So you spend 30 seconds once `O(1)` rather than spending 10 seconds each time someone reads that code in the future `O(n)`.

  2. The tricky shadowing bug described above is impossible. Say there is a 5% chance that any given edit produces a shadowing bug, and that resolving that shadowing bug takes one hour. That means the expected time for each edit increases by three minutes.

If you are still skeptical, I encourage you can play around with the number of edits, time costs, and probabilities here. When shadowing is not allowed, the resulting overhead for the entire lifetime of the code is the 30 seconds it takes to pick a better name, so that is what you need to beat!


## Summary

Without shadowing, the code easier to read and folks spend less time on pointless debugging. The net outcome is that folks have more time to make something wonderful with Elm!
