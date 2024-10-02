
# Implicit Casts

Many languages automatically convert from `Int` to `Float` when they think it is necessary. This conversion is often called an [implicit cast](https://en.wikipedia.org/wiki/Type_conversion).

Languages that will add in implicit casts for addition include:

  - JavaScript
  - Python
  - Ruby
  - C
  - C++
  - C#
  - Java
  - Scala

These languages generally agree that an `Int` may be implicitly cast to a `Float` when necessary. So everyone is doing it, why not Elm?!

## Type Inference + Implicit Casts

Elm comes from the ML-family of languages. Languages in the ML-family that **never** do implicit casts include:

  - Standard ML
  - OCaml
  - Elm
  - F#
  - Haskell

Why would so many languages from this lineage require explicit conversions though?

Well, we have to go back to the 1970s for some background. J. Roger Hindley and Robin Milner independently discovered an algorithm that could _efficiently_ figure out the type of everything in your program without any type annotations. Type Inference! Every ML-family language has some variation of this algorithm at the center of its design.

For decades, the problem was that nobody could figure out how to combine type inference with implicit casts AND make the resulting algorithm efficient enough for daily use. As far as I know, Scala was the first widely known language to figure out how to combine these two things! Its creator, Martin Odersky did a lot of work on combining type inference and subtyping to make this possible.

So for any ML-family language designed before Scala, it is safe to assume that implicit conversions just was not an option. Okay, but what about Elm?! It comes after Scala, so why not do it like them?!

  1. You pay performance cost to mix type inference and implicit conversions. At least as far as anyone knows, it defeats an optimization that is crucial to getting _reliably_ good performance. It is fine in most cases, but it can be a real issue in very large code bases.

  2. Based on experience reports from Scala users, it seemed like the convenience was not worth the hidden cost. Yes, you can convert `n` in `(n + 1.5)` and everything is nice, but when you are in larger programs that are sparsely annotated, it can be quite difficult to figure out what is going on.

This user data may be confounded by the fact that Scala allows quite extensive conversions, not just from `Int` to `Float`, but I think it is worth taking seriously nonetheless. So it is _possible_, but it has tradeoffs.


## Conclusion

First, based on the landscape of design possibilities, it seems like requiring _explicit_ conversions is a pretty nice balance. We can have type inference, it can produce friendly error messages, the algorithm is snappy, and an unintended implicit cast will not flow hundreds of lines before manifesting to the user.

Second, Elm very much favors explicit code, so this also fits in with the overall spirit of the language and libraries.

I hope that clarifies why you have to add those `toFloat` and `round` functions! It definitely can take some getting used to, but there are tons of folks who get past that acclimation period and really love the tradeoffs!
