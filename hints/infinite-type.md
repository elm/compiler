
# Hints for Infinite Types

Infinite types are probably the trickiest kind of bugs to track down. **Writing down type annotations is usually the fastest way to figure them out.** Let's work through an example to get a feel for how these errors usually work though!


## Example

A common way to get an infinite type error is very small typos. For example, do you see the problem in the following code?

```elm
incrementNumbers list =
  List.map incrementNumbers list

incrementNumber n =
  n + 1
```

The issue is that `incrementNumbers` calls itself, not the `incrementNumber` function defined below. So there is an extra `s` in this program! Let's focus on that:

```elm
incrementNumbers list =
  List.map incrementNumbers list -- BUG extra `s` makes this self-recursive
```

Now the compiler does not know that anything is wrong yet. It just tries to figure out the types like normal. It knows that `incrementNumbers` is a function. The definition uses `List.map` so we can deduce that `list : List t1` and the result of this function call should be some other `List t2`. This also means that `incrementNumbers : List t1 -> List t2`.

The issue is that `List.map` uses `incrementNumbers` on `list`! That means that each element of `list` (which has type `t1`) must be fed into `incrementNumbers` (which takes `List t1`)

That means that `t1 = List t1`, which is an infinite type! If we start expanding this, we get `List (List (List (List (List ...))))` out to infinity!

The point is mainly that we are in a confusing situation. The types are confusing. This explanation is confusing. The compiler is confused. It is a bad time. But luckily, the more type annotations you add, the better chance there is that you and the compiler can figure things out! So say we change our definition to:

```elm
incrementNumbers : List Int -> List Int
incrementNumbers list =
  List.map incrementNumbers list -- STILL HAS BUG
```

Now we are going to get a pretty normal type error. Hey, you said that each element in the `list` is an `Int` but I cannot feed that into a `List Int -> List Int` function! Something like that.

In summary, the root issue is often some small typo, and the best way out is to start adding type annotations on everything!
