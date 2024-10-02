
# Hints for Bad Recursion

There are two problems that will lead you here, both of them pretty tricky:

  1. [**No Mutation**](#no-mutation) &mdash; Defining values in Elm is slightly different than defining values in languages like JavaScript.

  2. [**Tricky Recursion**](#tricky-recursion) &mdash; Sometimes you need to define recursive values when creating generators, decoders, and parsers. A common case is a JSON decoder a discussion forums where a comment may have replies, which may have replies, which may have replies, etc.


## No Mutation

Languages like JavaScript let you “reassign” variables. When you say `x = x + 1` it means: whatever `x` was pointing to, have it point to `x + 1` instead. This is called *mutating* a variable. All values are immutable in Elm, so reassigning variables does not make any sense! Okay, so what *should* `x = x + 1` mean in Elm?

Well, what does it mean with functions? In Elm, we write recursive functions like this:

```elm
factorial : Int -> Int
factorial n =
  if n <= 0 then 1 else n * factorial (n - 1)
```

One cool thing about Elm is that whenever you see `factorial 3`, you can always replace that expression with `if 3 <= 0 then 1 else 3 * factorial (3 - 1)` and it will work exactly the same. So when Elm code gets evaluated, we will keep expanding `factorial` until the `if` produces a 1. At that point, we are done expanding and move on.

The thing that surprises newcomers is that recursion works the same way with values too. So take the following definition:

```elm
x = x + 1
```

We are actually defining `x` in terms of itself. So it would expand out to `x = ... + 1 + 1 + 1 + 1`, trying to add one to `x` an infinite number of times! This means your program would just run forever, endlessly expanding `x`. In practice, this means the page freezes and the computer starts to get kind of warm. No good! We can detect cases like this with the compiler, so we give an error at compile time so this does not happen in the wild.

The fix is usually to just give the new value a new name. So you could rewrite it to:

```elm
x1 = x + 1
```

Now `x` is the old value and `x1` is the new value. Again, one cool thing about Elm is that whenever you see a `factorial 3` you can safely replace it with its definition. Well, the same is true of values. Wherever I see `x1`, I can replace it with `x + 1`. Thanks to the way definitions work in Elm, this is always safe!


## Tricky Recursion

Now, there are some cases where you *do* want a recursive value. Say you are building a website with comments and replies. You may define a comment like this:

```elm
type alias Comment =
  { message : String
  , upvotes : Int
  , downvotes : Int
  , responses : Responses
  }

type Responses =
  Responses (List Comment)
```

You may have run into this definition in the [hints for recursive aliases](recursive-alias.md)! Anyway, once you have comments, you may want to turn them into JSON to send back to your server or to store in your database or whatever. So you will probably write some code like this:

```elm
import Json.Decode as Decode exposing (Decoder)

decodeComment : Decoder Comment
decodeComment =
  Decode.map4 Comment
    (Decode.field "message" Decode.string)
    (Decode.field "upvotes" Decode.int)
    (Decode.field "downvotes" Decode.int)
    (Decode.field "responses" decodeResponses)

-- PROBLEM
decodeResponses : Decoder Responses
decodeResponses =
  Decode.map Responses (Decode.list decodeComment)
```

The problem is that now `decodeComment` is defined in terms of itself! To know what `decodeComment` is, I need to expand `decodeResponses`. To know what `decodeResponses` is, I need to expand `decodeComment`. This loop will repeat endlessly!

In this case, the trick is to use `Json.Decode.lazy` which delays the evaluation of a decoder until it is needed. So the valid definition would look like this:

```elm
import Json.Decode as Decode exposing (Decoder)

decodeComment : Decoder Comment
decodeComment =
  Decode.map4 Comment
    (Decode.field "message" Decode.string)
    (Decode.field "upvotes" Decode.int)
    (Decode.field "downvotes" Decode.int)
    (Decode.field "responses" decodeResponses)

-- SOLUTION
decodeResponses : Decoder Responses
decodeResponses =
  Decode.map Responses (Decode.list (Decode.lazy (\_ -> decodeComment)))
```

Notice that in `decodeResponses`, we hide `decodeComment` behind an anonymous function. Elm cannot evaluate an anonymous function until it is given arguments, so it allows us to delay evaluation until it is needed. If there are no comments, we will not need to expand it!

This saves us from expanding the value infinitely. Instead we only expand the value if we need to.

> **Note:** The same kind of logic can be applied to tasks, random value generators, and parsers. Use `lazy` or `andThen` to make sure a recursive value is only expanded if needed.


## Understanding “Bad Recursion”

The compiler tries to detect bad recursion, but how does it know the difference between good and bad situations? Writing `factorial` is fine, but writing `x = x + 1` is not. One version of `decodeComment` was bad, but the other was fine. What is the rule?

**Elm will allow recursive definitions as long as there is at least one lambda before you get back to yourself.** So if we write `factorial` without any pretty syntax, it looks like this:

```elm
factorial =
  \n -> if n <= 0 then 1 else n * factorial (n - 1)
```

There is technically a lambda between the definition and the use, so it is okay! The same is true with the good version of `decodeComment`. There is a lambda between the definition and the use. As long as there is a lambda before you get back to yourself, the compiler will let it through.

**This rule is nice, but it does not catch everything.** It is pretty easy to write a definition where the recursion is hidden behind a lambda, but it still immediately expands forever:

```elm
x =
  (\_ -> x) () + 1
```

This follows the rules, but it immediately expands until our program runs out of stack space. It leads to a runtime error as soon as you start your program. It is nice to fail fast, but why not have the compiler detect this as well? It turns out this is much harder than it sounds!

This is called [the halting problem](https://en.wikipedia.org/wiki/Halting_problem) in computer science. Computational theorists were asking:

> Can we determine if a program will finish running (i.e. halt) or if it will continue to run forever?

It turns out that Alan Turing wrote a proof in 1936 showing that (1) in some cases you just have to check by running the program and (2) this check will take forever for programs that do not halt!

**So we cannot solve the halting problem *in general*, but our simple rule about lambdas can detect the majority of bad cases *in practice*.**
