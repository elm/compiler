
# Hints for Recursive Type Aliases

At the root of this issue is the distinction between a `type` and a `type alias`.


## What is a type alias?

When you create a type alias, you are just creating a shorthand to refer to an existing type. So when you say the following:

```elm
type alias Time = Float

type alias Degree = Float

type alias Weight = Float
```

You have not created any *new* types, you just made some alternate names for `Float`. You can write down things like this and it'll work fine:

```elm
add : Time -> Degree -> Weight
add time degree =
  time + degree
```

This is kind of a weird way to use type aliases though. The typical usage would be for records, where you do not want to write out the whole thing every time. Stuff like this:

```elm
type alias Person =
  { name : String
  , age : Int
  , height : Float
  }
```

It is much easier to write down `Person` in a type, and then it will just expand out to the underlying type when the compiler checks the program.


## Recursive type aliases?

Okay, so let's say you have some type that may contain itself. In Elm, a common example of this is a comment that might have subcomments:

```elm
type alias Comment =
  { message : String
  , upvotes : Int
  , downvotes : Int
  , responses : List Comment
  }
```

Now remember that type *aliases* are just alternate names for the real type. So to make `Comment` into a concrete type, the compiler would start expanding it out.

```elm
  { message : String
  , upvotes : Int
  , downvotes : Int
  , responses :
      List
        { message : String
        , upvotes : Int
        , downvotes : Int
        , responses :
            List
              { message : String
              , upvotes : Int
              , downvotes : Int
              , responses : List ...
              }
        }
  }
```

The compiler cannot deal with values like this. It would just keep expanding forever.


## Recursive types!

In cases where you want a recursive type, you need to actually create a brand new type. This is what the `type` keyword is for. A simple example of this can be seen when defining a linked list:

```elm
type List
    = Empty
    | Node Int List
```

No matter what, the type of `Node n xs` is going to be `List`. There is no expansion to be done. This means you can represent recursive structures with types that do not explode into infinity.

So let's return to wanting to represent a `Comment` that may have responses. There are a couple ways to do this:


### Obvious, but kind of annoying

```elm
type Comment =
   Comment
      { message : String
      , upvotes : Int
      , downvotes : Int
      , responses : List Comment
      }
```

Now let's say you want to register an upvote on a comment:

```elm
upvote : Comment -> Comment
upvote (Comment comment) =
  Comment { comment | upvotes = 1 + comment.upvotes }
```

It is kind of annoying that we now have to unwrap and wrap the record to do anything with it.


### Less obvious, but nicer

```elm
type alias Comment =
  { message : String
  , upvotes : Int
  , downvotes : Int
  , responses : Responses
  }

type Responses = Responses (List Comment)
```

In this world, we introduce the `Responses` type to capture the recursion, but `Comment` is still an alias for a record. This means the `upvote` function looks nice again:

```elm
upvote : Comment -> Comment
upvote comment =
  { comment | upvotes = 1 + comment.upvotes }
```

So rather than having to unwrap a `Comment` to do *anything* to it, you only have to do some unwrapping in the cases where you are doing something recursive. In practice, this means you will do less unwrapping which is nice.


## Mutually recursive type aliases

It is also possible to build type aliases that are *mutually* recursive. That might be something like this:

```elm
type alias Comment =
  { message : String
  , upvotes : Int
  , downvotes : Int
  , responses : Responses
  }

type alias Responses =
  { sortBy : SortBy
  , responses : List Comment
  }

type SortBy = Time | Score | MostResponses
```

When you try to expand `Comment` you have to expand `Responses` which needs to expand `Comment` which needs to expand `Responses`, etc.

So this is just a fancy case of a self-recursive type alias. The solution is the same. Somewhere in that cycle, you need to define an actual `type` to end the infinite expansion.
