
# Import Cycles

What is an import cycle? In practice you may see it if you create two modules with interrelated `User` and `Comment` types like this:

```elm
module Comment exposing (..)

import User

type alias Comment =
  { comment : String
  , author : User.User
  }
```

```elm
module User exposing (..)

import Comment

type alias User =
  { name : String
  , comments : List Comment.Comment
  }
```

Notice that to compile `Comment` we need to `import User`. And notice that to compile `User` we need to `import Comment`. We need both to compile either!

Now this is *possible* if the compiler figures out any module cycles and puts them all in one big file to compile them together. That seems fine in our small example, but imagine we have a cycle of 20 modules. If you change *one* of them, you must now recompile *all* of them. In a large code base, this causes extremely long compile times. It is also very hard to disentangle them in practice, so you just end up with slow builds. That is your life now.

The thing is that you can always write the code *without* cycles by shuffling declarations around, and the resulting code is often much clearer.


# How to Break Cycles

There are quite a few ways to break our `Comment` and `User` cycle from above, so let’s go through four useful strategies. The first one is by far the most common solution!


## 1. Combine the Modules

One approach is to just combine the two modules. If we check out the resulting code, we have actually revealed a problem in how we are representing our data:

```elm
module BadCombination1 exposing (..)

type alias Comment =
  { comment : String
  , author : User
  }

type alias User =
  { name : String
  , comments : List Comment
  }
```

Notice that the `Comment` type alias is defined in terms of the `User` type alias and vice versa. Having recursive type aliases like this does not work! That problem is described in depth [here](recursive-alias.md), but the quick takeaway is that one `type alias` needs to become a `type` to break the recursion. So let’s try again:

```elm
module BadCombination2 exposing (..)

type alias Comment =
  { comment : String
  , author : User
  }

type alias User =
  { name : String
  , comments : AllUserComments
  }

type AllUserComments = AllUserComments (List Comment)
```

Okay, now we have broken the recursion, but we need to ask ourselves, how are we going to actually instantiate these `Comment` and `User` types that we have described. A `Comment` will always have an author, and that `User` will always refer back to the `Comment`. So we seem to want cyclic data here. If we were in JavaScript we might instantiate all the comments in one pass, and then go back through and mutate the users to point to all the relevant comments. In other words, we need *mutation* to create this cyclic data!

All values are immutable in Elm, so we need to use a more functional strategy. One common approach is to use unique identifiers. Instead of referring directly to “the user object” we can refer to a user ID:

```elm
module GoodCombination exposing (..)

import Dict

type alias Comment =
  { comment : String
  , author : UserId
  }

type alias UserId = String

type alias AllComments =
  Dict.Dict UserId (List Comment)
```

Now in this world, we do not even have cycles in our types anymore! That means we can actually break these out into separate modules again:

```elm
module Comment exposing (..)

import Dict
import User

type alias Comment =
  { comment : String
  , author : User.Id
  }

type alias AllComments =
  Dict.Dict User.Id (List Comment)
```

```elm
module User exposing (..)

type alias Id = String
```

So now we are back to the two modules we wanted, but we have data structures that are going to work much better in a functional language like Elm! **This is the common approach, and it is what you hope will happen!**


## 2. Make a New Module

Now say there are actually a ton of functions and values in the `Comment` and `User` modules. Combining them into one does not seem like a good strategy. Instead you can create a *third* module that just has the shared types and functions. Let’s pretend we call that third module `GoodCombination`. So rather than having `Comment` and `User` depend on each other, they now both depend on `GoodCombination`. We broke our cycle!

**This strategy is less common.** You generally want to keep the core `type` of a module with all the functions that act upon it directly, so separating a `type` from everything else is a bad sign. So maybe there is a `User` module that contains a bunch of helper functions, but you *use* all those helper functions in a bunch of other modules that interact with users in various ways. In that scenario, it is still more sophisticated than “just throw the types in a module together” and hope it turns out alright.


## 3. Use Type Variables

Another way to avoid module cycles is to be more generic in how you represent your data:

```elm
module Comment exposing (..)

type alias Comment author =
  { comment : String
  , author : author
  }
```

```elm
module User exposing (..)

type alias User comment =
  { name : String
  , comments : List comment
  }
```

Notice that `Comment` and `User` no longer need to import each other! Instead, whenever we use these modules, we need to fill in the type variable. So we may import both `Comment` and `User` and try to combine them into a `Comment (User (Comment (User ...)))`. Gah, we ran into the recursive type alias thing again!

So this strategy fails pretty badly with our particular example. The code is more complicated and it still does not work! So **this strategy is rarely useful**, but when it works, it can simplify things quite a lot.


## 4. Hiding Implementation Details in Packages

This gets a little bit trickier when you are creating a package like `elm-lang/parser` which is built around the `Parser` type.

That package has a couple exposed modules: `Parser`, `Parser.LanguageKit`, and `Parser.LowLevel`. All of these modules want access to the internal details of the `Parser` type, but we do not want to ever expose those internal details to the *users* of this package. So where should the `Parser` type live?!

Usually you know which module should expose the type for the best public API. In this case, it makes sense for it to live in the `Parser` module. The way to manage this is to create a `Parser.Internal` module with a definition like:

```elm
module Parser.Internal exposing (..)

type Parser a =
  Parser ...
```

Now we can `import Parser.Internal` and use it in any of the modules in our package. The trick is that we never expose the `Parser.Internal` module to the *users* of our package. We can see what is inside, but they cannot! Then in the `Parser` module we can say:

```elm
module Parser exposing (..)

import Parser.Internal as Internal

type alias Parser a =
  Internal.Parser a
```

So now folks see a `Parser` type exposed by the `Parser` module, and it is the one that is used throughout all the modules in the package. Do not screw up your data representation to avoid this trick! I think we can improve how this appears in documentation, but overall this is the best way to go.

Now again, this strategy is particularly useful in packages. It is not as worthwhile in application code.
