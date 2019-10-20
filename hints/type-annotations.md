
# Hints for Type Annotation Problems

At the root of this kind of issue is always the fact that a type annotation in your code does not match the corresponding definition. Now that may mean that the type annotation is "wrong" or it may mean that the definition is "wrong". The compiler cannot figure out your intent, only that there is some mismatch.

This document is going to outline the various things that can go wrong and show some examples.


## Annotation vs. Definition

The most common issue is with user-defined type variables that are too general. So let's say you have defined a function like this:

```elm
addPair : (a, a) -> a
addPair (x, y) =
  x + y
```

The issue is that the type annotation is saying "I will accept a tuple containing literally *anything*" but the definition is using `(+)` which requires things to be numbers. So the compiler is going to infer that the true type of the definition is this:

```elm
addPair : (number, number) -> number
```

So you will probably see an error saying "I cannot match `a` with `number`" which is essentially saying, you are trying to provide a type annotation that is **too general**. You are saying `addPair` accepts anything, but in fact, it can only handle numbers.

In cases like this, you want to go with whatever the compiler inferred. It is good at figuring this kind of stuff out ;)


## Annotation vs. Itself

It is also possible to have a type annotation that clashes with itself. This is probably more rare, but someone will run into it eventually. Let's use another version of `addPair` with problems:

```elm
addPair : (Int, Int) -> number
addPair (x, y) =
  x + y
```

In this case the annotation says we should get a `number` out, but because we were specific about the inputs being `Int`, the output should also be an `Int`.


## Annotation vs. Internal Annotation

A quite tricky case is when an outer type annotation clashes with an inner type annotation. Here is an example of this:

```elm
filter : (a -> Bool) -> List a -> List a
filter isOkay list =
  let
    keepIfOkay : a -> Maybe a
    keepIfOkay x =
      if isOkay x then Just x else Nothing
  in
    List.filterMap keepIfOkay list
```

This case is very unfortunate because all the type annotations are correct, but there is a detail of how type inference works right now that **user-defined type variables are not shared between annotations**. This can lead to probably the worst type error messages we have because the problem here is that `a` in the outer annotation does not equal `a` in the inner annotation.

For now the best route is to leave off the inner annotation. It is unfortunate, and hopefully we will be able to do a nicer thing in future releases.
