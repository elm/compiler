# Hints for Infinite Types

Infinite types are probably the trickiest kind of bugs to track down. Usually it means you are working on a relatively sophisticated recursive function and something is very subtly off.

I am just going to show a couple examples of infinite types that have been reported and how they were resolved to illustrate the kinds of things you may be running into.


## Nested Record Types

(From issue [#987](https://github.com/elm-lang/elm-compiler/issues/987))

Someone was working with records, and wanted to do something kind of tricky with type aliases. In this case, they created a `Persisted` type alias that would say "whatever record you have, well I'm talking about one where there is an additional `id` field."

```elm
type alias Persisted record =
    { record | id : Int }

type alias Update record =
    record -> Int -> record

type Updatable record =
    Updatable (Update record) (Persisted record)

newRecord (Updatable myUpdate myRecord) =
  let
    updateThisRecord =
      myUpdate myRecord
  in
    "foo"
```

Now just looking at the code, it seems fine. You can create `Persisted` records, you can have a record `Update`, and you can bundle them together with `Updatable`. In our `newRecord` function we are partially applying the `myUpdate` function. Everything *seems* fine, but something is causing an infinite type.

The best thing to do is start writing down as many types for yourself as possible:

```elm
myUpdate : Update record     -- record -> Int -> record
myRecord : Persisted record  -- { record | id : Int }
```

The trouble is that it's the same `record` in both of these types! The `myUpdate` function wants t take a `record` and produce a `record`, but it is being given `{ record | id : Int }`. Type checking is going to say, "well, if `record` is actually an infinite sequence of `{ { ... | id : Int } | id : Int }` then they *can* be equal!" While that is true and very clever, it is not the intent of this code.

So the fix for this person was to add one thing into the `Updatable` type:

```elm
type Updatable record =
    Updatable (Update (Persisted record)) (Persisted record)
```

Notice the new `Persisted` in the `Update` part. That was the fix!


## Exploding Either

(From issue [#707](https://github.com/elm-lang/elm-compiler/issues/707))

There is a relatively obscure package called `evancz/automaton` that captures state and functionality using callbacks. So far, it does not seem to be extremely useful idea given its downsides, but it did lead to quite a tough infinite type.

Here is a snippet of code that combines two bacic automatons into one. The resulting automaton takes in either a `Left` or `Right` value. If it is a `Left` we will use the left automaton, if it is a `Right` we will use the right one.

```elm
type Either a b = Left a | Right b

type Automaton a b =
    Step (a -> (Automaton a b, b))

(+++) : Automaton a b -> Automaton c d -> Automaton (Either a c) (Either b d)
(+++) (Step leftStep) (Step rightStep) =
  Step <| \either ->
    case either of
        Left a ->
            let (newLeft, b) = leftStep a
            in  (newLeft, Left b)

        Right c ->
            let (newRight, d) = rightStep c
            in  (newRight, Right d)
```

This triggers a relatively confusing error message. The right thing to do in these scenarios is try to figure out the type of literally every piece of the definition. If you can, break parts out into other top-level declarations with type annotations so the compiler can check your work. So you might write down some notes to yourself like this:

```elm
left : Automaton a b
leftStep : a -> (Automaton a b, b)

right : Automaton c d
rightStep : c -> (Automaton c d, d)

either : Either a c

newLeft : Automaton a b
newRight : Automaton c d
```

Knowing that both branches of a `case` must end with the same type, it looks like `newLeft` and `newRight` are causing the issue here. So in the revised code we make it so both branches return the same type.

```elm
type Either a b = Left a | Right b

type Automaton a b =
    Step (a -> (Automaton a b, b))

(+++) : Automaton a b -> Automaton c d -> Automaton (Either a c) (Either b d)
(+++) (Step leftStep as left) (Step rightStep as right) =
  Step <| \either ->
    case either of
        Left a ->
            let (newLeft, b) = leftStep a
            in  (newLeft +++ right, Left b)

        Right c ->
            let (newRight, d) = rightStep c
            in  (left +++ newRight, Right d)
```

So yeah, if you are skimming this, you probably did not see the change immediately. My experience debugging these kinds of things is that my brain thinks what I wrote is correct so much that it becomes kind of blind to the issue. The best thing to do is slow down and break the problem apart. Or ask someone.