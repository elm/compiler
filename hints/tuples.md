
# From Tuples to Records

The largest tuple possible in Elm has three entries. Once you get to four, it is best to make a record with named entries.

For example, it is _conceivable_ to represent a rectangle as four numbers like `(10,10,100,100)` but it would be more self-documenting to use a record like this:

```elm
type alias Rectangle =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }
```

Now it is clear that the dimensions should be `Float` values. It is also clear that we are not using the convention of specifying the top-left and bottom-right corners. It could be clearer about whether the `x` and `y` is the point in the top-left or in the middle though!

Anyway, using records like this also gives you access to syntax like `rect.x`, `.x`, and `{ rect | x = 40 }`. It is not clear how to design features like that for arbitrarily sized tuples, so we did not. We already have a way, and it is more self-documenting!
