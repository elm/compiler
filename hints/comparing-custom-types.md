# Comparing Custom Types

The built-in comparison operators work on a fixed set of types, like `Int` and `String`. That covers a lot of cases, but what happens when you want to compare custom types?

This page aims to catalog these scenarios and offer alternative paths that can get you unstuck.


## Wrapped Types

It is common to try to get some extra type safety by creating really simple custom types:

```elm
type Id = Id Int
type Age = Age Int

type Comment = Comment String
type Description = Description String
```

By wrapping the primitive values like this, the type system can now help you make sure that you never mix up a `Id` and an `Age`. Those are different types! This trick is extra cool because it has no runtime cost in `--optimize` mode. The compiler can just use an `Int` or `String` directly when you use that flag!

The problem arises when you want to use a `Id` as a key in a dictionary. This is a totally reasonable thing to do, but the current version of Elm cannot handle this scenario.

Instead of creating a `Dict Id Info` type, one thing you can do is create a custom data structure like this:

```elm
module User exposing (Id, Table, empty, get, add)

import Dict exposing (Dict)


-- USER

type Id = Id Int


-- TABLE

type Table info =
  Table Int (Dict Int info)

empty : Table info
empty =
  Table 0 Dict.empty

get : Id -> Table info -> Maybe info
get (Id id) (Table _ dict) =
  Dict.get id dict

add : info -> Table info -> (Table info, Id)
add info (Table nextId dict) =
  ( Table (nextId + 1) (Dict.insert nextId info dict)
  , Id nextId
  )
```

There are a couple nice things about this approach:

1. The only way to get a new `User.Id` is to `add` information to a `User.Table`.
2. All the operations on a `User.Table` are explicit. Does it make sense to remove users? To merge two tables together? Are there any special details to consider in those cases? This will always be captured explicitly in the interface of the `User` module.
3. If you ever want to switch the internal representation from `Dict` to `Array` or something else, it is no problem. All the changes will be within the `User` module.

So while this approach is not as convenient as using a `Dict` directly, it has some benefits of its own that can be helpful in some cases.


## Enumerations to Ints

Say you need to define a `trafficLightToInt` function:

```elm
type TrafficLight = Green | Yellow | Red

trafficLightToInt : TrafficLight -> Int
trafficLightToInt trafficLight =
  ???
```

We have heard that some people would prefer to use a dictionary for this sort of thing. That way you do not need to write the numbers yourself, they can be generated such that you never have a typo.

I would recommend using a `case` expression though:

```elm
type TrafficLight = Green | Yellow | Red

trafficLightToInt : TrafficLight -> Int
trafficLightToInt trafficLight =
  case trafficLight of
    Green  -> 1
    Yellow -> 2
    Red    -> 3
```

This is really straight-forward while avoiding questions like “is `Green` less than or greater than `Red`?”


## Something else?

If you have some other situation, please tell us about it [here](https://github.com/elm/error-message-catalog/issues). That is a log of error messages that can be improved, and we can use the particulars of your scenario to add more advice on this page!
