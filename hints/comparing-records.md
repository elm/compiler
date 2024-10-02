# Comparing Records

The built-in comparison operators work on a fixed set of types, like `Int` and `String`. That covers a lot of cases, but what happens when you want to compare records?

This page aims to catalog these scenarios and offer alternative paths that can get you unstuck.


## Sorting Records

Say we want a `view` function that can show a list of students sorted by different characteristics.

We could create something like this:

```elm
import Html exposing (..)

type alias Student =
  { name : String
  , age : Int
  , gpa : Float
  }

type Order = Name | Age | GPA

viewStudents : Order -> List Student -> Html msg
viewStudents order students =
  let
    orderlyStudents =
      case order of
        Name -> List.sortBy .name students
        Age -> List.sortBy .age students
        GPA -> List.sortBy .gpa students
  in
  ul [] (List.map viewStudent orderlyStudents)

viewStudent : Student -> Html msg
viewStudent student =
  li [] [ text student.name ]
```

If you are worried about the performance of changing the order or updating information about particular students, you can start using the [`Html.Lazy`](https://package.elm-lang.org/packages/elm/html/latest/Html-Lazy) and [`Html.Keyed`](https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed) modules. The updated code would look something like this:

```elm
import Html exposing (..)
import Html.Lazy exposing (lazy)
import Html.Keyed as Keyed

type Order = Name | Age | GPA

type alias Student =
  { name : String
  , age : Int
  , gpa : Float
  }

viewStudents : Order -> List Student -> Html msg
viewStudents order students =
  let
    orderlyStudents =
      case order of
        Name -> List.sortBy .name students
        Age -> List.sortBy .age students
        GPA -> List.sortBy .gpa students
  in
  Keyed.ul [] (List.map viewKeyedStudent orderlyStudents)

viewKeyedStudent : Student -> (String, Html msg)
viewKeyedStudent student =
  ( student.name, lazy viewStudent student )

viewStudent : Student -> Html msg
viewStudent student =
  li [] [ text student.name ]
```

By using `Keyed.ul` we help the renderer move the DOM nodes around based on their key. This makes it much cheaper to reorder a bunch of students. And by using `lazy` we help the renderer skip a bunch of work. If the `Student` is the same as last time, the render can skip over it.

> **Note:** Some people are skeptical of having logic like this in `view` functions, but I think the alternative (maintaining sort order in your `Model`) has some serious downsides. Say a colleague is adding a message to `Add` students, but they do not know about the sort order rules needed for presentation. Bug! So in this alternate design, you must diligently test your `update` function to make sure that no message disturbs the sort order. This is bound to lead to bugs over time!
>
> With all the optimizations possible with `Html.Lazy` and `Html.Keyed`, I would always be inclined to work on optimizing my `view` functions rather than making my `update` functions more complicated and error prone.


## Something else?

If you have some other situation, please tell us about it [here](https://github.com/elm/error-message-catalog/issues). That is a log of error messages that can be improved, and we can use the particulars of your scenario to add more advice on this page!
