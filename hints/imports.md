
# Hints for Imports

When getting started with Elm, it is pretty common to have questions about how the `import` declarations work exactly. These questions usually arise when you start playing with the `Html` library so we will focus on that.


<br>

## `import`

An Elm file is called a **module**. To access code in other files, you need to `import` it!

So say you want to use the [`div`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#div) function from the [`elm-lang/html`](http://package.elm-lang.org/packages/elm-lang/html/latest) package. The simplest way is to import it like this:

```elm
import Html

main =
  Html.div [] []
```

After saying `import Html` we can refer to anything inside that module as long as it is *qualified*. This works for:

  - **Values** &mdash; we can refer to `Html.text`, `Html.h1`, etc.
  - **Types** &mdash; We can refer to [`Attribute`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#Attribute) as `Html.Attribute`.

So if we add a type annotation to `main` it would look like this:

```elm
import Html

main : Html.Html msg
main =
  Html.div [] []
```

We are referring to the [`Html`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#Html) type, using its *qualified* name `Html.Html`. This can feel weird at first, but it starts feeling natural quite quickly!

> **Note:** Modules do not contain other modules. So the `Html` module *does not* contain the `Html.Attributes` module. Those are separate names that happen to have some overlap. So if you say `import Html` you *do not* get access to `Html.Attributes.style`. You must `import Html.Attributes` module separately.


<br>

## `as`

It is best practice to always use *qualified* names, but sometimes module names are so long that it becomes unwieldy. This is common for the `Html.Attributes` module. We can use the `as` keyword to help with this:

```elm
import Html
import Html.Attributes as A

main =
  Html.div [ A.style "color" "red" ] [ Html.text "Hello!" ]
```

Saying `import Html.Attributes as A` lets us refer to any value or type in `Html.Attributes` as long as it is qualified with an `A`. So now we can refer to [`style`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#style) as `A.style`.


<br>

## `exposing`

In quick drafts, maybe you want to use *unqualified* names. You can do that with the `exposing` keyword like this:

```elm
import Html exposing (..)
import Html.Attributes exposing (style)

main : Html msg
main =
  div [ style "color" "red" ] [ text "Hello!" ]
```

Saying `import Html exposing (..)` means I can refer to any value or type from the `Html` module without qualification. Notice that I use the `Html` type, the `div` function, and the `text` function without qualification in the example above.

> **Note:** It seems neat to expose types and values directly, but it can get out of hand. Say you `import` ten modules `exposing` all of their content. It quickly becomes difficult to figure out what is going on in your code. “Wait, where is this function from?” And then trying to sort through all the imports to find it. Point is, use `exposing (..)` sparingly!

Saying `import Html.Attributes exposing (style)` is a bit more reasonable. It means I can refer to the `style` function without qualification, but that is it. You are still importing the `Html.Attributes` module like normal though, so you would say `Html.Attributes.class` or `Html.Attributes.id` to refer to other values and types from that module.


<br>

## `as` and `exposing`

There is one last way to import a module. You can combine `as` and `exposing` to try to get a nice balance of qualified names:

```elm
import Html exposing (Html, div, text)
import Html.Attributes as A exposing (style)

main : Html msg
main =
  div [ A.class "greeting", style "color" "red" ] [ text "Hello!" ]
```

Notice that I refer to `A.class` which is qualified and `style` which is unqualified.


<br>

## Default Imports

We just learned all the variations of the `import` syntax in Elm. You will use some version of that syntax to `import` any module you ever write.

It would be the best policy to make it so every module in the whole ecosystem works this way. We thought so in the past at least, but there are some modules that are so commonly used that the Elm compiler automatically adds the imports to every file. These default imports include:

```elm
import Basics exposing (..)
import List exposing (List, (::))
import Maybe exposing (Maybe(..))
import Result exposing (Result(..))
import String
import Tuple

import Debug

import Platform exposing (Program)
import Platform.Cmd as Cmd exposing (Cmd)
import Platform.Sub as Sub exposing (Sub)
```

You can think of these imports being at the top of any module you write.

One could argue that `Maybe` is so fundamental to how we handle errors in Elm code that it is *basically* part of the language. One could also argue that it is extraordinarily annoying to have to import `Maybe` once you get past your first couple weeks with Elm. Either way, we know that default imports are not ideal in some sense, so we have tried to keep the default imports as minimal as possible.

> **Note:** Elm performs dead code elimination, so if you do not use something from a module, it is not included in the generated code. So if you `import` a module with hundreds of functions, you do not need to worry about the size of your assets. You will only get what you use!
