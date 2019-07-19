
# REPL

The REPL lets you interact with Elm values and functions in your terminal.


## Use

You can type in expressions, definitions, custom types, and module imports using normal Elm syntax.

```elm
> 1 + 1
2 : number

> "hello" ++ "world"
"helloworld" : String
```

The same can be done with definitions and custom types:

```elm
> fortyTwo = 42
42 : number

> increment n = n + 1
<function> : number -> number

> increment 41
42 : number

> factorial n =
|   if n < 1 then
|     1
|   else
|     n * factorial (n-1)
|
<function> : number -> number

> factorial 5
120 : number

> type User
|   = Regular String
|   | Visitor String
|

> case Regular "Tom" of
|   Regular name -> "Hey again!"
|   Visitor name -> "Nice to meet you!"
|
"Hey again!" : String
```

When you run `elm repl` in a project with an [`elm.json`](https://github.com/elm/compiler/blob/master/docs/elm.json/application.md) file, you can import any module available in the project. So if your project has an `elm/html` dependency, you could say:

```elm
> import Html exposing (Html)

> Html.text "hello"
<internals> : Html msg

> Html.text
<function> : String -> Html msg
```

If you create a module in your project named `MyThing` in your project, you can say `import MyThing` in the REPL as well. Any module that is accessible in your project should be accessible in the REPL.


## Exit

To exit the REPL, you can type `:exit`.

You can also press `ctrl-d` or `ctrl-c` on some platforms.
