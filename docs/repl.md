# `elm repl`

The REPL lets you interact with Elm values and functions in your terminal.

<br>

## Install

You need [Elm][install] and [node.js][node] for the REPL. Elm generates JavaScript and we currently use node.js to evaluate it!

[install]: https://guide.elm-lang.org/install.html
[node]: https://nodejs.org/en/download/

<br>

## Use

You can type in expressions, definitions, union types, and module imports using normal Elm syntax.

```elm
> 1 + 1
2 : number

> "hello" ++ "world"
"helloworld" : String
```

The same can be done with definitions and union types:

```elm
> fortyTwo = 42
42 : number

> increment n = n + 1
<function> : number -> number

> increment 41
42 : number

> factorial n = if n < 1 then 1 else n * factorial (n-1)
<function> : number -> number

> factorial 5
120 : number

> type Either a b = Left a | Right b

> case Left 32 of \
|   Left n -> 2 * n \
|   Right m -> m + 1
64 : number
```

When you run `elm repl` in a project with an `elm.json` file, you can import any module available in the project:

```elm
> import String

> String.length "hello"
5 : Int

> String.reverse "flow"
"wolf" : String
```

Core modules, package modules, your modules. Whatever is available! So if you create a module named `MyThing` in your project, you can say `import MyThing` in the REPL and mess with anything it exposes.