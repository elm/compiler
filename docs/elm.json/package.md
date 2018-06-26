# `elm.json` for packages

This is roughly `elm.json` for the `elm/json` package:

```json
{
    "type": "package",
    "name": "elm/json",
    "summary": "Encode and decode JSON values",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Json.Decode",
        "Json.Encode"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}
```

<br>


## `"type"`

Either `"application"` or `"package"`. All the other fields are based on this choice.

<br>


## `"name"`

The name of a GitHub repo like `"elm-lang/core"` or `"rtfeldman/elm-css"`.

> **Note:** We currently only support GitHub repos to ensure that there are no author name collisions. This seems like a pretty tricky problem to solve in a pleasant way. For example, do we have to keep an author name registry and give them out as we see them? But if someone is the same person on two platforms? And how to make this all happen in a way this is really nice for typical Elm users? Etc. So adding other hosting endpoints is harder than it sounds.

<br>


## `"summary"`

A short summary that will appear on [`package.elm-lang.org`](https://package.elm-lang.org/) that describes what the package is for. Must be under 80 characters.

<br>


## `"license"`

An OSI approved SPDX code like `"BSD-3-Clause"` or `"MIT"`. These are the two most common licenses in the Elm ecosystem, but you can see the full list of options [here](https://spdx.org/licenses/).

<br>


## `"version"`

All packages start at `"1.0.0"` and from there, Elm automatically enforces semantic versioning by comparing API changes.

So if you make a PATCH change and call `elm bump` it will update you to `"1.0.1"`. And if you then decide to remove a function (a MAJOR change) and call `elm bump` it will update you to `"2.0.0"`. Etc.

<br>


## `"exposed-modules"`

A list of modules that will be exposed to people using your package. The order you list them will be the order they appear on [`package.elm-lang.org`](https://package.elm-lang.org/).

**Note:** If you have five or more modules, you can use a labelled list like [this](https://github.com/elm-lang/core/blob/master/elm.json). We show the labels on the package website to help people sort through larger packages with distinct categories. Labels must be under 20 characters.

<br>


## `"elm-version"`

The range of Elm compilers that work with your package. Right now `"0.19.0 <= v < 0.20.0"` is always what you want for this.

<br>


## `"dependencies"`

A list of packages that you depend upon. In each application, there can only be one version of each package, so wide ranges are great. Fewer dependencies is even better though!

> **Note:** Dependency ranges should only express _tested_ ranges. It is not nice to use optimistic ranges and end up causing build failures for your users down the line. Eventually we would like to have an automated system that tries to build and test packages as new packages come out. If it all works, we could send a PR to the author widening the range.

<br>


## `"test-dependencies"`

Dependencies that are only used in the `tests/` directory by `elm test`. Values from these packages will not appear in any final build artifacts.
