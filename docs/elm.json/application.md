# `elm.json` for applications

This is a decent baseline for pretty much any applications made with Elm. You will need these dependencies or more.

```json
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.0",
            "elm/core": "1.0.0",
            "elm/html": "1.0.0",
            "elm/json": "1.0.0"
        },
        "indirect": {
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
```

<br>


## `"type"`

Either `"application"` or `"package"`. All the other fields are based on this choice!

<br>


## `"source-directories"`

A list of directories where Elm code lives. Most projects just use `"src"` for everything.

<br>


## `"elm-version"`

The exact version of Elm this builds with. Should be `"0.19.0"` for most people!

<br>


## `"dependencies"`

All the packages you depend upon. We use exact versions, so your `elm.json` file doubles as a "lock file" that ensures reliable builds.

You can use modules from any `"direct"` dependency in your code. Some `"direct"` dependencies have their own dependencies that folks typically do not care about. These are the `"indirect"` dependencies. They are listed explicitly so that (1) builds are reproducible and (2) you can easily review the quantity and quality of dependencies.

**Note:** We plan to eventually have a screen in `reactor` that helps add, remove, and upgrade packages. It can sometimes be tricky to keep all of the constraints happy, so we think having a UI will help a lot. If you get into trouble in the meantime, adding things back one-by-one often helps, and I hope you do not get into trouble!

<br>


## `"test-dependencies"`

All the packages that you use in `tests/` with `elm-test` but not in the application you actually want to ship. This also uses exact versions to make tests more reliable.
