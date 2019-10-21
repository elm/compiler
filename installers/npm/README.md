# Elm Installer

[Elm](https://elm-lang.org) is a functional programming language that compiles to JavaScript.

Head over to [The Official Guide](https://guide.elm-lang.org/) to start learning Elm!


<br/>

## What is this package for?

**Normal installs** ❌

Use the instructions [here](https://guide.elm-lang.org/install/elm.html) instead.

**Multiple versions** ✅

People using Elm at work may use different versions of Elm in different projects. They can run `npm install elm@latest-0.19.1` in each project and use the binary at `./node_modules/.bin/elm` for compilation.

**Continuous integration** ⚠️

This works, but there are usually faster and more reliable options:

1. You can download `elm` directly from GitHub with [this script](https://github.com/elm/compiler/blob/master/installers/linux/README.md). This is all the `npm` installer does, but with extra HTTP requests to `npmjs.com` servers, making it slower and adding more failure points.
2. Many continuous integration have ways to cache files ([example](https://docs.travis-ci.com/user/caching/)) to make builds faster and more reliable.

That said, it will definitely work to use the `npm` installer on CI if you prefer that option.


<br/>

## Install Locally

The following command should download the latest Elm 0.19.1 binary:

```
npm install elm@latest-0.19.1
```

You should be able to run `./node_modules/bin/elm --version` within your project and see `0.19.1`. Now you can compile with `./node_modules/bin/elm make src/Main.elm` and not disrupt other packages.

Use `npm install elm@latest-0.19.0` or `npm install elm@latest-0.18.0` for earlier versions.

**Note:** The `latest-X.Y.Z` convention is used in case we need to publish patches for the `npm` installer within a given Elm release. For example, say `npm` decides that some transitive dependency is not secure. Nothing is changing about Elm or the binaries, but we need to publish a new `npm` installer that fixes this issue.

