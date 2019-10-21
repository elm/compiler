# npm install elm

[Elm](https://elm-lang.org) is a functional programming language that compiles to JavaScript.

Head over to [The Official Guide](https://guide.elm-lang.org/) to start learning Elm!


<br/>

## What is this package for?

**For normal installs? No.** Use the instructions [here](https://guide.elm-lang.org/install/elm.html) instead.

**For multiple versions? Yes.** People using Elm at work may use different versions of Elm in different projects. They can run `npm install elm@latest-0.19.1` in each project and use the binary at `./node_modules/.bin/elm` for compilation.

**For continuous integration? Maybe.** It is possible to download binaries directly from GitHub with a script like [this](https://github.com/elm/compiler/blob/master/installers/linux/README.md) which is strictly more reliable than going through `npm`. Even better is to host the binaries such that you are not downloading from external services on each build.


<br/>

## Install Locally

The following command should download the latest Elm 0.19.1 binary:

```
npm install elm@latest-0.19.1
```

You should be able to run `./node_modules/bin/elm --version` within your project and see `0.19.1`. Now you can compile with `./node_modules/bin/elm make src/Main.elm` and not disrupt other packages.

Use `npm install elm@latest-0.19.0` or `npm install elm@latest-0.18.0` for earlier versions.

**Note:** The `latest-X.Y.Z` convention is used in case we need to publish patches for the `npm` installer within a given Elm release. For example, say `npm` decides that some transitive dependency is not secure. Nothing is changing about Elm or the binaries, but we need to publish a new `npm` installer that fixes this issue.

