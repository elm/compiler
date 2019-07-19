
# Creating an Elm project

The main goal of `elm init` is to get you to this page!

It just creates an `elm.json` file and a `src/` directory for your code.


## What is `elm.json`?

This file describes your project. It lists all of the packages you depend upon, so it will say the particular version of [`elm/core`](https://package.elm-lang.org/packages/elm/core/latest/) and [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) that you are using. It makes builds reproducible! You can read a bit more about it [here](https://github.com/elm/compiler/blob/master/docs/elm.json/application.md).

You should generally not edit it by hand. It is better to add new dependencies with commands like `elm install elm/http` or `elm install elm/json`.


## What goes in `src/`?

This is where all of your Elm files live. It is best to start with a file called `src/Main.elm`. As you work through [the official guide](https://guide.elm-lang.org/), you can put the code examples in that `src/Main.elm` file.


## How do I compile it?

Run `elm reactor` in your project. Now you can go to [`http://localhost:8000`](http://localhost:8000) and browse through all the files in your project. If you navigate to `.elm` files, it will compile them for you!

If you want to do things more manually, you can run `elm make src/Main.elm` and it will produce an `index.html` file that you can look at in your browser.


## How do I structure my directories?

Many folks get anxious about their project structure. “If I get it wrong, I am doomed!” This anxiety makes sense in languages where refactoring is risky, but Elm is not one of those languages!

So we recommend that newcomers staying in one file until you get into the 600 to 1000 range. Push out of your comfort zone. Having the experience of being fine in large files will help you understand the boundaries in Elm, rather than just defaulting to the boundaries you learned in another language.

The talk [The Life of a File](https://youtu.be/XpDsk374LDE) gets into this a lot more. The advice about building modules around a specific [custom type](https://guide.elm-lang.org/types/custom_types.html) is particularly important! You will see that emphasized a lot as you work through the official guide.


## How do I write tests?

Elm will catch a bunch of errors statically, and I think it is worth skipping tests at first to get a feeling for when tests will actually help you _in Elm_.

From there, we have a great testing package called [`elm-explorations/test`](https://github.com/elm-explorations/test) that can help you out! It is particularly helpful for teams working on a large codebase. When you are editing code you have never seen before, tests can capture additional details and constraints that are not otherwise apparent!


## How do I start fancier projects?

I wanted `elm init` to generate as little code as possible. It is mainly meant to get you to this page! If you would like a more elaborate starting point, I recommend starting projects with commands like these:

```bash
git clone https://github.com/evancz/elm-todomvc.git
git clone https://github.com/rtfeldman/elm-spa-example.git
```

The idea is that Elm projects should be so simple that nobody needs a tool to generate a bunch of stuff. This also captures the fact that project structure _should_ evolve organically as your application develops, never ending up exactly the same as other projects.

But if you have something particular you want, I recommend creating your own starter recipe and using `git clone` when you start new projects. That way (1) you can get exactly what you want and (2) we do not end up with a complex `elm init` that ends up being confusing for beginners!
