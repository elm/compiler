# Contributing to Elm

Thanks helping with the development of Elm! This document
describes the basic standards for opening pull requests:

## Branches

  * [The master branch](https://github.com/elm-lang/Elm/tree/master) is the
    home of the next release of the compiler so new features and improvements
    get merged there. Most pull requests should target this branch!

  * [The stable branch](https://github.com/elm-lang/Elm/tree/stable) is for
    tagging previous releases and critical bug fixes. This branch is handy for
    folks who want to build the most recent public release from source.

If you are working on a fairly large feature, we will probably want to merge it
in as its own branch and do some testing before bringing it into the master
branch. This way we can keep releases of the master branch independent of new
features.

## Opening a pull request

**Please open PRs against the [master branch of
Elm](http://github.com/elm-lang/elm/tree/master) whenever possible.**

Note that changes in the master branch of [Elm](https://github.com/elm-lang/Elm/)
are able to be compiled against the master branch of the [elm-lang.org
repo](https://github.com/elm-lang/elm-lang.org), and the stable branch of
[Elm](https://github.com/elm-lang/Elm/) is able to be built with the stable
branch of the [elm-lang.org repo](https://github.com/elm-lang/elm-lang.org).
Please make sure that your changes maintain this compatibility.

## Licensing

You need to sign the [contributor agreement](ContributorAgreement.pdf)
and send it to <info@elm-lang.org> before opening your pull request.
