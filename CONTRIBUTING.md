# Contributing to Elm

Thanks helping with the development of Elm! This document
describes the basic standards for opening pull requests:

## Branches

  * The master branch of Elm is for tagging previous releases
    and non-breaking changes, like bug fixes. This branch is
    handy for folks who want to build the most recent public
    release from source.

  * The dev branch is where the most active development occurs.
    It is the home of the next release of the compiler so new
    features and improvements get merged there.

## Opening a pull request

**Please open PRs against the [dev branch of
Elm](http://github.com/evancz/elm/tree/dev) whenever possible.** Changes that
are compatible with the API of master will be merged to master as soon as
possible. Changes that affect Elm's API will be merged to master with a future
release of the compiler.

Note that changes in the master branch of [Elm](https://github.com/evancz/Elm/)
are able to be compiled against the master branch of the [elm-lang.org
repo](https://github.com/evancz/elm-lang.org), and the dev branch of
[Elm](https://github.com/evancz/Elm/) is able to be built with the dev branch of
the [elm-lang.org repo](https://github.com/evancz/elm-lang.org). Please make
sure that your changes maintain this compatibility.

## Licensing

You should sign the [contributor agreement](ContributorAgreement.pdf)
and send it to <info@elm-lang.org> before opening your pull request.