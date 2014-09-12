# Contributing to Elm

Thanks helping with the development of Elm! This document describes the basic
standards for opening pull requests:

## Branches

  * [The master branch][master] is the home of the next release of the compiler
    so new features and improvements get merged there. Most pull requests
    should target this branch!

  * [The stable branch][stable] is for tagging releases and critical bug fixes.
    This branch is handy for folks who want to build the most recent public
    release from source.

[master]: http://github.com/elm-lang/elm/tree/master
[stable]: http://github.com/elm-lang/elm/tree/stable

If you are working on a fairly large feature, we will probably want to merge it
in as its own branch and do some testing before bringing it into the master
branch. This way we can keep releases of the master branch independent of new
features.

## Opening a pull request

**Please open PRs against the [master branch of Elm][master] whenever possible.**

Note that the master branch of the compiler should always be in sync with the
master branch of the [website][], and the stable branch of the compiler should
always be in sync with the stable branch of the [website][]. Make sure that
your changes maintain this compatibility.

[website]: https://github.com/elm-lang/elm-lang.org

## Licensing

You need to sign the [contributor agreement](ContributorAgreement.pdf)
and send it to <info@elm-lang.org> before opening your pull request.
