
# No Ports in Packages

The package ecosystem is one of the most important parts of Elm. Right now, our ecosystem has some compelling benefits:

  - There are many obvious default packages that work well.
  - Adding dependencies cannot introduce runtime exceptions.
  - Patch changes cannot lead to surprise build failures.

These are really important factors if you want to *quickly* create *reliable* applications. The Elm community thinks this is valuable.

Other communities think that the *number* of packages is a better measure of ecosystem health. That is a fine metric to use, but it is not the one we use for Elm. We would rather have 50 great packages than 100k packages of wildly varying quality.


## So what about ports?

Imagine you install a new package that claims to support `localStorage`. You get it set up, working through any compile errors. You run it, but it does not seem to work! After trying to figure it out for hours, you realize there is some poorly documented `port` to hook up...

Okay, now you need to hook up some JavaScript code. Is that JS file in the Elm package? Or is it on `npm`? Wait, what version on `npm` though? And is this patch version going to work as well? Also, how does this file fit into my build process? And assuming we get through all that, maybe the `port` has the same name as one of the ports in your project. Or it clashes with a `port` name in another package.

**Suddenly adding dependencies is much more complicated and risky!** An experienced developer would always check for ports up front, spending a bunch of time manually classifying unacceptable packages. Most people would not know to do that and learn all the pitfalls through personal experience, ultimately spending even *more* time than the person who defensively checks to avoid these issues.

So “ports in packages” would impose an enormous cost on application developers, and in the end, we would have a less reliable package ecosystem overall.


## Conclusion

Our wager with the Elm package ecosystem is that it is better to get a package *right* than to get it *right now*. So while we could use “ports in packages” as a way to get twenty `localStorage` packages of varying quality *right now*, we are choosing not to go that route. Instead we ask that developers use ports directly in their application code, getting the same result a different way.

Now this may not be the right choice for your particular project, and that is okay! We will be expanding our core libraries over time, as explained [here](https://github.com/elm-lang/projects/blob/master/roadmap.md#where-is-the-localstorage-package), and we hope you will circle back later to see if Elm has grown into a better fit!

If you have more questions about this choice or what it means for your application, please come ask in [the Elm slack](http://elmlang.herokuapp.com/). Folks are friendly and happy to help out! Chances are that a `port` in your application will work great for your case once you learn more about how they are meant to be used.
