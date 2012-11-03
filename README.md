Elm
===

This is the Elm compiler and server, allowing you to develop Elm applications that run in any modern browser.

If you intend to serve Elm code with a Haskell backend, be sure to read all the way to the "Installation for Haskell-Users" section.


Installation for General Use
----------------------------

Download the [Haskell Platform](http://hackage.haskell.org/platform/). This will give you access to the Haskell compiler (needed to build Elm) and Haskell's package distribution system (to make installation of Elm easier). Elm works best
with version 7.4 of the Haskell compiler (i.e. GHC 7.4)

Once installed (even if it already was), you must update your listing of Haskell packages with:

    cabal update

This will ensure that the elm package is available. Then install Elm with:

    cabal install elm

Assuming everything goes correctly (potential problems are discussed later), this will install the `elm` compiler on your machine. `elm` is a standard compiler that takes `.elm` files and produces `.html` and/or `.js` files. You can then use these files with your favorite web-framework. Use `elm --help` for more directions on how to use it.

If you have this installed you can write and compile Elm locally.

You can also install `elm-server` which is both a compiler and server, allowing you to develop without designing and setting up a server yourself. Running `elm-server` starts a server in the current directory. It will compile and serve any `.elm` files in the current directory and its sub-directories. This is how I prefer to develop Elm programs. Install with:

    cabal install elm-server

Dependency issues have been cropping up with this step, so if you run into issues, know that they are being worked on.
Once installed, use the command `elm-server --help` to see some extra information.

#### Using the Elm compiler and server

To use these executables you need to add a new directory to your PATH. For me, the executables were placed in `/home/evan/.cabal/bin` which I appended to the end of my PATH variable in my .bashrc file. Cabal should tell you where your executables are located upon successful installation, so you can make a similar addition (see this tutorial if you are new to changing your PATH in [Unix/Linux](http://www.cyberciti.biz/faq/unix-linux-adding-path/)).

You can use the commands `elm --help` and `elm-server --help` to get more information about using these tools.

That is almost everything. Now, we will create a simple Elm project. The following commands will set-up a very basic project and start the Elm server.

    mkdir helloElm
    cd helloElm
    echo main = lift asText Mouse.position > main.elm
    elm-server

The first two commands create a new directory and navigate into it. The `echo` command places a simple program into `main.elm` (do this manually if you do not have `echo`). The final command starts the Elm server at [localhost](http://localhost:8000/), allowing you to navigate to `main.elm` and see your first program in action.


Installation for Haskell-users
------------------------------

Elm as described in the previous section is actually split into two packages: `elm` which contains the guts of the compiler and `elm-server` which provides a simple HAppStack-based server to simplify development. Those of you planning to write your own server in Haskell only need the `elm` package:

    cabal install elm

The `elm` package provides support for [compilation of Elm code directly in Haskell](http://hackage.haskell.org/packages/archive/Elm/0.1.2/doc/html/Language-Elm.html) and [QuasiQuoting](http://hackage.haskell.org/packages/archive/Elm/0.1.2/doc/html/Language-Elm-Quasi.html). See the [Examples/](https://github.com/evancz/Elm/tree/master/Examples) directory for information and examples on how to get started with Elm+Haskell.

Yesod users should also install the `elm-yesod` package which provides functions for idiomatically embedding Elm in Yesod:

    cabal install elm-yesod

Some extra tips on Elm+Yesod can be found [here](https://github.com/evancz/Elm/wiki/Elm-with-Yesod:-Getting-Started).

An important note: When you install the `elm` compiler, it automatically downloads Elm's JavaScript runtime system to `~/.cabal/share/Elm-x.y.z/`. The runtime system will follow the name scheme `elm-runtime-x.y.z.js` where `x.y.z` matches the version number of the compiler. If you want to serve this file from a different location, *copy* it from its home and always be sure that code compiled with version `x.y.z` of the compiler is served with version `x.y.z` of the runtime system.


Potential problems and their solutions
--------------------------------------

* Try `cabal install elm`. This will give you access to the `elm` executable (but not `elm-server`).

These problems all appeared before Elm version 0.1.1.4:

* Install errors having to do with `happstack-server-7.0.2`. This version of `happstack-server` has stricter dependency restrictions that conflict with other libraries required by Elm. Try installing with an earlier version of `happstack-server` with the following command: `cabal install elm --constrain="happstack-server<7.0.2"`
* When installing on Debian, `blaze-html-0.4.3.2` fails to compile. You must install `blaze-html-0.4.3.1` instead.
* Elm does not appear to work with the latest versions of `containers` (i.e. 0.4.2.*). I know it works with earlier versions of containers, so to avoid this problem, you can try: `cabal install elm --constrain="containers==0.4.1.0" --force-reinstall`
* On Windows, HAppStack has trouble installing because of issues with the "network" package. I struggled with this problem on Windows 7 until I found the suggestion at the bottom of [this page](http://hackage.haskell.org/trac/ghc/ticket/5159).

If you are still stuck, email the list or ask a question in the [#Elm IRC channel](http://webchat.freenode.net/?channels=elm).