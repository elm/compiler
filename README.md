Elm
===

The source code is available at [github](https://github.com/evancz/Elm), and the compiler/dev-server is available through [Hackage](http://hackage.haskell.org/package/Elm). I have only tested the following installation process on a fresh copy of Ubuntu 11.10, so please comment here if you are having any trouble or have some additional tips. Here is the process:

Download the [Haskell Platform](http://hackage.haskell.org/platform/). This will give you access to the Haskell compiler (needed to build Elm) and Haskell's package distrubution system (to make installation of Elm easier). If you already have this installed, just make sure you update your listing of packages with:

    cabal update

This will ensure that the elm package is available. Then install Elm with:

    cabal install elm

Assuming everything goes correctly (potential problems are discussed later), this will build two executables on your machine:

* elm :: A standard compiler that takes .elm files and produces .html files. You can then use these HTML files with your favorite web-framework.

* elm-server :: This is both a compiler and server, allowing you to develop without designing and setting up a server yourself. Running the "elm-server" program starts a server in the current directory. It will compile and serve any .elm files in the current directory and its sub-directories. This is how I prefer to develop Elm programs.

To use these executables you may need to add a new directory to your PATH. For me, the executables were placed in /home/evan/.cabal/bin which I appended to the end of my PATH variable in my .bashrc file. Cabal should tell you where your executables are located, so you can make a similar addition.

That is almost everything. Create a new directory that contains only the [elm-mini.js](http://elm-lang.org/Library/elm-mini.js) file. elm-mini.js is the Elm runtime system, and it is separate from the compiler for now. In this directory, create the file "main.elm" with the contents:

    main = lift asText Mouse.position

Now start up "elm-server" in this directory and navigate to your [localhost](http://localhost:8000/). You should see a directory listing. Navigate to "main.elm" to see your first Elm program in action. Note that elm-mini.js is required for this to work!

Potential problems and their solutions:

* HAppStack has trouble installing because of issues with the "network" package. I struggled with this problem on Windows 7 until I found the suggestion at the bottom of [this page](http://hackage.haskell.org/trac/ghc/ticket/5159).
* Likely more to come...
