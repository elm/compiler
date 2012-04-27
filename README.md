Elm
===

This is the Elm compiler and server, allowing you to develop Elm applications that run in any modern browser.

Installation Process
--------------------

Download the [Haskell Platform](http://hackage.haskell.org/platform/). This will give you access to the Haskell compiler (needed to build Elm) and Haskell's package distribution system (to make installation of Elm easier). Once installed (even if it already was), you must update your listing of Haskell packages with:

    cabal update

This will ensure that the elm package is available. Then install Elm with:

    cabal install elm

Assuming everything goes correctly (potential problems are discussed later), this will build two executables on your machine:

* elm :: A standard compiler that takes .elm files and produces .html files. You can then use these HTML files with your favorite web-framework.

* elm-server :: This is both a compiler and server, allowing you to develop without designing and setting up a server yourself. Running the "elm-server" program starts a server in the current directory. It will compile and serve any .elm files in the current directory and its sub-directories. This is how I prefer to develop Elm programs.

To use these executables you need to add a new directory to your PATH. For me, the executables were placed in /home/evan/.cabal/bin which I appended to the end of my PATH variable in my .bashrc file. Cabal should tell you where your executables are located, so you can make a similar addition (see this tutorial if you are new to changing your PATH in [Unix/Linux](http://www.cyberciti.biz/faq/unix-linux-adding-path/)).

That is almost everything. Now, we will create a simple Elm project. The following commands will set-up a very basic project and start the Elm server.

    mkdir helloElm
    cd helloElm
    wget https://raw.github.com/evancz/Elm/master/elm-mini.js
    echo main = lift asText Mouse.position > main.elm
    elm-server

The first two commands create a new directory and navigate into it. Then next command (wget) downloads the [elm-mini.js](https://raw.github.com/evancz/Elm/master/elm-mini.js) file which is the Elm runtime system and must be in the root directory of your Elm project. If you do not have wget, just follow [this link](https://raw.github.com/evancz/Elm/master/elm-mini.js) and download it directly. The 'echo' command places a simple program into 'main.elm'. The final command starts the Elm server at [localhost](http://localhost:8000/), allowing you to navigate to 'main.elm' and see your first program in action.

Areas for further work:
-----------------------

Error messages need work in general. Syntax and Parsing errors are reported, but the messages are not very helpful. Type errors currently go unreported. I hope to fix this as soon as possible.

If you are interested in contributing, please contact me at info (at) elm-lang (dot) org.


Potential problems and their solutions:
---------------------------------------

* When installing on Debian, "blaze-html-0.4.3.2" fails to compile. You must install "blaze-html-0.4.3.1" instead.
* Elm does not appear to work with the latest versions of "containers" (i.e. 0.4.2.*). I know it works with containers-0.3.0.0, so to avoid this problem, you can try: cabal install elm --constrain="containers==0.3.0.0"
* On Windows, HAppStack has trouble installing because of issues with the "network" package. I struggled with this problem on Windows 7 until I found the suggestion at the bottom of [this page](http://hackage.haskell.org/trac/ghc/ticket/5159).
* Likely more to come...
