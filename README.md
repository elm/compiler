
Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).


### Installing the Compiler

Download the [Haskell Platform 2012.2.0.0](http://hackage.haskell.org/platform/).
Elm definitely works with GHC 7.4, so newer versions of the Haskell Platform may work too.

Once the Haskell Platform is installed:

    cabal update ; cabal install elm

### Installing the Server

    cabal install elm-server

### Using the executables

To use `elm` and `elm-server` you need to add a new directory to your PATH.

Cabal should tell you where your executables are located upon
successful installation.

For me, the executables were placed in `/home/evan/.cabal/bin` which I
appended to the end of my PATH variable in my .bashrc file.

See this tutorial if you are new to changing your PATH in
[Unix/Linux](http://www.cyberciti.biz/faq/unix-linux-adding-path/).

## A Test project

Now we will create a simple Elm project.
The following commands will set-up a very basic project and start the Elm server.

    mkdir helloElm
    cd helloElm
    echo import Mouse > Main.elm
    echo main = lift asText Mouse.position > Main.elm
    elm-server

The first two commands create a new directory and navigate into it. The `echo`
commands place a simple program into `Main.elm`. Do this manually if you do not
have `echo`. The final command starts the Elm server at [localhost:8000](http://localhost:8000/),
allowing you to navigate to `Main.elm` and see your first program in action.

#### Wrap up

The `elm` package provides support for compilation of Elm code directly in Haskell and QuasiQuoting.
Check it out on Hackage if you are interested.

If you are stuck, email [the list](https://groups.google.com/forum/?fromgroups#!forum/elm-discuss)
or ask a question in the [#Elm IRC channel](http://webchat.freenode.net/?channels=elm).
