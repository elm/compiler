FROM alpine:3.10

# branch
ARG branch=master
# commit or tag
ARG commit=HEAD

# Install required packages
RUN apk add --update ghc cabal git musl-dev zlib-dev ncurses-dev ncurses-static wget

# Checkout elm compiler
WORKDIR /tmp
RUN git clone -b $branch https://github.com/elm/compiler.git

# Build a statically linked elm binary
WORKDIR /tmp/compiler
RUN git checkout $commit
RUN rm worker/elm.cabal
RUN cabal new-update
RUN cabal new-configure --disable-executable-dynamic --ghc-option=-optl=-static --ghc-option=-optl=-pthread
RUN cabal new-build
RUN strip -s ./dist-newstyle/build/x86_64-linux/ghc-8.4.3/elm-0.19.1/x/elm/build/elm/elm
