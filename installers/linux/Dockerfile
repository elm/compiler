# Based initially on https://gist.github.com/rlefevre/1523f47e75310e28eee243c9c5651ac9
#
# Build Linux x64 binary from elm compiler top-level directory:
# $ docker build -t elm -f installers/linux/Dockerfile .
#
# Retrieve elm Linux binary:
# $ docker cp $(docker create elm):/usr/local/bin/elm DESTINATION_DIRECTORY
#
# Delete docker elm image:
# $ docker rmi elm
#
# Display all images:
# $ docker images -a
#
# Delete all unused docker images:
# $ docker system prune -a

# Use Alpine 3.11 with GHC 8.6.5
FROM alpine:3.11

# Install packages required to build elm
RUN apk add --no-cache ghc cabal wget musl-dev zlib-dev zlib-static ncurses-dev ncurses-static

WORKDIR /elm

# Import source code
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY cabal.config elm.cabal LICENSE ./

# Build statically linked elm binary
RUN cabal new-update
RUN cabal new-build --ghc-option=-optl=-static --ghc-option=-split-sections
RUN cp ./dist-newstyle/build/x86_64-linux/ghc-*/elm-*/x/elm/build/elm/elm /usr/local/bin/elm

# Remove debug symbols to optimize the binary size
RUN strip -s /usr/local/bin/elm
