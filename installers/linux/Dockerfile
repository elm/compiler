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

# Use Alpine 3.23 with GHC 9.8.2
FROM alpine:3.23

# Install packages required to build elm
RUN apk add --no-cache build-base ghc cabal wget musl-dev gmp-dev gmp-static zlib-dev zlib-static ncurses-dev ncurses-static

WORKDIR /elm

# Import source code
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY cabal.config cabal.project elm.cabal LICENSE ./

# Build sendfile64 shim: musl only has sendfile; snap-server references sendfile64 (glibc-only)
COPY installers/linux/sendfile64.c /tmp/sendfile64.c
RUN cc -c /tmp/sendfile64.c -o /tmp/sf64.o \
    && ar rcs /usr/lib/libsf64.a /tmp/sf64.o

# Build statically linked elm binary
RUN cabal new-update
RUN cabal new-build --enable-executable-static --ghc-option=-split-sections --ghc-option=-optl=-lsf64
RUN cp ./dist-newstyle/build/*-linux/ghc-*/elm-*/x/elm/build/elm/elm /usr/local/bin/elm

# Remove debug symbols to optimize the binary size
RUN strip -s /usr/local/bin/elm
