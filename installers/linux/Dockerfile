# Based initially on https://gist.github.com/rlefevre/1523f47e75310e28eee243c9c5651ac9
#
# Build Linux x64 binary from elm compiler top-level directory:
#
#     container build --arch=amd64 --tag=elm --file=installers/linux/Dockerfile .
#
# Retrieve the Linux binary:
#
#     container run --name=elm --arch=amd64 --detach elm sleep infinity
#     container copy elm:/usr/local/bin/elm ~/Desktop
#     container stop --all
#     container prune
#     container image prune --all
#     container system df
#

FROM alpine:3.24

RUN apk add --no-cache ghc cabal pkgconfig gcc g++ musl-dev zlib-dev zlib-static gmp-dev gmp-static ncurses-dev ncurses-static

WORKDIR /elm
RUN cabal update

COPY elm.cabal ./
RUN cabal build --enable-executable-static --only-dependencies

COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY src src

RUN cabal build elm --enable-executable-static --ghc-option=-split-sections
RUN cp $(cabal list-bin elm) /usr/local/bin/elm
RUN strip -s /usr/local/bin/elm
