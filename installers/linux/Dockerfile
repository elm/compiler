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

FROM alpine:3.17

RUN apk add --no-cache curl pkgconfig build-base musl-dev zlib-dev zlib-static gmp-dev ncurses-dev ncurses-static

WORKDIR /elm

ARG GHCUP_INSTALL_BASE_PREFIX=/elm
ARG PATH="${PATH}:/elm/.ghcup/bin"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
RUN ghcup install ghc --set 9.8.4
RUN ghcup install cabal --set 3.12.1.0

RUN cabal update

COPY elm.cabal ./
COPY builder builder
COPY compiler compiler
COPY reactor reactor
COPY terminal terminal
COPY src src

RUN cabal build elm --enable-executable-static --ghc-option=-split-sections
RUN cp $(cabal list-bin elm) /usr/local/bin/elm
RUN strip -s /usr/local/bin/elm
