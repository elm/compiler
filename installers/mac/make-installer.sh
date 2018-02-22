#!/bin/sh
# Run the following command to create an installer:
#
#     bash make-installer.sh 0.13
#
# This will work iff the version you give is defined in BuildFromSource.hs


#### SETUP ####

set -e

version=$1

# Create directory structure for new pkgs
pkg_root=$(mktemp -d -t package-artifacts)
pkg_binaries=$pkg_root
pkg_scripts=$pkg_root/Scripts

mkdir -p $pkg_binaries
mkdir -p $pkg_scripts

usr_binaries=/usr/local/bin


#### BUILD ELM PLATFORM ####

runhaskell ../BuildFromSource.hs $version

platform=Elm-Platform/$version


#### COPY BINARIES ####

# Copy executables into pkg_binaries directory
for exe in elm elm-package elm-make elm-repl elm-reactor
do
    cp $platform/.cabal-sandbox/bin/$exe $pkg_binaries/$exe
done

cp $(pwd)/preinstall $pkg_scripts
cp $(pwd)/postinstall $pkg_scripts

pkgbuild \
    --identifier org.elm-lang.binaries.pkg \
    --install-location $usr_binaries \
    --scripts $pkg_scripts \
    --filter 'Scripts.*' \
    --root $pkg_root \
    binaries.pkg


#### CREATE PACKAGE ####

rm -f Elm-Platform-$version.pkg

productbuild \
    --distribution Distribution.xml \
    --package-path . \
    --resources Resources \
    Elm-Platform-$version.pkg


#### CLEAN UP ####

rm binaries.pkg
rm -rf $pkg_root
