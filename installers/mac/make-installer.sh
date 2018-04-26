#!/bin/sh
# Run the following command to create an installer:
#
#     bash make-installer.sh
#



#### SETUP ####

set -e

# Create directory structure for new pkgs
pkg_root=$(mktemp -d -t package-artifacts)
pkg_binaries=$pkg_root
pkg_scripts=$pkg_root/Scripts

mkdir -p $pkg_binaries
mkdir -p $pkg_scripts

usr_binaries=/usr/local/bin


#### BUILD ASSETS ####

cp ../../dist/build/elm/elm $pkg_binaries/elm

cp $(pwd)/preinstall $pkg_scripts
cp $(pwd)/postinstall $pkg_scripts

pkgbuild \
    --identifier org.elm-lang.binaries.pkg \
    --install-location $usr_binaries \
    --scripts $pkg_scripts \
    --filter 'Scripts.*' \
    --root $pkg_root \
    binaries.pkg


#### BUNDLE ASSETS ####

rm -f Elm.pkg

productbuild \
    --distribution Distribution.xml \
    --package-path . \
    --resources Resources \
    Elm.pkg


#### CLEAN UP ####

rm binaries.pkg
rm -rf $pkg_root
