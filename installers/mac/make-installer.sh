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
    --sign "Developer ID Installer: <NAME>" \
    --identifier org.elm-lang.binary \
    --install-location $usr_binaries \
    --scripts $pkg_scripts \
    --filter 'Scripts.*' \
    --root $pkg_root \
    binaries.pkg


#### BUNDLE ASSETS ####

rm -f installer-for-mac.pkg

productbuild \
    --sign "Developer ID Installer: <NAME>" \
    --identifier org.elm-lang.installer \
    --distribution Distribution.xml \
    --package-path . \
    --resources Resources \
    installer-for-mac.pkg


#### CLEAN UP ####

rm binaries.pkg
rm -rf $pkg_root


#### BEGIN NOTARIZATION ####

xcrun altool \
    --notarize-app \
    --primary-bundle-id "org.elm-lang.installer" \
    --username "<EMAIL>" \
    --password "@keychain:Developer-altool" \
    --file "installer-for-mac.pkg"

# From https://scriptingosx.com/2019/09/notarize-a-command-line-tool/
#
#### Check on notarization:
#
# xcrun altool \
#     --notarization-info "<RequestUUID>" \
#     --username "<EMAIL>" \
#     --password "@keychain:Developer-altool"
#
#
#### Staple Notarization:
#
# xcrun stapler staple installer-for-mac.pkg
