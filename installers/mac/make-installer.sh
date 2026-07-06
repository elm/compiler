#!/bin/sh
# Run the following command to create an installer:
#
#     bash make-installer.sh universal-binary
#


#### ARGUMENTS ####

set -e

if [[ $# -eq 1 && -x "$1" ]]; then
    BINARY=$1
else
    echo "expecting one argument, the path to the universal binary"
    exit 1
fi


#### SETUP ####

function cleanup {
    rm -f elm.entitlements
}
trap cleanup EXIT

# Create directory structure for new pkgs
pkg_root=$(mktemp -d -t package-artifacts)
pkg_binaries=$pkg_root
pkg_scripts=$pkg_root/Scripts

mkdir -p $pkg_binaries
mkdir -p $pkg_scripts

usr_binaries=/usr/local/bin


#### BUILD ASSETS ####


DEVELOPER_NAME="NAME"
TEAM_ID="TEAM"
EMAIL="EMAIL"
PASSWORD="PASSWORD"


cp $BINARY $pkg_binaries/elm

/usr/libexec/PlistBuddy -c "Add :com.apple.security.files.all bool true" elm.entitlements

codesign --sign "Developer ID Application: $DEVELOPER_NAME ($TEAM_ID)" \
    --identifier=org.elm-lang.binary \
    --options=runtime \
    --entitlements=elm.entitlements \
    --strict \
    --strip-disallowed-xattrs \
    --force \
    $pkg_binaries/elm

cp $(pwd)/preinstall $pkg_scripts
cp $(pwd)/postinstall $pkg_scripts

pkgbuild \
    --sign "Developer ID Installer: $DEVELOPER_NAME ($TEAM_ID)" \
    --identifier org.elm-lang.binary \
    --install-location $usr_binaries \
    --scripts $pkg_scripts \
    --filter 'Scripts.*' \
    --root $pkg_root \
    binaries.pkg


#### BUNDLE ASSETS ####

rm -f installer-for-mac.pkg

productbuild \
    --sign "Developer ID Installer: $DEVELOPER_NAME ($TEAM_ID)" \
    --identifier org.elm-lang.installer \
    --distribution Distribution.xml \
    --package-path . \
    --resources Resources \
    installer-for-mac.pkg


#### CLEAN UP ####

rm binaries.pkg
rm -rf $pkg_root


#### NOTARIZE
#
# https://developer.apple.com/documentation/security/resolving-common-notarization-issues

xcrun notarytool submit \
    --apple-id $EMAIL \
    --team-id $TEAM_ID \
    --password $PASSWORD \
    --wait \
    installer-for-mac.pkg

xcrun stapler staple installer-for-mac.pkg


