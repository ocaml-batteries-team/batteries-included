#!/usr/bin/env sh

# Replace annotations of the form @since NEXT_RELEASE by the
# version number given on the command line

VERSION="$1"

echo "version number: $VERSION"

if [ -e "$VERSION" ] ; then
    echo "please give a version number"
    exit 1
fi

find src/ -name '*.ml*' -exec sed -i'' "s/NEXT_RELEASE/$VERSION/g" {} \;
