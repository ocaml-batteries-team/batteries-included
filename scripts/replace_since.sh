#!/usr/bin/env sh

# Replace annotations of the form @since NEXT_RELEASE by the
# version number given on the command line

VERSION="$1"

echo "version number: $VERSION"

if [ -z "$VERSION" ] ; then
    echo "please give a version number, for example:"
    echo "sh scripts/replace_since.sh 2.8.0"
    exit 1
fi

sed -e "s/NEXT_RELEASE/$VERSION/" -i '' VERSION
find src/ -name '*.ml*' -exec sed -e "s/NEXT_RELEASE/$VERSION/g" -i '' {} \;
