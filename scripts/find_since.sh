#!/usr/bin/env sh

# Find remaining NEXT_RELEASE tags

find src/ -name '*.ml*' -exec grep NEXT_RELEASE -n {} \; -print

