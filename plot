#!/bin/bash

[ $# -eq 0 ] || {
    echo 'unexpected command line arguments'
    echo 'usage: $0'
    echo 'Expects a stream on the standard input and draws'
    echo 'gnuplot graphs when it recognizes some gnuplot data'
    exit 1
}

trap cleanup EXIT SIGINT
cleanup() {
    rm -f "$tmp"
}
tmp="$(mktemp)"

while read line; do
    case "$line" in
        '#'*)
            # reading blocks of line starting with a line
            # starting with a sharp and ending with an empty line
            # this line has format #title size\tname1\tname2 etc.
            names=$(echo "$line" | cut -d '	' -f 2-)
            title=$(echo "$line" | sed 's/^#\([^ ]*\) .*$/\1/')
            > "$tmp" # emptying the file
            while read line && [ "$line" != "" ]; do
                echo "$line" >> "$tmp"
            done
            gnuplot -p <(
                echo set key left top
                echo set logscale x
                echo set title "'$title'"
                echo -n 'plot '
                counter=1
                for name in $names; do
                    counter=$((counter+1))
                    if [ $counter -ne 2 ]; then
                        echo -e -n ', \\\n     '
                    fi
                    echo -n \'"$tmp"\' using 1:$counter title \'"$name"\' with linespoints
                done
                echo
            )
    esac
done
