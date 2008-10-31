#!/bin/sh

ocamlbuild t_list.byte

TESTS="nth map mapx folr folrx map2"
for a in byte
do
    for b in $TESTS
    do
#	echo -n ${b}_g.$a,
	./t_list.$a -s 200 ${b}_g | tee data/${b}_gallium.$a
#	echo -n ${b}_e.$a,
	./t_list.$a -s 200 ${b}_e | tee data/${b}_extlib.$a
    done
done

cd data

for a in byte
do
    for b in $TESTS
    do
	G_PRE="set logscale x; set logscale y; set terminal png; set xlabel \"List Length\"; set ylabel \"actions per second\";"
	echo "$G_PRE set output \"$b.$a.png\"; plot \"${b}_extlib.$a\" w lp, \"${b}_gallium.$a\" w lp" | gnuplot
    done
done