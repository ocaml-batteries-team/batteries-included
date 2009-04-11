TESTS:=nth map mapx folr folrx map2 append flatten filter1 filter2
MAX_I:=--max-i 10_000_000

pngs: $(addsuffix .png, $(TESTS))

t_list.byte t_list.native: t_list.ml
	ocamlbuild t_list.byte t_list.native

%.gallium_byte: t_list.byte
#The following doesn't succeed for large I -- not tail-recursive
	- ./t_list.byte $(MAX_I) -g $(basename $@) > $@

%.extlib_byte: t_list.byte
	./t_list.byte $(MAX_I) -e $(basename $@) > $@

%.blue_byte: t_list.byte
	./t_list.byte $(MAX_I) -b $(basename $@) > $@

%.core_byte: t_list.byte
	./t_list.byte $(MAX_I) -c $(basename $@) > $@

%.gallium_native: t_list.native
#The following doesn't succeed for large I -- not tail-recursive
	- ./t_list.native $(MAX_I) -g $(basename $@) > $@

%.extlib_native: t_list.native
	./t_list.native $(MAX_I) -e $(basename $@) > $@

%.blue_native: t_list.native
	./t_list.native $(MAX_I) -b $(basename $@) > $@

%.core_native: t_list.native
	./t_list.native $(MAX_I) -c $(basename $@) > $@

%.png: %.gallium_byte %.extlib_byte %.gallium_native %.extlib_native %.blue_byte %.blue_byte %.core_native %.core_native
	TEST=$(basename $@)
	echo "set logscale x; set logscale y; set terminal png; set xlabel \"List Length\"; set ylabel \"List-operations per second\"; set output \"$@\"; plot \"$(basename $@).gallium_byte\" w lp, \"$(basename $@).extlib_byte\" w lp, \"$(basename $@).gallium_native\" w lp, \"$(basename $@).extlib_native\" w lp, plot \"$(basename $@).blue_byte\" w lp, \"$(basename $@).core_byte\" w lp, \"$(basename $@).blue_native\" w lp, \"$(basename $@).core_native\" w lp" | gnuplot

t_byte: $(addsuffix .gallium_byte, $(TESTS)) $(addsuffix .extlib_byte, $(TESTS))

clean: 
	- rm *.*_byte *.*_native
	- rm *.png