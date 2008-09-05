EXTS=pa_openin

FILES=''
for ext in $EXTS
do
    FILES="$FILES _build/$ext/$ext.cmo"
done

ocamlfind install batsyntax META $FILES