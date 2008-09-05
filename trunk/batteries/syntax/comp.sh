EXTS=pa_openin

for ext in $EXTS
do
    ocamlbuild $ext.cmo
done
