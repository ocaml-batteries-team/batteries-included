# set -e

for k in src/*.ml
do
  echo Doing $k...
  sed -i.bakk -e 's/(\*\*T\s/(*$T /g' -e 's/(\*\*Q\s/(*$Q /g' -e 's/(\*\*\*\s/(*$R /g' $k
  diff -u $k.bakk $k
done