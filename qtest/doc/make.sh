set -e

git log -n 5 -- . | par 80 > gitlog
# git diff HEAD -- . > gitdiff

hevea -exec xxdate.exe -moreentities -fix qtest.tex
hacha -tocbis qtest.html