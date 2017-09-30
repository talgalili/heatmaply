.PHONY: test

document:
	R --slave --no-save -e 'library("devtools"); document()'

test:
	R --slave --no-save -e 'library("devtools"); test()'

vignettes:
	R --slave --no-save -e 'library("devtools"); build_vignettes()'

install:
	R --slave --no-save -e 'library("devtools"); install()'
