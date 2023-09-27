.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -f 60_seconds.zip
	zip -r 60_seconds.zip . -x@exclude.lst

clean: bisect-clean
	dune clean
	rm -f 60_seconds.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

loc:
	dune clean
	rm -f 60_seconds.zip
	cloc --by-file --include-lang=OCaml .
	dune build