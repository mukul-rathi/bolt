default:
	opam install . --deps-only
	dune build

install:
	opam install --yes . --deps-only

test:
	dune runtest 

clean:
	dune clean
	git clean -dfX

doc:
	make clean
	dune build @doc
	mkdir docs/
	cp	-r ./_build/default/_doc/_html/* docs/

format:
	(dune build @fmt || dune promote)