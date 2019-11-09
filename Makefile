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
