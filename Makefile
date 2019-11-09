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

hook:
	cp ./hooks/* .git/hooks

coverage:
	make clean
	BISECT_ENABLE=yes dune build
	./run_test_coverage.sh	
	bisect-ppx-report html
	bisect-ppx-report summary
