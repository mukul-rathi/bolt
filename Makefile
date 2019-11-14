default:
	opam install . --deps-only
	dune build

build:
	dune build

install:
	opam install --yes . --deps-only

lint:
	make clean
	dune build @lint
	dune build @fmt
	
test:
	dune runtest 
	scripts/run_integration_E2E_tests.sh --all

clean:
	dune clean
	git clean -dfX
	rm -rf docs/

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
	scripts/run_test_coverage.sh	
	bisect-ppx-report html
	bisect-ppx-report summary
