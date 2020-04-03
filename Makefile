default:
	opam update
	opam install . --deps-only
	make build

.PHONY: build
build:
	make pre-build
	dune build
	bazel build //src/llvm-backend:main

install:
	eval $(opam config env)
	opam update
	opam install --yes . --deps-only
	eval $(opam env)

lint:
	make clean
	make pre-build
	dune build @lint
	dune build @fmt

test:
	make pre-build
	dune runtest 
	scripts/run_frontend_integration_tests.sh
	bazel test tests/llvm-backend/llvm_ir_codegen:test_llvm_ir_codegen
	scripts/run_e2e_tests.sh 


.SILENT: clean
clean:
	dune clean
	@git clean -dfX
	rm -rf docs/

doc:
	make clean
	make pre-build
	dune build @doc
	mkdir docs/
	cp	-r ./_build/default/_doc/_html/* docs/

format:
	make pre-build
	dune build @fmt --auto-promote
	find **/llvm-backend/** -name "*.h" -o -name "*.cc" | xargs clang-format -i --style=file 

hook:
	cp ./hooks/* .git/hooks

coverage:
	make clean
	make pre-build
	BISECT_ENABLE=yes dune build
	dune runtest 
	scripts/run_frontend_integration_tests.sh
	bisect-ppx-report html
	bisect-ppx-report summary

.SILENT: pre-build
pre-build:
	# hack: create opam files so libraries can be exposed publicly
	cp bolt.opam ast.opam
	cp bolt.opam parsing.opam
	cp bolt.opam typing.opam
	cp bolt.opam desugaring.opam
	cp bolt.opam data_race_checker.opam	
	cp bolt.opam ir_gen.opam
	rm -rf bazel-bolt