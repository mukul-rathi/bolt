default:
	opam install . --deps-only
	make build

build:
	make clean
	make pre-build
	dune build
	bazel build //src/llvm-backend/main

install:
	eval $(opam config env)
	opam install --yes . --deps-only
	eval $(opam env)
	opam update

lint:
	make clean
	make pre-build
	dune build @lint
	dune build @fmt
	
test:
	make clean
	make pre-build
	dune runtest 
	scripts/run_e2e_tests.sh
	bazel test tests/llvm-backend:test_llvm_backend

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

hook:
	cp ./hooks/* .git/hooks

coverage:
	make clean
	make pre-build
	BISECT_ENABLE=yes dune build
	scripts/run_test_coverage.sh	
	bisect-ppx-report html
	bisect-ppx-report summary

.SILENT: pre-build
pre-build:
	# hack: create opam files so libraries can be exposed publicly
	cp bolt.opam ast.opam
	cp bolt.opam parsing.opam
	cp bolt.opam typing.opam
	cp bolt.opam desugaring.opam	
	cp bolt.opam ir_gen.opam


ci-install:
	sudo apt-get install -y m4 curl
	curl https://bazel.build/bazel-release.pub.gpg | sudo apt-key add - 
	echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
	sudo apt update -y && sudo apt install -y bazel 
	sudo apt update -y && sudo apt full-upgrade -y
	sudo apt install -y bazel-2.0.0
