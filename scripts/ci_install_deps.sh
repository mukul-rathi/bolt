#!/bin/bash
	sudo apt-get install -y m4 curl
	# install bazel	
	# install opam dependencies
	eval $(opam env)
	make install
