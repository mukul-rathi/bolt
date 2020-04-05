#!/bin/bash
	sudo apt-get install -y m4 curl
	# install opam dependencies
	cd /home/opam/opam-repository && git pull
	opam update 
	cd -
	eval $(opam env)
	make install
