#!/bin/bash
	sudo apt-get install -y m4 curl 		sudo apt-get install -y m4 
	# install bazel	
	sudo apt-get install -y default-jdk	
	curl https://bazel.build/bazel-release.pub.gpg | sudo apt-key add - 	
	echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list	
	sudo apt update -y && sudo apt install -y bazel 	
	sudo apt update -y && sudo apt full-upgrade -y	
	sudo apt install -y bazel-2.2.0

	# install opam dependencies
	make install
