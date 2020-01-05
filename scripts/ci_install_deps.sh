#!/bin/bash
	sudo apt-get install -y m4 curl 
	# install bazel
	sudo apt-get install -y default-jdk
	curl https://bazel.build/bazel-release.pub.gpg | sudo apt-key add - 
	echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list
	sudo apt update -y && sudo apt install -y bazel 
	sudo apt update -y && sudo apt full-upgrade -y
	sudo apt install -y bazel-2.0.0
	# install coveralls report generator
	sudo apt install -y python-pip
	pip install cpp-coveralls
	# install protoc
	sudo apt-get install -y autoconf automake libtool curl make g++ unzip
	curl -L  https://github.com/protocolbuffers/protobuf/releases/download/v3.11.2/protobuf-cpp-3.11.2.tar.gz | tar xvz
	cd protobuf-3.11.2
  ./configure
     make
     make check
     sudo make install
     sudo ldconfig
	cd ..

	# install opam dependencies
	make install