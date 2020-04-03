FROM ocaml/opam2:ubuntu-18.04-opam
USER root 

RUN sudo apt-get -y upgrade
RUN sudo apt-get update

# install Bazel
RUN sudo apt-get install -y m4 curl wget default-jdk	
RUN curl https://bazel.build/bazel-release.pub.gpg | sudo apt-key add - 	
RUN echo "deb [arch=amd64] https://storage.googleapis.com/bazel-apt stable jdk1.8" | sudo tee /etc/apt/sources.list.d/bazel.list	
RUN 	sudo apt update -y && sudo apt install -y bazel 	
RUN sudo apt update -y && sudo apt install -y bazel-2.2.0

# Set up workspace
RUN mkdir -p /usr/src/app
ENV WORKSPACE /usr/src/app
WORKDIR /usr/src/app
COPY . .

#set up opam
RUN opam init --disable-sandboxing --compiler=4.08.0 -y 
RUN eval $(opam env)
RUN echo "test -r /home/opam/.opam/opam-init/init.sh && . /home/opam/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> ~/.profile 

# install clang
RUN  wget https://releases.llvm.org/9.0.0/clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
RUN tar xf clang*
RUN sudo cp -r clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04/* /usr/local/
