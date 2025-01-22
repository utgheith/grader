

arm64_Darwin=aarch64-apple-darwin
i386_Darwin=x86_64-apple-darwin
x86_64_Linux=x86_64-pc-linux
CS_URL="https://github.com/coursier/launchers/raw/master/cs-${${shell arch}_${shell uname}}.gz"

all : stage;

cs:
	curl -fL "${CS_URL}" | gzip -d > cs
	chmod +x cs

Makefile:;

% : sbt_%;

sbt_%: cs
	./cs launch --jvm 23 sbt -- $*

