

arm64_Darwin=aarch64-apple-darwin
CS_URL="https://github.com/coursier/launchers/raw/master/cs-${${shell arch}_${shell uname}}.gz"
SELECTOR=${$(shell arch)_$(shell uname)}

all : stage;

cs:
	curl -fL "${CS_URL}" | gzip -d > cs
	chmod +x cs

Makefile:;

% : sbt_%;

sbt_%: cs
	./cs launch --jvm 23 sbt -- $*

