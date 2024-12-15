
all : stage;

cs:
	curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
	chmod +x cs

Makefile:;

% : sbt_%;

sbt_%: cs
	./cs launch --jvm 23 sbt -- $*

