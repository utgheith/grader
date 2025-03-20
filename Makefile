

ALL_FILES = $(shell find src -type f -print) project/build.properties build.sbt jvm cs java

stage : ${ALL_FILES}
	rm -f stage
	./sbt stage
	touch stage

% : sbt_%;

sbt_%:
	./sbt $*

clean:
	rm -rf stage target

