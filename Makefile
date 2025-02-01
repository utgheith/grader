
TARGET=target/universal/stage/bin/grader 

ALL_FILES = $(shell find src -type f -print) project/build.properties build.sbt

${TARGET} : ${ALL_FILES}
	./sbt stage

% : sbt_%;

sbt_%:
	./sbt $*

