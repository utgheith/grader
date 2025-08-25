

ALL_FILES = $(shell find src -type f -print) project/build.properties build.sbt jvm cs java

stage : ${ALL_FILES}
	rm -f stage
	./sbt --warn stage 2> sbt_errors
	touch stage

% : sbt_%;

sbt_%:
	./sbt --warn $* 2> sbt_errors

clean:
	rm -rf stage target

