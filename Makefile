

all : compile;

% : sbt_%;

sbt_%:
	./sbt --warn $*

format : scalafmt;

clean:
	rm -rf target

