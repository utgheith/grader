
all : stage;

% : sbt_%;

sbt_%:
	cs launch --jvm 23 sbt -- $*



