JVM=graalvm-java21:21

all : compile

native:
	./scala-cli.sh --power package --jvm ${JVM} src -o grader -f --native-image --graalvm-jvm-id ${JVM}

compile:
	./scala-cli.sh --power compile --jvm ${JVM} src

test:
	./scala-cli.sh --power test --jvm ${JVM} src

format:
	./scala-cli.sh --power format src

clean:
	./scala-cli.sh --power clean src
	rm -rf grader

