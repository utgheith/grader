JVM=graalvm-java23:23

all : compile

native:
	./scala-cli.sh --power package --jvm ${JVM} src -o grader -f --native-image --graalvm-jvm-id ${JVM}

compile:
	./scala-cli.sh compile --jvm ${JVM} src

test:
	./scala-cli.sh test --jvm ${JVM} src

format:
	./scala-cli.sh format src

clean:
	./scala-cli.sh clean src
	rm -rf grader

