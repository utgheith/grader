all : compile

native:
	./scala-cli.sh --power package src -o grader -f --native-image --graalvm-jvm-id graalvm-java21:21

compile:
	./scala-cli.sh compile src

test:
	./scala-cli.sh test src

format:
	./scala-cli.sh format src

clean:
	./scala-cli.sh clean src
	rm -rf grader

