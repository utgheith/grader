#!/bin/bash

D=$(dirname $0)

exec $D/cs launch --jvm graalvm-java23:23.0.2 sbt -- "$@"

