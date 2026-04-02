#!/bin/bash

D=$(dirname $0)

exec $D/cs launch --java-opt=--sun-misc-unsafe-memory-access=allow --java-opt=--enable-native-access=ALL-UNNAMED --jvm $($D/jvm) sbt -- "$@"

