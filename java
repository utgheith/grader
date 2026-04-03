#!/bin/bash

D=$(dirname $0)

JVM=$($D/jvm)
eval "$($D/cs java --jvm "$JVM" --env)"
export JDK_JAVA_OPTIONS="$JDK_JAVA_OPTIONS --sun-misc-unsafe-memory-access=allow --enable-native-access=ALL-UNNAMED"

exec "$JAVA_HOME"/bin/java "$*"
