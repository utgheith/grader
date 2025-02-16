#!/bin/bash

D=$(dirname $0)

JVM=$($D/jvm)
eval "$($D/cs java --jvm "$JVM" --env)"

exec "$JAVA_HOME"/bin/java "$*"
