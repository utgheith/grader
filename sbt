#!/bin/bash

D=$(dirname $0)

exec $D/cs launch --jvm 23 sbt -- "$@"

