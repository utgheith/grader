#!/bin/bash

D=$(dirname $0)
echo "D=$D"

exec $D/cs launch --jvm 23 sbt

