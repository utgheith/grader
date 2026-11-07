#!/bin/bash


# snapshot the home directory; a failure here shouldn't stop the loop
snapshot="$(zfs list -H -o name "$HOME")@grader-loop-$(date +%Y-%m-%d_%H-%M-%S)"
zfs snapshot "$snapshot" && echo "took snapshot $snapshot"


