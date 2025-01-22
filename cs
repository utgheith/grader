#!/bin/bash

D=$(dirname $0)

arm64_Darwin=aarch64-apple-darwin
i386_Darwin=x86_64-apple-darwin
x86_64_Linux=x86_64-pc-linux
PLATFORM=$(arch)_$(uname)
CS_SUFFIX=${!PLATFORM}
CS_COMMAND=cs_${CS_SUFFIX}
CS_PATH=$D/$CS_COMMAND


CS_URL="https://github.com/coursier/launchers/raw/master/cs-${CS_SUFFIX}.gz"

test -x ${CS_PATH} || (rm -f ${CS_PATH} && (curl -fL "${CS_URL}" | gzip -d > ${CS_PATH}) && chmod +x  ${CS_PATH})

exec ${CS_PATH} "$@"

