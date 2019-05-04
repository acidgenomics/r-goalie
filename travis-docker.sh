#!/usr/bin/env bash
set -Eeuxo pipefail

if [[ -z "${tag:-}" ]]
then
    tag="latest"
fi

image="acidgenomics/basejump:${tag}"
package="$(basename "$TRAVIS_BUILD_DIR")"

docker pull "$image"
docker run -ti --volume="${PWD}:/${package}" --workdir="/${package}" "$image" Rscript -e 'source("travis-docker.R")'
