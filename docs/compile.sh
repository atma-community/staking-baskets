#!/bin/bash

docker build -t doc docker
docker run --privileged -v $(pwd)/doc:/doc doc
