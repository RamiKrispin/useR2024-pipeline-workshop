#!/bin/bash

echo "Build the docker"

image="rkrispin/vscode_r_dev:0.1.0"

docker build . -f Dockerfile.dev \
               --progress=plain \
               -t $image

if [[ $? = 0 ]] ; then
echo "Pushing docker..."
docker push  $image
else
echo "Docker build failed"
fi
