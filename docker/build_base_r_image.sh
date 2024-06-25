#!/bin/bash

echo "Build the docker"
image="rkrispin/base-r:4.3.3"
docker build . -f Dockerfile.base \
               --progress=plain \
               --build-arg PROJECT_NAME="R Dev Env" \
               --build-arg VENV_NAME="R_ENV" \
               --build-arg R_VERSION_MAJOR=4 \
               --build-arg R_VERSION_MINOR=3 \
               --build-arg R_VERSION_PATCH=3 \
                --build-arg DEBIAN_FRONTEND=noninteractive \
                 --build-arg CRAN_MIRROR="https://cran.rstudio.com/" \
               --build-arg QUARTO_VER="1.4.552" \
               -t $image

if [[ $? = 0 ]] ; then
echo "Pushing docker..."
docker push $image
else
echo "Docker build failed"
fi
