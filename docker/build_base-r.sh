#!/usr/bin/env bash

echo "Build the docker"

# Parameters
user_name="rkrispin"
image_label="baser"
r_major=4
r_minor=4
r_patch=0
quarto_ver="1.5.47"
python_ver="3.10"
venv_name="r-env"

# Identify the CPU type (M1 vs Intel)
if [[ $(uname -m) ==  "aarch64" ]] ; then
  CPU="arm64"
elif [[ $(uname -m) ==  "arm64" ]] ; then
  CPU="arm64"
else
  CPU="amd64"
fi

# Setting the image name
ver=${r_major}.${r_minor}.${r_patch}
tag="${CPU}.${ver}"
docker_file=Dockerfile.base-r
image_name=$user_name/$image_label:$tag

echo "Image name: $image_name"

# Build
docker build . \
  -f $docker_file --progress=plain \
  --build-arg PYTHON_VER=$python_ver \
  --build-arg R_VERSION_MAJOR=$r_major \
  --build-arg R_VERSION_MINOR=$r_minor \
  --build-arg R_VERSION_PATCH=$r_patch \
  --build-arg QUARTO_VERSION=$quarto_ver \
  --build-arg VENV_NAME=$venv_name \
   -t $image_name

# Push
if [[ $? = 0 ]] ; then
echo "Pushing docker..."
docker push $image_name
else
echo "Docker build failed"
fi