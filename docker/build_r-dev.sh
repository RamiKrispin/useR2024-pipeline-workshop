#!/usr/bin/env bash

echo "Build the docker"

# Parameters
user_name="rkrispin"
image_label="user2024workshop"
r_major=4
r_minor=4
r_patch=0


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
docker_file=Dockerfile.r-dev
image_name=$user_name/$image_label:$tag

echo "Image name: $image_name"

# Build
docker build . \
  -f $docker_file --progress=plain \
  --build-arg CPU=$CPU \
   -t $image_name

# Push
if [[ $? = 0 ]] ; then
echo "Pushing docker..."
docker push $image_name
else
echo "Docker build failed"
fi