#!/usr/bin/env bash

# Installing dependencies
apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \
    gfortran \
    git \
    g++ \
    jq \
    libreadline-dev \
    libfontconfig1-dev \
    libx11-dev \
    libxt-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    r-cran-rjava \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgdal-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    ca-certificates \
    locales \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libbz2-dev \
    libzstd-dev \
    liblzma-dev \
    libpcre2-dev \
    openjdk-8-jdk \
    screen \
    texinfo \
    texlive \
    texlive-fonts-extra \
    vim \
    wget \
    xvfb \
    tzdata \
    sudo\
    curl\
    libgit2-dev \
    libmagick++-dev \
    make \
    tmux \
    python3-launchpadlib \
    python3.10-dev \
    python3.10-venv \
    python3-pip \
    r-cran-rgdal \
    libproj-dev \
    pandoc \
    unixodbc \
    && rm -rf /var/lib/apt/lists/*