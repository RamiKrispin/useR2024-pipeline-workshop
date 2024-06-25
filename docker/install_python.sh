#!/bin/bash

VENV_NAME=$1

python3 -m venv /opt/$VENV_NAME  \
    && export PATH=/opt/$VENV_NAME/bin:$PATH \
    && echo "source /opt/$VENV_NAME/bin/activate" >> ~/.bashrc \
    && echo "alias r='radian --profile=/.Rprofile'" >> ~/.bashrc


pip3 install -r ./settings/requirements.txt