#!/usr/bin/env bash
VENV_NAME=$1

# Setting Python and Radian
# # Set Python Environment
python3 -m venv /opt/$VENV_NAME  \
    && export PATH=/opt/$VENV_NAME/bin:$PATH \
    && echo "source /opt/$VENV_NAME/bin/activate" >> ~/.bashrc


pip3 install -r ./pkgs/requirements.txt