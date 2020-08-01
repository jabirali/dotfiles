#!/usr/bin/env bash

# Plugins retrieved from `codium --list-extensions`.
EXTENSIONS=(
    "ccy.ayu-adaptive"        \
    "auiworks.amvim"          \
    "ms-python.python"        \
    "james-yu.latex-workshop" \
)

# Install the plugins above for Visual Studio Codium.
for EXTENSION in ${EXTENSIONS[@]}
do
    codium --install-extension $EXTENSION
done
