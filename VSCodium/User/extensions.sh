#!/usr/bin/env bash

# Plugins retrieved from `codium --list-extensions`.
EXTENSIONS=(
    "vscodevim.vim"    \
    "ms-python.python" \
)

# Install the plugins above for Visual Studio Codium.
for EXTENSION in ${EXTENSIONS[@]}
do
    codium --install-extension $EXTENSION
done
