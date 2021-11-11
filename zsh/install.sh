#!/bin/sh

# Install Z4H
sh -c "$(curl -fsSL https://raw.githubusercontent.com/romkatv/zsh4humans/v5/install)"

# Custom config.
ln -sf ~/.config/zshrc ~/.zshrc
ln -sf ~/.config/p10k.zsh ~/.p10k.zsh
