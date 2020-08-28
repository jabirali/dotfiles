# Dotfiles
This repository contains my Linux config files. Nearly all programs I use
support the XDG specification, so e.g. `stow` is overkill for me. Instead,
I just clone this repository to `~/.config`, and have an install script
in `bin` that link the remaining few files to the right place.

Note that the most interesting parts of my config files have been exported to
separate plugins for Tmux, Fish, and Neovim. Most are hosted on [my GitHub][1].
Note also that this repository references some external Git repositories for
e.g. `nvim` plugins; thus, for a full clone of this repository including all
dependencies (i.e. Git submodules), you have to use `git clone --recursive`.
Since I have replaced `vim-plug` and `tpm` update scripts with Git submodules,
updating all external plugins is now done via `git pull --recurse-submodules`.

## Quickstart
After cloning to `~/.config`, install [Homebrew][2] for Linux or macOS:

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

Ensure that `brew shellenv` is correctly added to your shell init file
as suggested in the output of the installation command above. Then run
either `~/.config/bin/setup-macos` or `~/.config/setup-linux`, followed
by `brew bundle install --global`, to setup the whole system correctly.

[1]: https://github.com/jabirali?tab=repositories&type=source
[2]: https://brew.sh/
