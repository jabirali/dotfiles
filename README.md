# Dotfiles
This repository contains my config files for Linux and Mac. Most apps
I use support the XDG specification, so e.g. `stow` is overkill for me.
Instead, I just clone this repository to `~/.config`, and have install
scripts in `bin` that link the remaining few files to the right place.

Much of my config files have been exported to separate plugins hosted
on [my GitHub][1]. Note also that this repository references some external
Git repositories for things like `nvim` plugins; thus, for a full clone of
this repository including all dependencies (i.e. Git submodules).

## Quickstart
After cloning to `~/.config`, install [Homebrew][2] for Linux or macOS:

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

Ensure that `brew shellenv` is correctly added to your shell init file.
Then run either `~/.config/bin/setup-macos` or `~/.config/setup-linux`,
followed by `brew bundle install --global`, to setup the whole system.

Since I have replaced tool-specific package managers like `vim-plug`
and `tpm` with Git submodules, updating all external plugins is done
via `git pull --recurse-submodules`.

[1]: https://github.com/jabirali?tab=repositories&type=source
[2]: https://brew.sh/
