# Dotfiles
This repository contains my Linux config files. Nearly all programs I use
support the XDG specification, so e.g. `stow` is overkill for me. Instead,
I just clone this repository to `~/.config`, and have an install script
in `bin` that link the remaining few files to the right place.

Note that the most interesting parts of my config files have been exported to
separate plugins for Tmux, Fish, and Neovim. Most are hosted on [my GitHub][1].
Note also that this repository references some external Git repositories for
e.g. color themes; thus, for a full clone of this repository including all
dependencies, you have to use `git clone --recurse-submoules`.

[1]: https://github.com/jabirali?tab=repositories&type=source

