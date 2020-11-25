# Dotfiles
This repo contains my config files for Linux and Mac. Most apps I
use support the XDG specification, so e.g. `stow` is overkill for
me. Instead, I just clone this repo to `~/.config`, and have install
scripts in `bin` that link the remaining few files to the right place.

## Homebrew
After cloning to `~/.config`, install [Homebrew][2] for Linux or macOS:

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

Ensure that `brew shellenv` is correctly added to your shell init file.
Depending on whether you use a home or work computer, choose a profile
in `~/.config/brewfile/` and link it to `~/.config/brewfile/this.brew`:

    ln -sf ~/.config/brewfile/{home,this}.brew

To install all packages needed, make sure `~/.config/bin` is in `$PATH`,
restart your shell, and run `brew sync` to update your system from this.

## Doom Emacs
A suitable version of Emacs should be installed by Homebrew above.
After this, proceed to install Doom Emacs via the following commands:

    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
    ln -sf ~/.config/doom.d ~/.doom.d
    ~/.emacs.d/bin/doom install

This should install and setup Doom with the bundled configuration.
See the literate configuration in [doom.d](./doom.d/config.org) for
details; this file is automatically tangled to `config.el` by Doom.

## Miscellaneous
Run `~/.config/bin/setup-macos` or `~/.config/bin/setup-linux`
to setup the respective platforms. Note that you might also
need to run `git pull --recurse-submodules` to update the
repo, as `tmux` and `nvim` plugins are handled via Git
submodules instead of using a dedicated package manager.

[1]: https://github.com/jabirali?tab=repositories&type=source
[2]: https://brew.sh/
