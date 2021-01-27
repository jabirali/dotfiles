# Dotfiles
This repo contains my config files for Mac and Linux. Most of my apps support
the XDG specification, so systems like GNU Stow is overkill for my needs.
Instead, I just clone this repo to `~/.config`, and have an install script
under `~/.config/bin/` that then link the remaining few files in place.

Note that some dependencies and plugins are handled via Git submodules.
This means you need to run `git submodule init` to fetch dependencies.

## Homebrew
After cloning to `~/.config`, install [Homebrew][2] for macOS or Linux:

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

Ensure that `brew shellenv` is correctly added to your shell init file.
Depending on whether you use a home or work computer, choose a profile
in `~/.config/brewfile/` and link it to `~/.config/brewfile/this.brew`:

    ln -sf ~/.config/brewfile/{home,this}.brew
    ln -sf ~/.config/brewfile/{work,this}.brew

To install all packages needed, make sure `~/.config/bin` is in `$PATH`,
restart your shell, and run `brew sync` to update your system from this.

## Miscellaneous
Run `~/.config/bin/setup-macos` or `~/.config/bin/setup-linux` to setup
the respective platforms. Note that you might also need to run `git pull
--recurse-submodules` to update the repo, as `tmux` and `nvim` plugins are
handled via Git submodules instead of using a dedicated package manager.

[1]: https://github.com/jabirali?tab=repositories&type=source
[2]: https://brew.sh/
