# Dotfiles
This repo contains my config files for Mac and Linux. Most apps support
the XDG specification, so systems like GNU Stow is overkill for my needs.
Instead, I just clone this repo to `~/.config`, and have install scripts
in `~/.config/bin/` that link the remaining few files in place.

## Homebrew
After cloning to `~/.config`, install [Homebrew][1] for macOS or Linux:

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

Ensure that `brew shellenv` is correctly added to your shell init file.
To install all packages needed, make sure `~/.config/bin` is in `$PATH`,
restart your shell, and run `brew sync` to update your system from this.

[1]: https://brew.sh/
