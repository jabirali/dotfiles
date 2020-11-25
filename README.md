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

Then run `~/.config/bin/setup-macos` or `~/.config/bin/setup-linux`
followed by `brew bundle install --global`. Since I have replaced
package managers like `vim-plug` and `tpm` with Git submodules,
updating plugins is done via `git pull --recurse-submodules`.

    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install

[1]: https://github.com/jabirali?tab=repositories&type=source
[2]: https://brew.sh/
