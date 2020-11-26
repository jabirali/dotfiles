# Dotfiles
This repo contains my config files for Linux and Mac. Most apps I
use support the XDG specification, so e.g. `stow` is overkill for
me. Instead, I just clone this repo to `~/.config`, and have install
scripts in `bin` that link the remaining few files in place.

Note that many dependencies and plugins (`emacs`, `tmux`, `nvim`, etc.)
are handled via Git submodules. This means that after cloning this repo,
you should run `git submodule init` to fetch the dependencies.

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
A suitable version of Emacs should be installed by Homebrew above,
and Doom Emacs is bundled with this repository as a Git submodule.
Moreover, `~/.config/emacs/bin` should already be added to `$PATH`.
Thus, after performing a recursive clone of this repository and
(re)starting the `fish` shell, we just need to sync Doom Emacs:

    doom install

## Miscellaneous
Run `~/.config/bin/setup-macos` or `~/.config/bin/setup-linux`
to setup the respective platforms. Note that you might also
need to run `git pull --recurse-submodules` to update the
repo, as `tmux` and `nvim` plugins are handled via Git
submodules instead of using a dedicated package manager.

[1]: https://github.com/jabirali?tab=repositories&type=source
[2]: https://brew.sh/
