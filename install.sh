#!/bin/bash

REPO=$(pwd)

ln -sf $REPO/spacemacs.el ~/.spacemacs

mkdir -p ~/.config/fish
ln -sf $REPO/config.fish ~/.config/fish/config.fish
ln -sf $REPO/fishfile.txt ~/.config/fish/fishfile

mkdir -p ~/.config/nvim
ln -sf $REPO/init.vim ~/.config/nvim/init.vim
ln -sf $REPO/tmux.conf ~/.tmux.conf

ln -sf $REPO/gnuplot.plt ~/.gnuplot

ln -sf $REPO/latexmk.pl ~/.latexmkrc

ln -sf $REPO/XCompose ~/.XCompose

ln -sf $REPO/ignore.conf ~/.ignore
git config --global core.excludesfile ~/.ignore

ln -sf $REPO/offlineimap.conf ~/.offlineimaprc
ln -f  $REPO/msmtprc ~/.msmtprc
