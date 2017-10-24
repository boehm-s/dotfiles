#!/bin/bash

rm -rf .emacs
ln -s .emacs.d/conf/dotemacs.el .emacs

cp -r .emacs.d dotfiles/
cp .emacs.d/conf/emacs-config.org dotfiles/emacs.org
cp .emacs.d/conf/emacs-config.org dotfiles/README.org
cp .bashrc dotfiles/
cp dotfiles.sh dotfiles/
cp .tern-config dotfiles/
