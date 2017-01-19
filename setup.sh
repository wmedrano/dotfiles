#!/bin/bash

rm ~/.bashrc
ln -s ~/dotfiles/bash/.bashrc ~/.bashrc

rm ~/.xinitrc
ln -s ~/dotfiles/xinitrc ~/.xinitrc

rm ~/.Xresources
ln -s ~/dotfiles/Xresources ~/.Xresources

rm -r ~/.config/i3
ln -s ~/dotfiles/i3 ~/.config

rm ~/.xsession
ln -s ~/dotfiles/xsession ~/.xsession

rm -r ~/.emacs.d
ln -s ~/dotfiles/emacs.d ~/.emacs.d
#mv ~/emacs.d ~/.emacs.d
