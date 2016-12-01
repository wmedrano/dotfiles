#!/bin/bash

rm ~/.bashrc
ln -s ~/dotfiles/bash/.bashrc ~/.bashrc

rm ~/.xinitrc
ln -s ~/xinitrc ~/.xinitrc

rm ~/.Xresources
ln -s ~/Xresources ~/.Xresources

rm -r ~/.config/i3
ln -s ~/dotfiles/i3 ~/.config
