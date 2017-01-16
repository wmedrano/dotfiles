#!/bin/bash

rm ~/.bashrc
ln -s ~/dotfiles/bash/.bashrc ~/.bashrc

rm ~/.xinitrc
ln -s ~/dotfiles/xinitrc ~/.xinitrc

rm ~/.Xresources
ln -s ~/dotfiles/Xresources ~/.Xresources

rm -r ~/.config/i3
ln -s ~/dotfiles/i3 ~/.config

feh --bg-scale ~/wallpaper.png
