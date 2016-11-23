#!/bin/bash

rm ~/.bashrc
ln -s ~/dotfiles/bash/.bashrc ~/.bashrc

rm -r ~/.config/i3
ln -s ~/dotfiles/i3 ~/.config
