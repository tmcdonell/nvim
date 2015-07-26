#!/bin/bash -ex

curl -fLo \
    $HOME/.nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
