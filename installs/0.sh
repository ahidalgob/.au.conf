#!/bin/bash
sudo apt install --yes git
sudo apt install --yes vim
sudo apt install --yes python python3 python-dev python3-dev
sudo apt install --yes cmake build-essential

git clone https://github.com/ahidalgob/.au_conf ~/.au_conf

mkdir -p ~/.vim/
ln --backup --symbolic ~/.au_conf/vim/ftplugin ~/.vim/ftplugin

ln --backup --symbolic ~/.au_conf/vim/.vimrc ~/.vimrc
git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim

vim +PluginInstall

cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer

cd -
ln --backup --symbolic ~/.au_conf/git/.gitconfig ~/.gitconfig
