#!/bin/bash

alias INSTALL='sudo apt install -y'

#TODO bashrc
#TODO gtk

$INSTALL xserver-xorg-core xserver-xorg-video-dummy xserver-xorg-video-intel
# when installed 18.04, these two were needed by a xmonad dependency
$INSTALL libxrandr-dev libxss-dev

$INSTALL xmonad cabal-install
cabal update

cabal install --force-reinstalls xmonad xmonad-contrib

$INSTALL stalonetray dzen2 conky feh xdotool compton xclip curl git vim tmux \
    gcc build-essential python cmake python3-dev suckless-tools feh \
    rxvt-unicode vim-gui-common alsa-base pulseaudio thunar nm-tray

$INSTALL xserver-xorg-input-synaptics
# this would copy a template config file to the correct place. Might want to
# modify it and save it.

#mkdir -p /etc/X11/xorg.conf.d
#sudo cp /usr/share/X11/xorg.conf.d/70-synaptics.conf /etc/X11/xorg.conf.d


################################################################################

git clone https://github.com/ahidalgob/.au.conf

mkdir -p ~/.vim
ln -s ~/.au.conf/vim/.vimrc ~/
ln -s ~/.au.conf/vim/ftplugin ~/.vim
git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim
vim +PluginInstall
cd ~/.vim/bundle/YouCompleteMe
./install.py --clang-completer
cd -

ln -s ~/.au.conf/git/.gitconfig ~/.gitconfig

ln -s ~/.au.conf/tmux/.tmux.conf ~/
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
# you have to run prefix + I then (I think)


ln -s ~/.au.conf/.stalonetrayrc ~/
ln -s ~/.au.conf/xmonad/.xmonad ~/
ln -s ~/.au.conf/.icons ~/
ln -s ~/.au.conf/X/.xinitrc ~/
ln -s ~/.au.conf/X/.Xresources ~/
ln -s ~/.au.conf/urxvt/.urxvt ~/

ln -s ~/.au.conf/.themes ~/
ln -s ~/.au.conf/.gtkrc-2.0 ~/

mkdir -p ~/.config
ln -s ~/.au.conf/gtk-3.0 ~/.config/





################################################################################

wget -O- https://telegram.org/dl/desktop/linux | sudo tar xJ -C /opt/

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
    --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90
echo deb http://repository.spotify.com stable non-free | \
    sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt update
$INSTALL spotify-client

unalias INSTALL
