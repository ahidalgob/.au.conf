#!/bin/bash

# this was tested installing crouton in the following way
#$ sudo crouton -r bionic -t core
# then inside the chroot installed
#$ xserver-xorg-core xserver-xorg-video-dummy xserver-xorg-video-intel
# finally updated the chroot with
#$ sudo crouton -n bionic -u -t extension,keyboard,core,cli-extra,x11,xorg,chrome
# Wasn't able to install the chroot with all the targets directly. TODO test
# upgrading just after the chroot creation, without installing the xorg packages.


alias INSTALL='sudo apt install -y'

INSTALL xserver-xorg-core xserver-xorg-video-dummy xserver-xorg-video-intel
# when installed 18.04, these two were needed by a xmonad dependency
INSTALL libxrandr-dev libxss-dev

INSTALL xmonad cabal-install
cabal update

cabal install --force-reinstalls xmonad xmonad-contrib

INSTALL stalonetray dzen2 conky feh xdotool compton xclip curl git vim tmux \
    gcc build-essential python cmake python-dev python3-dev suckless-tools feh \
    rxvt-unicode vim-gui-common alsa-base pulseaudio thunar nm-tray
INSTALL software-properties-common python-software-properties

INSTALL xserver-xorg-input-synaptics
# this would copy a template config file to the correct place. Might want to
# modify it and save it.

#mkdir -p /etc/X11/xorg.conf.d
#sudo cp /usr/share/X11/xorg.conf.d/70-synaptics.conf /etc/X11/xorg.conf.d


################################################################################

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

ln -s ~/.au.conf/xmonad/.stalonetrayrc ~/
ln -s ~/.au.conf/xmonad/.xmonad ~/
ln -s ~/.au.conf/.icons ~/
ln -s ~/.au.conf/X/.xinitrc ~/
ln -s ~/.au.conf/X/.Xresources ~/
ln -s ~/.au.conf/urxvt/.urxvt ~/

ln -s ~/.au.conf/.themes ~/
ln -s ~/.au.conf/.gtkrc-2.0 ~/

mkdir -p ~/.config
ln -s ~/.au.conf/gtk-3.0 ~/.config/


check_in_file() {
    if cat $1 | grep "$2" > /dev/null; then
        return 0
    fi
    return 1
}
if ! check_in_file $HOME/.bashrc ". ~/.au_conf/bash/bashrcConfig"; then
    echo "" >> $HOME/.bashrc
    echo "if [ -f ~/.au.conf/bash/bashrcConfig ]; then" >> $HOME/.bashrc
    echo "    . ~/.au.conf/bash/bashrcConfig" >> $HOME/.bashrc
    echo "fi" >> $HOME/.bashrc
fi


################################################################################

wget -O- https://telegram.org/dl/desktop/linux | sudo tar xJ -C /opt/
sudo ln -s /opt/Telegram/Telegram /usr/bin/

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
    --recv-keys 931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90
echo deb http://repository.spotify.com stable non-free | \
    sudo tee /etc/apt/sources.list.d/spotify.list
sudo apt update
INSTALL spotify-client

unalias INSTALL

