
sudo add-apt-repository ppa:numix/ppa
sudo apt update
sudo apt install numix-icon-theme-circle

if [ ! -d ~/.config ]; then
    mkdir ~/.config
fi

ln --symbolic --backup ~/.au_conf/.themes ~/

ln --symbolic --backup ~/.au_conf/gtk-3.0 ~/.config

ln --symbolic --backup ~/.au_conf/.gtkrc-2.0 ~/

