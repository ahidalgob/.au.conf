

## Function defintions

check_package_installed(){
    if dpkg-query -l "$1" > /dev/null &> /dev/null; then
        return 0
    fi
    return 1
}

install_package_if_not_installed(){
    if check_package_installed "$1"; then
        return 0;
    fi
    if [ "$2" != "" ]; then
        yes | sudo add-apt-repository "$2"
        yes | sudo apt update
    fi
    yes | sudo apt install "$1"
}

check_in_file() {
    if cat $1 | grep "$2" > /dev/null; then
        return 0
    fi
    return 1
}

check_command() { hash $1 &> /dev/null; }

## End of functions

if [ "$1" != "light" ] && [ "$1" != "complete" ]; then
    echo "The first argument should be either 'complete' or 'light'"
    exit 1
fi

if [ "$2" != "ubuntu" ] && [ "$2" != "debian" ]; then
    echo "The second argument should be either 'ubuntu' or 'debian'"
    exit 1
fi

TYPE=$1
DISTRO=$2

if [ ! -e $HOME/.au_conf ]; then
    echo "Please make sure .au_conf is in your home."
    exit 1
fi

cd $HOME/.au_conf


# Prompt questions at start so later it can run up to the end


HASKELL_REPO=0
if [ "$TYPE" == 'complete' ]; then
    if ! check_command ghci; then
        echo "Do you want to install haskell-platform from the"
        echo "distro's repo? It might not be the last version."
        read -p "Do you want to use distro's repo? (Y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            HASKELL_REPO=1
        fi
    fi
fi





## vim
install_package_if_not_installed vim
install_package_if_not_installed vim-gtk
install_package_if_not_installed curl
install_package_if_not_installed xclip

if [ $TYPE == "complete" ]; then
    if [ ! -e $HOME/.vim-anywhere ]; then
        install_package_if_not_installed curl
        curl -fsSL https://raw.github.com/cknadler/vim-anywhere/master/install | bash
    fi
fi
ln --symbolic --relative --force ./vim/.vimrc $HOME/.vimrc


## guake
install_package_if_not_installed guake
# TO DO: Configure


install_package_if_not_installed git
ln --symbolic --relative --force ./git/.gitconfig $HOME/.gitconfig


# vlc
if [ $TYPE == "complete" ]; then
    install_package_if_not_installed vlc
fi

# mpv
if [ $DISTRO == "ubuntu" ]; then
    check_package_installed mpv "ppa:mc3man/mpv-tests"
fi
if [ $DISTRO == "debian" ]; then
    echo "Please install mpv and complete the configuration script"
fi

if [ $TYPE == "complete" ]; then
    install_package_if_not_installed gparted
fi

if check_command mousepad; then
    yes | sudo apt remove mousepad
fi

if [ $TYPE == "complete" ]; then
    install_package_if_not_installed python-pip
fi

if [ $TYPE == "complete" ]; then
    install_package_if_not_installed virtualenv
fi

if [ $HASKELL_REPO == 1 ]; then
    install_package_if_not_installed haskell-platform
fi

# check if light
if [ $TYPE == "complete" ]; then
    install_package_if_not_installed banshee
fi

if [ $TYPE == "complete" ]; then
    install_package_if_not_installed youtube-dl
fi

# check if light
if [ $TYPE == "complete" ] && [ $DISTRO == "ubuntu" ]; then
    if ! check_command wine; then
        yes | sudo add-apt-repository ppa:ubuntu-wine/ppa
        sudo apt update
        yes | sudo apt install wine1.8 winetricks

        echo "wine might require some configuration, enough to launch it"
        echo "a couple of times for it to configure"
    fi
fi


install_package_if_not_installed g++

install_package_if_not_installed default-jre
install_package_if_not_installed icedtea-plugin




if [ $TYPE == "complete" ]; then
    if ! check_package_installed texlive-latex-base; then
        yes | sudo apt install texlive-latex-base texlive-latex-extra texlive-latex-recommended texlive-fonts-recommended
        yes | sudo apt install texstudio
        yes | apt install texlive-lang-spanish
    fi
fi











## Download and install chrome .deb
## Download and install telegram


## spotify
## megatools


# Download and install dropbox .deb (o sudo apt install nautilus-dropbox)
# May need to change (or add) custom command on
# session and startup>application autostart:
# /bin/bash -c "sleep 15 && dropbox stop && env DBUS_SESSION_BUS_ADDRESS="" dropbox start -i"

# -- thunar dropbox extension
# sudo add-apt-repository ppa:xubuntu-dev/extras
# sudo apt update
# sudo apt install thunar-dropbox-plugin






if ! check_in_file $HOME/.bashrc ". ~/.au_conf/bash/bashrcConfig"; then
    echo "" >> $HOME/.bashrc
    echo "if [ -f ~/.au_conf/bash/bashrcConfig ]; then" >> $HOME/.bashrc
    echo "    . ~/.au_conf/bash/bashrcConfig" >> $HOME/.bashrc
    echo "fi" >> $HOME/.bashrc
fi






########## XFCE
# appearance:
# Style: Adwaita-xfce dark
# icons: numix circle
# Window manager:
# Style theme: Numix

# numix circle icons
install_package_if_not_installed "numix-icon-theme-circle" "ppa:numix/ppa"

install_package_if_not_installed xfce4-whiskermenu-plugin

ln --symbolic --relative --force ./xfce/.themes $HOME/.themes
ln --symbolic --relative --force ./.config/xfce4 $HOME/.config/xfce4

exit 0


