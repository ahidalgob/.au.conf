check_package_installed(){
    if dpkg-query -l "$1" 2>1 > /dev/null; then
        return 0
    fi
    return 1
}
check_in_file() {
    if cat $1 | grep "$2" > /dev/null; then
        return 0
    fi
    return 1
}

install_packege_if_not_installed(){
    if check_package_installed "$1"; then
        return 0;
    fi
    if [ "$2" != "" ]; then
        sudo add-apt-repository "$2"
    fi
    sudo apt update
    sudo apt install "$1"
}


check_command() { hash $1 &> /dev/null; }

if [ ! -d ~/.au_conf ]; then
    echo "Please make sure .au_conf is in your home."
    exit 1
fi


if [ "$1" != "light" ] && [ "$1" != "complete" ]; then
    echo "please provide one argument: either 'complete' or 'light'"
fi


TYPE="$1"

if [ "$TYPE" == "complete" ]; then
    echo "complete"
fi

if [ ! -e $HOME/.vim-anywhere ]; then
    echo "missing"
fi

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


