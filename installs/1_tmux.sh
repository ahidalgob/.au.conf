sudo apt install --yes automake
sudo apt install --yes build-essential
sudo apt install --yes pkg-config
sudo apt install --yes libevent-dev
sudo apt install --yes libncurses5-dev

rm -fr /tmp/tmux

git clone https://github.com/tmux/tmux /tmp/tmux

cd /tmp/tmux

git checkout 2.7

sh autogen.sh
./configure && make
sudo make install
cd -
rm -fr /tmp/tmux

ln --backup --symbolic ~/.au_conf/tmux/.tmux.conf ~/.tmux.conf

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
# you have to run prefix + I then (I think)
