H=/home/david

for i in .gitconfig .tmux.conf .bash_aliases .zshrc .oh-my-zsh .emacs.d .lein/profiles.clj .lein/init.clj .ssh/config; do
    ln -fs  "${PWD}/$i" "${H}/$i"
done

sudo apt-get -y install cpanminus
sudo cpanm git://github.com/creaktive/rainbarf.git
sudo apt-get -y install python-pip
sudo apt-get -y install python-virtualenv
sudo apt-get -y install virtualenvwrapper
sudo apt-get -y install golang
