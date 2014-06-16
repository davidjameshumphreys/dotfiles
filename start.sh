H=/home/david

for i in .gitconfig .tmux.conf .bash_aliases .zshrc .oh-my-zsh .emacs.d .lein/profiles.clj .lein/init.clj .ssh/config zaw; do
    ln -fs  "${PWD}/$i" "${H}/$i"
done

cpanm 2> /dev/null || sudo apt-get -y install cpanminus
rainbarf 2> /dev/null || sudo cpanm git://github.com/creaktive/rainbarf.git
sudo apt-get -y -qq install python-pip
sudo apt-get -y -qq install python-virtualenv
sudo apt-get -y -qq install virtualenvwrapper
sudo apt-get -y -qq install golang
