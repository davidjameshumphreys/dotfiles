H=~david

for i in .gitconfig .tmux.conf .bash_aliases .zshrc .oh-my-zsh .emacs.d .lein/profiles.clj .lein/init.clj .ssh/config zaw; do
    ln -fs  "${PWD}/$i" "${H}/$i"
done

if [ "$1" = 'install' ] ; then
    dpkg -l cpanminus 2>&1         | grep -sq '^ii' || sudo apt-get -y -qq install cpanminus
    dpkg -l python-pip 2>&1        | grep -sq '^ii' || sudo apt-get -y -qq install python-pip
    dpkg -l python-virtualenv 2>&1 | grep -sq '^ii' || sudo apt-get -y -qq install python-virtualenv
    dpkg -l virtualenvwrapper 2>&1 | grep -sq '^ii' || sudo apt-get -y -qq install virtualenvwrapper
    dpkg -l golang 2>&1            | grep -sq '^ii' || sudo apt-get -y -qq install golang

    which rainbarf 2>&1 > /dev/null || sudo cpanm git://github.com/creaktive/rainbarf.git
else
    echo "Skipping install"
fi
