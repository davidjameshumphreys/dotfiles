git submodule init
git submodule update

cp .ackrc ${HOME}/.ackrc
cp .gitconfig ${HOME}/.gitconfig
cp -r .lein ${HOME}/.lein
ln -s emacs-live ${HOME}/.emacs.d
