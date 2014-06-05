H=/home/david

#rsync --filter='- .git' --filter='- start.sh' -av . $H
for i in .gitconfig .tmux.conf .bash_aliases .zshrc .oh-my-zsh .emacs.d; do
    ln -fs  "${PWD}/$i" "${H}/$i"
done

cp -r .lein/* ${H}/.lein
cp .ssh/config ${H}/.ssh/config
#ln -fs "${PWD}/.emacs.d" "${H}/.emacs.d"

sudo apt-get -y install cpanminus
sudo cpanm git://github.com/creaktive/rainbarf.git
