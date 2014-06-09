H=/home/david

for i in .gitconfig .tmux.conf .bash_aliases .zshrc .oh-my-zsh .emacs.d .lein/profiles.clj .lein/init.clj .ssh/config; do
    ln -fs  "${PWD}/$i" "${H}/$i"
done

cpanm || sudo apt-get -y install cpanminus
rainbarf || sudo cpanm git://github.com/creaktive/rainbarf.git
