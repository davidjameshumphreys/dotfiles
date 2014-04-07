H=/home/david

#rsync --filter='- .git' --filter='- start.sh' -av . $H
for i in .ackrc .gitconfig .tmux.conf .zshrc; do
    cp $i ${H}/$i
done

cp -r .oh-my-zsh ${H}/.oh-my-zsh
cp -r .lein ${H}/.lein
cp -r .emacs.d ${H}/.emacs.d
