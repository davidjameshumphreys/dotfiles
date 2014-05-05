H=/home/david

#rsync --filter='- .git' --filter='- start.sh' -av . $H
for i in .ackrc .gitconfig .tmux.conf .bash_aliases; do
    ln -s $i ${H}/$i
done

cp -r .lein/* ${H}/.lein
ln -s .emacs.d ${H}/.emacs.d
