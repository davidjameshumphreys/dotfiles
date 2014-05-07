H=/home/david

#rsync --filter='- .git' --filter='- start.sh' -av . $H
for i in .gitconfig .tmux.conf .bash_aliases; do
    ln -fs  "${PWD}/$i" "${H}/$i"
done

cp -r .lein/* ${H}/.lein
ln -fs "${PWD}/.emacs.d" "${H}/.emacs.d"
