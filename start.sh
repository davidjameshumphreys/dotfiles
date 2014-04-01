H=/home/david
for i in .ackrc .gitconfig .tmux.conf; do
    cp $i ${H}/$i
done

cp -r .lein ${H}/.lein
cp -r .emacs.d ${H}/.emacs.d
