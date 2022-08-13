a.out:copy git

copy:
	rsync -auv ~/src/github.com/minorugh/dotfiles/.emacs.d/ ~/src/github.com/minorugh/emacs.d/

git:
	git add . && git diff --cached --exit-code --quiet && echo "\nnothing to commit, working tree clean!"|| \
	git commit -a -m "Updated: `date +'%Y-%m-%d %H:%M:%S'`" && \
	git push origin main
