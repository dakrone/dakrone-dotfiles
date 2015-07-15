# Directory where this Makefile exists (the dotfiles directory)
DOTFILES_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

all: *.org
	bin/tangle bootstrap.org
	bin/tangle zsh.org
	bin/tangle git.org
	bin/tangle tmux.org
	bin/tangle emacs.org

install: all
	ln -s -v -i $(DOTFILES_DIR)/.zsh* ~/
	ln -s -v -i $(DOTFILES_DIR)/.git?* ~/
	ln -s -v -i $(DOTFILES_DIR)/.tmux* ~/
	ln -s -v -i $(DOTFILES_DIR)/.emacs.d ~/.emacs.d

force-install: all
	ln -s -v $(DOTFILES_DIR)/.zsh* ~/
	ln -s -v $(DOTFILES_DIR)/.git?* ~/
	ln -s -v $(DOTFILES_DIR)/.tmux* ~/
	ln -s -v $(DOTFILES_DIR)/.emacs.d ~/.emacs.d
