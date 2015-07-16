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
	ln -s -v -f $(DOTFILES_DIR)/.zsh* ~/
	ln -s -v -f $(DOTFILES_DIR)/.git?* ~/
	ln -s -v -f $(DOTFILES_DIR)/.tmux* ~/
	ln -s -v -f $(DOTFILES_DIR)/.emacs.d ~/.emacs.d
