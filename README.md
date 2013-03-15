A small collection of my configuration files and Emacs/Vim plugins

Tango terminal colors are recommended with a dark background. Unicode
is used, so hopefully your terminal supports it.

See the images directory for an example of what some of the stuff looks like.

# Bootstrap the config

```
curl -s https://raw.github.com/dakrone/dakrone-dotfiles/master/bootstrap | zsh
```

# ZSH randomness:

A random prompt pet is chosen, `export PROMPT_PETS=("a" "b" "c" ...)` to
specify a list, or set PROMPT_PET explicitly to use that pet.

# Instructions for using the Emacs stuff (requires Emacs 24):

1. Move the `.emacs.d` directory to `~/.emacs.d`
2. Run:

```
emacs -q -batch -l ~/.emacs.d/setup.el -kill
```

Much package installing will commence.

3. Run `emacs`
