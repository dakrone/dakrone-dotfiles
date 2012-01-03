A small collection of my configuration files and Emacs/Vim plugins

See the images directory for an example of what some of the stuff looks like.

# ZSH randomness:

A random prompt pet is chosen, `export PROMPT_PETS=("a" "b" "c" ...)` to
specify a list, or set PROMPT_PET explicitly to use that pet.

# Instructions for using the Emacs stuff (requires Emacs 24):

1. Move .emacs.d to ~/.emacs.d
2. Rename ~/.emacs.d/hinmanm to ~/.emacs.d/<youruserid>
3. find .emacs.d/elpa -name "*.el" | awk '{print "(byte-compile-file \"" $1 "\")";}' >> ~/runme.el
4. find .emacs.d/<youruserid> -name "*.el" | awk '{print "(byte-compile-file \"" $1 "\")";}' >> ~/runme.el
5. emacs -batch -l ~/runme.el -kill
6. ???
7. Profit!
