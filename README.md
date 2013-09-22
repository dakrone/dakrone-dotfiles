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

Put important commands in ~/.important_commands and they'll always be
in your backwards search history.

# Instructions for using the Emacs stuff (requires Emacs 24):

1. Move the `.emacs.d` directory to `~/.emacs.d`
2. Run:

```
emacs -q -batch -l ~/.emacs.d/setup.el -kill
```

Much package installing will commence.

3. Run `emacs`

## Notes

My emacs config uses
[mu4e](http://www.djcbsoftware.nl/code/mu/mu4e.html) for email, ERC
for IRC, twittering-mode for twitter and all kinds of goodies for
clojure development.

All files in ~/.emacs.d/init.d are loaded in order and errors are
displayed when loading. I've tried to make everything as portable as
possible, so if you use this and have loading errors other than mail
stuff, which is purposefully non-portable (unless you're trying to
send email as me?!), let me know and I'll try to fix it.

I recommend you install [ack](http://betterthangrep.com/) and
[ag](https://github.com/ggreer/the_silver_searcher) for searching
also and [gpg](http://www.gnupg.org/) for crypto stuff to get the
full benefit.

### Emacs themes

I used a custom Emacs theme called 'color-theme-dakrone' for dark
background, and tsdh-light for light-colored backgrounds (working
outside).

To swap between them, change the line at the end of
`~/.emacs.d/init.d/95_theme.el` to whichever you'd like to use (the
dark probably looks better, but sucks for working outdoors):

```
;; Currently using light-colored theme
;;(dakrone-dark)
(dakrone-light)
```

You also need to change which mode-line is being set up, in
`~/.emacs.d/init.d/96_modeline.el`:

```
;;(modeline-setup-face-light)
(modeline-setup-face-dark)
```

(you can call these functions interactively also)

# Polipo + pdnsd caching proxy

Loosely based on:
https://wiki.archlinux.org/index.php/Pdnsd
http://nakkaya.com/2009/05/07/speeding-up-your-net-browsing-with-pdnsd-domain-name-caching-on-mac-os-x/

- install polipo
- install pdnsd
- copy polipo.conf where it's needed, change the data directory and
  log file location
- copy pdnsd.conf where it's needed, create the directories needed
  (running pdnsd will complain about where they are if they don't
  already exist)
- set DNS to 127.0.0.1
- set HTTP proxy to 127.0.0.1, port 8118
