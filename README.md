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

Much package installing will commence. If there are errors, just keep
running it until all the packages are installed.

3. Run `emacs`

## Notes

My emacs config uses [mu4e](http://www.djcbsoftware.nl/code/mu/mu4e.html) for
email, ERC for IRC, twittering-mode for twitter and all kinds of goodies for
clojure development. It requires manually calling the method `mail` in order to
start the mail functionality. For IRC I use
[ERC](http://www.emacswiki.org/emacs/ERC), which can be started with the
`start-irc` function.

All of the settings are in `~/.emacs.d/settings.org`, and are tangled and then
loaded from `settings.el` in the same directory. This (hopefully) makes reading
the configuration a bit easier.

I recommend you install [ag](https://github.com/ggreer/the_silver_searcher) for
searching also and [gpg](http://www.gnupg.org/) for crypto stuff to get the full
benefit.

### Emacs themes

I my custom Emacs theme called
[dakrone-theme](https://github.com/dakrone/dakrone-theme) for dark background,
and [leuven](https://github.com/fniessen/emacs-leuven-theme) for light-colored
backgrounds (working outside).

To swap between them, change the line at the beginning of
`~/.emacs.d/settings.org` that looks like:

```
;; Currently using dark-colored theme
;;(defvar my/background 'light)
(defvar my/background 'dark)
```

# Polipo + pdnsd caching proxy

Loosely based on:
- https://wiki.archlinux.org/index.php/Pdnsd
- http://nakkaya.com/2009/05/07/speeding-up-your-net-browsing-with-pdnsd-domain-name-caching-on-mac-os-x/

Steps:
- install polipo
- install pdnsd
- copy polipo.conf where it's needed, change the data directory and
  log file location
- copy pdnsd.conf where it's needed, create the directories needed
  (running pdnsd will complain about where they are if they don't
  already exist)
- set DNS to 127.0.0.1
- set HTTP proxy to 127.0.0.1, port 8118

# Privoxy

There's also some stuff for privoxy in here. Proxies all over the place!
