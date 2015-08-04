
#!/bin/bash

#####################################################
## Warning, this file was automatically generated! ##
## Change bootstrap.org if you need to update it.  ##
#####################################################

# Bash strict mode
set -euo pipefail

function setup_osx() {
    echo "[-] Setting up OSX"
    echo "[-] Done setting up OSX"
}

function setup_linux() {
    # returns a string like "Fedora" or "Ubuntu"
    DISTRO=`lsb_release -i | cut -d: -f 2 | tr -d '[:space:]'`

    case $DISTRO in
        Fedora)
            setup_fedora
            setup_linux_generic
            ;;
        Ubuntu)
            setup_ubuntu
            setup_linux_generic
            ;;
        Debian)
            # We can *try* to set up Debian like Ubuntu, but it's not tested
            setup_ubuntu
            setup_linux_generic
            ;;
        *)
            echo "Sorry, I haven't implemented anything for this OS ($DISTRO) yet"
            exit 1
    esac
}

# Common things for all Linux distributions
function setup_linux_generic() {
    echo "[-] Setting up generic Linux things"

    echo -n "[!] Don't forget to update the max_file_descriptors, currently: "
    ulimit -n

    echo "[-] Done setting up generic Linux"
}

function setup_fedora() {
    PACKAGES="emacs git tmux zsh htop keychain the_silver_searcher python-pip cmake"

    echo "[-] Setting up Fedora"

    echo "Enabling sshd..."
    sudo systemctl enable sshd.service
    sudo systemctl start sshd.service

    # Install the minimal necessary software I need
    echo "Installing software..."
    sudo dnf group install -y "Development Tools"
    sudo dnf group install -y "C Development Tools and Libraries"
    sudo dnf install -y $PACKAGES

    echo "[-] Done setting up Fedora"
}

function setup_ubuntu() {
    PACKAGES="git tmux zsh htop keychain silversearcher-ag"

    echo "[-] Setting up Ubuntu"

    echo "Installing software..."
    sudo apt-get update
    sudo apt-get install -y --force-yes build-essential
    sudo apt-get install -y --force-yes $PACKAGES

    echo "[-] Done setting up Ubuntu"
}

function setup_generic() {
    echo "[-] Setting up OS-agnostic things..."

    mkdir -p ~/.zsh
    if [ ! -d ~/.zsh/zsh-completions ]; then
        echo "Installing zsh completions"
        cd ~/.zsh && git clone https://github.com/zsh-users/zsh-completions.git
    fi

    if [ ! -d ~/.zsh/zsh-syntax-highlighting ]; then
        echo "Installing zsh syntax highlighting"
        cd ~/.zsh && git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
    fi

    echo "[-] Done Setting up OS-agnostic things"
}

function main() {
    echo "[+] Starting setup"

    OS=`uname`

    case $OS in
        Darwin)
            setup_osx
            ;;
        Linux)
            setup_linux
            ;;
        *)
            echo "Operating system ($OS) not supported!"
            exit 1
    esac

    setup_generic

    echo "[+] Finished setup"
}

### Start of actual script

main
