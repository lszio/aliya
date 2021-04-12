#!/usr/bin/env bash

ALIYA=${ALIYA:-~/Aliya}
export ALIYA=$ALIYA

function check() {
    if command-exists lsb_release ;then
        RELEASE_NAME=`lsb_release -a | grep ID | awk '{print $3; exit}'`
    else
        RELEASE_NAME=`cat /etc/issue | awk '{print $1; exit}'`
        [ $RELEASE_NAME ] || {
            RELEASE_NAME="Unknown Release"
        }
    fi

    echo "Release name: $RELEASE_NAME"

    command-exists pacman && {
        PACKAGE_MANAGER="pacman"
    }

    command-exists apt && {
        PACKAGE_MANAGER="apt"
    }

    command-exists yum && {
        PACKAGE_MANAGER="yum"
    }

    echo "Package manager: $PACKAGE_MANAGER"

    [ $http_proxy ] || [ $HTTP_PROXY ] && {
        USE_PROXY=true
        echo "Using proxy $http_proxy$HTTP_PROXY"
    }

    RC_FILE="$HOME/.bashrc"
    [ -f "$HOME/.zshrc" ] && {
        RC_FILE="$HOME/.zshrc"
    }
}

function main() {
    echo "Hello $USER!!!"
    check
    [ $1 ] && [ $1 == "uninstall" ] && {
        uninstall
        exit
    }
    prepare
    install
    finish
}

function prepare() {
    [ $PACKAGE_MANAGER == "pacman" ] && {
        echo "Using pacman"
        # MIRROR=`cat /etc/pacman.d/mirrorlist | grep -e "^Server" | awk '{print $3}'`
        # echo "$MIRROR"
        # command_exists reflector && {
        #     reflector --verbose --country China --sort rate --save /etc/pacman.d/mirrorlist
        # }
        sudo pacman -S git wget base-devel --needed --noconfirm
    }

    [ $PACKAGE_MANAGER == "apt" ] && {
        echo "Using apt"
        sudo apt-get install git build-essential libcurl4-openssl-dev automake zlib1g-dev -y
    }

    [ $PACKAGE_MANAGER == "yum" ] && {
        echo "Using yum"
        sudo yum install git automake libcurl-devel zlib-devel -y
    }
}

function install() {
    [ ! -d "$ALIYA" ] && {
        echo "Cloning aliya..."
        git clone https://github.com/Liszt21/Aliya $ALIYA
    }

    ROSWELL_HOME="$ALIYA/app/roswell"

    [ -d $ROSWELL_HOME ] && {
        echo "Roswell is already installed..."
        return
    }

    echo "Installing roswell..."
    [ ! -d "$ALIYA/var/cache/roswell" ] && {
        echo "Cloning roswell..."
        git clone -b release https://github.com/roswell/roswell.git $ALIYA/var/cache/roswell
    }
    cd $ALIYA/var/cache/roswell
    sh bootstrap
    ./configure --prefix=$ROSWELL_HOME
    make
    make install
    PATH=$ROSWELL_HOME/bin:$PATH ros setup
    cd $OLDPWD
}

function finish() {
    text-in-file "etc/entry" "$RC_FILE" || {
        echo "Write entry to $RC_FILE"
        echo "source $ALIYA/etc/entry" >> "$RC_FILE"
    }
}

function uninstall() {
    [ -d "$ALIYA" ] && {
        echo "Deleting $ALIYA..."
        rm -rf "$ALIYA"
    }

    text-in-file "etc/entry" "$RC_FILE" && {
        echo "Removing entry from $RC_FILE"
        sed -i "/entry/d" "$RC_FILE"
    }
}

function command-exists() {
	command -v "$@" >/dev/null 2>&1
}

function text-in-file() {
    # $1 text
    # $2 file
    [ $1 ] && [ $2 ] && {
        [ ! -e $2 ] && {
            echo "File $2 not exist..."
            exit
        }
        grep "$1" < "$2" >/dev/null 2>&1
    }
}

function ln-and-backup() {
    [ -e $2 ] && {
        echo "backup $2"
        mv $2 "$2.bak"
    }
    ln -s $1 $2
}

function rm-if-exist() {
    [ -e $1 ] && {
        rm -rf $1
    }
}

main $@
