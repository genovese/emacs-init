#!/bin/sh

# Default option values
USE_CASK=''
TARGET=".."
VERSION="0.0.3.20171029"
PACKAGE_LABEL="package"
PACKAGE_IN="((package-system :$PACKAGE_LABEL))"
PACKAGE_OUT="$PACKAGE_IN"
NO_INIT=''
NO_EXTRAS=''
WITH_CUSTOM=''
WITH_ENV=''
SAFE=''
DRY_RUN=''
VERBOSE=''

usage () {
    echo "Installs emacs init files in target directory"
    echo ""
    echo "sh Install.sh [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --target=DIR    Use DIR as directory in which to install init subdirectory"
    echo "                  (default: ..)."
    echo "  --package=TYPE  Emacs package system: package, cask, cask-homebrew."
    echo "                  With --cask, attempts to autodetect if not supplied."
    echo "  --cask          Use cask command to install packages in Cask file."
    echo "  --safe          Do not overwrite existing files where possible."
    echo "  --with-custom   Copy emacs-custom.el from Extras."
    echo "                  The file is always created if it does not exist."
    echo "  --with-env      Copy my-env.el from Extras."
    echo "  --no-init       Do not install $HOME/.emacs.el."
    echo "  --no-extras     Do not install extra files (Cask, themes, site-lisp)."
    echo "  --minimal       Install base code only, no extras or packages."
    echo "                  Sets --no-init and --no-extras; and overrides --with-custom,"
    echo "                  --with-env, and --cask."
    echo "  --full          Equivalent to --with-custom --cask --verbose."
    echo "  --dry-run       Display steps but do not execute them."
    echo "  --verbose       Display steps while execute them."
    echo "  --help          Show this usage message."
    echo "  --version       Version information."

}    

check_cask () {
    USE_CASK=`command -v cask`
    if [[ -z "$USE_CASK" ]]; then
      echo "ERROR: --cask specified but cask command cannot be found"
      exit 1
    fi
}    

# Process Options
while [ "$1" != "" ]; do
    case "$1" in
        -h | --help)
            usage
            exit
            ;;
        -v | --version)
            echo "$VERSION"
            exit
            ;;
        --safe)
            SAFE='-n'
            ;;
        --with-custom)
            WITH_CUSTOM='true'
            ;;
        --with-env)
            WITH_ENV='true'
            ;;
        --no-home)
            NO_INIT='true'
            ;;
        --no-extras)
            NO_EXTRAS='true'
            ;;
        --cask)
            check_cask
            ;;
        --minimal)
            NO_INIT='true'
            WITH_CUSTOM=''
            WITH_ENV=''
            USE_CASE=''
            ;;
        --full)
            WITH_CUSTOM='true'
            VERBOSE='--verbose'
            check_cask
            ;;
        --dry-run)
            DRY_RUN='true'
            ;;
        --verbose)
            VERBOSE='--verbose'
            ;;
        *)
            PARAM=`echo $1 | sed 's/=.*$//'`
            VALUE=`echo $1 | sed 's/^[^=]*=//'`
            case $PARAM in
                --target)
                    TARGET=$VALUE
                    ;;
                --package)
                    case $VALUE in
                        package|cask|cask-homebrew)
                            PACKAGE_LABEL="$VALUE"
                            PACKAGE_OUT="((package-system :$VALUE))"
                            ;;
                        *)
                            echo "ERROR: --package value should be package, cask, or cask-homebrew"
                            echo ""
                            usage
                            exit
                            ;;
                    esac
                    ;;
                *)
                    echo "ERROR: unknown parameter \"$PARAM\""
                    echo ""
                    usage
                    exit 1
                    ;;
            esac
    esac
    shift
done

# Need target as absolute path for the home init file
if [[ -z "$NO_INIT" ]]; then
  ABS_TARGET=`cd "$TARGET"; pwd`
fi

# Recursive copies
COPY=`command -v rsync`
if [[ -z "$COPY" ]]; then
  COPY='cp -R $SAFE'
else
  COPY="$COPY -a ${SAFE:+ --ignore-existing}"
fi

# Main Steps (This is most definitely *not* DRY, but it does give a dry run. Ha!)
if [[ -n "$DRY_RUN" || -n "$VERBOSE" ]]; then
    [[ -d $TARGET/init ]]      || echo "mkdir -p $TARGET/init"
    [[ -d $TARGET/site-lisp ]] || echo "mkdir -p $TARGET/site-lisp"
    [[ -d $TARGET/themes ]]    || echo "mkdir -p $TARGET/themes"
    [[ ./ -ef $TARGET/init ]]  || echo "$COPY ./[a-z]* $TARGET/init"
    [[ -n "$NO_EXTRAS" ]]      || echo "$COPY Extras/site-lisp $TARGET"
    [[ -n "$NO_EXTRAS" ]]      || echo "$COPY Extras/themes $TARGET"
    [[ -n "$NO_EXTRAS" ]]      || echo "cp $SAFE Extras/Cask $TARGET"
    [[ -n "$WITH_CUSTOM" ]] && echo "cp $SAFE Extras/emacs-custom.el $TARGET"
    [[ -z "$WITH_CUSTOM" && ! -e $TARGET/emacs-custom.el ]] && echo "echo '' > $TARGET/emacs-custom.el"
    [[ -n "$WITH_ENV" ]] && echo "cp $SAFE Extras/my-env.el $TARGET"
    if [[ -z "$NO_INIT" ]]; then
        if [[ "$PACKAGE_IN" = "$PACKAGE_OUT" ]]; then
            echo "cat Extras/home-dot-emacs.el | sed 's:TARGET:$ABS_TARGET:' > $HOME/.emacs.el"
        else
            echo "cat Extras/home-dot-emacs.el | sed 's/$PACKAGE_IN/$PACKAGE_OUT/;s:TARGET:$ABS_TARGET:' > $HOME/.emacs.el"
        fi
    fi
    [[ -n "$USE_CASK" ]] && echo "(cd $TARGET; $USE_CASK install $VERBOSE)"
fi

if [[ -z "$DRY_RUN" ]]; then
    [[ -d $TARGET/init ]]      || mkdir -p $TARGET/init
    [[ -d $TARGET/site-lisp ]] || mkdir -p $TARGET/site-lisp
    [[ -d $TARGET/themes ]]    || mkdir -p $TARGET/themes
    [[ ./ -ef $TARGET/init ]]  || $COPY ./[a-z]* $TARGET/init
    [[ -n "$NO_EXTRAS" ]]      || $COPY Extras/site-lisp $TARGET
    [[ -n "$NO_EXTRAS" ]]      || $COPY Extras/themes $TARGET
    [[ -n "$NO_EXTRAS" ]]      || cp $SAFE Extras/Cask $TARGET
    [[ -n "$WITH_CUSTOM" ]] && cp $SAFE Extras/emacs-custom.el $TARGET
    [[ -z "$WITH_CUSTOM" && ! -e $TARGET/emacs-custom.el ]] && echo '' > $TARGET/emacs-custom.el
    [[ -n "$WITH_ENV" ]] && cp $SAFE Extras/my-env.el $TARGET
    if [[ -z "$NO_INIT" ]]; then
        if [[ "$PACKAGE_IN" = "$PACKAGE_OUT" ]]; then
            cat Extras/home-dot-emacs.el | sed "s/TARGET/$ABS_TARGET/" > $HOME/.emacs.el
        else
            cat Extras/home-dot-emacs.el | sed "s/$PACKAGE_IN/$PACKAGE_OUT/;s:TARGET:$ABS_TARGET:" > $HOME/.emacs.el
        fi
    fi
    [[ -n "$USE_CASK" ]] && (cd $TARGET; $USE_CASK install $VERBOSE)
    echo "Emacs initialization installed in $TARGET (package $PACKAGE_LABEL, use cask? ${USE_CASK:-false})."
    echo "Next step: Edit $TARGET/init/data/preferences.el to set your individual preferences."
fi  

