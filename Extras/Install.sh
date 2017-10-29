#!/bin/sh

# Default option values
USE_CASK=''
TARGET="$HOME/.emacs.d"
VERSION="0.0.2.20171028"
PACKAGE_LABEL="package"
PACKAGE_IN="((package-system :$PACKAGE_LABEL))"
PACKAGE_OUT="$PACKAGE_IN"
NO_HOME=''
NO_CUSTOM=''
WITH_ENV=''
DRY_RUN=''
VERBOSE=''

usage () {
    echo "Installs emacs init files in target directory"
    echo ""
    echo "sh Install.sh [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --target=DIRECTORY Directory in which to install the init subdirectory"
    echo "                     (default: $HOME/.emacs.d)."
    echo "  --package=TYPE     Emacs package system: package, cask, cask-homebrew."
    echo "  --cask             Use cask command to install packages in Cask file."
    echo "  --no-home          Do not install $HOME/.emacs.el."
    echo "  --no-custom        Do not copy emacs-custom.el from Extras, but create if missing."
    echo "  --with-env         Copy my-env.el from Extras."
    echo "  --dry-run          Display steps but do not execute them."
    echo "  --verbose          Display steps while execute them."
    echo "  --help             Show this usage message."
    echo "  --version          Version information."

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
        --cask)
            USE_CASK=`command -v cask`
            if [[ -z "$USE_CASK" ]]; then
              echo "ERROR: --cask specified but cask command cannot be found"
              exit 1
            fi
            ;;
        --no-home)
            NO_HOME='true'
            ;;
        --no-custom)
            NO_CUSTOM='true'
            ;;
        --with-env)
            WITH_ENV='true'
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

# Recursive copies
COPY=`command -v rsync`
if [[ -z "$COPY" ]]; then
  COPY='cp -R'
else
  COPY="$COPY -a"
fi

# Main Steps (This isn't DRY but it gives a dry run. Ha!)
if [[ -n "$DRY_RUN" || -n "$VERBOSE" ]]; then
    echo "[[ -d $TARGET/init ]] || mkdir -p $TARGET/init"
    echo "[[ -d $TARGET/site-lisp ]] || mkdir -p $TARGET/site-lisp"
    echo "[[ -d $TARGET/themes ]] || mkdir -p $TARGET/themes"
    [[ ./ -ef $TARGET/init ]] || echo "$COPY ./[a-z]* $TARGET/init"
    echo "$COPY Extras/site-lisp $TARGET"
    echo "$COPY Extras/themes $TARGET"
    [[ -z "$NO_CUSTOM" ]] && echo "cp Extras/emacs-custom.el $TARGET"
    [[ -n "$NO_CUSTOM" && ! -e $TARGET/emacs-custom.el ]] && echo "echo '' > $TARGET/emacs-custom.el"
    [[ -n "$WITH_ENV" ]] && echo "cp Extras/my-env.el $TARGET"
    echo "cp Extras/Cask $TARGET"
    if [[ -z "$NO_HOME" ]]; then
        if [[ "$PACKAGE_IN" = "$PACKAGE_OUT" ]]; then
            echo "cat Extras/home-dot-emacs.el | sed 's:TARGET:$TARGET:' > $HOME/.emacs.el"
        else
            echo "cat Extras/home-dot-emacs.el | sed 's/$PACKAGE_IN/$PACKAGE_OUT/' | sed 's:TARGET:$TARGET:' > $HOME/.emacs.el"
        fi
    fi
    [[ -n "$USE_CASK" ]] && echo "(cd $TARGET; $USE_CASK install $VERBOSE)"
fi

if [[ -z "$DRY_RUN" ]]; then
    [[ -d $TARGET/init ]] || mkdir -p $TARGET/init
    [[ -d $TARGET/site-lisp ]] || mkdir -p $TARGET/site-lisp
    [[ -d $TARGET/themes ]] || mkdir -p $TARGET/themes
    [[ ./ -ef $TARGET/init ]] || $COPY ./[a-z]* $TARGET/init
    $COPY Extras/site-lisp $TARGET
    $COPY Extras/themes $TARGET
    [[ -z "$NO_CUSTOM" ]] && cp Extras/emacs-custom.el $TARGET
    [[ -n "$NO_CUSTOM" && ! -e $TARGET/emacs-custom.el ]] && echo '' > $TARGET/emacs-custom.el
    [[ -n "$WITH_ENV" ]] && cp Extras/my-env.el $TARGET
    cp Extras/Cask $TARGET
    if [[ -z "$NO_HOME" ]]; then
        if [[ "$PACKAGE_IN" = "$PACKAGE_OUT" ]]; then
            cat Extras/home-dot-emacs.el | sed 's/TARGET/$TARGET/' > $HOME/.emacs.el
        else
            cat Extras/home-dot-emacs.el | sed "s/$PACKAGE_IN/$PACKAGE_OUT/" | sed "s:TARGET:$TARGET:" > $HOME/.emacs.el
        fi
    fi
    [[ -n "$USE_CASK" ]] && (cd $TARGET; $USE_CASK install $VERBOSE)
    echo "Emacs initialization installed in $TARGET (package $PACKAGE_LABEL, use cask? ${USE_CASK:-false})."
fi  

