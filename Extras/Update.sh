#!/bin/sh

# Default option values
TARGET="$HOME/.emacs.d"
VERSION="0.0.2.20171028"
NO_CUSTOM=''
NO_ENV=''
WITH_THEMES=''
WITH_SITE_LISP=''
DRY_RUN=''
VERBOSE=''

usage () {
    echo "Gets copies of extra files from active emacs.d directory"
    echo "This is only useful if the repository is kept separate from the active directory"
    echo ""
    echo "sh Update.sh [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --target=DIRECTORY Directory from which to copy files"
    echo "                     (default: $HOME/.emacs.d)."
    echo "  --no-custom        Do not update emacs-custom.el from Extras."
    echo "  --no-env           Do not update my-env.el from Extras."
    echo "  --with-themes      Update from the active themes directory."
    echo "  --with-site-lisp   Update from the active site-lisp directory."
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
        --no-custom)
            NO_CUSTOM='true'
            ;;
        --no-env)
            NO_ENV='true'
            ;;
        --with-themes)
            WITH_THEMES='true'
            ;;
        --with-site-lisp)
            WITH_SITE_LISP='true'
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
    echo "cp $TARGET/Cask Extras"
    [[ -z "$NO_CUSTOM" ]] && echo "cp $TARGET/emacs-custom.el Extras"
    [[ -z "$NO_ENV" ]] && echo "cp $TARGET/my-env.el Extras"
    [[ -n "$WITH_THEMES" ]] && echo "$COPY $TARGET/themes Extras"
    [[ -n "$WITH_SITE_LISP" ]] && echo "$COPY $TARGET/site-lisp Extras"
fi

if [[ -z "$DRY_RUN" ]]; then
    cp $TARGET/Cask Extras
    [[ -z "$NO_CUSTOM" ]] && cp $TARGET/emacs-custom.el Extras
    [[ -z "$NO_ENV" ]] && cp $TARGET/my-env.el Extras
    [[ -n "$WITH_THEMES" ]] && $COPY $TARGET/themes Extras
    [[ -n "$WITH_SITE_LISP" ]] && $COPY $TARGET/site-lisp Extras
    echo "Updated extras from active emacs directory $TARGET."
fi  

