#!/bin/sh

# Default option values
use_cask=''
target=".."
version="0.0.3.20171029"
package_label="package"
package_in="((package-system :$package_label))"
package_out="$package_in"
no_init=''
no_extras=''
with_custom=''
with_env=''
safe=''
dry_run=''
verbose=''

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
    use_cask=`command -v cask`
    if [[ -z "$use_cask" ]]; then
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
            echo "$version"
            exit
            ;;
        --safe)
            safe='-n'
            ;;
        --with-custom)
            with_custom='true'
            ;;
        --with-env)
            with_env='true'
            ;;
        --no-home)
            no_init='true'
            ;;
        --no-extras)
            no_extras='true'
            ;;
        --cask)
            check_cask
            ;;
        --minimal)
            no_init='true'
            with_custom=''
            with_env=''
            use_case=''
            ;;
        --full)
            with_custom='true'
            verbose='--verbose'
            check_cask
            ;;
        --dry-run)
            dry_run='true'
            ;;
        --verbose)
            verbose='--verbose'
            ;;
        *)
            param=`echo $1 | sed 's/=.*$//'`
            value=`echo $1 | sed 's/^[^=]*=//'`
            case $param in
                --target)
                    target=$value
                    ;;
                --package)
                    case $value in
                        package|cask|cask-homebrew)
                            package_label="$value"
                            package_out="((package-system :$value))"
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
                    echo "ERROR: unknown parameter \"$param\""
                    echo ""
                    usage
                    exit 1
                    ;;
            esac
    esac
    shift
done

# Need target as absolute path for the home init file
if [[ -z "$no_init" ]]; then
  abs_target=`cd "$target"; pwd`
fi

# Recursive copies
copy=`command -v rsync`
if [[ -z "$copy" ]]; then
  copy='cp -R $safe'
else
  copy="$copy -a ${safe:+ --ignore-existing}"
fi

# Main Steps (This is most definitely *not* DRY, but it does give a dry run. Ha!)
if [[ -n "$dry_run" || -n "$verbose" ]]; then
    [[ -d $target/init ]]      || echo "mkdir -p $target/init"
    [[ -d $target/site-lisp ]] || echo "mkdir -p $target/site-lisp"
    [[ -d $target/themes ]]    || echo "mkdir -p $target/themes"
    [[ ./ -ef $target/init ]]  || echo "$copy ./[a-z]* $target/init"
    [[ -n "$no_extras" ]]      || echo "$copy Extras/site-lisp $target"
    [[ -n "$no_extras" ]]      || echo "$copy Extras/themes $target"
    [[ -n "$no_extras" ]]      || echo "cp $safe Extras/Cask $target"
    [[ -n "$with_custom" ]] && echo "cp $safe Extras/emacs-custom.el $target"
    [[ -z "$with_custom" && ! -e $target/emacs-custom.el ]] && echo "echo '' > $target/emacs-custom.el"
    [[ -n "$with_env" ]] && echo "cp $safe Extras/my-env.el $target"
    if [[ -z "$no_init" ]]; then
        if [[ "$package_in" = "$package_out" ]]; then
            echo "cat Extras/home-dot-emacs.el | sed 's:TARGET:$abs_target:' > $HOME/.emacs.el"
        else
            echo "cat Extras/home-dot-emacs.el | sed 's/$package_in/$package_out/;s:TARGET:$abs_target:' > $HOME/.emacs.el"
        fi
    fi
    [[ -n "$use_cask" ]] && echo "(cd $target; $use_cask install $verbose)"
fi

if [[ -z "$dry_run" ]]; then
    [[ -d $target/init ]]      || mkdir -p $target/init
    [[ -d $target/site-lisp ]] || mkdir -p $target/site-lisp
    [[ -d $target/themes ]]    || mkdir -p $target/themes
    [[ ./ -ef $target/init ]]  || $copy ./[a-z]* $target/init
    [[ -n "$no_extras" ]]      || $copy Extras/site-lisp $target
    [[ -n "$no_extras" ]]      || $copy Extras/themes $target
    [[ -n "$no_extras" ]]      || cp $safe Extras/Cask $target
    [[ -n "$with_custom" ]] && cp $safe Extras/emacs-custom.el $target
    [[ -z "$with_custom" && ! -e $target/emacs-custom.el ]] && echo '' > $target/emacs-custom.el
    [[ -n "$with_env" ]] && cp $safe Extras/my-env.el $target
    if [[ -z "$no_init" ]]; then
        if [[ "$package_in" = "$package_out" ]]; then
            cat Extras/home-dot-emacs.el | sed "s:TARGET:$abs_target:" > $HOME/.emacs.el
        else
            cat Extras/home-dot-emacs.el | sed "s/$package_in/$package_out/;s:TARGET:$abs_target:" > $HOME/.emacs.el
        fi
    fi
    [[ -n "$use_cask" ]] && (cd $target; $use_cask install $verbose)
    echo "Emacs initialization installed in $target (package $package_label, use cask? ${use_cask:-false})."
    echo "Next step: Edit $target/init/data/preferences.el to set your individual preferences."
fi  

