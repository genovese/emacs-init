#!/bin/bash

# configuration

if shopt -q nullglob; then
    reset_nullglob=''
else
    shopt -qs nullglob
    reset_nullglob='true'
fi

# data
version="0.0.5.20171117"
package_in="((package-system :package))"
min_emacs_version=24  
min_emacs_preferred=25
app_emacs=( ~/Applications/Emacs.app/Contents/MacOS/Emacs \
            /Applications/Emacs.app/Contents/MacOS/Emacs )
brew_emacs=( /usr/local/Cellar/emacs/*/Emacs.app/Contents/MacOS/Emacs \
             /usr/local/bin/emacs )
alt_emacs=( ~/bin/emacs /usr/local/bin/emacs )

# default option values
target=".."
emacs_bin=''
package_label='auto'
package_out=''
use_homebrew=''
no_install=''
no_init=''
no_backup=''
no_extras=''
no_custom=''
no_env=''
force_custom=''
force_env=''
force_review=''
safe=''
dry_run=''
verbose=''

# additional state
brew_x=''
cask_x=''
emacs_x=''          # vvv use  tr '[:upper:]' '[:lower:]' if tr available
platform=$(uname -s | sed 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/')
emacs_version=0
found_exes=''
tmp_myenv=''
tmp_emacs=''
no_review=''
review_type=''


usage () {
    echo "Installs emacs init files and packages in target directory"
    echo ""
    echo "sh Install.sh [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --target=DIR    Use DIR as directory in which to install init subdirectory"
    echo "                  (default: ..)."
    echo "  --package=TYPE  Emacs package system used to install required emacs packages."
    echo "                  Value can be: package, cask, cask-homebrew, auto (default: auto)."
    echo "                  auto attempts to deduce type from cask and homebrew availability."
    echo "  --emacs=PATH    Path to the emacs executable, if otherwise ambiguous or unusual."
    echo "                  Not usually needed, as emacs is auto-detected by default."
    echo ""
    echo "  --no-install    Do not install emacs packages during script. Note: package"
    echo "                  installation is required for full functionality of the config."
    echo "  --no-custom     Do not copy emacs-custom.el from Extras, though the file is "
    echo "                  always created if it does not exist. Without this, the file is"
    echo "                  copied unless it already exists; see also --force-custom."
    echo "  --no-env        Do not install my-env.el from Extras. Without this, my-env.el is"
    echo "                  installed with user's PATH for Mac OS X only. See --force-env."
    echo "  --no-home       Do not install $HOME/.emacs.el."
    echo "  --no-backup     Do not backup $HOME/.emacs.el during install"
    echo "                  (ignored and irrelevant if --no-home is supplied)."
    echo "  --no-extras     Do not install extra files (Cask, themes, site-lisp)."
    echo ""
    echo "  --force-custom  Install emacs-custom.el even if it already exists in target."
    echo "                  Overrides --no-custom."
    echo "  --force-env     Install my-env.el even on non-Mac platforms."
    echo "                  Overrides --no-env."
    echo "  --force-review  Force full review even if packages are not installed."
    echo "                  Use this if the emacs packages have been pre-installed."
    echo ""
    echo "  --safe          Do not overwrite existing files where possible."
    echo "  --homebrew      On Mac OS X, prefer homebrew versions of emacs and cask."
    echo "  --verbose       Display steps while executing them."
    echo "  --dry-run       Display steps but do not execute them."
    echo "  --help          Show this usage message."
    echo "  --version       Version information."
    echo ""
    echo "After initial installation, this starts an interactive interface through"
    echo "which the emacs init configuration and preferences can be customized."
}    

find_cask () {
    cask_x=$(command -v cask)
    if [[ -n "$cask_x" ]]; then
        return 0
    fi
    return 1
}

find_homebrew () {
    if [[ "$platform" != "darwin" ]]; then
        return 1
    fi

    brew_x=$(command -v brew)
    if [[ -n "$brew_x" ]] && ! $brew_x --version | grep -q '^Homebrew'; then
        brew_x=''
        return 1
    fi
    return 0
}

is_emacs_supported() {
    if [[ -n "$emacs_x" && -x "$emacs_x" ]]; then
        emacs_version=$($emacs_x --version | sed -E '1s/GNU Emacs ([0-9]+)\..*$/\1/;2,$d')
        if (( $emacs_version > $min_emacs_version )); then
            return 0
        else
            return 1
        fi
    fi
}

find_emacs () {
    local emacs_exes=''
    local found_emacs=''
    local found_version=0

    # First check user-specified value, then in order according to platform

    if [[ -n "$emacs_bin" ]]; then
        emacs_x="$emacs_bin"
        if is_emacs_supported; then
            return 0
        fi
    fi

    cv_emacs=$(command -v emacs)
    if [[ "$platform" == "darwin" ]]; then
        if [[ -n "$use_homebrew" ]]; then
            emacs_exes=( "${brew_emacs[@]}" "${app_emacs[@]}" "$cv_emacs" )
        else
            emacs_exes=( "${app_emacs[@]}" "${brew_emacs[@]}" "$cv_emacs" )
        fi    
    else
        emacs_exes=( "$cv_emacs" "${alt_emacs[@]}" )
    fi

    for ee in "${emacs_exes[@]}"
    do
        emacs_x="${ee/#\~/$HOME}"
        if is_emacs_supported; then
            if (( $emacs_version > $found_version )); then
                found_emacs="$emacs_x"
                found_version=$emacs_version
            fi
        fi
    done    

    if [[ -n "$found_emacs" ]]; then
        emacs_x="$found_emacs"
        emacs_version="$found_version"
        return 0
    else
        emacs_x=''
        return 1
    fi
}

find_executables () {
    find_cask
    find_homebrew
    if ! find_emacs; then
        if [[ -z "$no_install" ]]; then
            echo "$0 ERROR: cannot find suitable version of emacs, no files installed"
            echo "  Try specifying --emacs=PATH for emacs version >= $min_emacs_version ($min_emacs_preferred preferred)"
            exit 1
        else
            echo "$0 WARNING: cannot find suitable versino of emacs; cannot perform data review step."
            no_review='true' # ATTN: redundant for the moment
        fi
    fi
    if [[ -n "$verbose" ]]; then
        [[ -n "$cask_x" ]] && echo "Found cask at $cask_x"
        [[ -n "$brew_x" ]] && echo "Found homebrew at $brew_x"
        [[ -n "$emacs_x" ]] && echo "Found emacs version $emacs_version at $emacs_x"
    fi
    found_exes='true'
}

deduce_package () {
    local brew_cask_el="/usr/local/share/emacs/site-lisp/cask/cask.el"
    [[ -n "$found_exes" ]] || find_executables
    if [[ "$package_label" == "auto" ]]; then
        if [[ -n "$cask_x" ]]; then
            if [[ -n "$use_homebrew" && -n "$brew_x" && -e "$brew_cask_el" ]]; then
                package_label="cask-homebrew"
            else
                package_label="cask"
            fi
        else
            package_label="package"
        fi
    fi
    package_out="((package-system :$package_label))"
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
        --no-install)
            no_install='true'
            ;;
        --no-custom)
            no_custom='true'
            ;;
        --no-env)
            no_env='true'
            ;;
        --no-home)
            no_init='true'
            ;;
        --no-backup)
            no_backup='true'
            ;;
        --no-extras)
            no_extras='true'
            ;;
        --force-custom)
            force_custom='true'
            no_custom=''
            ;;
        --force-env)
            force_env='true'
            no_env=''
            ;;
        --force-review)
            force_review='true'
            ;;
        --safe)
            safe='-n'
            ;;
        --homebrew)
            use_homebrew='true'
            ;;
        --verbose)
            verbose='--verbose'
            ;;
        --dry-run)
            dry_run='true'
            ;;
        *)
            param=$(echo $1 | sed 's/=.*$//')
            value=$(echo $1 | sed 's/^[^=]*=//')
            case $param in
                --target)
                    target="$value"
                    ;;
                --emacs)
                    emacs_bin="$value"
                    ;;
                --package)
                    case $value in
                        package|cask|cask-homebrew|auto)
                            package_label="$value"
                            package_out="((package-system :$value))"
                            ;;
                        *)
                            echo "$0 ERROR: --package value should be package, cask, cask-homebrew, or auto"
                            echo ""
                            usage
                            exit
                            ;;
                    esac
                    ;;
                *)
                    echo "$0 ERROR: unknown parameter \"$param\""
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
  abs_target=$(cd "$target"; pwd)
fi

# Recursive copies
copy=$(command -v rsync)
if [[ -z "$copy" ]]; then
  copy='cp -R $safe'
else
  copy="$copy -a ${safe:+ --ignore-existing}"
fi

# Temporary files for --safe processing, portably
if [[ -n "$safe" ]]; then
    if [[ -n "$(command -v mktemp)" ]]; then
        tmp_myenv=$(mktemp)
        tmp_emacs=$(mktemp)
    elif [[ -d ${TMPDIR:-/tmp/} ]]; then
        tmp_myenv=${TMPDIR:-/tmp/}my-env.el
        tmp_emacs=${TMPDIR:-/tmp/}.emacs.el
    else
         echo "$0 ERROR: cannot create temporary files;"
         echo "  try evoking *without* the --safe flag."
         exit 1
    fi
else
    tmp_myenv=$target/my-env.el
    tmp_emacs=$HOME/.emacs.el
fi

# Scan environment
find_executables
deduce_package

# Display File Installation Steps
# (This is most definitely *not* DRY, but it does give a dry run. Ha!)
if [[ -n "$dry_run" || -n "$verbose" ]]; then
    [[ -d $target/init ]]      || echo "mkdir -p $target/init"
    [[ -d $target/site-lisp ]] || echo "mkdir -p $target/site-lisp"
    [[ -d $target/themes ]]    || echo "mkdir -p $target/themes"
    [[ ./ -ef $target/init ]]  || echo "$copy ./[a-z]* $target/init"
    if [[ -z "$no_extras" ]]; then
        echo "$copy Extras/site-lisp $target"
        echo "$copy Extras/themes $target"
        echo "cp $safe Extras/Cask $target"
    fi
    if [[ -n "$force_custom" || ( -z "$no_custom" && ! -e $target/emacs-custom.el ) ]]; then
        echo "cp $safe Extras/emacs-custom.el $target"
    elif [[ -n "$no_custom" && ! -e $target/emacs-custom.el ]]; then
        echo "echo '' > $target/emacs-custom.el"
    fi
    if [[ -n "$force_env" || ( -z "$no_env" && "$platform" == "darwin" ) ]]; then
        echo 'cat Extras/my-env.el | sed -E "s|\(setenv \"PATH\" \"[^\"]+\" *\)|(setenv \"PATH\" \"$PATH\")|" >' "$tmp_myenv"
        [[ -n "$safe" ]] && echo "cp $safe $tmp_myenv $target/my-env.el"
    fi
    if [[ -z "$no_init" ]]; then
        if [[ -z "$no_backup" && -e "$HOME/.emacs.el" ]]; then
            echo "cp $safe $HOME/.emacs.el $HOME/.emacs.el.backup"
        fi
        if [[ "$package_in" == "$package_out" ]]; then
            echo "cat Extras/home-dot-emacs.el | sed 's:TARGET:$abs_target:' > $tmp_emacs"
        else
            echo "cat Extras/home-dot-emacs.el | sed 's/$package_in/$package_out/;s:TARGET:$abs_target:' > $tmp_emacs"
        fi
        [[ -n "$safe" ]] && echo "cp $safe $tmp_emacs $HOME/.emacs.el"
    fi
fi

# Do File Installation Steps
# (This is most definitely *not* DRY, but it does give a dry run. Ha!)
if [[ -z "$dry_run" ]]; then
    [[ -d $target/init ]]      || mkdir -p $target/init
    [[ -d $target/site-lisp ]] || mkdir -p $target/site-lisp
    [[ -d $target/themes ]]    || mkdir -p $target/themes
    [[ ./ -ef $target/init ]]  || $copy ./[a-z]* $target/init
    if [[ -z "$no_extras" ]]; then
        $copy Extras/site-lisp $target
        $copy Extras/themes $target
        cp $safe Extras/Cask $target
    fi
    if [[ -n "$force_custom" || ( -z "$no_custom" && ! -e $target/emacs-custom.el ) ]]; then
        cp $safe Extras/emacs-custom.el $target
    elif [[ -n "$no_custom" && ! -e $target/emacs-custom.el ]]; then
        echo '' > $target/emacs-custom.el
    fi
    if [[ -n "$force_env" || ( -z "$no_env" && "$platform" == "darwin" ) ]]; then
        cat Extras/my-env.el | sed -E "s|\(setenv \"PATH\" \"[^\"]+\" *\)|(setenv \"PATH\" \"$PATH\")|" > $tmp_myenv
        [[ -n "$safe" ]] && cp $safe $tmp_myenv $target/my-env.el
    fi
    if [[ -z "$no_init" ]]; then
        if [[ -z "$no_backup" && -e "$HOME/.emacs.el" ]]; then
            cp $safe $HOME/.emacs.el $HOME/.emacs.el.backup
        fi
        if [[ "$package_in" = "$package_out" ]]; then
            cat Extras/home-dot-emacs.el | sed "s:TARGET:$abs_target:" > $tmp_emacs
        else
            cat Extras/home-dot-emacs.el | sed "s/$package_in/$package_out/;s:TARGET:$abs_target:" > $tmp_emacs
        fi
        [[ -n "$safe" ]] && cp $safe $tmp_emacs $HOME/.emacs.el
    fi
fi  

# Package Installation
if [[ -z "$no_install" ]]; then
    if [[ "$package_label" == "package" && -n "$emacs_x" ]]; then
        if [[ -n "$dry_run" || -n "$verbose" ]]; then
            echo "(cd $target; $emacs_x --batch -Q --load init/Extras/packages.el --funcall install-init-packages)"
        fi
        if [[ -z "$dry_run" ]]; then
            (cd $target; $emacs_x --batch -Q --load init/Extras/packages.el --funcall install-init-packages)
        fi
    elif [[ -n "$cask_x" ]]; then
        if [[ -n "$dry_run" || -n "$verbose" ]]; then
            echo "(cd $target; $cask_x install $verbose)"
        fi
        if [[ -z "$dry_run" ]]; then
            (cd $target; $cask_x install $verbose)
        fi
    else
        echo "$0 ERROR: cannot install packages with type $package_label and cask $cask_x."
    fi
    review_type='' # packages installed, can do full review
elif [[ -z "$force_review" ]]; then
    review_type='-no-config' # if packages not installed, adjust review entry point
    echo "$0 WARNING: Without emacs packages, customization review will have reduced functionality."
fi

# User Review of customizations and preferences
if [[ -n "$emacs_x" && -z "$no_review" ]]; then
    if [[ -n "$dry_run" || -n "$verbose" ]]; then
        echo "(cd $target; $emacs_x -Q --load init/Extras/review.el --funcall review-init-settings$review_type)"
    fi
    if [[ -z "$dry_run" ]]; then
        (cd $target; $emacs_x -Q --load init/Extras/review.el --funcall review-init-settings$review_type)
    fi
fi

# Restore prior configuration, if necessary
if [[ -n "$reset_nullglob" ]]; then
    shopt -qu nullglob
fi

# Wrap up messaging
echo ""
echo "Emacs initialization now installed in $target with emacs $emacs_x."
echo "Configuration:"
echo "  package type: $package_label, homebrew preferred? ${use_homebrew:-false},"
echo "  emacs packages were ${no_install:+not yet }installed, and also"
echo "  installation suppressed for ${no_custom:+custom file, }${no_env:+my-env.el, }${no_init:+HOME/emacs.el, }${no_extras:+extra files, }and"
echo "  the following options were set: ${safe:+--safe }${verbose:+--verbose }${dry_run:+--dry-run}."
echo "Next: Start up emacs and get editing..."

