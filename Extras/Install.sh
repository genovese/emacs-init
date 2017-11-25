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
do_install=''
package_label='auto'
package_out=''
use_homebrew=''
no_init=''
no_backup=''
no_extras=''
with_custom=''
with_env=''
safe=''
dry_run=''
verbose=''

# additional state
brew_x=''
cask_x=''
emacs_x=''         # use  tr '[:upper:]' '[:lower:]' if tr available
platform=$(uname -s | sed 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/')
no_review=''
emacs_version=0
found_exes=''
tmp_myenv=''
tmp_emacs=''


usage () {
    echo "Installs emacs init files and packages in target directory"
    echo ""
    echo "sh Install.sh [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --target=DIR    Use DIR as directory in which to install init subdirectory"
    echo "                  (default: ..)."
    echo "  --emacs=PATH    Path to the emacs executable, if otherwise ambiguous or unusual."
    echo "                  Not usually needed, as emacs is auto-detected by default."
    echo "  --package=TYPE  Emacs package system: package, cask, cask-homebrew, auto."
    echo "                  auto looks for cask and homebrew to set the type."
    echo "                  (default: auto)"
    echo "  --install       Install emacs packages, using method specified in --package."
    echo "  --with-custom   Copy emacs-custom.el from Extras."
    echo "                  The file is always created if it does not exist."
    echo "  --with-env      Copy my-env.el from Extras. User's PATH is personalized"
    echo "                  during installation. (Mac OS X only unless given twice.)"
    echo "  --no-home       Do not install $HOME/.emacs(.el)."
    echo "  --no-backup     Do not backup $HOME/.emacs(.el) during install"
    echo "                  (ignored and irrelevant if --no-home is supplied)."
    echo "  --no-extras     Do not install extra files (Cask, themes, site-lisp)."
    echo "  --minimal       Install base code only, no extras or packages."
    echo "                  Sets --no-init and --no-extras, overriding --with-custom,"
    echo "                  --with-env, and --install, though these can be set after."
    echo "  --full          Equivalent to --install --with-custom --with-env."
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
        if [[ -n "$do_install" ]]; then
            echo "$0 ERROR: cannot find suitable version of emacs, no files installed"
            echo "  Try specifying --emacs=PATH for emacs version >= $min_emacs_version ($min_emacs_preferred preferred)"
            exit 1
        else
            echo "$0 WARNING: cannot find suitable versino of emacs; cannot perform data review step."
            no_review='true'
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
        --install)
            do_install='true'
            ;;
        --with-custom)
            with_custom='true'
            ;;
        --with-env)
            if [[ -z "$with_env" ]]; then
                with_env='true'
            else
                with_env='force'
            fi
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
        --minimal)
            no_init='true'
            no_extras='true'
            do_install=''
            with_custom=''
            with_env=''
            ;;
        --full)
            do_install='true'
            with_custom='true'
            with_env='true'
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
    [[ -n "$no_extras" ]]      || echo "$copy Extras/site-lisp $target"
    [[ -n "$no_extras" ]]      || echo "$copy Extras/themes $target"
    [[ -n "$no_extras" ]]      || echo "cp $safe Extras/Cask $target"
    [[ -n "$with_custom" ]] && echo "cp $safe Extras/emacs-custom.el $target"
    [[ -z "$with_custom" && ! -e $target/emacs-custom.el ]] && echo "echo '' > $target/emacs-custom.el"
    if [[ "$with_env" == "force" || ( -n "$with_env" && "$platform" == "darwin" ) ]]; then
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
    [[ -n "$no_extras" ]]      || $copy Extras/site-lisp $target
    [[ -n "$no_extras" ]]      || $copy Extras/themes $target
    [[ -n "$no_extras" ]]      || cp $safe Extras/Cask $target
    [[ -n "$with_custom" ]] && cp $safe Extras/emacs-custom.el $target
    [[ -z "$with_custom" && ! -e $target/emacs-custom.el ]] && echo '' > $target/emacs-custom.el
    if [[ "$with_env" == "force" || ( -n "$with_env" && "$platform" == "darwin" ) ]]; then
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
if [[ -n "$do_install" ]]; then
    if [[ "$package_label" == "package" && -n "$emacs_x" ]]; then
        if [[ -n "$dry_run" || -n "$verbose" ]]; then
            echo "(cd $target; $emacs_x --batch -Q --load init/Extras/loader.el --funcall install-init-packages)"
        fi
        if [[ -z "$dry_run" ]]; then
            (cd $target; $emacs_x --batch -Q --load init/Extras/loader.el --funcall install-init-packages)
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
fi

# User Review of customizations and preferences
if [[ -n "$emacs_x" ]]; then
    if [[ -n "$dry_run" || -n "$verbose" ]]; then
        echo "(cd $target; $emacs_x -Q --load init/Extras/review.el --funcall review-init-settings)"
    fi
    if [[ -z "$dry_run" ]]; then
        (cd $target; $emacs_x -Q --load init/Extras/review.el --funcall review-init-settings)
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
echo "  package type: $package_label, install? ${do_install:-false}, homebrew preferred? ${use_homebrew:-false},"
echo "  with custom? ${with_custom:-false}, with env? ${with_env:-false}, no home? ${no_init:-false}, no extras? ${no_extras:-false},"
echo "  with the following options set: ${safe:+--safe }${verbose:+--verbose }${dry_run:+--dry-run}."
echo "Next: Start up emacs and get editing..."

