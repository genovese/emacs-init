;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(set-preferences
 'user-home-page      "http://www.stat.cmu.edu/~genovese?p=daily"
 'user-email-address  "genovese@cmu.edu"
 'shell-command       "/bin/zsh"
 'theme-function      'my/zenburn
 'time-format         "%a %d %b %Y %T %Z"
 'date-format         "%a %d %b %Y"
 'date-format:long    "%a %d %b %Y"
 'date-format:short   "%a %d %b"
 'date-format:listing "%Y %b %d %a"
 'python-command      "python3"
 'cursor-color        "#dfaf8f"
 'lighters            '((magit-auto-revert-mode)
                        (auto-revert-mode)
                        (ivy-mode) ; "\u2767" is a nice ivy leaf alternative
                        (company-mode)
                        (paredit-mode "()")
                        (eldoc-mode "Ed")))




