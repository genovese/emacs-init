;;; Customizations that must be done by the Emacs customization mechanism
;;; Amazingly, some packages do not allow for setq for some options!
;;; Added 19 Oct 2010


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"] t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c22fba50bb97d635bfbe52e70f5a3600878db673e4a6c0ee76bcb4d579a100cb" "4548e7ae80135d30b739d9d93d2e38321339bb2c2987d44725e691eb3cc529aa" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)) t)
 '(delete-selection-mode t)
 '(display-time-mode t)
 '(display-time-string-forms
   (quote
    ((if
         (and
          (not display-time-format)
          display-time-day-and-date)
         (format-time-string "%a %b %e " now)
       "")
     (propertize
      (format-time-string
       (or display-time-format
           (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
       now)
      (quote help-echo)
      (format-time-string "%a %d %b %Y" now)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    (("~/software/Emacs/Startup" "Emacs Startup")
     ("~/Documents/Eclipse35workspace" "Android Dev")
     ("~/Programming/Python" "Python General")
     ("~/Programming/Arduino" "Arduino")
     ("~/Documents/Processing" "Processing")
     ("~/Programming/Java" "Java General")
     ("~/Programming/Emacs" "Elisp General")
     ("~/Programming/C++" "C++ General")
     ("~/Programming/Mac" "OS X"))))
 '(ecb-tip-of-the-day nil)
 '(el-get-recipe-path
   (quote
    ("~/.emacs.d/el-get/recipes/local" "~/.emacs.d/el-get/el-get/recipes" "~/.emacs.d/el-get/recipes/elpa/" "~/.emacs.d/el-get/recipes/emacswiki/")))
 '(el-get-user-package-directory "~/.emacs.d/el-get/init")
 '(el-get-verbose t)
 '(electric-indent-mode nil)
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#383838")
 '(ibuffer-saved-filter-groups
   (quote
    ((#("default" 0 1
        (idx 0))
      ("Org"
       (directory . "~/org/"))
      ("Teaching"
       (directory . "~/class/"))
      ("Documents"
       (or
        (mode . plain-tex-mode)
        (mode . latex-mode)
        (mode . bibtex-mode)
        (mode . amstex-mode)
        (and
         (mode . org-mode)
         (not starred-name))
        (and
         (mode . fundamental-mode)
         (not starred-name))))
      ("Dirs"
       (mode . dired-mode))
      ("Code"
       (or
        (mode . c-mode)
        (mode . python-mode)
        (mode . java-mode)
        (mode . jde-mode)
        (mode . ess-mode)
        (mode . ruby-mode)
        (mode . clojure-mode)
        (mode . clojurescript-mode)
        (mode . lisp-mode)
        (mode . haskell-mode)
        (mode . perl-mode)
        (mode . cperl-mode)
        (mode . tuareg-mode)
        (mode . c++-mode)
        (mode . sh-mode)
        (mode . php-mode)
        (mode . objc-mode)
        (mode . processing-mode)
        (mode . arduino-mode)
        (mode . sql-mode)
        (mode . ps-mode)
        (mode . R-mode)
        (mode . r-mode)
        (mode . R-transcript-mode)
        (mode . r-transcript-mode)
        (basename . "[Mm]akefile")))
      ("Web"
       (or
        (mode . html-mode)
        (mode . sass-mode)
        (mode . scss-mode)
        (mode . css-mode)
        (mode . js2-mode)
        (mode . js-mode)
        (mode . nxml-mode)
        (mode . nxhtml-mode)))
      ("Emacs"
       (or
        (and
         (mode . emacs-lisp-mode)
         (not starred-name))
        (name . "^\\*elisp\\*")))
      ("Arduino"
       (directory . "~/Programming/Arduino/"))
      ("REPLs"
       (or
        (mode . shell-mode)
        (mode . comint-mode)
        (mode . inferior-ess-mode)
        (mode . eshell-mode)
        (mode . lisp-interaction-mode)
        (name . "^\\*[iI][pP]ython\\*")
        (mode . slime-repl-mode)))
      ("Help"
       (or
        (name . "^\\*Help\\*")
        (name . "^\\*help\\[R\\]")
        (name . "^\\*Man\\s-")
        (name . "^\\*Apropos\\*")
        (mode . help-mode)
        (mode . Man-mode)
        (mode . Info-mode)))
      ("Magit"
       (name . "^\\*magit"))
      ("Customize"
       (name . "^\\*Customize"))
      ("Tools"
       (or
        (name . "^\\*Calendar\\*")
        (name . "^\\*Messages\\*")
        (name . "^\\*Backtrace\\*")
        (name . "^\\*Customize")
        (name . "^\\*ESS\\*")
        (name . "^\\*Apropos\\*")
        (mode . help-mode)
        (mode . Man-mode)
        (mode . Info-mode)))
      ("Output"
       (name . "^\\*.*\\*\\(<[0-9]+>\\)?$"))
      ("Misc"
       (name . "^\\S-")))
     (#("all" 0 1
        (idx 1))
      ("All"
       (name . "."))))))
 '(jde-jdk (quote ("1.5.0")))
 '(jde-jdk-registry
   (quote
    (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0")
     ("1.5.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0")
     ("1.4.2" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.4.2"))))
 '(nxhtml-skip-welcome t)
 '(org-export-backends (quote (ascii beamer html icalendar latex md texinfo deck)))
 '(org-export-with-toc nil)
 '(org-structure-template-alist
   (quote
    (("s" "#+begin_src ?

#+end_src")
     ("e" "#+begin_example
?
#+end_example")
     ("q" "#+begin_quote
?
#+end_quote")
     ("v" "#+begin_verbatim
?
#+end_verbatim")
     ("V" "#+begin_verse
?
#+end_verse")
     ("c" "#+begin_center
?
#+end_center")
     ("l" "#+begin_latex
?
#+end_latex")
     ("L" "#+LaTeX: ")
     ("h" "#+begin_html
?
#+end_html")
     ("H" "#+HTML: ")
     ("a" "#+begin_ascii
?
#+end_ascii")
     ("A" "#+ASCII: ")
     ("i" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?"))))
 '(org-tag-faces (quote (("urgent" . font-lock-warning-face))))
 '(package-selected-packages
   (quote
    (helm-cider clojure-mode cider-eval-sexp-fu cider origami spaceline ein py-autopep8 treemacs-projectile treemacs elpy git-timemachine helpful parinfer rust-mode simple-call-tree counsel-dash counsel-projectile ivy region-bindings-mode twittering-mode ripgrep lispy ov org-ref helm-ispell helm-fuzzier helm-flx alert aggressive-indent crux auctex-latexmk company-math helm-flyspell avy flycheck-pos-tip flycheck-clojure counsel zencoding-mode yaml-mode xkcd wrap-region workgroups2 win-switch which-key websocket web-mode use-package undo-tree swiper-helm smex smartparens smart-mode-line-powerline-theme slamhound skewer-mode scss-mode sass-mode restclient rainbow-mode quickrun processing-mode popwin polymode paradox pallet ox-reveal org-plus-contrib names markdown-mode magit macrostep levenshtein latest-clojure-libraries js2-refactor js2-closure iterator icicles ht helm-swoop helm-pydoc helm-projectile helm-ls-git helm-itunes helm-git helm-firefox helm-descbinds helm-dash helm-bibtex helm-ack helm-R heap haskell-mode git font-lock-studio flycheck-package flx-ido find-file-in-project fill-column-indicator expand-region exec-path-from-shell ess-smart-equals ess-R-object-popup ess-R-data-view epc emmet-mode elnode ecukes ecb ebib direx dired-subtree dired-ranger dired-rainbow dired-open dired-narrow dired-filter dired-details+ dired+ define-word dash-functional dash-at-point css-eldoc company-flx company-c-headers company-auctex clojure-snippets clojure-mode-extra-font-locking clojure-cheatsheet cljr-helm cider-decompile beacon ag ack-and-a-half ack ace-jump-mode)))
 '(paradox-github-token t)
 '(projectile-mode-line
   (quote
    (:eval
     (let
         ((project-name
           (projectile-project-name)))
       (if
           (or
            (equal project-name "-")
            (and
             (fboundp
              (quote magit-get-current-branch))
             (magit-get-current-branch)))
           ""
         (format " Proj[%s]"
                 (projectile-project-name)))))))
 '(rm-blacklist (quote (" MRev" " hl-p")))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/run)
           (user/browser-repl))"))))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(win-switch-feedback-background-color "#93e0e3")
 '(win-switch-feedback-foreground-color "#5f7f5f")
 '(win-switch-window-threshold 0))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Anonymous Pro"))))
 '(calendar-today ((t (:foreground "firebrick" :weight bold))))
 '(ediff-even-diff-A ((t (:background "#696059"))))
 '(ediff-even-diff-B ((t (:background "#696059"))))
 '(ediff-fine-diff-B ((t (:background "#228822"))))
 '(ediff-odd-diff-A ((t (:background "grey40"))))
 '(ediff-odd-diff-B ((t (:background "grey40"))))
 '(font-latex-keyword-face ((t (:inherit font-lock-keyword-face :foreground "gray75"))) t)
 '(org-archived ((t (:foreground "grey52" :family "Optima"))))
 '(org-block ((t (:inherit shadow :foreground "#e0cf9f"))))
 '(org-code ((t (:foreground "#dc8cc3" :height 1.14 :family "Anonymous Pro"))))
 '(org-document-info ((t (:foreground "#f0dfaf" :height 1.22 :family "Optima"))))
 '(org-document-title ((t (:foreground "#93e0e3" :weight bold :height 1.55 :family "Optima"))))
 '(org-level-1 ((t (:foreground "#dfaf8f" :height 1.44 :family "Optima"))))
 '(org-level-2 ((t (:foreground "#8fb28f" :height 1.33 :family "Optima"))))
 '(org-level-3 ((t (:foreground "#7cb8bb" :height 1.22 :family "Optima"))))
 '(org-level-4 ((t (:inherit font-lock-constant-face))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "grey52"))))
 '(org-verbatim ((t (:foreground "#afd8af")))))
