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
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c22fba50bb97d635bfbe52e70f5a3600878db673e4a6c0ee76bcb4d579a100cb" "4548e7ae80135d30b739d9d93d2e38321339bb2c2987d44725e691eb3cc529aa" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(custom-theme-load-path (quote (custom-theme-directory t)) t)
 '(delete-selection-mode nil)
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
 '(electric-indent-mode nil)
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#383838")
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
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
     ("all"
      ("All"
       (name . "."))))))
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
    (org-plus-contrib ace-jump-mode ace-window ack ag aggressive-indent alert anaphora ansi async auctex auctex-latexmk auto-complete avy beacon biblio biblio-core bind-key cask cider cider-decompile cider-eval-sexp-fu clj-refactor cljr-helm clojure-cheatsheet clojure-mode clojure-mode-extra-font-locking clojure-snippets commander company company-auctex company-c-headers company-flx company-math concurrent counsel counsel-dash counsel-projectile creole crux css-eldoc ctable dash dash-at-point dash-functional db deferred define-word diminish dired+ dired-details dired-details+ dired-filter dired-hacks-utils dired-narrow dired-open dired-rainbow dired-ranger dired-subtree direx ebib ecb ecukes edn ein elisp-refs elnode elpy emmet-mode epc epl espuds ess ess-R-data-view ess-smart-equals eval-sexp-fu exec-path-from-shell expand-region f faceup fakir fill-column-indicator find-file-in-project flx flx-ido flycheck flycheck-clojure flycheck-package flycheck-pos-tip font-lock-studio git git-commit git-timemachine gntp haml-mode haskell-mode heap helm helm-R helm-ack helm-bibtex helm-cider helm-core helm-dash helm-descbinds helm-firefox helm-flx helm-flyspell helm-fuzzier helm-git helm-ispell helm-itunes helm-ls-git helm-projectile helm-pydoc helm-swoop helpful highlight highlight-indentation ht hydra icicles iedit inflections iterator ivy ivy-hydra javap-mode js2-closure js2-mode js2-refactor julia-mode key-chord kv let-alist levenshtein lispy list-utils log4e loop macrostep magit magit-popup markdown-mode math-symbol-lists multiple-cursors names noflet org-ref origami ov ox-reveal package-build package-lint pallet paradox paredit parinfer parsebib pdf-tools peg pfuture pkg-info polymode popup popwin pos-tip powerline processing-mode projectile py-autopep8 pyvenv queue quickrun racket-mode rainbow-mode region-bindings-mode request request-deferred restclient rich-minority ripgrep rust-mode s sass-mode scss-mode seq shut-up simple-call-tree simple-httpd skewer-mode smart-mode-line smart-mode-line-powerline-theme smartparens smex spaceline spinner swiper swiper-helm tablist treemacs treemacs-projectile twittering-mode undo-tree use-package web web-mode websocket which-key win-switch with-editor workgroups2 wrap-region xkcd yaml-mode yasnippet zencoding-mode zoutline)))
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
