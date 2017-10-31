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
 '(ibuffer-saved-filters
   (quote
    (("gnu"
      ((filename . ".")
       (or
        (derived-mode . prog-mode)
        (mode . "compilation-mode"))))
     ("foo"
      ((name . "foo")
       (derived-mode . text-mode)))
     ("gnus"
      (filename . ".")
      (or
       (derived-mode . prog-mode)
       (mode . "compilation-mode")))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(icicle-top-level-key-bindings
   (quote
    (([pause]
      icicle-switch-to/from-minibuffer t)
     ("ig" icicle-search-generic t)
     ("iw" icicle-search-word t)
     ("ik" icicle-search-keywords t)
     ("io" icicle-occur t)
     ("=" icicle-imenu t)
     ("\"" icicle-search-text-property t)
     ("/" icicle-complete-thesaurus-entry
      (fboundp
       (quote icicle-complete-thesaurus-entry)))
     ([24 134217829]
      icicle-execute-named-keyboard-macro t)
     (" " icicle-command-abbrev t)
     ("5o" icicle-select-frame t)
     ([134217736 15]
      icicle-describe-option-of-type t)
     ("i4" icicle-kmacro t)
     (abort-recursive-edit icicle-abort-recursive-edit t)
     (apropos icicle-apropos t)
     (apropos-command icicle-apropos-command t)
     (apropos-value icicle-apropos-value t)
     (apropos-variable icicle-apropos-option
                       (fboundp
                        (quote icicle-apropos-option)))
     (apropos-variable icicle-apropos-variable
                       (not
                        (fboundp
                         (quote icicle-apropos-option))))
     (apropos-zippy icicle-apropos-zippy t)
     (bookmark-jump icicle-bookmark t)
     (bookmark-jump-other-window icicle-bookmark-other-window t)
     (bookmark-set icicle-bookmark-cmd t)
     (customize-apropos icicle-customize-apropos t)
     (customize-apropos-faces icicle-customize-apropos-faces t)
     (customize-apropos-groups icicle-customize-apropos-groups t)
     (customize-apropos-options icicle-customize-apropos-options t)
     (customize-face icicle-customize-face t)
     (customize-face-other-window icicle-customize-face-other-window t)
     (dabbrev-completion icicle-dabbrev-completion t)
     (delete-window icicle-delete-window t)
     (delete-windows-for icicle-delete-window t)
     (dired icicle-dired t)
     (dired-other-window icicle-dired-other-window t)
     (exchange-point-and-mark icicle-exchange-point-and-mark t)
     (execute-extended-command icicle-execute-extended-command t)
     (find-file icicle-file t)
     (find-file-other-window icicle-file-other-window t)
     (find-file-read-only icicle-find-file-read-only t)
     (find-file-read-only-other-window icicle-find-file-read-only-other-window t)
     (insert-buffer icicle-insert-buffer t)
     (kill-buffer icicle-kill-buffer t)
     (kill-buffer-and-its-windows icicle-kill-buffer t)
     (minibuffer-keyboard-quit icicle-abort-recursive-edit
                               (fboundp
                                (quote minibuffer-keyboard-quit)))
     (other-window icicle-other-window-or-frame t)
     (other-window-or-frame icicle-other-window-or-frame t)
     (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t)
     (repeat-complex-command icicle-repeat-complex-command t)
     (set-mark-command icicle-goto-marker-or-set-mark-command t)
     (switch-to-buffer icicle-buffer t)
     (switch-to-buffer-other-window icicle-buffer-other-window t)
     (where-is icicle-where-is t)
     (yank icicle-yank-maybe-completing t)
     (yank-pop icicle-yank-pop-commands
               (featurep
                (quote second-sel)))
     (yank-pop-commands icicle-yank-pop-commands
                        (featurep
                         (quote second-sel)))
     (zap-to-char icicle-zap-to-char
                  (fboundp
                   (quote read-char-by-name)))
     ("jt" icicle-find-file-tagged
      (featurep
       (quote bookmark+)))
     ("4jt" icicle-find-file-tagged-other-window
      (featurep
       (quote bookmark+)))
     (bmkp-autofile-set icicle-bookmark-a-file
                        (fboundp
                         (quote bmkp-bookmark-a-file)))
     (bmkp-tag-a-file icicle-tag-a-file
                      (fboundp
                       (quote bmkp-tag-a-file)))
     (bmkp-untag-a-file icicle-untag-a-file
                        (fboundp
                         (quote bmkp-untag-a-file)))
     (bmkp-find-file icicle-find-file-handle-bookmark
                     (fboundp
                      (quote bmkp-find-file)))
     (bmkp-find-file-other-window icicle-find-file-handle-bookmark-other-window
                                  (fboundp
                                   (quote bmkp-find-file-other-window)))
     (bmkp-autofile-jump icicle-bookmark-autofile
                         (fboundp
                          (quote bmkp-autofile-jump)))
     (bmkp-autofile-jump-other-window icicle-bookmark-autofile-other-window
                                      (fboundp
                                       (quote bmkp-autofile-jump)))
     (bmkp-autonamed-jump icicle-bookmark-autonamed
                          (fboundp
                           (quote bmkp-autonamed-jump)))
     (bmkp-autonamed-jump-other-window icicle-bookmark-autonamed-other-window
                                       (fboundp
                                        (quote bmkp-autonamed-jump)))
     (bmkp-autonamed-this-buffer-jump icicle-bookmark-autonamed-this-buffer
                                      (fboundp
                                       (quote bmkp-autonamed-this-buffer-jump)))
     (bmkp-bookmark-file-jump icicle-bookmark-bookmark-file
                              (fboundp
                               (quote bmkp-bookmark-file-jump)))
     (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list
                              (fboundp
                               (quote bmkp-bookmark-list-jump)))
     (bmkp-desktop-jump icicle-bookmark-desktop
                        (fboundp
                         (quote bmkp-desktop-jump)))
     (bmkp-dired-jump icicle-bookmark-dired
                      (fboundp
                       (quote bmkp-dired-jump)))
     (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window
                                   (fboundp
                                    (quote bmkp-dired-jump)))
     (bmkp-file-jump icicle-bookmark-file
                     (fboundp
                      (quote bmkp-file-jump)))
     (bmkp-file-jump-other-window icicle-bookmark-file-other-window
                                  (fboundp
                                   (quote bmkp-file-jump)))
     (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir
                              (fboundp
                               (quote bmkp-file-this-dir-jump)))
     (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window
                                           (fboundp
                                            (quote bmkp-file-this-dir-jump)))
     (bmkp-gnus-jump icicle-bookmark-gnus
                     (fboundp
                      (quote bmkp-gnus-jump)))
     (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window
                                  (fboundp
                                   (quote bmkp-gnus-jump)))
     (bmkp-image-jump icicle-bookmark-image
                      (fboundp
                       (quote bmkp-image-jump)))
     (bmkp-image-jump-other-window icicle-bookmark-image-other-window
                                   (fboundp
                                    (quote bmkp-image-jump)))
     (bmkp-info-jump icicle-bookmark-info
                     (fboundp
                      (quote bmkp-info-jump)))
     (bmkp-info-jump-other-window icicle-bookmark-info-other-window
                                  (fboundp
                                   (quote bmkp-info-jump)))
     (bmkp-local-file-jump icicle-bookmark-local-file
                           (fboundp
                            (quote bmkp-local-file-jump)))
     (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window
                                        (fboundp
                                         (quote bmkp-local-file-jump)))
     (bmkp-man-jump icicle-bookmark-man
                    (fboundp
                     (quote bmkp-man-jump)))
     (bmkp-man-jump-other-window icicle-bookmark-man-other-window
                                 (fboundp
                                  (quote bmkp-man-jump)))
     (bmkp-non-file-jump icicle-bookmark-non-file
                         (fboundp
                          (quote bmkp-non-file-jump)))
     (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window
                                      (fboundp
                                       (quote bmkp-non-file-jump)))
     (bmkp-region-jump icicle-bookmark-region
                       (fboundp
                        (quote bmkp-region-jump)))
     (bmkp-region-jump-other-window icicle-bookmark-region-other-window
                                    (fboundp
                                     (quote bmkp-region-jump)))
     (bmkp-remote-file-jump icicle-bookmark-remote-file
                            (fboundp
                             (quote bmkp-remote-file-jump)))
     (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window
                                         (fboundp
                                          (quote bmkp-remote-file-jump)))
     (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers
                                 (fboundp
                                  (quote bmkp-specific-buffers-jump)))
     (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window
                                              (fboundp
                                               (quote bmkp-specific-buffers-jump)))
     (bmkp-specific-files-jump icicle-bookmark-specific-files
                               (fboundp
                                (quote bmkp-specific-files-jump)))
     (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window
                                            (fboundp
                                             (quote bmkp-specific-files-jump)))
     (bmkp-temporary-jump icicle-bookmark-temporary
                          (fboundp
                           (quote bmkp-temporary-jump)))
     (bmkp-temporary-jump-other-window icicle-bookmark-temporary-other-window
                                       (fboundp
                                        (quote bmkp-temporary-jump)))
     (bmkp-this-buffer-jump icicle-bookmark-this-buffer
                            (fboundp
                             (quote bmkp-this-buffer-jump)))
     (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window
                                         (fboundp
                                          (quote bmkp-this-buffer-jump)))
     (bmkp-url-jump icicle-bookmark-url
                    (fboundp
                     (quote bmkp-url-jump)))
     (bmkp-url-jump-other-window icicle-bookmark-url-other-window
                                 (fboundp
                                  (quote bmkp-url-jump)))
     (bmkp-w3m-jump icicle-bookmark-w3m
                    (fboundp
                     (quote bmkp-w3m-jump)))
     (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window
                                 (fboundp
                                  (quote bmkp-w3m-jump)))
     (bmkp-find-file-all-tags icicle-find-file-all-tags
                              (fboundp
                               (quote bmkp-find-file-all-tags)))
     (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window
                                           (fboundp
                                            (quote bmkp-find-file-all-tags)))
     (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp
                                     (fboundp
                                      (quote bmkp-find-file-all-tags-regexp)))
     (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window
                                                  (fboundp
                                                   (quote bmkp-find-file-all-tags-regexp-other-window)))
     (bmkp-find-file-some-tags icicle-find-file-some-tags
                               (fboundp
                                (quote bmkp-find-file-some-tags)))
     (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window
                                            (fboundp
                                             (quote bmkp-find-file-some-tags-other-window)))
     (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp
                                      (fboundp
                                       (quote bmkp-find-file-some-tags-regexp)))
     (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window
                                                   (fboundp
                                                    (quote bmkp-find-file-some-tags-regexp-other-window)))
     (bmkp-autofile-all-tags-jump icicle-bookmark-autofile-all-tags
                                  (fboundp
                                   (quote bmkp-autofile-all-tags-jump)))
     (bmkp-autofile-all-tags-jump-other-window icicle-bookmark-autofile-all-tags-other-window
                                               (fboundp
                                                (quote bmkp-autofile-all-tags-jump)))
     (bmkp-autofile-all-tags-regexp-jump icicle-bookmark-autofile-all-tags-regexp
                                         (fboundp
                                          (quote bmkp-autofile-all-tags-regexp-jump)))
     (bmkp-autofile-all-tags-regexp-jump-other-window icicle-bookmark-autofile-all-tags-regexp-other-window
                                                      (fboundp
                                                       (quote bmkp-autofile-all-tags-regexp-jump)))
     (bmkp-autofile-some-tags-jump icicle-bookmark-autofile-some-tags
                                   (fboundp
                                    (quote bmkp-autofile-some-tags-jump)))
     (bmkp-autofile-some-tags-jump-other-window icicle-bookmark-autofile-some-tags-other-window
                                                (fboundp
                                                 (quote bmkp-autofile-some-tags-jump)))
     (bmkp-autofile-some-tags-regexp-jump icicle-bookmark-autofile-some-tags-regexp
                                          (fboundp
                                           (quote bmkp-autofile-some-tags-regexp-jump)))
     (bmkp-autofile-some-tags-regexp-jump-other-window icicle-bookmark-autofile-some-tags-regexp-other-window
                                                       (fboundp
                                                        (quote bmkp-autofile-some-tags-regexp-jump)))
     (bmkp-all-tags-jump icicle-bookmark-all-tags
                         (fboundp
                          (quote bmkp-all-tags-jump)))
     (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window
                                      (fboundp
                                       (quote bmkp-all-tags-jump)))
     (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp
                                (fboundp
                                 (quote bmkp-all-tags-regexp-jump)))
     (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window
                                             (fboundp
                                              (quote bmkp-all-tags-regexp-jump)))
     (bmkp-some-tags-jump icicle-bookmark-some-tags
                          (fboundp
                           (quote bmkp-some-tags-jump)))
     (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window
                                       (fboundp
                                        (quote bmkp-some-tags-jump)))
     (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp
                                 (fboundp
                                  (quote bmkp-some-tags-regexp-jump)))
     (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window
                                              (fboundp
                                               (quote bmkp-some-tags-regexp-jump)))
     (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags
                              (fboundp
                               (quote bmkp-file-all-tags-jump)))
     (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window
                                           (fboundp
                                            (quote bmkp-file-all-tags-jump)))
     (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp
                                     (fboundp
                                      (quote bmkp-file-all-tags-regexp-jump)))
     (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window
                                                  (fboundp
                                                   (quote bmkp-file-all-tags-regexp-jump)))
     (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags
                               (fboundp
                                (quote bmkp-file-some-tags-jump)))
     (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window
                                            (fboundp
                                             (quote bmkp-file-some-tags-jump)))
     (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp
                                      (fboundp
                                       (quote bmkp-file-some-tags-regexp-jump)))
     (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window
                                                   (fboundp
                                                    (quote bmkp-file-some-tags-regexp-jump)))
     (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags
                                       (fboundp
                                        (quote bmkp-file-this-dir-all-tags-jump)))
     (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window
                                                    (fboundp
                                                     (quote bmkp-file-this-dir-all-tags-jump)))
     (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp
                                              (fboundp
                                               (quote bmkp-file-this-dir-all-tags-regexp-jump)))
     (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window
                                                           (fboundp
                                                            (quote bmkp-file-this-dir-all-tags-regexp-jump)))
     (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags
                                        (fboundp
                                         (quote bmkp-file-this-dir-some-tags-jump)))
     (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window
                                                     (fboundp
                                                      (quote bmkp-file-this-dir-some-tags-jump)))
     (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp
                                               (fboundp
                                                (quote bmkp-file-this-dir-some-tags-regexp-jump)))
     (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window
                                                            (fboundp
                                                             (quote bmkp-file-this-dir-some-tags-regexp-jump)))
     (find-tag icicle-find-tag
               (fboundp
                (quote command-remapping)))
     (find-tag-other-window icicle-find-first-tag-other-window t)
     (pop-tag-mark icicle-pop-tag-mark
                   (fboundp
                    (quote command-remapping)))
     (pp-eval-expression icicle-pp-eval-expression
                         (fboundp
                          (quote command-remapping)))
     ([27 134217848]
      lacarte-execute-command
      (fboundp
       (quote lacarte-execute-command)))
     ([134217824]
      lacarte-execute-menu-command
      (fboundp
       (quote lacarte-execute-menu-command)))
     ([f10]
      lacarte-execute-menu-command
      (fboundp
       (quote lacarte-execute-menu-command))))))
 '(jde-jdk (quote ("1.5.0")))
 '(jde-jdk-registry
   (quote
    (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0")
     ("1.5.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0")
     ("1.4.2" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.4.2"))))
 '(magit-emacsclient-executable "/usr/local/bin/emacsclient" t)
 '(nxhtml-skip-welcome t)
 '(org-agenda-files
   (quote
    ("~/org/today" "~/org/head/immediate" "~/org/head/next" "~/org/admin" "~/org/research" "~/org/work" "~/org/home" "~/org/class" "~/org/head/recurring" "~/org/head/long-term" "~/org/head/seasonal" "~/org/coding" "~/org/misc" "~/org/fun" "~/org/head/someday")))
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
     ("v" "#+begin_verse
?
#+end_verse")
     ("V" "#+begin_verbatim
?
#+end_verbatim")
     ("c" "#+begin_center
?
#+end_center")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX")
     ("L" "#+LaTeX: ")
     ("h" "#+BEGIN_HTML
?
#+END_HTML")
     ("H" "#+HTML: ")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII")
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
 ;'(user-full-name "Christopher R. Genovese")
 ;'(user-mail-address "genovese@cmu.edu")
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
 '(org-verbatim ((t (:foreground "#afd8af"))))
 '(semantic-decoration-on-private-members-face ((((class color) (background light)) (:background "#fafad2")))))
