;;; dot-emacs.el --- main emacs initialization file -*- lexical-binding: t; -*-

;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 2.4.3

;;; Commentary
;;  This file is designed to be loaded from a higher-level entry point.
;;  The entry script is responsible for initializing the package system,
;;  either directly, or through cask.

;;; Code


;;; Setup

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'use-package)
  (setq use-package-verbose t))

(use-package dash)
(use-package s :config (defun s-str (obj) (format "%s" obj)))
(use-package f :config (defalias 'f-slash* 'file-name-as-directory))
(use-package ht) ;ATTN: could use builtin map library instead
(use-package dash-functional)
(use-package bind-key)
(use-package hydra)
(use-package diminish)
(use-package pallet)  ; use with cask to keep up-to-date package list

(let ((init-dir (if load-file-name 
                    (f-dirname load-file-name)
                  (f-join user-emacs-directory "init"))))
  ;; init-dir is captured lexically in a closure for later use
  (defun load-local (name &optional type)
    "Load a package or module from the local init area."
    (let* ((ltype (s-str (or type "")))
           (ltype (if (keywordp type) (substring ltype 1) ltype))
           (file  (f-join init-dir ltype (s-str name))))
      (message ">>> %s %s %s" init-dir file ltype)
      (condition-case err
          (load file)
        (error (signal
                (car err) (cons
                           (format "Cannot load local init module %s (dir %s)."
                                   file init-dir)
                           (cdr err))))))))

(defvar preferences (ht)
  "Table of user-specific preferences for configuring initialization.
See `set-preferences' and `get-preference'.")

(defmacro set-preferences (&rest kv-pairs)
  "Add one or more preferences given as successive keys and values."
  (declare (indent 0))
  (let ((x (cl-gensym)))
    `(cl-loop for ,x = (list ,@kv-pairs) then (cddr ,x)
              while ,x do
              (puthash (car ,x) (cadr ,x) preferences))))

(defun get-preference (key &optional default)
  "Get preference for KEY."
  (gethash key preferences default))


;;; Load custom startup code

(load-local 'macros)
(load-local 'utils)
(load-local 'ops)
(load-local 'theme-support)
(load-local 'preferences :data)
(load-local 'user-system :data)

(setq my/theme-func (get-preference 'theme-function))

(prepend-subtree-to-load-path user-site-lisp-dir)


;;; Main operating settings

(setq inhibit-startup-message  t
      load-prefer-newer        t
      case-fold-search         nil
      indent-tabs-mode         nil
      shift-select-mode        nil
      scroll-error-top-bottom  t
      switch-to-visible-buffer nil
      history-length           (get-preference 'history-length 1024)
      indicate-empty-lines     t  ; show empty lines at end of file
      kill-read-only-ok        t  ; use kill to copy text in read-only buffer
      initial-major-mode       'lisp-interaction-mode
      python-shell-interpreter (get-preference 'python-command "python"))

(setq paragraph-start paragraph-separate            ; separate paragraphs w/blank lines
      adaptive-fill-regexp "[ \t]*\\([>*%#]+ +\\)?" ; comments and email replies
      sentence-end-double-space nil                 ; Allow frenchspacing
      page-delimiter "^\\(\f\\|\n\n+\\)")           ; pages: FF or 2+ blank lines

(setq print-length 1024            ; Give more information about objects in help
      print-level  8
      eval-expression-print-length 1024 ; ...and in *elisp*
      eval-expression-print-level  8)

(setq display-time-format      "%R"
      display-time-24hr-format t)

(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-new-window-is-tab t
      browse-url-new-window-flag  nil) 

(setq-default major-mode             'org-mode
              fill-column            (get-preference 'fill-column 72)
              case-fold-search       nil
              indent-tabs-mode       nil
              shift-select-mode      nil
              next-line-add-newlines nil
              truncate-lines         t
              indicate-empty-lines   t)

(foreach (s '(tool-bar-mode scroll-bar-mode tooltip-mode delete-selection-mode))
  (if (fboundp s) (funcall s -1))) ; disable some operational modes

(foreach (s '(eval-expression narrow-to-region narrow-to-page scroll-left
              downcase-region upcase-region set-goal-column))
  (put s 'disabled nil)) ; allow some useful operations


;;; Windowing-Specific Operational Settings

(when window-system
  (mouse-wheel-mode  t)
  (blink-cursor-mode t)
  (setq visible-bell t)
  (set-cursor-color (get-preference 'cursor-color)))

(load-local 'frames)


;;; Package imports, hooks, and modifications

(setq package-enable-at-startup t
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ;("SC" . "http://joseito.republika.pl/sunrise-commander/")
                         ))

(load-local 'org-mode :components)
(load-local 'completion :components)
(load-local 'version-control :components)
(load-local 'movement :components)
(load-local 'display :components)
(load-local 'search :components)
(load-local 'dired :components)
(load-local 'tools :components)
(load-local 'programming :components)
(load-local 'projects :components)
(load-local 'paredit :components)
(load-local 'emacs-lisp :components)
(load-local 'clojure :components)
(load-local 'python :components)
(load-local 'common-lisp :components)
(load-local 'cc-modes :components) ; java, c, c++, objective-c
(load-local 'rust :components)
(load-local 'perl :components)
(load-local 'ruby :components)
(load-local 'tex :components)
(load-local 'ess :components)
(load-local 'web :components)

(load-local 'all-mods :mods) ; new tools and commands and/or structural changes


;;; Keymaps and Keybindings

(load-local 'translations) ; key translation table, esp. on Mac OS X
(load-local 'keybindings)


;;; Hooks (especially for built-in modes)

(load-local 'hooks)


;;; Modes and Magic Recognition
;;
;; Some of these are set in individual components, using :mode to use-package

(let ((xml-re
       (concat "\\."
               (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "rss") t)
               "\\'")))
  (defvar my/auto-mode-alist
    `(("\\.org\\'"  . org-mode)
      ("\\.txt\\'"  . org-mode)
      ("\\.js\\'"   . js2-mode)
      ("\\.Rmd\\'"  . markdown-mode)
      ("\\.[rR]\\'" . R-mode)
      ("\\.[sS]\\'" . S-mode)
      ("\\.pde\\'" . arduino-mode)
      (,xml-re . nxml-mode)
      ("\\.\\([pP]\\([LlmM]?\\|od\\|erl\\)\\|al\\)\\'" . perl-mode)
      ("\\.[0-9]\\'" . fundamental-mode))))

(defvar my/magic-mode-alist
  '(("^#!.*inlein" . clojure-mode)
    ("\\\\document\\(style\\|class\\)" . LaTeX-mode)
    ("\\\\input " . plain-TeX-mode)
    ("#!.*\\(t?c\\|ba\\|k\\|z\\|\\)sh" . shell-script-mode)
    ("#!.*python" . python-mode)
    ("#!.*perl" . perl-mode)))

(dolist (mode (reverse my/auto-mode-alist))
  (add-to-list 'auto-mode-alist mode))

(dolist (mode (reverse my/magic-mode-alist))
  (add-to-list 'magic-mode-alist mode))


;;; Initial State

(auto-compression-mode '1)               ; Auto view compressed files
(transient-mark-mode '1)                 ; Highlight region
(turn-on-show-paren-mode)                ; show matching parens
(display-time)                           ; Time on Mode Line
(enable-cua-rectangles)                  ; C-c C-RET starts rectangle

;; Rename scratch buffer if requested in preferences
(when-let ((scratch-name (get-preference 'scratch-buffer))
           (scratch-buf (get-buffer "*scratch*")))  
    (cond
     ((stringp scratch-name)
      (with-current-buffer scratch-buf
        (rename-buffer scratch-name)
        (funcall initial-major-mode)))
     ((eq scratch-name :delete)
      (kill-buffer scratch-buf))))

;; Load extra settings from the environment and from customize
(when user-env-file (load-file user-env-file))  ; my environment variables
(when user-custom-file                          ; saved customize settings
  (setq custom-file user-custom-file)
  (load-file user-custom-file)) 

;; Tidy up minor-mode lighters on the mode line
(ignore-errors
  (when (featurep 'diminish)
    (let ((lighters (get-preference 'lighters)))
      (dolist (lighter lighters)
        (diminish (car lighter) (cdr lighter))))))

;; Load current prototype code, except for names prefixed with '-'
(dolist (proto (directory-files
                (locate-user-emacs-file "init/prototypes") t "\\.el\\'"))
  (when (and (file-exists-p proto) (not (string-match-p "^-" proto)))
    (load-file proto)))

;; Load specified theme, if any
(when (and window-system my/theme-func (fboundp my/theme-func))
  (funcall my/theme-func))


;; Startup actions unless in batch mode or cold loading
(unless (or noninteractive (boundp 'no-user-init-startup-actions))
  (message "Performing startup actions")
  ;; Start in the shell, in a single, full-frame window.
  ;; Name buffer *unix* rather than *shell* because it's easier to complete.
  (let* ((shell-buf (generate-new-buffer-name "*unix*")))
    (shell shell-buf)
    (with-selected-window (get-buffer-window shell-buf)
      (delete-other-windows)))

  ;; Allow access through emacsclient
  (require 'server)
  (unless (server-running-p)
    (setq confirm-kill-emacs #'yes-or-no-p)
    (server-start)
    (global-set-key (kbd "C-x C-3") 'server-edit)))


;; Load local (user-specific) modifications
;;
;; If any .el or .elc exist in local/, subdirectories are added to
;; the load path and all these files are loaded. Files beginning with
;; '-' are excluded from this.
;;
;; With the default .gitignore settings, the local directory is not
;; tracked by git (except for the README file contained within), so
;; it can be used for private data and for any individual
;; personalized changes that are desired. One approach is to mimic
;; the thematic structure of the init directory; another approach
;; is to track it within a personal branch.
;; 

(when-let* ((local-dir (locate-user-emacs-file "init/local"))
            (_ (and (file-readable-p local-dir) (file-directory-p local-dir)))
            (local-files (directory-files local-dir t "^[^-].*\\.el\\'")))
  (let ((default-directory (expand-file-name local-dir)))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path))
  (dolist (local local-files)
    (load (file-name-sans-extension local) t)))


;;; dot-emacs.el ends here
