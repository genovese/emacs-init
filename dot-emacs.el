;;; -*- mode: emacs-lisp; lexical-binding: t; -*-


;;; Setup

;; The script loading this one should initialize the package system, either
;; directly or through cask. Otherwise, we need here something like
;; (require 'package)
;; (package-initialize)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'use-package)
  (setq use-package-verbose t))

(use-package dash)
(use-package s :config (defun s-str (obj) (format "%s" obj)))
(use-package f :config (defalias 'f-slash* 'file-name-as-directory))
(use-package ht)
(use-package bind-key)
(use-package hydra)
(use-package diminish)
(use-package pallet)  ; use with cask to keep up-to-date package list

(defun load-local (name &optional type)
  "Load a package or module in the local init area."
  (let* ((ltype (s-str (or type "")))
         (ltype (if (keywordp type) (substring ltype 1) ltype)))
    (load (f-join user-emacs-directory "init" ltype (s-str name)) t)))

(defvar preferences (ht)
  "Map of symbols to corresponding preferences")

(defmacro set-preferences (&rest kv-pairs)
  "Add one or more preferences given as successive keys and values."
  (declare (indent 1))
  (let ((x (cl-gensym)))
    `(cl-loop for ,x = (list ,@kv-pairs) then (cddr ,x)
              while ,x do
              (puthash (car ,x) (cadr ,x) preferences))))

(defun get-preference (key &optional default)
  "Get preference for KEY."
  (gethash key preferences default))


;;; Load custom startup code

(load-local 'crg-macros)
(load-local 'crg-utils)
(load-local 'crg-ops)
(load-local 'crg-themes)


;;; User and System Data

(defconst my-platform
  (cond ((or (string-equal system-type "darwin")
             (eq window-system 'ns)
             (eq window-system 'mac))
	 'macosx)
	((or (string-equal system-type "ms-dos")
             (string-equal system-type "windows-nt")
	     (string-equal system-type "cygwin"))
	 'windows)
        (t
         'linux))
  "The platform on which we are currently running, derived from
the variables `system-type' and `window-system'. Value is a
symbol. For the moment, all *nix variants are converted to
`linux', though this can be generalized later if needed")

(defvar user-home-directory
  (let ((user-env (getenv "USER"))
        (home-env (getenv "HOME")))
    (cond
     ((and (equal user-login-name user-env) home-env)
      (f-full home-env))
     ((f-directory? (expand-file-name "~/"))
      (f-full (expand-file-name "~/")))
     (t
      (f-full default-directory))))
  "The users home directory, as a directory name with slash appended.")

(defvar user-site-lisp-dir
  (f-slash* (locate-user-emacs-file "site-lisp"))
  "The site-lisp directory (absolute path) containing the globally available
   add-on modes and .el files. This name must end in /.")

(prepend-subtree-to-load-path user-site-lisp-dir)

(setq package-enable-at-startup t
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ;("SC" . "http://joseito.republika.pl/sunrise-commander/")
                         ))

(load-local "preferences" :data)
(defvar user-home-page     (get-preference 'user-home-page))
(defvar user-email-address (get-preference 'user-email-address))

(setq my/custom-file
      (let ((custom (locate-user-emacs-file "emacs-custom.el")))
        (when (f-exists? custom) custom)))

(setq my/env-file
      (let ((envir (locate-user-emacs-file "my-env.el")))
        (when (f-exists? envir) envir)))

(setq my/theme-func (get-preference 'theme-function))


;;; Main operating settings

(setq inhibit-startup-message  t
      load-prefer-newer        t
      case-fold-search         nil
      indent-tabs-mode         nil
      shift-select-mode        nil
      scroll-error-top-bottom  t
      switch-to-visible-buffer nil
      history-length           1024
      initial-major-mode       'lisp-interaction-mode)

(setq-default fill-column            '72
              case-fold-search       nil
              indent-tabs-mode       nil
              shift-select-mode      nil
              next-line-add-newlines nil)

(foreach (s '(tool-bar-mode scroll-bar-mode tooltip-mode delete-selection-mode))
  (if (fboundp s) (funcall s -1))) ; disable some operational modes

(foreach (s '(eval-expression narrow-to-region narrow-to-page scroll-left
              downcase-region upcase-region set-goal-column))
  (put s 'disabled nil)) ; allow some useful operations

(setq paragraph-start paragraph-separate            ; Use blank lines to separate paragraphs by default
      adaptive-fill-regexp "[ \t]*\\([>*%#]+ +\\)?" ; Fill around comment beginnings and yanked-messages
      sentence-end-double-space nil                 ; Allow frenchspacing
      page-delimiter "^\\(\f\\|\n\n+\\)")           ; FF or 2+ consecutive blank lines

(setq print-length 1024                 ; Give more information about objects in help
      print-level  8
      eval-expression-print-length 1024 ; ...and in *elisp*
      eval-expression-print-level  8)

(setq-default major-mode 'org-mode)
(setq display-time-format      "%R"
      display-time-24hr-format t)

(setq kill-read-only-ok     t  ; OK to use kill to copy text in read-only buffer
      indicate-empty-lines  t) ; show empty lines at end of file

(setq my/keep-scratch-buf "*elisp*") ; nil: delete; string: new name; else: as is

(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-new-window-is-tab t
      browse-url-new-window-flag  nil) 

; Use C-M-h for help, leaving M-h for better uses
(setq my/help-events (list [?\M-\C-h])  
      help-event-list (nconc (list ?\M-\C-h) help-event-list) ; only keys allowed
      help-char 0) ; want ?\M-\C-h but that's not a char, causes problems

(setq python-shell-interpreter
      (get-preference 'python-command "python3"))


;;; Windowing-Specific Operational Settings

(when window-system
  (mouse-wheel-mode  t)
  (blink-cursor-mode t)
  (setq visible-bell t)
  (set-cursor-color (get-preference 'cursor-color)))


;;; Frames

(setq initial-frame-alist `((top . 1) (left . 1)
                            (width . 206) (height . 70) ; in Anonymous Pro; 268 x 72 in Andale Mono
                            (cursor-color . ,(get-preference 'cursor-color)))
      default-frame-alist `((top . 32) (left . 16)      ; was 48,24
                            (width . 196) (height . 64)
                            (cursor-color . ,(get-preference 'cursor-color))
                            (menu-bar-lines . 1) (tool-bar-lines . 0)))

(defvar frame-title-prefix nil
  "Optional default label in frame title")

(setq frame-title-format
      '("" (:eval (or (frame-parameter nil 'frame-base-title)
                      frame-title-prefix
                      invocation-name)) " <%b>"))

(defun my/set-frame-title (title &optional frame)
  "Sets the base frame title to TITLE in FRAME, selected fame by default"
  (interactive "sFrame title: ")
  (let ((f (or frame (selected-frame))))
    (set-frame-parameter f 'frame-base-title title)))

(defun my/make-frame-command (&optional name)
  "Like `make-frame-command' but with prefix arg, prompt for frame title."
  (interactive (list (and current-prefix-arg
                          (read-from-minibuffer "Frame Title: "))))
  (let ((set-title? (and name (stringp name) (> (length name) 0)))
        (frame      (make-frame-command)))
    (when set-title?
      (my/set-frame-title name frame))
    frame))


;;; Package imports

(use-package org
  :config (progn
            ;; Need to clarify when org-load-hook is called
            ;; many things set here do not seem to "stick"
            ;; eg., org-file-apps, babel-do-load-languages, ...
            (add-my-hook org-load-hook
              (setq org-startup-folded 'content) ; nil also good
              (setq org-cycle-separator-lines 2)
              (setq org-log-into-drawer t)
              (setq org-special-ctrl-a/e t)
              (setq org-src-window-setup 'current-window) ; try 'other-frame
              (setq org-file-apps   ; not taking, perhaps move to org-mode-hook
                    '((auto-mode . emacs)
                      (directory . emacs)
                      (system . "open %s")
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . system)
                      ("\\.pdf\\'" . system)
                      ("\\.\\(?:png\\|jpe?g\\|gif\\|tiff?\\|bmp\\)\\'" . system)
                      ("\\`\\(?:.*/\\)?[^.]+\\'" . emacs)))
              (org-babel-do-load-languages
               'org-babel-load-languages
               '((emacs-lisp . t)
                 (R . t)
                 (python . t)
                 (clojure . t)
                 (latex . t)
                 (lisp . t)
                 (haskell . t)
                 (C . t)
                 (ruby . t)
                 (org . t)
                 (css . t)
                 (sass . t)
                 (dot . t)
                 (java . t)
                 (ocaml . t)
                 (calc . t)
                 (ditaa . t)
                 (sql . t)
                 (asymptote . t)))
              (setq org-todo-keywords
                    '((sequence "TODO" "WAIT" "DONE")))
              (setq org-agenda-custom-commands '(("A" "Agenda for two week span" agenda ""
                                                  ((org-agenda-span 14) (org-agenda-start-day "-1mon")))))
              (setq-default org-tags-column -116) ;; -96 a good generic choice, -116 better in practice; convert to a function
              (copy-face 'org-todo 'org-wait-face) ; bug with string when doing org-write-agenda
              (set-face-foreground 'org-wait-face "lightgoldenrod2")
              (setq org-todo-keyword-faces '(("WAIT" . org-wait-face))))
            (add-my-hook org-mode-hook
              ;; ATTN: These are in load hook, which should be right
              ;; but they didn't work last time. Check proper placement
              (setq org-todo-keywords
                    '((sequence "TODO" "WAIT" "DONE")))
              (setq org-file-apps   ; not taking, perhaps move to org-mode-hook
                    '((auto-mode . emacs)
                      (directory . emacs)
                      (system . "open %s")
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . system)
                      ("\\.pdf\\'" . system)
                      ("\\.\\(?:png\\|jpe?g\\|gif\\|tiff?\\|bmp\\)\\'" . system)
                      ("\\`\\(?:.*/\\)?[^.]+\\'" . emacs)))              (org-babel-do-load-languages
               'org-babel-load-languages
               '((emacs-lisp . t)
                 (R . t)
                 (python . t)
                 (clojure . t)
                 (latex . t)
                 (lisp . t)
                 (haskell . t)
                 (C . t)
                 (ruby . t)
                 (org . t)
                 (css . t)
                 (sass . t)
                 (dot . t)
                 (java . t)
                 (ocaml . t)
                 (calc . t)
                 (ditaa . t)
                 (sql . t)
                 (asymptote . t)))
              (copy-face 'org-todo 'org-wait-face) ; bug with string when doing org-write-agenda
              (set-face-foreground 'org-wait-face "lightgoldenrod2")
              (setq org-todo-keyword-faces '(("WAIT" . org-wait-face)))
              (setq org-tags-column -116) ;; -96 a good generic choice, -116 better in practice; convert to a function
              ;; END ATTN
              (setq org-archive-location "~/org/archive::")
              (setq org-hide-emphasis-markers t)
              (setq org-src-window-setup 'current-window)
              (setq org-babel-python-command "python3")
              (local-set-key "\C-c\C-x\C-i" 'org-display-inline-images)
              (local-set-key "\C-c\C-xt"    'org-clock-in)
              (local-set-key "\C-c\C-x\M-k" 'org-cut-special)
              (local-set-key "\C-c\C-x\M-c" 'org-copy-special)
              (local-set-key "\C-c\M-c"     'org-edit-src-code) ; see which I like best
              (local-set-key "\C-c\C-v\M-\C-h" 'org-babel-describe-bindings)
              (local-set-key "\C-c\C-vh" 'org-hide-block-all)
              (local-set-key [(control ?c) (control ?v) (control ?\ )] 'org-babel-mark-block)
              (local-set-key [(control ?c) (control ?')] 'org-edit-src-code) ; C-c ' taken by icicles
              (local-set-key [\C-\M-return] 'org-insert-subheading)
              (local-set-key [\C-\M-\S-return] 'org-insert-todo-subheading)
              (local-set-key "\C-\M-q" 'fill-region) ; possibly do some indenting here instead
              (local-unset-key "\M-h") ; new org version shadows backward-kill-word with mark-element
              (local-set-key [(control meta ?\ )] 'org-mark-element)) 

            (add-hook 'org-mode-hook 'turn-on-font-lock)))

(use-package win-switch
  :config (progn
            (win-switch-authors-configuration)
            (setq win-switch-other-window-first nil)
            (setq win-switch-window-threshold 1)))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'post-forward 
              uniquify-separator " in "
              uniquify-after-kill-buffer-p t))

(use-package yasnippet
  :config (setq yas-prompt-functions '(yas/ido-prompt
                                       yas/dropdown-prompt
                                       yas/completing-prompt
                                       yas/x-prompt
                                       yas/no-prompt)))

(use-package fill-column-indicator
  :init (setq-default fci-rule-column 80))

(use-package company
  :config (progn
            (bind-key [(control ?\')] 'company-complete)
            (bind-key "C-d" 'company-show-doc-buffer company-active-map)
            (bind-key [(control return)] 'company-show-location company-active-map)))

(use-package which-func
  :config (setq which-func-modes 
              '(emacs-lisp-mode c-mode c++-mode python-mode
                perl-mode cperl-mode makefile-mode sh-mode
                fortran-mode f90-mode ada-mode)))

(use-package dash-functional)

(use-package calendar
  :init (progn
          (load-local 'calendar :mods)
          (use-package astronomy :config (load-local 'astronomy :mods)))
  :config (setq calendar-font-lock-keywords ; Change how weekends are highlighted 
                (cl-subst 'font-lock-keyword-face 'font-lock-comment-face
                       calendar-font-lock-keywords)))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config (bind-key "C-," 'ace-jump-mode-pop-mark)) ;;ATTN: maybe temporary

(use-package ack)

(use-package ag)

(use-package ripgrep)

(use-package helm
  :config (progn
            (setq helm-split-window-in-side-p       t
                  helm-move-to-line-cycle-in-source t
                  helm-recentf-fuzzy-match          t
                  helm-buffers-fuzzy-matching       t)
            (bind-key "C-v"     'helm-next-page           helm-map)
            (bind-key "C-w"     'helm-previous-page       helm-map)
            (bind-key "M-C-n"   'helm-next-source         helm-map)
            (bind-key "M-C-p"   'helm-previous-source     helm-map)
            (bind-key "M-v"     'helm-end-of-buffer       helm-map)
            (bind-key "M-w"     'helm-beginning-of-buffer helm-map)
            (bind-key "M-k"     'helm-yank-text-at-point  helm-map)
            (cl-loop for n from 0 to 9 do
                     (bind-key (vector (list 'control (+ ?0 n)))
                               `(lambda ()
                                  (interactive)
                                  (helm-select-nth-action ,n))
                               helm-map))
            ;; External helm access bindings
            (bind-key "C-x r h" 'helm-register)
            (bind-key "M-C-y"   'helm-show-kill-ring)
            (bind-key "A-y"     'append-next-kill)))

(use-package auctex)

(use-package reftex
  :init (setq reftex-plug-into-AUCTeX t))

(use-package clojure-mode
  ;:commands clojure-mode
  :demand t
  :config (progn
            (add-my-hook clojure-mode-hook
              (local-set-key "\M-[" 'paredit-wrap-square)
              (local-set-key "\M-{" 'paredit-wrap-curly)
              (local-set-key "\C-ch" 'hs-toggle-hiding)
              (local-set-key "\C-c " 'clojure-cheatsheet)
              (local-set-key "\C-c\C-y" 'my/clojure-snippet-prefix)
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c r")
              (local-set-key "\C-c\C-r" 'cljr-helm)
              (local-set-key "\C-c\C-f" 'find-file-in-project))

            (add-to-list 'same-window-regexps "\\`\\*cider ")

            (add-hook 'clojure-mode-hook 'company-mode)
            (add-hook 'clojure-mode-hook 'eldoc-mode)
            (add-hook 'clojure-mode-hook 'hs-minor-mode)
            (add-hook 'clojure-mode-hook 'subword-mode)
            (add-hook 'clojure-mode-hook 'column-number-mode)
            ;; Done elsewhere, but doesn't hurt to do here either
            (add-hook 'clojure-mode-hook 'enable-paredit-mode)
            (add-hook 'clojure-mode-hook 'fci-mode)
            (add-to-list 'auto-mode-alist '("\\.clj[xc]?\\'"  . clojure-mode))
            (add-to-list 'auto-mode-alist '("\\.cljs\\'"  . clojurescript-mode))))

(use-package cider
  :commands cider-jack-in
  :config (progn
            (add-my-hook cider-repl-mode-hook
              (enable-paredit-mode)
              (subword-mode)
              (local-set-key "\C-cn" 'cider-browse-ns)
              (local-set-key "\C-cN" 'cider-browse-ns-all)
              (local-set-key "\M-{" 'paredit-wrap-curly)
              (local-set-key "\C-c " 'clojure-cheatsheet)
              (setq cider-popup-stacktraces t)
              (setq cider-repl-popup-stacktraces nil))
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-repl-mode-hook 'eldoc-mode)
            (add-hook 'cider-interaction-mode-hook 'company-mode)
            (add-hook 'cider-interaction-mode-hook 'eldoc-mode)
            (add-hook 'cider-mode-hook 'company-mode)
            (add-hook 'cider-mode-hook 'eldoc-mode)))

; ATTN: paredit makes it somewhat painful to make substantial changes
; in the key commands, with all the examples and documentation intact.
; This config uses my adjusted settings (changes marked with CRG)
; and resets the entire keymap and annotations.
(use-package paredit  
  :init
  (setq my/paredit-commands
        `("Basic Insertion Commands"
          ("("         paredit-open-round
           ("(a b |c d)"
            "(a b (|) c d)")
           ("(foo \"bar |baz\" quux)"
            "(foo \"bar (|baz\" quux)"))
          (")"         paredit-close-round
           ("(a b |c   )" "(a b c)|")
           ("; Hello,| world!"
            "; Hello,)| world!"))
          ("M-)"       paredit-close-round-and-newline
           ("(defun f (x|  ))"
            "(defun f (x)\n  |)")
           ("; (Foo.|"
            "; (Foo.)|"))
          ("["         paredit-open-square
           ("(a b |c d)"
            "(a b [|] c d)")
           ("(foo \"bar |baz\" quux)"
            "(foo \"bar [|baz\" quux)"))
          ("]"         paredit-close-square
           ("(define-key keymap [frob|  ] 'frobnicate)"
            "(define-key keymap [frob]| 'frobnicate)")
           ("; [Bar.|"
            "; [Bar.]|"))
        
          ("\""        paredit-doublequote
           ("(frob grovel |full lexical)"
            "(frob grovel \"|\" full lexical)"
            "(frob grovel \"\"| full lexical)")
           ("(foo \"bar |baz\" quux)"
            "(foo \"bar \\\"|baz\" quux)")
           ("(frob grovel)   ; full |lexical"
            "(frob grovel)   ; full \"|lexical"))
          ("M-\""      paredit-meta-doublequote
           ("(foo \"bar |baz\" quux)"
            "(foo \"bar baz\"| quux)")
           ("(foo |(bar #\\x \"baz \\\\ quux\") zot)"
            ,(concat "(foo \"|(bar #\\\\x \\\"baz \\\\"
                     "\\\\ quux\\\")\" zot)")))
          ("\\"        paredit-backslash
           ("(string #|)\n  ; Character to escape: x"
            "(string #\\x|)")
           ("\"foo|bar\"\n  ; Character to escape: \""
            "\"foo\\\"|bar\""))
          (";"         paredit-semicolon
           ("|(frob grovel)"
            ";|(frob grovel)")
           ("(frob |grovel)"
            "(frob ;|grovel\n )")
           ("(frob |grovel (bloit\n               zargh))"
            "(frob ;|grovel\n (bloit\n  zargh))")
           ("(frob grovel)          |"
            "(frob grovel)          ;|"))
          ("M-;"       paredit-comment-dwim
           ("(foo |bar)   ; baz"
            "(foo bar)                               ; |baz")
           ("(frob grovel)|"
            "(frob grovel)                           ;|")
           ("(zot (foo bar)\n|\n     (baz quux))"
            "(zot (foo bar)\n     ;; |\n     (baz quux))")
           ("(zot (foo bar) |(baz quux))"
            "(zot (foo bar)\n     ;; |\n     (baz quux))")
           ("|(defun hello-world ...)"
            ";;; |\n(defun hello-world ...)"))
        
          ("C-j"       paredit-newline
           ("(let ((n (frobbotz))) |(display (+ n 1)\nport))"
            ,(concat "(let ((n (frobbotz)))"
                     "\n  |(display (+ n 1)"
                     "\n           port))")))

          "Deleting & Killing"
          (("C-d" "<delete>" "<deletechar>")
           paredit-forward-delete
           ("(quu|x \"zot\")" "(quu| \"zot\")")
           ("(quux |\"zot\")"
            "(quux \"|zot\")"
            "(quux \"|ot\")")
           ("(foo (|) bar)" "(foo | bar)")
           ("|(foo bar)" "(|foo bar)"))
          ("DEL"
           paredit-backward-delete
           ("(\"zot\" q|uux)" "(\"zot\" |uux)")
           ("(\"zot\"| quux)"
            "(\"zot|\" quux)"
            "(\"zo|\" quux)")
           ("(foo (|) bar)" "(foo | bar)")
           ("(foo bar)|" "(foo bar|)"))
          ("C-k"       paredit-kill
           ("(foo bar)|     ; Useless comment!"
            "(foo bar)|")
           ("(|foo bar)     ; Useful comment!"
            "(|)     ; Useful comment!")
           ("|(foo bar)     ; Useless line!"
            "|")
           ("(foo \"|bar baz\"\n     quux)"
            "(foo \"|\"\n     quux)"))
          ("M-d"       paredit-forward-kill-word
           ("|(foo bar)    ; baz"
            "(| bar)    ; baz"
            "(|)    ; baz"
            "()    ;|")
           (";;;| Frobnicate\n(defun frobnicate ...)"
            ";;;|\n(defun frobnicate ...)"
            ";;;\n(| frobnicate ...)"))
          ("M-h" ;CRG  -- was ,(concat "M-" paredit-backward-delete-key)
           paredit-backward-kill-word
           ("(foo bar)    ; baz\n(quux)|"
            "(foo bar)    ; baz\n(|)"
            "(foo bar)    ; |\n()"
            "(foo |)    ; \n()"
            "(|)    ; \n()"))

          "Movement & Navigation"
          ("C-M-f"     paredit-forward
           ("(foo |(bar baz) quux)"
            "(foo (bar baz)| quux)")
           ("(foo (bar)|)"
            "(foo (bar))|"))
          ("C-M-b"     paredit-backward
           ("(foo (bar baz)| quux)"
            "(foo |(bar baz) quux)")
           ("(|(foo) bar)"
            "|((foo) bar)"))
          ("C-M-u"     paredit-backward-up)
          ("C-M-d"     paredit-forward-down)
          ("C-M-p"     paredit-backward-down) ; Built-in, these are FORWARD-
          ("C-M-n"     paredit-forward-up) ; & BACKWARD-LIST, which have
                                        ; no need given C-M-f & C-M-b.
        
          "Depth-Changing Commands"
          ("M-("       paredit-wrap-round
           ("(foo |bar baz)"
            "(foo (|bar) baz)"))
          ("M-i k"     paredit-splice-sexp ; CRG -- was "M-s"
           ("(foo (bar| baz) quux)"
            "(foo bar| baz quux)"))
          ("M-i u"                    ; CRG -- was ("M-<up>" "ESC <up>")
           paredit-splice-sexp-killing-backward
           ("(foo (let ((x 5)) |(sqrt n)) bar)"
            "(foo |(sqrt n) bar)"))
          ("M-i o"                ; CRG -- was ("M-<down>" "ESC <down>")
           paredit-splice-sexp-killing-forward
           ("(a (b c| d e) f)"
            "(a b c| f)"))
          ("M-i i"     paredit-raise-sexp ; CRG -- was "M-r"
           ("(dynamic-wind in (lambda () |body) out)"
            "(dynamic-wind in |body out)"
            "|body"))
          ("M-i c"     paredit-convolute-sexp ; CRG -- was "M-?"
           ("(let ((x 5) (y 3)) (frob |(zwonk)) (wibblethwop))"
            "(frob |(let ((x 5) (y 3)) (zwonk) (wibblethwop)))"))

          "Barfage & Slurpage"
          (("M-i 0" "C-)" "C-<right>")  ; CRG -- added mine
           paredit-forward-slurp-sexp
           ("(foo (bar |baz) quux zot)"
            "(foo (bar |baz quux) zot)")
           ("(a b ((c| d)) e f)"
            "(a b ((c| d) e) f)"))
          (("M-i )" "C-}" "C-<left>")   ; CRG -- added mine
           paredit-forward-barf-sexp
           ("(foo (bar |baz quux) zot)"
            "(foo (bar |baz) quux zot)"))
          (("M-i 9" "C-(" "C-M-<left>" "ESC C-<left>") ; CRG -- added mine 
           paredit-backward-slurp-sexp
           ("(foo bar (baz| quux) zot)"
            "(foo (bar baz| quux) zot)")
           ("(a b ((c| d)) e f)"
            "(a (b (c| d)) e f)"))
          (("M-i (" "C-{" "C-M-<right>" "ESC C-<right>") ; CRG -- added mine
           paredit-backward-barf-sexp
           ("(foo (bar baz |quux) zot)"
            "(foo bar (baz |quux) zot)"))

          "Miscellaneous Commands"
          ("M-i l"     paredit-split-sexp ; CRG -- was "M-S"
           ("(hello| world)"
            "(hello)| (world)")
           ("\"Hello, |world!\""
            "\"Hello, \"| \"world!\""))
          ("M-i j"     paredit-join-sexps ; CRG -- was "M-J"
           ("(hello)| (world)"
            "(hello| world)")
           ("\"Hello, \"| \"world!\""
            "\"Hello, |world!\"")
           ("hello-\n|  world"
            "hello-|world"))
          (("M-i RET" "C-c C-M-l") paredit-recenter-on-sexp) ; CRG -- added mine
          ("M-q"       paredit-reindent-defun)))
  :config (progn      
            (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode t)
            (add-hook 'lisp-mode-hook #'enable-paredit-mode t)
            (add-hook 'clojure-mode-hook #'enable-paredit-mode)
            (define-prefix-command 'paredit-sexp-map) 
            (define-key paredit-mode-map [(meta ?i)] 'paredit-sexp-map)
            (setq paredit-commands my/paredit-commands)
            (paredit-define-keys)
            (paredit-annotate-mode-with-examples)))

(use-package clojure-cheatsheet
  :commands clojure-cheatsheet)

(use-package clj-refactor
  :commands clj-refactor-mode)

(use-package magit
  :config (progn
            (defalias 'magit 'magit-status)
            ; (global-set-key "\M-gm" 'magit-status) ;; put in my/go-map
            (define-key magit-mode-map "\C-w" 'scroll-down-command)
            (define-key magit-mode-map "\M-w" 'beginning-of-buffer)
            (define-key magit-mode-map [?\A-w] 'magit-copy-section-value)
            (define-key magit-mode-map [?\s-W] 'magit-copy-buffer-revision)
            (define-key magit-mode-map [?\A-W] 'magit-copy-buffer-revision)
            (define-key magit-mode-map "\M-k" 'magit-copy-item-as-kill)
            (add-to-list 'same-window-regexps "\\`\\*magit: ")
            ;; NEW VERSION OF EMACS AND MAGIT  (VERSION DEPENDENT) OCT 2013
            (-when-let (client (executable-find "emacsclient"))
              (setq magit-emacsclient-executable client))))

(use-package smex
  ;:init (progn
  ;        (bind-key "M-x" 'smex)
  ;        (bind-key "M-X" 'smex-major-mode-commands))
  :config (progn
            (bind-key "M-x" 'smex)
            (bind-key "M-X" 'smex-major-mode-commands)
            (setq smex-save-file (locate-user-emacs-file "smex-items"))
            (advice-add #'smex-prepare-ido-bindings :override
              (lambda  ()
                (let ((m (make-keymap)))
                  (define-key m (kbd "f") 'smex-describe-function)
                  (define-key m (kbd "w") 'smex-where-is)
                  (define-key ido-completion-map (kbd "M-C-h") m))
                (define-key ido-completion-map (kbd "TAB") 'minibuffer-complete)
                (define-key ido-completion-map (kbd "M-.") 'smex-find-function)
                (define-key ido-completion-map (kbd "C-a") 'move-beginning-of-line))
              '((name . my-smex-ido-help)))
            (smex-initialize)))

(use-package my-workgroups
  :config (progn
            (setq wg-prefix-key "\C-cw")
            (wg-set-prefix-key)
            (setq wg-default-buffer "*unix*")
            (setq wg-file "~/.emacs.d/saved-workgroups/u13a.el")
            (setq wg-morph-on nil)
            (wg-set-prefix-key)
            (global-set-key "\C-cwl" 'wg-echo-all-workgroups)))

(use-package gtags
  :commands gtags-mode
  :defer t)

(use-package dired-aux)
(use-package dired-x)
(use-package dired+
  :init (setq diredp-hide-details-initially-flag nil)
  :config (progn
            (set-face-foreground 'diredp-dir-priv    "#33cc33") ; was "magenta3"
            (set-face-background 'diredp-dir-priv    nil)
            (set-face-foreground 'diredp-file-suffix "cornflower blue")
            (set-face-foreground 'diredp-file-name   "#E0CF9F")
            (set-face-foreground 'diredp-number      "gray60")
            (set-face-foreground 'diredp-dir-heading "Blue")
            (set-face-background 'diredp-dir-heading "bisque1")
            (set-face-background 'diredp-no-priv     "black")
            (set-face-foreground 'diredp-date-time   "#74749A9AF7F7")))
(use-package dired-filter)
(use-package dired-open)
(use-package dired-subtree) ;ATTN: set prefix command for subtree commands in C-/
(use-package dired-ranger)

(use-package ess
  :defer t
  :init (progn
          (setq ess-R-smart-operators t) ; smart commas only for now
          (autoload 'R-mode "ess-site"
            "Major mode for editing R source.  See `ess-mode' for more help."
            t))
  :config (progn
            (eval-after-load 'ess-smart-equals
              '(progn
                 (add-hook 'ess-mode-hook 'ess-smart-equals-mode)
                 (add-hook 'inferior-ess-mode-hook 'ess-smart-equals-mode)))
            (require 'ess-smart-equals nil t)))

(use-package expand-region
  :bind (("C-="   . er/expand-region)
         ([?\A- ] . er/expand-region)
         ("C-+"   . er/contract-region)
         ([?\s- ] . er/contract-region)))

(use-package smart-mode-line
  :config (setq sml/theme 'respectful))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package macrostep
  :commands macrostep-expand
  :config (progn
            (bind-key "C-c e" 'macrostep-expand lisp-interaction-mode-map)
            (bind-key "C-c e" 'macrostep-expand emacs-lisp-mode-map)))

(use-package find-file-in-project
  :commands (find-file-in-project find-file-in-project-by-selected)
  :config (add-to-list 'ffip-project-file "project.clj" t))

(use-package ivy
  :config
  (progn
    (define-key ivy-minibuffer-map (kbd "M-v") nil)
    (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-scroll-down-command)
    (setq ivy-wrap t)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (setq counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
    (ivy-mode 1)))

(use-package rust-mode
  :commands rust-mode
  :mode "\\.rs\\'")


(use-package auctex-latexmk
  :defer t
  :config (progn
            (setq auctex-latexmk-inherit-TeX-PDF-mode t)
            (auctex-latexmk-setup)))

(use-package counsel-projectile)

(use-package projectile
  :config (progn
            (counsel-projectile-on)
            (projectile-mode 1)))

(use-package simple-call-tree
  :commands simple-call-tree-display-buffer)


;;; Local initial mods

(load-local 'init-mods :mods)

;;; Keymaps and Keybindings

;; Modifier Keys and Mouse Emulation (super/fn for mouse2, option for mouse3)

(setq ns-command-modifier  'meta) 
(setq ns-option-modifier   'alt)
(setq ns-function-modifier 'super)

(define-key key-translation-map     [s-mouse-1]          [mouse-2])
(define-key key-translation-map   [C-s-mouse-1]        [C-mouse-2])
(define-key key-translation-map   [M-s-mouse-1]      [M-C-mouse-2])

(define-key key-translation-map   [s-S-mouse-1]     [down-mouse-2])
(define-key key-translation-map [C-s-S-mouse-1]   [C-down-mouse-2])
(define-key key-translation-map [M-s-S-mouse-1] [M-C-down-mouse-2])

(define-key key-translation-map     [A-mouse-1]          [mouse-3])
(define-key key-translation-map   [C-A-mouse-1]        [C-mouse-3])
(define-key key-translation-map   [M-A-mouse-1]      [M-C-mouse-3])

(define-key key-translation-map   [A-S-mouse-1]     [down-mouse-3])
(define-key key-translation-map [C-A-S-mouse-1]   [C-down-mouse-3])
(define-key key-translation-map [M-A-S-mouse-1] [M-C-down-mouse-3])

;; System Shadowing

(define-key key-translation-map [(alt ?\ )] nil) ; Alt-space unshadowed
(define-key key-translation-map [(alt ?u)] nil)

;; Auxilliary Keymaps

(define-prefix-command 'my/string-replace-prefix 'my/string-replace-map
  "String Replacement")
(define-key my/string-replace-map "q" 'query-replace)
(define-key my/string-replace-map "x" 'query-replace-regexp)
(define-key my/string-replace-map "s" 'replace-string)
(define-key my/string-replace-map "r" 'replace-regexp)
(define-key my/string-replace-map "t" 'tags-query-replace)
(define-key my/string-replace-map "f" 'flush-lines)
(define-key my/string-replace-map "k" 'keep-lines)
(define-key my/string-replace-map "#" 'how-many)
(define-key my/string-replace-map "n" 'how-many)
(define-key my/string-replace-map "h" 'highlight-regexp)
(define-key my/string-replace-map "o" 'occur)
(define-key my/string-replace-map "m" 'multi-occur)

(define-prefix-command 'my/transpose-prefix 'my/transpose-map
  "Transpose Commands")
(define-key my/transpose-map "c"   'transpose-chars)
(define-key my/transpose-map "l"   'transpose-lines)
(define-key my/transpose-map "p"   'transpose-paragraphs)
(define-key my/transpose-map "s"   'transpose-sentences)
(define-key my/transpose-map "w"   'transpose-words)
(define-key my/transpose-map "x"   'transpose-sexps)

(define-prefix-command 'my/go-prefix 'my/go-map
  "Go")
(define-key my/go-map "g"      'goto-line)
(define-key my/go-map "\M-g"   'goto-line)
(define-key my/go-map "h"      'help-command)
(define-key my/go-map "m"      'magit-status)        ; package dependence
(define-key my/go-map "w"      'helm-google-suggest) ; package dependence
(define-key my/go-map "n"      'next-error)
(define-key my/go-map "\M-n"   'next-error)
(define-key my/go-map "o"      'org-open-at-point)
(define-key my/go-map "p"      'previous-error)
(define-key my/go-map "\M-p"   'previous-error)
(define-key my/go-map "q"      'keyboard-quit)  
(define-key my/go-map "Q"      'keyboard-escape-quit)  
(define-key my/go-map "t"      'top-level)
(define-key my/go-map "u"      'browse-url)
(define-key my/go-map "x"      'exit-recursive-edit)
(define-key my/go-map "c"      'goto-char)
(define-key my/go-map [tab]    'move-to-column)
(define-key my/go-map [escape] 'keyboard-escape-quit)

(define-prefix-command 'my-search-prefix 'my-search-map
  "Search")
(define-key my-search-map "a" 'ack)
(define-key my-search-map "g" 'ag)
(define-key my-search-map "h" 'helm) ;ATTN
(define-key my-search-map "o" 'helm-occur)
(define-key my-search-map "r" 'ripgrep-regexp)
(define-key my-search-map "\C-f" 'find-file-in-project)
(define-key my-search-map "\M-f" 'find-file-in-project-by-selected)
(define-key my-search-map "f" 'helm-find-files)


(define-prefix-command 'my-links-prefix 'my-links-map
  "Links")
(define-key my-links-map "o" 'org-store-link)

(define-prefix-command 'my/tags-prefix 'my/tags-map
  "Tag Management")
(define-key my/tags-map "l"   'list-tags)
(define-key my/tags-map "."   'find-tag)
(define-key my/tags-map "f"   'find-tag)
(define-key my/tags-map "r"   'find-tag-regexp)
(define-key my/tags-map "4"   'find-tag-other-window)
(define-key my/tags-map "5"   'find-tag-other-frame)
(define-key my/tags-map "n"   'find-tag-noselect)
(define-key my/tags-map "c"   'tags-loop-continue)
(define-key my/tags-map "s"   'tags-search)
(define-key my/tags-map "\C-i" 'complete-tag)
(define-key my/tags-map "t"   'select-tags-table)
(define-key my/tags-map "v"   'visit-tags-table)
(define-key my/tags-map "z"   'tags-reset-tags-table)
(define-key my/tags-map "a"   'tags-apropos)
(define-key my/tags-map "q"   'tags-query-replace)

(define-prefix-command 'my-abbrev-prefix 'my-abbrev-map
  "Abbreviations")
(define-key my-abbrev-map "c"      'dabbrev-completion)
(define-key my-abbrev-map "d"      'define-global-abbrev)
(define-key my-abbrev-map "\C-d"   'define-mode-abbrev)
(define-key my-abbrev-map "e"      'expand-abbrev)
(define-key my-abbrev-map "g"      'add-global-abbrev)
(define-key my-abbrev-map "\C-g"   'inverse-add-global-abbrev)
(define-key my-abbrev-map "k"      'kill-all-abbrevs)
(define-key my-abbrev-map "l"      'list-abbrevs)
(define-key my-abbrev-map "m"      'add-mode-abbrev)
(define-key my-abbrev-map "\C-m"   'inverse-add-mode-abbrev)
(define-key my-abbrev-map "r"      'expand-region-abbrevs)
(define-key my-abbrev-map "u"      'unexpand-abbrev)
(define-key my-abbrev-map "v"      'edit-abbrevs)
(define-key my-abbrev-map "x"      'dabbrev-expand)
(define-key my-abbrev-map " "      'set-abbrev-mark)

(define-prefix-command 'my-org-prefix 'my-org-map
  "Org Mode")
(define-key my-org-map "a" 'org-agenda)
(define-key my-org-map "A" 'org-attach)
(define-key my-org-map "b" 'org-babel-demarcate-block)
(define-key my-org-map "c" 'org-capture)
(define-key my-org-map "l" 'org-store-link)
(define-key my-org-map "L" 'org-insert-link-global)
(define-key my-org-map "o" 'org-open-at-point)
(define-key my-org-map "O" 'org-open-at-point-global)
(define-key my-org-map "3" 'calendar)

(define-prefix-command 'my-icicles-prefix 'my-icicles-map
  "Icicles")
(define-key my-icicles-map "b" 'icicle-search-buffer)
(define-key my-icicles-map "c" 'icicle-imenu-command)
(define-key my-icicles-map "d" 'icicle-search-defs)
(define-key my-icicles-map "f" 'icicle-search-file)
(define-key my-icicles-map "g" 'icicle-search-generic)
(define-key my-icicles-map "G" 'icicle-grep-saved-file-candidates)
(define-key my-icicles-map "i" 'icicle-imenu)
(define-key my-icicles-map "k" 'icicle-search-keywords)
(define-key my-icicles-map "l" 'icicle-search-lines)
(define-key my-icicles-map "m" 'icicle-search-bookmark)
(define-key my-icicles-map "M" 'icicle-search-bookmarks-together)
(define-key my-icicles-map "o" 'icicle-occur)
(define-key my-icicles-map "s" 'icicle-search-paragraphs)
(define-key my-icicles-map "s" 'icicle-search)
(define-key my-icicles-map "t" 'icicle-search-thing)
(define-key my-icicles-map "T" 'icicle-complete-thesaurus-entry) ; from "\C-c/" 
(define-key my-icicles-map "v" 'icicle-search-overlay-property)
(define-key my-icicles-map "w" 'icicle-search-word)
(define-key my-icicles-map "x" 'icicle-search-xml-element)
(define-key my-icicles-map "X" 'icicle-search-xml-element-text-node)
(define-key my-icicles-map "`" 'icicle-compilation-search)
(define-key my-icicles-map "?" 'icicle-search-char-property) 
(define-key my-icicles-map " " 'icicle-comint-search)
(define-key my-icicles-map "," 'icicle-tags-search)
(define-key my-icicles-map "." 'icicle-search-sentences)
(define-key my-icicles-map "\"" 'icicle-search-text-property)
(define-key my-icicles-map "\C-l" 'icicle-search-pages)
(define-key my-icicles-map [tab] 'icicle-comint-command)
(define-key my-icicles-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys

;; Global Keybindings

;; Movement
(global-set-key "\C-a"        'my/move-beginning-of-line)
(global-set-key "\C-w"        'scroll-down-command)
(global-set-key "\C-v"        'scroll-up-command)
(global-set-key "\M-v"        'end-of-buffer)
(global-set-key "\M-w"        'beginning-of-buffer)
(global-set-key "\M-\C-v"     'scroll-other-window)
(global-set-key "\M-\C-w"     'scroll-other-window-down)
(global-set-key "\M-p"        'backward-up-list)
(global-set-key "\M-n"        'up-list)
(global-set-key "\M-\C-f"     'forward-sexp-or-char)
(global-set-key "\M-\C-b"     'backward-sexp-or-char)
(global-set-key [?\A-a]       'back-to-indentation)

(cl-macrolet ((by-five (cmd)  ; Move more quickly
                `(lambda ()
                   (interactive)
                   (ignore-errors (,cmd 5)))))
  (bind-key "C-S-n" (by-five next-line))
  (bind-key "C-S-p" (by-five previous-line))
  (bind-key "C-S-f" (by-five forward-char))
  (bind-key "C-S-b" (by-five backward-char))
  (bind-key [(meta shift ?f)] (by-five forward-word))
  (bind-key [(meta shift ?b)] (by-five backward-word)))

;; Basic Functions
(global-set-key "\C-x\C-m"    'execute-extended-command)
(global-set-key "\C-c\C-m"    'execute-extended-command)
(global-set-key "\C-x\M-m"  'lacarte-execute-command)
(global-set-key "\C-c\M-m"  'lacarte-execute-command)
(global-set-key [?\C-0]       'digit-argument) 
(global-set-key "\C-x\C-d"    'dired)
(global-set-key "\C-x4\C-d"   'dired-other-window)
(global-set-key "\C-xd"       'list-directory)
(global-set-key "\M-\C-]"     'top-level)
(global-set-key "\C-\M-g"     'keyboard-escape-quit)

;; Help
;;   I've moved the help command to enable more efficient
;;   access to some editing commands. But this necessitates
;;   a bit of extra work to ensure consistency elswhere,
;;   especially isearch and icicles.
(define-key help-map "\C-c"   'describe-key-briefly)
(define-key help-map "\M-c"   'describe-copying)
(define-key help-map "\C-f"   'describe-face)
(define-key help-map "E"      'view-emacs-FAQ)
(define-key help-map "F"      'describe-face)
(define-key help-map "I"      'Info-goto-emacs-command-node) ; for help-mode consistency
(define-key help-map "\M-i"   'describe-input-method)        ; not often needed
(define-key help-map "T"      'describe-text-properties) 
(dolist (event my/help-events)
  (global-set-key event 'help-command)) 

;; Searching
(global-set-key "\M-s"    'isearch-forward-regexp)
(global-set-key "\M-r"    'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'query-replace)
(global-set-key "\C-\M-r" 'query-replace-regexp)
(global-set-key [?\C-5]   'query-replace)
(global-set-key [?\C-6]   'query-replace-regexp)
(global-set-key "\C-x "   'my-search-prefix)
(defalias 'qr  'query-replace)
(defalias 'qrr 'query-replace-regexp)
;; To end a search with a specially defined key, like a keypad key
;; and to have the search not automatically continue with a single
;; C-s, do the following for the key.  For example, this makes
;; kp-2 act like a proper search ender.
;; (define-key isearch-mode-map [kp-2] 'isearch-other-control-char)
(define-key isearch-mode-map "\M-w" 'isearch-yank-word-or-char)
(define-key isearch-mode-map "\C-w" 'isearch-other-control-char)
(define-key isearch-mode-map [(control return)] 'isearch-exit)
(define-key isearch-mode-map (car my/help-events) (lookup-key isearch-mode-map "\C-h"))
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(define-key minibuffer-local-isearch-map "\M-s" 'isearch-forward-exit-minibuffer)
(define-key minibuffer-local-isearch-map "\M-r" 'isearch-reverse-exit-minibuffer)
(define-key minibuffer-local-isearch-map [(control meta tab)] 'isearch-complete-edit)
(define-key minibuffer-local-isearch-map [(meta control ?i)]  nil)

;; Kills and Copies
(global-set-key "\C-k"            'kill-line)
(global-set-key "\M-k"            'kill-region)
(global-set-key "\M-\C-k"         'kill-sexp)
(global-set-key [?\S-\C-k]        'kill-sentence)
(global-set-key [?\S-\M-\C-k]     'kill-paragraph)
(global-set-key [?\A-k]           'kill-whole-line)
(global-set-key [?\M-B]           'backward-kill-word)
(global-set-key [?\A-b]           'backward-kill-word)
(global-set-key [\M-backspace]    'backward-kill-word)
(global-set-key [?\A-K]           'backward-kill-sentence)
(global-set-key [?\S-\C-\A-k]     'backward-kill-paragraph)
(global-set-key [?\C-\A-k]        'backward-kill-sexp)
(global-set-key [\C-\S-backspace] 'backward-kill-sentence)
(global-set-key [\C-\M-backspace] 'backward-kill-sexp)
(global-set-key "\M-c"            'my/kill-ring-save) ;alternate: my/copy-region-as-kill
(global-set-key "\M-\C-y"         'append-next-kill)
(global-set-key "\C-z"            'zap-up-to-char)
(global-set-key "\M-z"            'undo)
(global-set-key "\C-h"            'delete-backward-char) ; Move help command below
(global-set-key "\M-h"            'backward-kill-word)
(global-set-key "\C-cd"           'yank-cwd)

;; Marking
(global-set-key [?\M- ]       'mark-sexp)
(global-set-key [?\C-\M- ]    'mark-defun)
(global-set-key "\M-m"        'mark-paragraph)
(global-set-key "\C-\M-m"     'mark-end-of-sentence)
(global-set-key [?\S-\C-\M-m] 'mark-page)
(global-set-key [?\A-m]       'mark-whole-buffer)
(global-set-key [?\A-M]       'mark-word)

;; Buffers, Files, Windows, and Frames
(global-set-key (kbd "C-x 5 2")  'my/make-frame-command)
(global-set-key "\C-x\C-b"       'ibuffer-list-buffers-select)
(global-set-key [?\C-x ?\C-\S-b] 'ibuffer-list-buffers) ; no select and files arg
(global-set-key "\C-xy"          'bury-buffer)
(global-set-key "\C-x\M-w"       'append-to-file)
(global-set-key "\C-x\M-\C-f"    'find-file-at-point)
(global-set-key "\C-x:"          'view-url-in-buffer)
(global-set-key "\C-\M-z"        'iconify-or-deiconify-frame)
(global-set-key [?\C-8]          'toggle-window-dedication)
;(global-set-key "\C-xo"          'other-window)    ;; now handled by win-switch
;(global-set-key "\C-x\C-o"       'other-window)        

;; Positioning
(global-set-key "\M-l"        'reposition-window)
(global-set-key "\M-\C-l"     'move-to-window-line)
(global-set-key "\C-xL"       'count-lines-page) ;; ATTN replace with something smarter
(global-set-key "\C-xl"       'what-line)
(global-set-key "\C-xg"       'goto-line)    ;; ATTN: used to be C-xt
(global-set-key "\M-g"        'my/go-prefix) ;; replaces similar map

;; Macros   
(global-set-key "\C-x\M-k"    'kmacro-keymap)
(global-set-key [?\C-3]       'kmacro-start-macro-or-insert-counter)
(global-set-key [?\C-4]       'kmacro-end-or-call-macro)

;; Miscellaneous Operations
(global-set-key [?\C-7]       'align-regexp)  ;; very useful! (mnemnoic: & for align)
(global-set-key "\C-x7"       'align-regexp)  
(global-set-key [?\A-q]       'unfill-paragraph)
(global-set-key [\A-return]   'delete-blank-lines)
(global-set-key [?\A- ]       'cycle-spacing)  ; was just-one-space
(global-set-key [(meta ?\\)]  'delete-whitespace)
(global-set-key [?\C-\A- ]    'delete-trailing-whitespace)
(global-set-key "\C-\\"       'comment-dwim)
(global-set-key "\M-/"        'hippie-expand)

;; Rectangle and Register Commands
(global-set-key "\C-xra"      'append-to-register)
(global-set-key "\C-xrc"      'copy-to-register)            
(global-set-key "\C-xrl"      'list-registers)            
(global-set-key "\C-xrp"      'prepend-to-register)
(global-set-key "\C-xrs"      'string-rectangle)
(global-set-key "\C-xrt"      'register-to-point)
(global-set-key "\C-xrv"      'view-register)
(global-set-key "\C-xrx"      'clear-rectangle)
(global-set-key "\C-xr."      'point-to-register)
(global-set-key "\C-xrr"      'rectangle-mark-mode)


;; Attach auxilliary keymaps and related rebindings
(define-key ctl-x-map "t"  'my-tags-prefix)  ;; ATTN: used to be C-xg (see goto-line above)
(define-key ctl-x-map "%"  'my-string-replace-prefix)
(define-key ctl-x-map "m"  'my-mark-prefix)
(define-key ctl-x-map "a"  'my-abbrev-prefix)
(global-set-key [?\M-T]    'my-transpose-prefix)
(global-set-key [?\A-t]    'my-transpose-prefix)

;; Modified Arrow Keys get added functionality
(windmove-default-keybindings 'control) ;  [C-left] [C-right] [C-up] [C-down]
(setq windmove-wrap-around t) ; obviated by win-switch
(global-set-key [(alt left)]  'shrink-window-horizontally)
(global-set-key [(alt right)] 'enlarge-window-horizontally)
(global-set-key [(alt up)]    'enlarge-window)
(global-set-key [(alt down)]  `shrink-window)
(global-set-key [M-left]      'backward-word)
(global-set-key [M-right]     'forward-word)
(global-set-key [C-M-left]    'backward-list)
(global-set-key [C-M-right]   'forward-list)
(global-set-key [M-up]        'backward-paragraph)
(global-set-key [M-down]      'forward-paragraph)
(global-set-key [C-M-up]      'backward-page)
(global-set-key [C-M-down]    'forward-page)

;; Third-Party Mode Global Access Keys
(global-set-key "\C-ci"       'my-icicles-prefix)
(global-set-key "\C-cl"       'my-links-prefix)
(global-set-key "\C-co"       'my-org-prefix)
(global-set-key "\C-x\M-\C-o" 'icicle-switch-to/from-minibuffer)
(global-set-key [?\e ?\M-x]   'lacarte-execute-command)
(global-set-key [?\M-`]       'lacarte-execute-command)

;;; Hooks (especially for built-in modes)

(defun isearch-yank-hook ()
  (make-local-variable 'isearch-mode-map)
  (define-key isearch-mode-map "\M-w" 'isearch-yank-lisp-symbol))
  ; consider making this \C-w  because it is easier to use

(add-hook 'minibuffer-exit-hook 'my/bury-completions t)

(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(add-hook 'view-mode-hook 'visual-line-mode) ; viewing is easier with wrapping, no editing ambiguity

(add-my-hook emacs-lisp-mode-hook
  (local-set-key "\C-c\C-e" 'eval-defun)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-s" 'eval-last-sexp)
  (local-set-key [(control ?\;)] 'comment-indent-new-line)
  (defun emacs-lisp-outline-minor-setup ()
    (setq outline-regexp ";;; \\|;; \\|(....")
    (local-set-key "\C-c\C-m" outline-mode-prefix-map)
    (when (featurep 'org)
      (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
      (define-key outline-minor-mode-map [(shift tab)]   'org-global-cycle))) 
  (make-local-variable 'outline-minor-mode-hook)
  (add-hook 'outline-minor-mode-hook 'emacs-lisp-outline-minor-setup))

(add-hook 'emacs-lisp-mode-hook 'highlight-attn-words t)
(add-hook 'emacs-lisp-mode-hook 'isearch-yank-hook)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode t)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode t)
(add-hook 'emacs-lisp-mode-hook 'fci-mode t)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(font-lock-add-keywords 'emacs-lisp-mode
 '(("(\\(use-package\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    1 'font-lock-keyword-face)))


(add-hook 'inferior-emacs-lisp-mode-hook 'enable-paredit-mode)

(add-my-hook lisp-interaction-mode-hook
  "Elisp interaction as in *elisp* buffer."
  (local-set-key "\C-j"     'newline-and-indent)
  (local-set-key [C-return] 'eval-print-last-sexp)
  (local-set-key "\C-c\C-c" 'eval-print-last-sexp))
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)

(add-hook 'lisp-mode-hook 'enable-paredit-mode t)
(add-hook 'lisp-mode-hook 'fci-mode t)

(add-my-hook python-mode-hook
  (local-set-key "\M-\C-a"  'beginning-of-python-def-or-class)
  (local-set-key [(control ?\;)] 'comment-indent-new-line))
(add-hook 'python-mode-hook 'fci-mode t)

(add-hook 'perl-mode-hook 'highlight-attn-words)
(add-my-hook perl-mode-hook
  "Use C-style indenting"
  (setq perl-indent-level '4)
  (setq perl-continued-statement-offset '4)
  (setq perl-continued-brace-offset '-4)
  (setq perl-brace-offset '0)
  (setq perl-brace-imaginary-offset '0)
  (setq indent-tabs-mode 'nil) 
  (setq perl-label-offset '-2))

(add-my-hook lisp-mode-hook
  "Lisp with slime"
  :append t
  (local-set-key [(control ?\;)] 'comment-indent-new-line)
  (local-set-key "\C-c\C-ds" 'slime-describe-symbol)
  (local-set-key "\C-c\C-dh" 'cl-hyperspec-lookup)
  (setq lisp-simple-loop-indentation  1
        lisp-loop-keyword-indentation 6
        lisp-loop-forms-indentation   6))

(add-my-hook view-mode-hook
  "Navigation consistent with my movement commands"
  (define-key view-mode-map "\C-\M-w" 'View-scroll-half-page-backward)
  (define-key view-mode-map "\C-\M-v" 'View-scroll-half-page-forward)
  (define-key view-mode-map "\M-v"    'end-of-buffer)
  (define-key view-mode-map "k"       'View-kill-and-leave)
  (define-key view-mode-map "l"       'View-leave))

(add-my-hook doc-view-mode-hook
  (auto-revert-mode 1))

(add-my-hook calc-mode-hook
  (local-set-key "\M-i" 'calc-pop)
  (local-set-key "\C-w" 'scroll-down-or-beg)
  (local-set-key "\M-w" 'beginning-of-buffer)
  (local-set-key "\M-k" 'calc-kill-region)
  (local-set-key "\M-c" 'calc-copy-region-as-kill)
  (local-set-key "\C-c\C-l" 'calc-copy-as-kill))

(add-my-hook nxhtml-mode-hook
  (local-set-key "\M-m" 'nxml-mark-paragraph)
  (local-set-key "\M-h" 'backward-kill-word))

;;; Common C Modes (cc-mode) (C, C++, Java)

(autoload 'c-mode    "cc-mode" "C Editing Mode" t)    ;; Use cc-mode for all C editing
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(add-my-hook c-initialization-hook
  (define-key c-mode-base-map [(control meta h)] nil)
  (define-key c-mode-base-map [(control c) (control h)] 'c-mark-function))

(add-my-hook c-mode-common-hook
  "Base hook for cc-mode programming languages"
  (if (not (assoc "crg" c-style-alist))
      (c-add-style "crg" c-crg-style))
  (c-set-style "crg")
  (if (assq 'c-auto-hungry-string minor-mode-alist)   ;; Inhibit minor mode notation
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string "") (remove-matching-elements 'c-auto-hungry-string minor-mode-alist 'eq-car))))
  (local-set-key "\C-c\C-a" 'c-beginning-of-statement)
  (local-set-key "\C-c\C-e" 'c-end-of-statement)
  (local-set-key "\C-c\C-b" 'c-backward-conditional)
  (local-set-key "\C-c\C-f" 'c-forward-conditional)
  (local-set-key "\C-c\C-c" 'comment-region)
  (local-set-key "\M-q"     'c-fill-paragraph)
  (local-set-key "\C-c\C-d" 'c-down-conditional)
  (local-set-key "\C-c\C-u" 'c-up-conditional)
  (local-set-key "\C-c\C-i" 'c-indent-exp)
  (local-set-key "\C-c\C-q" 'c-indent-defun)
  (local-set-key "\C-c\C-m" 'compile);; m for make
  (local-set-key "\C-c\C-n" 'up-list)
  (local-set-key "\C-c\C-p" 'c-up-list-neg)
  (local-set-key "\C-c\C-o" 'outline-minor-mode)
  (local-set-key "\C-c\C-t" 'c-set-offset)
  (local-set-key "\C-c\C-x" 'c-macro-expand)
  (local-set-key [f4]       'c-fill-paragraph))
  
(defvar c-crg-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(label . 0)
			(case-label . *)
			(access-label . /)
			(statement-case-intro . *)
			(statement-cont . +)
			(cpp-macro . 0)
                        (inline-open . 0)
			(inher-intro . ++)))  ; was c-lineup-inher-intro but that seems gone now
    (c-auto-newline . t)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-list-close)
			       (substatement-open before after)
			       (block-close before after)))
    (c-hanging-comment-ender-p . nil)
    (c-cleanup-list . '(scope-operator list-close-comma)))
  "My personal formatting style for editing C/C++ source.")

(defconst java-crg-style
  `((c-recognize-knr-p . nil) ; K&R style argument declarations are not valid
    (c-basic-offset . 4)
    (indent-tabs-mode . nil) ; tabs are evil!
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (brace-entry-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . /)
                        (statement-case-open . +)
                        (statement-case-intro . +)
                        (access-label . /) ; case w/o {
                        (func-decl-cont . c-lineup-java-throws)
                        (inher-cont . c-lineup-java-inher)
                        (innamespace . 0)
                        (arglist-intro  . c-lineup-arglist-intro-after-paren)
                        (arglist-close  . c-lineup-arglist)
                        (arglist-cont-nonempty . c-lineup-arglist)
                        (annotation-var-cont . 0)))
    (c-block-comment-prefix . "* "))
  "My Java Programming Style")

(add-my-hook java-mode-hook
  (setq indent-tabs-mode nil)
  (if (not (assoc "java-crg" c-style-alist))
      (c-add-style "java-crg" java-crg-style))
  (c-set-style "java-crg")
  ;(c-set-style "java")
  (local-set-key "{" 'my/c-mode-balanced-brace)
  (subword-mode))


;;; Modes and Magic

(add-to-list 'auto-mode-alist '("\\.[0-9]\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.\\([pP]\\([LlmM]?\\|od\\|erl\\)\\|al\\)\\'" . perl-mode))
(add-to-list 'auto-mode-alist 
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
;(add-to-list 'auto-mode-alist '("\\.s?html?\\'" . nxhtml-mode)) ; ATTN: Where is nxhtml now?
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.[sS]\\'" . S-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'"  . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"   . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.clj[xc]?\\'"  . clojure-mode)) ; added in clojure mode section
(add-to-list 'auto-mode-alist '("\\.org\\'"  . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'"  . org-mode))

; ATTN: where is nxhtml mode now?
;(fset 'xml-mode  'nxml-mode)    ;; should rhs be (symbol-function 'nxml-mode) ??
;(fset 'html-mode 'nxhtml-mode)

;(add-to-list 'magic-mode-alist '("<\\?xml " . nxml-mode))
(add-to-list 'magic-mode-alist '("#!.*perl" . perl-mode))
(add-to-list 'magic-mode-alist '("#!.*python" . python-mode))
(add-to-list 'magic-mode-alist '("#!.*\\(t?c\\|ba\\|k\\|z\\|\\)sh" . shell-script-mode))
(add-to-list 'magic-mode-alist '("\\\\input " . plain-TeX-mode))
(add-to-list 'magic-mode-alist '("\\\\document\\(style\\|class\\)" . LaTeX-mode))
(add-to-list 'magic-mode-alist '("^#!.*inlein" . clojure-mode))



;;; Initial State

(auto-compression-mode '1)               ; Auto view compressed files
(transient-mark-mode '1)                 ; Highlight region
(turn-on-show-paren-mode)                ; show matching parens
(display-time)                           ; Time on Mode Line
(enable-cua-rectangles)

;; rename scratch buffer if requested
(-when-let (scratch-buf (get-buffer "*scratch*"))  
    (cond
     ((stringp my/keep-scratch-buf)
      (with-current-buffer scratch-buf
        (rename-buffer my/keep-scratch-buf)
        (funcall initial-major-mode)))
     ((null my/keep-scratch-buf)
      (kill-buffer scratch-buf))))

;; Load extra settings from the environment and from customize
(when my/env-file (load-file my/env-file))       ; my environment variables
(when my/custom-file (load-file my/custom-file)) ; saved customize settings

;; start in the shell, using *unix* rather than *shell* as buffer name
(let* ((shell-buf (generate-new-buffer-name "*unix*")))
  (shell shell-buf)
  (with-selected-window (get-buffer-window shell-buf)
    (delete-other-windows)))

(when (and window-system my/theme-func (fboundp my/theme-func))
  (funcall my/theme-func))

;; Tidy up minor-mode lighters on the mode line
(ignore-errors
  (when (featurep 'diminish)
    (let ((lighters (get-preference 'lighters)))
      (dolist (lighter lighters)
        (diminish (car lighter) (cdr lighter))))))

;; Load current prototype code
(let ((proto (locate-user-emacs-file "init/proto.el")))
  (when (file-exists-p proto)
    (load-file proto)))


(server-start)


;;; dot-emacs.el ends here


