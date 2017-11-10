;;; keybindings.el -- key maps and global bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017 Christopher R. Genovese, all rights reserved.
;; Author: Christopher Genovese <genovese@cmu.edu>
;; Version: 2.8.0

;;; Commentary:
;;
;;  This defines my global keybindings, using a variety of (optional)
;;  auxilliary key maps. Mode specific bindings are set in individual
;;  components and mods.
;;
;;  In general, I attempt to use mnemnic layering of keys, with a
;;  hierarchy of function from a key, C-key, M-key, C-M-key, A-key, et
;;  cetera. In addition, there are a few non-default features worth
;;  noting.
;;  
;;  + Help
;;  
;;    Default emacs keybindings reserve C-h for help. While
;;    help is important and commonly used, it does not
;;    need a top-level (control) key. Instead, we do the
;;    following:
;;  
;;    - C-h -- delete-backward-char
;;    - M-h -- backward-kill-word
;;    - C-M-h -- help
;;  
;;    This is much more efficient than the defaults (no more need for
;;    backspace and =backward-kill-word= is very commonly useful). The main
;;    complication is that most modes (and to some extend emacs itself,
;;    see =help-char=) assume =C-h= for help, which requires additional
;;    effort to configure.
;;  
;;  + Scrolling and Cutting
;;  
;;    Also in the default keybindings use C-v and M-v for scrolling up and
;;    down. This cuts across levels (control to meta) for a
;;    comparable-level operation and is slow, as a result. It also
;;    leaves C-w, non-mnemonically, for a kill operation. Instead,
;;    the keybindings here do the following:
;;  
;;    - C-w -- scroll down
;;    - C-v -- scroll up
;;    - M-w -- beginning of buffer
;;    - M-v -- end of buffer
;;    - M-C-w -- scroll down other window
;;    - M-C-v -- scroll up other window
;;    - C-k -- kill line
;;    - M-k -- kill region
;;    - M-C-k -- kill sexp
;;  
;;    Again, this requires some remapping in configuring some common
;;    modes.
;;  
;;  + Help Navigation
;;  
;;    To make the Emacs help system even easier to use, this adds
;;    two mechanisms for navigating. First C-M-h g from any buffer
;;    moves to the help buffer, where g moves back to the previous
;;    location. Second, C-M-h followed by various navigation commands
;;    moves the help buffer without changing the active buffer.
;;    These commands are as follows:
;;  
;;    - C-w   -- scroll down
;;    - C-v   -- scroll up
;;    - M-w   -- go to beginning of buffer
;;    - M-v   -- go to end of buffer
;;    - C-b   -- back to previous help page
;;    - C-f   -- forward to next help page
;;    - RET   -- push button
;;    - TAB   -- forward-button
;;    - S-TAB -- backward-button
;;    - q     -- quit help
;;
;;  Note also that some of these keybindings depend on loaded third-party
;;  packages. The keybindings will execute without error if these packages
;;  are missing. I attempt to flag these with comments.
;;
;;  The sections are divided into pages for easy navigation with C-x [ and ].

;;; Code:


;; Auxilliary Keymaps and Prefix Keys

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
(define-key my/go-map "s"      'counsel-switch-to-shell-buffer)
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
(define-key my-search-map "h" 'helm)
(define-key my-search-map "o" 'helm-occur)
(define-key my-search-map "r" 'ripgrep-regexp)
(define-key my-search-map "R" 'counsel-rg)  ; ATTN: trying this out
(define-key my-search-map "s" 'swiper)      ; ATTN: trying this out
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
;;   access to some editing commands. Specifically, use C-M-h
;;   for help, leaving C-h and M-h for better uses.
;;
;;   But this necessitates a bit of extra work to ensure consistency
;;   elswhere, especially isearch and icicles.
;;
;;   Important note: the help-mode keybindings (in help-mode-map, i.e.,
;;   the bindings used *within* a help buffer) are set in mods/help.el
;;   for a variety of reasons. See in particular the variable
;;   `my/help-keybindings'. The keys here are those that can follow the
;;   help-key C-M-h. Also see, `my/define-remote-help-command' in
;;   mods/help.el for bindings below that operate on help buffer from
;;   other buffers.
;;   
(setq my/help-events (list [?\M-\C-h])  
      help-event-list (nconc (list ?\M-\C-h) help-event-list) ; only keys allowed
      help-char 0) ; want ?\M-\C-h but that's not a char, causes problems

(dolist (event my/help-events)
  (global-set-key event 'help-command)) 

(with-eval-after-load 'help
  (bind-keys :map help-map
    ("C-c" . describe-key-briefly)
    ("M-c" . describe-copying)
    ("E"   . view-emacs-FAQ)
    ("f"   . counsel-describe-function)
    ("F"   . counsel-describe-face)
    ("C-f" . counsel-describe-face)
    ("I"   . Info-goto-emacs-command-node) ; for help-mode consistency
    ("M-i" . describe-input-method) ; not often needed
    ("T"   . describe-text-properties) 
    ("B"   . helm-descbinds) ; requires package helm-descbinds
    ("M-b" . counsel-descbinds)
    ;("C-v" . scroll-help-window-forward)
    ;("C-w" . scroll-help-window-backward)
    ;("M-v" . end-of-help-buffer)
    ;("M-w" . beginning-of-help-buffer)
    ("g"   . my/help-goto-help))

  (my/define-remote-help-command
   "Move forward to the next help button, or the nth next with prefix arg."
   :key "TAB"
   (forward-button
    (prefix-numeric-value current-prefix-arg) 'wrap))

  (my/define-remote-help-command
   "Move backward to the previous help button, or the nth previous with prefix arg."
   :key "<backtab>"
   (backward-button
    (prefix-numeric-value current-prefix-arg) 'wrap))

  (my/define-remote-help-command :key "C-b" (help-go-back))
  (my/define-remote-help-command :key "C-f" (help-go-forward))
  (my/define-remote-help-command :key "C-v" (cua-scroll-up))
  (my/define-remote-help-command :key "C-w" (cua-scroll-down))
  (my/define-remote-help-command :key "M-v" (end-of-buffer))
  (my/define-remote-help-command :key "M-w" (beginning-of-buffer))
  (my/define-remote-help-command :key "RET" (push-button)) ;was (help-follow-symbol)
  (my/define-remote-help-command :key "q"   (quit-window)))


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


;; Rectangle and Register Commands (mostly use cua-rectangle C-c RET instead)
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


;;; keybindings.el ends here
