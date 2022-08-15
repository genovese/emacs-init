;;; Simplest self-contained and tolerable init file for remote systems


;;; Main operating settings

(setq inhibit-startup-message  t
      load-prefer-newer        t
      case-fold-search         nil
      indent-tabs-mode         nil
      shift-select-mode        nil
      scroll-error-top-bottom  t
      switch-to-visible-buffer nil
      history-length           1024
      indicate-empty-lines     t  ; show empty lines at end of file
      kill-read-only-ok        t  ; use kill to copy text in read-only buffer
      initial-major-mode       'lisp-interaction-mode
      python-shell-interpreter "python")

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

(setq-default major-mode             'fundamental-mode
              fill-column            72
              case-fold-search       nil
              indent-tabs-mode       nil
              shift-select-mode      nil
              next-line-add-newlines nil
              truncate-lines         t
              indicate-empty-lines   t)

(dolist (s '(tool-bar-mode scroll-bar-mode tooltip-mode delete-selection-mode))
  (if (fboundp s) (funcall s -1))) ; disable some operational modes

(dolist (s '(eval-expression narrow-to-region narrow-to-page scroll-left
              downcase-region upcase-region set-goal-column))
  (put s 'disabled nil)) ; allow some useful operations


;;; Windowing-Specific Operational Settings

(when window-system
  (mouse-wheel-mode  t)
  (blink-cursor-mode t)
  (setq visible-bell t)
  (set-cursor-color "#dfaf8f"))

(defun turn-on-show-paren-mode ()
  "Set up and initiate `show-paren-mode'. If
   This turns on the mode globally if the
   mode variable is not local."
  (setq show-paren-delay 0)  ; default 0.125
  (setq show-paren-style 'parenthesis)
  (show-paren-mode '1)
  (set-face-background 'show-paren-mismatch "red"))

(auto-compression-mode '1)               ; Auto view compressed files
(transient-mark-mode '1)                 ; Highlight region
(turn-on-show-paren-mode)                ; show matching parens
(display-time)                           ; Time on Mode Line

;; Global Keybindings

;; Movement
(global-set-key "\C-a"        'beginning-of-line) ;'my/move-beginning-of-line)
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

;; Basic Functions
(global-set-key "\C-x\C-m"    'execute-extended-command)
(global-set-key "\C-c\C-m"    'execute-extended-command)
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
(global-set-key "\M-c"            'copy-region-as-kill) ;alternate: my/kill-ring-save, my/copy-region-as-kill
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
;(global-set-key (kbd "C-x 5 2")  'my/make-frame-command)
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
;(global-set-key "\M-g"        'my/go-prefix) ;; replaces similar map


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

