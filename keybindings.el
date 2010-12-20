;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Global Key Bindings")                                 ;;;;;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Auxilliary Global Keymaps
;;
    ;; String Replacement Commands

(define-prefix-command 'my-string-replace-prefix 'my-string-replace-map "String Replacement")
(define-key my-string-replace-map "q" 'query-replace)
(define-key my-string-replace-map "x" 'query-replace-regexp)
(define-key my-string-replace-map "s" 'replace-string)
(define-key my-string-replace-map "r" 'replace-regexp)
(define-key my-string-replace-map "t" 'tags-query-replace)

(when (featurep 'icicles)
  (define-key my-string-replace-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-string-replace-map t))

    ;; Transpose Commands

(define-prefix-command 'my-transpose-prefix 'my-transpose-map "Transpose Commands")
(define-key my-transpose-map "c"   'transpose-chars)
(define-key my-transpose-map "l"   'transpose-lines)
(define-key my-transpose-map "p"   'transpose-paragraphs)
(define-key my-transpose-map "s"   'transpose-sentences)
(define-key my-transpose-map "w"   'transpose-words)
(define-key my-transpose-map "x"   'transpose-sexps)

    ;; Tag Management Commands

(define-prefix-command 'my-tags-prefix 'my-tags-map "Tag Management")
(define-key my-tags-map "l"   'list-tags)
(define-key my-tags-map "."   'find-tag)
(define-key my-tags-map "f"   'find-tag)
(define-key my-tags-map "r"   'find-tag-regexp)
(define-key my-tags-map "4"   'find-tag-other-window)
(define-key my-tags-map "5"   'find-tag-other-frame)
(define-key my-tags-map "n"   'find-tag-noselect)
(define-key my-tags-map "c"   'tags-loop-continue)
(define-key my-tags-map "s"   'tags-search)
(define-key my-tags-map "\C-i" 'complete-tag)
(define-key my-tags-map "t"   'select-tags-table)
(define-key my-tags-map "v"   'visit-tags-table)
(define-key my-tags-map "z"   'tags-reset-tags-table)
(define-key my-tags-map "a"   'tags-apropos)
(define-key my-tags-map "q"   'tags-query-replace)

(when (featurep 'icicles)
  (define-key my-tags-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-tags-map t))

    ;; Marks and so forth (e.g., Bookmarks)

(defvar my-mark-map (copy-keymap bookmark-map) "Mark/Bookmark Management")
(fset 'my-mark-prefix my-mark-map)

(when (featurep 'icicles)
  (define-key my-mark-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-mark-map t))

    ;; Go Map  (Goto's and Jumps of All Kinds)

(define-prefix-command 'my-go-prefix 'my-go-map "Go")
(define-key my-go-map "g"      'goto-line)
(define-key my-go-map "\M-g"   'goto-line)
(define-key my-go-map "h"      'help-command)
(when (featurep 'linkd)
  (define-key my-go-map "l"    'linkd-follow-at-point))
(define-key my-go-map "n"      'next-error)
(define-key my-go-map "\M-n"   'next-error)
(when (featurep 'org-install)
  (define-key my-go-map "o"    'org-open-at-point))
(define-key my-go-map "p"      'previous-error)
(define-key my-go-map "\M-p"   'previous-error)
(define-key my-go-map "q"      'keyboard-quit)  
(define-key my-go-map "t"      'top-level)
(define-key my-go-map "u"      'browse-url)
(define-key my-go-map "x"      'exit-recursive-edit)
(define-key my-go-map [escape] 'keyboard-escape-quit)

(when (featurep 'icicles)
  (define-key my-go-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-go-map t))

(when (featurep 'semantic)
  (define-prefix-command 'my-go-semantic-prefix 'my-go-semantic-map "Go Places with Semantic")
  (when (featurep 'senator)
    (define-key my-go-semantic-map "j" 'senator-jump)
    (define-key my-go-semantic-map "n" 'senator-next-tag)
    (define-key my-go-semantic-map "p" 'senator-previous-tag)
    (define-key my-go-semantic-map "u" 'senator-go-to-up-reference))
  (define-key my-go-semantic-map "B" 'semantic-mrub-switch-tags)
  (define-key my-go-semantic-map "c" 'semantic-complete-jump)
  (define-key my-go-semantic-map "f" 'semantic-ia-fast-jump)
  (define-key my-go-semantic-map "h" 'semantic-analyze-proto-impl-toggle)
  (define-key my-go-semantic-map "r" 'semantic-symref)
  (define-key my-go-semantic-map "s" 'semantic-symref-symbol)
  (define-key my-go-semantic-map "x" 'semantic-symref-regexp)
  (define-key my-go-map "s" 'my-go-semantic-prefix))

(if (featurep 'bookmark+)
    (progn
      (define-key my-go-map "b"    'bmkp-jump-map)
      (define-key my-go-map "B"    'bmkp-jump-other-window-map))
  (define-key my-go-map "b"    'bookmark-jump)
  (define-key my-go-map "B"    'bookmark-jump-other-window))


    ;; Abbrevs

(define-prefix-command 'my-abbrev-prefix 'my-abbrev-map "Abbreviations")
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

(when (featurep 'icicles)
  (define-key my-abbrev-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-abbrev-map t))

    ;; Org-Mode

(define-prefix-command 'my-org-prefix 'my-org-map "Org Mode")
(when (featurep 'org-install)
  (define-key my-org-map "a" 'org-agenda)
  (define-key my-org-map "c" 'org-capture)
  (define-key my-org-map "l" 'org-store-link))

(when (featurep 'icicles)
  (define-key my-org-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-org-map t))

    ;; Icicles

(define-prefix-command 'my-icicles-prefix 'my-icicles-map "Icicles")
(when (featurep 'icicles)
  (define-key my-icicles-map "b" 'icicle-search-buffer)
  (define-key my-icicles-map "c" 'icicle-imenu-command)
  (define-key my-icicles-map "f" 'icicle-search-file)
  (define-key my-icicles-map "g" 'icicle-search-generic)
  (define-key my-icicles-map "o" 'icicle-occur)
  (define-key my-icicles-map "m" 'icicle-search-bookmark)
  (define-key my-icicles-map "s" 'icicle-search)
  (define-key my-icicles-map "t" 'icicle-search-bookmarks-together)
  (define-key my-icicles-map "$" 'icicle-search-word)
  (define-key my-icicles-map "^" 'icicle-search-keywords)
  (define-key my-icicles-map "`" 'icicle-compilation-search)
  (define-key my-icicles-map "=" 'icicle-imenu)
  (define-key my-icicles-map [?\"] 'icicle-search-char-property) 
  (define-key my-icicles-map [tab] 'icicle-comint-command)
  (define-key my-icicles-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-icicles-map t))


    ;; Anything

(define-prefix-command 'my-anything-prefix 'my-anything-map "Anything Interface")
(when (featurep 'anything)
  (define-key my-anything-map "a" 'anything)
  (define-key my-anything-map "b" 'anything-bookmarks)
  (define-key my-anything-map "c" 'anything-browse-code))

(when (featurep 'icicles)
  (define-key my-anything-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-anything-map t))

    ;; Links

(define-prefix-command 'my-links-prefix 'my-links-map "Links")
(when (featurep 'org)
  (define-key my-links-map "o" 'org-store-link))
(when (featurep 'linkd)
  (define-key my-links-map "b" 'linkd-block-around-point)
  (define-key my-links-map "e" 'linkd-edit-link-at-point)
  (define-key my-links-map "i" 'linkd-insert-link)
  (define-key my-links-map "p" 'linkd-process-block)
  (define-key my-links-map "s" 'linkd-send-block-to-shell)
  (define-key my-links-map [return] 'linkd-follow-at-point)
  (define-key my-links-map "\C-n" 'linkd-next-link)
  (define-key my-links-map "\C-p" 'linkd-previous-link))

(when (featurep 'icicles)
  (define-key my-links-map [escape] (make-sparse-keymap)) ; to allow icicle-complete-keys
  (add-to-list 'icicle-keymaps-for-key-completion 'my-links-map t))


;;
;; Keybinding Setters
;;

(defun my-set-global-sysdep-keybindings ()
  "Sets global keybindings that are system
   dependent (operating system or emacs version)."
   nil
  )

(defun my-set-global-keybindings ()
  "Adjusts default global keybindings to
   conform to my preferences."
  ;;
  ;; Movement
  ;;
    (global-set-key "\C-a"        'my-move-beginning-of-line)
    (global-set-key "\C-w"        'scroll-down-or-beg)
    (global-set-key "\C-v"        'scroll-up-or-end)
    (global-set-key "\M-v"        'end-of-buffer)
    (global-set-key "\M-w"        'beginning-of-buffer)
    (global-set-key "\M-\C-v"     'scroll-other-window)
    (global-set-key "\M-\C-w"     'scroll-other-window-down)
    (global-set-key "\M-p"        'backward-up-list)
    (global-set-key "\M-n"        'up-list)
    (global-set-key "\M-\C-f"     'forward-sexp-or-char)
    (global-set-key "\M-\C-b"     'backward-sexp-or-char)
    (global-set-key [?\A-a]       'back-to-indentation)
  ;;
  ;; Basic Functions
  ;;
    (global-set-key "\C-x\C-m"    'execute-extended-command)
    (global-set-key "\C-c\C-m"    'execute-extended-command)
    (when (featurep 'lacarte)
      (global-set-key "\C-x\M-m"  'lacarte-execute-command)
      (global-set-key "\C-c\M-m"  'lacarte-execute-command))
    (global-set-key [?\C-0]       'universal-argument)
  ;;
  ;; Help
  ;;   I've moved the help command to enable more efficient
  ;;   access to some editing commands. But this necessitates
  ;;   a bit of extra work to ensure consistency elswhere,
  ;;   especially isearch and icicles (see below for the former)
  ;;   and (@> "File and Navigation Modes") for the latter.
  ;;
    (define-key help-map "\C-c"   'describe-key-briefly)
    (define-key help-map "\M-c"   'describe-copying)
    (dolist (event my-help-events)
      (global-set-key event 'help-command)) 
  ;;
  ;; Searching
  ;;
    (global-set-key "\M-s"    'isearch-forward-regexp)
    (global-set-key "\M-r"    'isearch-backward-regexp)
    (global-set-key "\C-\M-s" 'query-replace)
    (global-set-key "\C-\M-r" 'query-replace-regexp)
    (global-set-key [?\C-5]   'query-replace)
    (global-set-key [?\C-6]   'query-replace-regexp)
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
    (define-key isearch-mode-map (car my-help-events) (lookup-key isearch-mode-map "\C-h"))
    (define-key isearch-mode-map "\C-h" 'isearch-delete-char)
    (define-key minibuffer-local-isearch-map "\M-s" 'isearch-forward-exit-minibuffer)
    (define-key minibuffer-local-isearch-map "\M-r" 'isearch-reverse-exit-minibuffer)
    (define-key minibuffer-local-isearch-map [(control meta tab)] 'isearch-complete-edit)
    (define-key minibuffer-local-isearch-map [(meta control ?i)]  nil)
  ;;
  ;; Kills and Copies
  ;;
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
    (global-set-key "\M-c"            'my-kill-ring-save) ;alternate: my-copy-region-as-kill
    (global-set-key "\M-\C-y"         'append-next-kill)
    (global-set-key "\C-z"            'zap-to-char)
    (global-set-key "\M-z"            'undo)
    (global-set-key "\C-h"            'delete-backward-char) ; Move help command below
    (global-set-key "\M-h"            'backward-kill-word)
  ;;
  ;; Marking
  ;;
    (global-set-key [?\M- ]       'mark-sexp)
    (global-set-key [?\C-\M- ]    'mark-defun)
    (global-set-key "\M-m"        'mark-paragraph)
    (global-set-key "\C-\M-m"     'mark-end-of-sentence)
    (global-set-key [?\S-\C-\M-m] 'mark-page)
    (global-set-key [?\A-m]       'mark-whole-buffer)
    (global-set-key [?\A-M]       'mark-word)
  ;;
  ;; Buffers, Files, Windows, and Frames
  ;;
    (global-set-key "\C-x\C-b"    'ibuffer-list-buffers-select)
    (global-set-key [?\C-x ?\C-\S-b] 'ibuffer-list-buffers) ; no select and files arg
    (global-set-key "\C-xy"       'bury-buffer)
    (global-set-key "\C-xo"       'other-window)         ;; also bound to C-x C-o
    (global-set-key "\C-x\C-o"    'other-window)         ;; same to avoid collisions
    (global-set-key "\C-x\M-w"    'append-to-file)
    (global-set-key "\C-x\M-\C-f" 'find-file-at-point)
    (global-set-key "\C-x:"       'view-url-in-buffer)
    (global-set-key "\C-\M-z"     'iconify-or-deiconify-frame)
  ;;
  ;; Positioning
  ;;
    (global-set-key "\M-l"        'reposition-window)
    (global-set-key "\M-\C-l"     'move-to-window-line)
    (global-set-key "\C-xL"       'count-lines-page) ;; ATTN replace with something smarter
    (global-set-key "\C-xl"       'what-line)
    (global-set-key "\C-xg"       'goto-line)    ;; ATTN: used to be C-xt
    (global-set-key "\M-g"        'my-go-prefix) ;; replaces similar map
  ;;
  ;; Macros   
  ;;
    (global-set-key "\C-x\M-k"    'kmacro-keymap)
    (global-set-key [?\C-3]       'kmacro-start-macro-or-insert-counter)
    (global-set-key [?\C-4]       'kmacro-end-or-call-macro)
  ;;
  ;; Miscellaneous Operations
  ;;
    (global-set-key [?\C-7]       'align-regexp)  ;; very useful! (mnemnoic: & for align)
    (global-set-key "\C-x7"       'align-regexp)  
    (global-set-key [?\A-q]       'unfill-paragraph)
    (global-set-key [\A-return]   'delete-blank-lines)
    (global-set-key [?\A- ]       'just-one-space)
    (global-set-key [?\C-\A- ]    'delete-trailing-whitespace)
    (global-set-key "\C-\\"       'comment-dwim)
    (global-set-key "\M-/"        'hippie-expand)
  ;;
  ;; Rectangle and Register Commands
  ;;
    (global-set-key "\C-xra"      'append-to-register)
    (global-set-key "\C-xrc"      'copy-to-register)            
    (global-set-key "\C-xrl"      'list-registers)            
    (global-set-key "\C-xrp"      'prepend-to-register)
    (global-set-key "\C-xrs"      'string-rectangle)
    (global-set-key "\C-xrt"      'register-to-point)
    (global-set-key "\C-xrv"      'view-register)
    (global-set-key "\C-xrx"      'clear-rectangle)
    (global-set-key "\C-xr "      'point-to-register)
  ;;
  ;; Attach additional keymaps and related rebindings
  ;;
    (define-key ctl-x-map "t"  'my-tags-prefix)  ;; ATTN: used to be C-xg (see goto-line above)
    (define-key ctl-x-map "%"  'my-string-replace-prefix)
    (define-key ctl-x-map "m"  'my-mark-prefix)
    (define-key ctl-x-map "a"  'my-abbrev-prefix)
    (global-set-key [?\A-t]    'my-transpose-prefix)
    (global-set-key [?\S-\M-t] 'my-transpose-prefix)
  ;;
  ;; Modified Arrow Keys get added functionality
  ;;
    (global-set-key [C-left]      'back-to-indentation)
    (global-set-key [C-right]     'end-of-line)
    (global-set-key [M-left]      'backward-word)
    (global-set-key [M-right]     'forward-word)
    (global-set-key [C-M-left]    'backward-list)
    (global-set-key [C-M-right]   'forward-list)
    (global-set-key [C-up]        'scroll-down-or-beg)
    (global-set-key [C-down]      'scroll-up-or-end)
    (global-set-key [M-up]        'backward-paragraph)
    (global-set-key [M-down]      'forward-paragraph)
    (global-set-key [C-M-up]      'backward-page)
    (global-set-key [C-M-down]    'forward-page)
  ;;
  ;; System Dependent Global Bindings
  ;;
    (my-set-global-sysdep-keybindings)
  ;;
  ;; Third-Party Mode Global Access Keys
  ;;
    (global-set-key "\C-ca"       'my-anything-prefix)
    (global-set-key "\C-ci"       'my-icicles-prefix)
    (global-set-key "\C-cl"       'my-links-prefix)
    (global-set-key "\C-co"       'my-org-prefix)
    (when (featurep 'icicles)
      (global-set-key "\C-x\M-\C-o" 'icicle-switch-to/from-minibuffer))
    (when (featurep 'lacarte)
      (global-set-key [?\e ?\M-x]   'lacarte-execute-command)
      (global-set-key [?\M-`]       'lacarte-execute-command))
)


