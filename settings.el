;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;(@* "Settings and Initialization")                         ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start-up configuration

(setq inhibit-startup-message 't)
(setq initial-major-mode 'lisp-interaction-mode) ; formerly fundamental-mode
(setq initial-frame-alist my-frame-alist)        ; see (@> "Environment Configuration")

  ; load custom-file in (@> "Initial State")
(setq custom-file (expand-file-name "~/.emacs.d/emacs-custom.el")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable Disabled Commands

(put 'eval-expression  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'scroll-left      'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic Operation

(defun my-enable-cua-rectangles ()
  "Turn on (only) the rectangle feature of cua mode.
Also, take care of conflicting keybindings with icicles. Should
be called before `my-set-completion-mode' in
`my-set-operating-state' so that icicles properly sets up its
bindings. But if called manually, then icy-mode should be cycled
on and off afterwards."
  (require-soft 'cua-rect)
  (when (featurep 'cua-rect)
    (setq cua-enable-cua-keys nil)
    (cua-mode t)
    (when (featurep 'icicles)
      (defun my-ctrl-return ()
        "When in minibuffer use `icicle-candidate-action',
       otherwise use `cua-set-rectangle-mark'."
        (interactive)
        (if (or (window-minibuffer-p (selected-window))
                (not (boundp 'cua-mode))
                (null cua-mode))
            (call-interactively 'icicle-candidate-action)
          (call-interactively 'cua-set-rectangle-mark)))
      (add-to-list 'icicle-top-level-key-bindings '(cua-set-rectangle-mark my-ctrl-return t)))))

(defun my-set-completion-mode (sym)
  "Choose among various intelligent completion methods
   according to symbol. Current choices are 'icicles, 'ido,
   'iswitchb, 'none."
   (cond
    ((equal sym 'icicles)
     (if (not (featurep 'icicles))
         (require-soft 'icicles))
     (if (featurep 'icicles)
         (icicle-mode 1)
       (message "Cannot load icicles")))
    ((equal sym 'ido)
     (ido-mode '1)
     (setq ido-case-fold t))
    ((equal sym 'iswitchb) ;;; Use smart buffer switching
     (iswitchb-mode '1)                      
     (setq iswitchb-case t))))
    
(defun my-set-operating-state ()
  "Set Emacs to preferred modes, maps, and more."
  (setq-default case-fold-search nil)
  (setq-default indent-tabs-mode nil)
  (setq-default next-line-add-newlines nil)
    
  (setq-default fill-column '72)
  (setq paragraph-start paragraph-separate)            ;;; Use blank lines to separate paragraphs by default
  (setq adaptive-fill-regexp "[ \t]*\\([>*%#]+ +\\)?") ;;; Fill around comment beginnings and yanked-messages
  (setq sentence-end-double-space nil)                 ;;; Allow frenchspacing
  (setq page-delimiter "^\\(\f\\|\n\n+\\)")            ;;; FF or 2+ consecutive blank lines

  (setq history-length 256)
  (setq print-length 1024)                 ;;; Give more information in help
  (setq print-level  8)

  (if (>= emacs-major-version 23)
      (setq-default major-mode 'org-mode)
    (setq default-major-mode 'org-mode))   ;; back to fundamental-mode if org-mode causes strange behavior??
  (setq my-keep-scratch-buf "*elisp*")     ;; if nil, delete; if string, new name; otherwise leave alone.
  
  (setq display-time-24hr-format t)
  (display-time)                           ;;; Time on Mode Line
    
  (transient-mark-mode '1)                 ;;; Highlight region
  (setq kill-read-only-ok t)               ;;; OK to use kill to copy text in read-only buffer
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode  1))                  ;;; Make menu bar available, just in case
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode  -1))                 ;;; Disable tool bar
  (setq-default scroll-bar-mode 'right)    ;;; Put scroll bars on the right
  (toggle-scroll-bar  '1)                  ;;; Use scroll bars (mostly for visual sense of buffer size)
  (auto-compression-mode '1)               ;;; Auto view compressed files
  (turn-on-global-show-paren-mode)         ;;; show matching parens
  (setq indicate-empty-lines t)            ;;; show empty lines at end of file

  (setq browse-url-browser-function 'browse-url-firefox) ;; use new tabs in firefox
  (setq browse-url-firefox-new-window-is-tab t)
  (setq browse-url-new-window-flag  t)

  (my-set-completion-mode 'icicles)
  (my-set-global-keybindings)
  )



