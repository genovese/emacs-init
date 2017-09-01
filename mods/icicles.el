;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Icicles

;; Added Functionality
;;
;; * Show sort order label in completions buffer:
;;
;;   Changing the sort order of the completions causes a brief
;;   echo that is easily missed. The command
;;   `my/icicle-show-sort-order' changes the sort order and
;;   labels the sort order in the completion buffer. It is
;;   configured by variable `my/icicle-describe-sort-in-completions'.
;; 
;; * Move to beginning or end of completions list from minibuffer:
;;
;;   It is convenient when completion lists are long to be able
;;   to move to the beginning or end of the completions list from
;;   the minibuffer. The commands `icicle-beginning-of-Completions'
;;   and `icicle-end-of-Completions' do this.
;;
;; * Fixes double-default problem that sometimes occurs.
;; 
;;   See `icicle-suppress-default-pattern' and rewrite of
;;   icicle function `icicle-read-from-minibuffer'.
;;

(defvar my/icicle-describe-sort-in-completions (and t (featurep 'icicles))
  "If non-nil, show the current sort order in *Completions* buffer.")

(defun my/icicle-show-sort-order ()
  "Show current sort order in completions list.
   Add this to completion-setup-hook to achieve
   the effect."
  (when (and my/icicle-describe-sort-in-completions icicle-mode)
    (with-current-buffer standard-output
      (let* ((sort-order (icicle-current-sort-order nil))
             (sort-string (concat ", sorting "
                                  (if (string-equal sort-order "alphabetical")
                                      "alphabetically" sort-order))))
        (goto-char (point-min))
        (when (re-search-forward "Possible completions are" nil t)
          (put-text-property 0 (length sort-string) 'face 'icicle-Completions-instruction-1 sort-string)
          (insert sort-string)))))
  )

(defun icicle-beginning-of-Completions ()
  "Scroll to the beginning of *Completions* window."
  (interactive)
  (let ((window (get-buffer-window "*Completions*" 0)))
    (when window
      (save-selected-window
        (select-window window)
        (unless (= (window-start) (point-min))
          (goto-char (icicle-start-of-candidates-in-Completions)))
        ))))

(defun icicle-end-of-Completions ()
  "Scroll to the end of the *Completions* window, showing
   as many of the completions as possible."
  (interactive)
  (let ((window (get-buffer-window "*Completions*" 0)))
  (when window
    (save-selected-window
      (select-window window)
      (unless (= (window-end) (point-max))
        (goto-char (point-max))
        (scroll-down (1- (/ (window-height) 2)))
        (beginning-of-line))
      ))))

;; Configuration
;;
;; Besides incorporating the above functionality, the
;; configuration below adapts the default icicles keybindings
;; to my theme as described in the introduction. In particular,
;; C-w/C-v instead of C-v/M-v for scrolling, and M-w/M-v for
;; beginning and end of buffer. These are reflected in
;; the corresponding minibuffer commands below. In addition,
;; I add C-n/C-p as keys for moving among completions,
;; which is more convenient than up/down and is easy on
;; the mac keyboard as well.
;; 

(when (featurep 'icicles)
  (setq icicle-search-key-prefix (kbd "C-c i S"))
  (add-to-list 'icicle-modal-cycle-up-keys              "\C-p")
  (add-to-list 'icicle-modal-cycle-up-action-keys       "\C-\M-p")
  (add-to-list 'icicle-modal-cycle-up-alt-action-keys   [?\C-\S-p])
  (add-to-list 'icicle-modal-cycle-up-help-keys         [?\C-\M-\S-p])
  (add-to-list 'icicle-modal-cycle-down-keys            "\C-n")
  (add-to-list 'icicle-modal-cycle-down-action-keys     "\C-\M-n")
  (add-to-list 'icicle-modal-cycle-down-alt-action-keys [?\C-\S-n])
  (add-to-list 'icicle-modal-cycle-down-help-keys       [?\C-\M-\S-n])
  (add-hook 'completion-list-mode-hook
            (lambda ()
              (define-key completion-list-mode-map [\C-return] 'icicle-insert-completion) ;ATTN: switch these?
              (define-key completion-list-mode-map [return] 'icicle-switch-to/from-minibuffer))))

(when (featurep 'icicles)
  ;; This and the next two forms fixes a problems with the icicles minibuffer
  ;; prompt defaults when icicles is engaged. In Emacs 23 the default is
  ;; sometimes given twice in the minibuffer prompt, and in Emacs 24, the
  ;; wrong default is often given (at least for buffers).
  ;;
  ;; To start, we creates a new icicles variable
  ;; `icicle-suppress-default-pattern' to control this. The forms below
  ;; modify an existing function `icicle-read-from-minibuffer', in a
  ;; version dependent way
  (setq icicle-default-value t)
  (setq icicle-suppress-default-pattern-1 "\\(?:(.+)\\|\\[.+\\]\\|{.+}\\)")  ; internal delimiter match ok
  (defvar icicle-suppress-default-pattern 
    (concat "\\(?::\\s-*" icicle-suppress-default-pattern-1 "\\|" icicle-suppress-default-pattern-1 "\\s-*:\\)\\s-*$")
    "Regular expression used to determine if a default value should
be added to the minibuffer prompt. If the variable
`icicle-default-value' equals t and this pattern matches the
existing prompt, no default is added.

This is intended for cases, such as help-command, when the
minibuffer already contains a default, so icicles would add a
second copy when `icicle-default-value' is t. Setting
`icicle-default-value' to nil does not solve the problem as one
might want a default in the prompt, just not two. The default
value of this pattern is designed to catch the common case where
the end of the existing prompt is a string delimited by parens,
braces, or brackets that precedes or follows a colon and optional
whitespace."))

(when (featurep 'icicles)
  ;; The following function is a modified from that given in 
  ;; icicles-fn.el update # 13795 version 22.0.
  (defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read
                                             hist-m@%=!$+&^*z default-value inherit-input-method)
    "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
Icicles does not.  It is discussed in more detail below.

Third arg KEYMAP is a keymap to use while reading;
if omitted or nil, the default is `minibuffer-local-map'.

If fourth arg READ is non-nil, then interpret the result as a Lisp
object and return that object.  In other words, return this:

 (car (read-from-string INPUT-STRING))

Fifth arg HIST, if non-nil, specifies a history list and optionally
the initial position in the list.  It can be a symbol, which is the
history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  If a cons cell, HISTVAR is the history list
variable to use and HISTPOS is the initial position for use by the
minibuffer history commands.  For consistency, you should also
specify that element of the history as the value of
INITIAL-CONTENTS.  Positions are counted starting from 1 at the
beginning of the list.

Sixth arg DEFAULT-VALUE is a string, nil, or (for Emacs 23 and later)
a non-empty list of strings.  The strings are available to the user
as input via `\\<minibuffer-local-map>\\[next-history-element]'.

NOTE: Unlike a default-value parameter for some other functions such
as `completing-read', if the user hits `RET' with empty input then
DEFAULT-VALUE is NOT returned.  In that case, if READ is nil then
the empty string, \"\", is returned.  If READ is non-nil then the
DEFAULT-VALUE string (or the first string in DEFAULT-VALUE if
DEFAULT-VALUE is a list) is read.

Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
inherits the current input method and the setting of
`enable-multibyte-characters'.

If variable `minibuffer-allow-text-properties' is non-nil then the
string returned includes whatever text properties were present in
the minibuffer.  Otherwise the return value has no text properties.

Option `icicle-default-value' controls how the default value,
DEFAULT-VALUE, is treated.

The remainder of this documentation string describes parameter
INITIAL-CONTENTS in more detail.

If non-nil, INITIAL-CONTENTS is a string to be inserted into the
minibuffer before reading input.  Normally, point is put at the end of
that string.  However, if INITIAL-CONTENTS is (STRING . POSITION), the
initial input is STRING and point is placed at one-indexed position
POSITION in the minibuffer.  Any integer value less than or equal to
one puts point at the beginning of the string.  Note that this
behavior differs from the way such arguments are used in
`completing-read' and some other functions, which use zero-indexing
for POSITION."
    (unless initial-contents (setq initial-contents  ""))

    ;; Filter DEFAULT-VALUE using `icicle-filter-wo-input'. ;
    (when default-value
      (setq default-value
            (if (atom default-value)
                (icicle-filter-wo-input default-value)
              (delq nil (mapcar #'icicle-filter-wo-input default-value))))) ; Emacs 23 accepts a list. ;
    ;; Save new default value for caller (e.g. `icicle-lisp-vanilla-completing-read'. ;
    (setq icicle-filtered-default-value  default-value)

    ;; If a list of strings, use the first one for prompt etc. ;
    (let ((def-value  (if (consp default-value) (car default-value) default-value)))
      ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also. ;
      (when (and icicle-default-value  (not (eq icicle-default-value t))
                 def-value  (stringp initial-contents)  (string= "" initial-contents))
        (setq initial-contents  (if (integerp def-value) ; Character ;
                                    (string def-value)
                                  def-value)))
      (when (and def-value (eq icicle-default-value t)) ; Add DEFAULT-VALUE to PROMPT. ;
        (unless (and icicle-suppress-default-pattern
                     (string-match icicle-suppress-default-pattern prompt))
          (when (icicle-file-name-input-p) (setq def-value  (file-name-nondirectory def-value)))
          (setq prompt  (if (string-match "\\(.*\\)\\(: *\\)$" prompt)
                            (concat (substring prompt (match-beginning 1) (match-end 1)) " (" def-value
                                    ")" (substring prompt (match-beginning 2) (match-end 2)))
                          (concat prompt def-value))))))
    (icicle-ORIG-read-from-minibuffer
     prompt initial-contents keymap read hist-m@%=!$+&^*z default-value inherit-input-method)))

(add-my-hook icicle-mode-hook
  (setq read-file-name-completion-ignore-case t) ; Emacs >= 22
  (setq read-buffer-completion-ignore-case t)    ; Emacs >= 23
  (setq icicle-show-Completions-help-flag nil)
  (when (and icicle-mode (lookup-key icicle-mode-map "\C-h"))
    (map-keymap              ; move help keymap to each new help key
     (lambda (event binding)
       (mapcar
        (lambda (base-event)
          (define-key icicle-mode-map
            (vconcat base-event (vector event)) binding))
        my/help-events))
     (lookup-key icicle-mode-map "\C-h"))
    (define-key icicle-mode-map "\C-h" nil))
  (when icicle-mode
    (define-key icicle-mode-map "\C-c/" nil)  ; conflicts with org-sparse-tree in org-mode
    (define-key icicle-mode-map "\C-c'" nil)) ; conflicts with org and AucTeX
  (if icicle-mode
      (add-hook 'completion-setup-hook 'my/icicle-show-sort-order t)
    (remove-hook 'completion-setup-hook 'my/icicle-show-sort-order)))

(add-my-hook icicle-minibuffer-setup-hook
  (when (not (fboundp 'icicle-scroll-Completions-forward))
    (defalias 'icicle-scroll-Completions-forward 'icicle-scroll-Completions))
  (when (not (fboundp 'icicle-scroll-Completions-backward))
    (defalias 'icicle-scroll-Completions-backward 'icicle-scroll-Completions-up))
  (define-key minibuffer-local-completion-map "\C-w"     'icicle-scroll-Completions-backward)
  (define-key minibuffer-local-completion-map "\C-v"     'icicle-scroll-Completions-forward)
  (define-key minibuffer-local-completion-map "\M-w"     'icicle-beginning-of-Completions)
  (define-key minibuffer-local-completion-map "\M-v"     'icicle-end-of-Completions)
  (define-key minibuffer-local-completion-map "\M-k"     'icicle-kill-region)
  (define-key minibuffer-local-completion-map "\M-e"     'icicle-erase-minibuffer-or-history-element)
  (define-key minibuffer-local-completion-map "\M-c"     'icicle-switch-to-Completions-buf)
  (define-key minibuffer-local-completion-map "\C-\M-c"  'icicle-toggle-case-sensitivity)
  (define-key minibuffer-local-completion-map "\C-x\C-o" 'icicle-switch-to/from-minibuffer)
  ;; process new help key position
  (define-key minibuffer-local-completion-map "\M-y" 'icicle-history)
  (define-key minibuffer-local-completion-map "\M-h" nil)
  ;; the following no longer appear necessary
  ;(define-key minibuffer-local-completion-map "\C-n" 'icicle-next-candidate-per-mode)
  ;(define-key minibuffer-local-completion-map "\C-p" 'icicle-previous-candidate-per-mode)
  )


(when (and (featurep 'icicles)
           (>= emacs-major-version 24))
  ;; Free var here: `icicle-bufflist' is bound by `icicle-buffer-bindings'.
  (defun icicle-default-buffer-names (&optional arg)
    "Default buffer names (Emacs 23+) or name (< Emacs 23).
For Emacs 23+, up to four names are returned.

Optional ARG is used only for Emacs 23+.  Its meaning is the same as
the prefix argument in Icicles buffer commands:
 * nil       :  all buffers
 * Number > 0: buffers visiting files or directories (Dired)
 * Number < 0: buffers associated with the selected frame
 * Number = 0: buffers with the same mode as the current buffer
 * Cons      : buffers with the same mode as current, or with
               a mode that the current mode is derived from"
    ;; Note: CRG 16 Mar 2013
    ;; The current version 22.0/25437 is broken in that it only
    ;; returns the buffer-list (excluding the current buffer)
    ;; in the leading case (no prefix arg).  This is almost always
    ;; useless as it is tends to include visible buffers.
    ;; Note arg is ignored in this version as well.
    ;; Here I restore the other-buffer functionality and will
    ;; continue to ignore arg for now.
    (let ((bname  (buffer-name (if (fboundp 'another-buffer) ; In `misc-fns.el'.
                                   (another-buffer nil t)
                                 (other-buffer (current-buffer))))))
      (if (< emacs-major-version 23)
          (if (and icicle-bufflist  (not (member bname icicle-bufflist)))
              (car icicle-bufflist)
            bname)
        ;; Emacs 23 accepts a list of default values.  ; Just keep the first 4.  (This could be an option.)
        (let* ((bfnames
                (cons bname
                      (mapcar #'buffer-name
                              (delete (current-buffer)
                                      (or icicle-bufflist (buffer-list)))))))
          (when icicle-buffer-ignore-space-prefix-flag
            (setq bfnames (icicle-remove-if (lambda (bfname)
                                              (icicle-string-match-p "^ " bfname))
                                            bfnames)))
          (icicle-first-N 4 bfnames))))))


(global-set-key "\C-x\M-\C-o" 'icicle-switch-to/from-minibuffer)


; Icicle
;(setq debug-on-error nil)
;(setq eval-expression-debug-on-error nil)

(defun my/icicle-fixes ()
  ;; Removing eval-expression from top-level-keybindings should work better
  ;; if done at the right time.
  ;(delete-if (lambda (x) (eq (car x) 'eval-expression)) icicle-top-level-key-bindings)
  ;; Better yet: customize icicle-top-level-key-bindings
  ;; Similarly remove icicle-search-generic on C-c` because this interferes with org
  ;; Couldn't hurt to remove other icicle-search-* from these as I have my own submap for this
  ;; Except icicle-search-word which doesn't interfere yet and might be useful
  (when (and (boundp 'icicle-mode-map)
             icicle-mode-map
             (keymapp icicle-mode-map))
    (define-key icicle-mode-map [remap pp-eval-expression] 'eval-expression)
    (define-key icicle-mode-map [remap eval-expression] nil))
  (add-to-list 'icicle-keymaps-for-key-completion 'my-icicles-map t))

(add-hook 'icicle-mode-hook 'my/icicle-fixes t)



