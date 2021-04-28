;; ATTN: Need to have separate escape keymaps for (at least) string versus delimiter pairs
;; For instance, typing 'a${index}' will be escaped by the second } inappropriately.
;; Perhaps need to have `escape classes` that determine the keymap in the magic space.
;; A map of escape classes to keymap in the code below
;;    '(let ((pt (point)))
;;       (add-text-properties
;;        (1- pt) pt
;;        '(js2smp--magic-space t keymap js2smp--magic-map)))
;; e.g., replace js2smp--magic-map by
;;
;;      ,(map-elt js2smp--magic-maps (string open-key close-key) (js2smp--default-magic-map open-key close-key))
;; where a ` is used instead of ' earlier. The js2smp--magic-maps is an alist or hash
;; that (eventually) can customized and is initially nil. The default-magic map
;; function includes tab, return, ;, close-key (checked for stringized version),
;; and maybe the slurp/expand keys (still tbd) and so will be useful for most
;; current purposes. Can extend this to have classes at this level, but seems better to asssociate
;; the (string open-key close-key) pairs with classes within the customized maps.
;;
;; Q: How much of this functionality is implementable in smart-parens customization??
;; My current thinking is to provide a shim to add this to smart-parens if one is using it and
;; otherwise do it natively.

(defun js2-inside-string-or-comment-p ()
  "Are we currently inside a string or comment?"
  (let ((node-name (js2-node-short-name (js2-node-at-point))))
    (member node-name '("js2-comment-node" "js2-string-node"))))

(defun js2-inside-string-p ()
  "Are we currently inside a string?"
  (let ((node-name (js2-node-short-name (js2-node-at-point))))
    (string-equal node-name "js2-string-node")))

(defun js2smp--paren-escape ()
  "Escape paren pair, deleting magic space if starting there."
  (interactive)
  (when (= (char-after) ?\ ) (delete-char 1))
  (up-list 1 t t))  ;; formerly 1 t,  the second t helps with ''; does it break anything?

(defun js2smp--paren-escape-or-string ()
  "Escape pair if not in string, else Escape paren pair, deleting magic space if starting there."
  (interactive)
  (if (js2-inside-string-p)
      (js2smp--paren-escape)
    (js2-smart-pair-open* last-command-event last-command-event)))

(defun js2smp--paren-escape-semicolon ()
  "Escape paren pair, deleting magic space if starting there."
  (interactive)
  (js2smp--paren-escape)
  (insert ";"))

(defun js2smp--paren-escape-return ()
  "Escape paren pair, deleting magic space if starting there."
  (interactive)
  (insert "\n")
  (js2smp--paren-escape)
  (js2-indent-line))

(defun js2smp--paren-comma ()
  "Insert spaced comma, keeping point on magic space."
  (interactive)
  (insert ", ")
  (indent-according-to-mode))

(defun js2smp--paren-expand ()
  "With point on magic space, expand region over following balanced expressions.
This can be followed with `js2smp--paren-slurp' (M-;) to move
those expressions inside the parentheses."
  (interactive)
  (save-excursion
    (up-list 1 t)
    (mark-sexp nil t)))

(defun js2smp--paren-slurp (&optional save-initial-space)
  "Moves marked region following paren pair inside parentheses.
Initial spaces following the end of the current paren pair are
deleted unless a prefix argument is given (SAVE-INITIAL-SPACE
non-nil). This is usually preceded by `js2smp--paren-expand' but
applies to any region from point forward."
  (interactive "P")
  (when (and mark-active (> (mark) (point)))
    (let* ((end-of-list (save-excursion (up-list 1 t) (point)))
           (delta-ws (if save-initial-space
                         0
                       (save-excursion
                         (up-list 1 t)
                         (skip-syntax-forward " "))))
           (end-of-slurp (- (mark) 2 delta-ws))
           (yank-excluded-properties (remq 'keymap yank-excluded-properties)))
      (unless save-initial-space
        (delete-region end-of-list (+ end-of-list delta-ws)))
      (kill-region (point) end-of-list)
      (goto-char end-of-slurp)
      (yank)
      (forward-char -2))))

(defvar js2smp--magic-map (let ((m (make-sparse-keymap)))
                            (define-key m (kbd ",") 'js2smp--paren-comma)
                            (define-key m (kbd ")") 'js2smp--paren-escape)
                            (define-key m (kbd "]") 'js2smp--paren-escape)
                            (define-key m (kbd "}") 'js2smp--paren-escape)
                            (define-key m (kbd "\"") 'js2smp--paren-escape-or-string)
                            (define-key m (kbd "'") 'js2smp--paren-escape-or-string)
                            (define-key m (kbd "`") 'js2smp--paren-escape-or-string)
                            ;; Consider whether TAB better for this or completion
                            (define-key m (kbd "<tab>") 'js2smp--paren-escape)
                            (define-key m (kbd "<return>") 'js2smp--paren-escape-return)
                            (define-key m (kbd ";") 'js2smp--paren-escape-semicolon)
                            (define-key m (kbd "C-;") 'js2smp--paren-expand)
                            (define-key m (kbd "M-;") 'js2smp--paren-slurp)
                            m)
  "Keymap active in fresh space in the middle of a new smart open paren.")
(fset 'js2smp--magic-map js2smp--magic-map)

(defvar js2smp--match-table
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ?\`)))

(defun js2-smart-pair-open (&optional literal)
  "Inserts properly a properly spaced paren pair with an active keymap inside.
Point is left in the middle of the paren pair and associated with
a special keymap, where tab deletes the extra space and moves
point out of the parentheses and comma inserts a spaced comma,
keeping point on the special space character. "
  (interactive "P")
  (if (or literal (js2-inside-string-or-comment-p))
      (self-insert-command (if (integerp literal) literal 1))
    (let* ((this-key last-command-event)
           (match-key (cdr (assoc this-key js2smp--match-table))))
      (js2-smart-pair-open* this-key match-key))))

(defun js2-smart-pair-open* (open-key close-key)
  "Inserts properly a properly spaced paren pair with an active keymap inside.
Point is left in the middle of the paren pair and associated with
a special keymap, where tab deletes the extra space and moves
point out of the parentheses and comma inserts a spaced comma,
keeping point on the special space character. "
  (interactive "P")
  (let* ((skeleton-pair t)
         (skeleton-pair-alist `((,open-key
                                 _ _ " "
                                 '(let ((pt (point)))
                                    (add-text-properties
                                     (1- pt) pt
                                     '(js2smp--magic-space t keymap js2smp--magic-map)))
                                 ,close-key))))
    (skeleton-pair-insert-maybe nil)))

(defun js2-smart-pair-setup ()
  (interactive)
  (when (derived-mode-p 'js2-mode)
    (local-set-key (kbd "(")  #'js2-smart-pair-open)
    (local-set-key (kbd "[")  #'js2-smart-pair-open)
    (local-set-key (kbd "{")  #'js2-smart-pair-open)
    (local-set-key (kbd "\"") #'js2-smart-pair-open)
    (local-set-key (kbd "'")  #'js2-smart-pair-open)
    (local-set-key (kbd "`")  #'js2-smart-pair-open)))
