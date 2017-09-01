;;; mods/help.el --- help modifications -*- lexical-binding: t; -*-

;; Copyright (C) 2017- Christopher R. Genovese, all rights reserved.

;; Author: Christopher R. Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; Keywords: emacs help
;; Version: 0.0.1


;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;;; Commentary:
;;
;; Help-mode Added Functionality 
;;
;; * Easy navigation to and from *Help* buffer/window
;;
;;   I often navigate through links in help information in a single help
;;   buffer, while working in another primary buffer, so it is useful to
;;   be able to move from the given buffer into and out of an existing
;;   help buffer. This is especially true when the window structure in
;;   the frame is complicated. The functions `my/help-return-from-help'
;;   and `my/help-goto-help' provide that service. By default, they are
;;   bound to g in help-mode and <help-char>-g below.
;;
;; * Adjusted keybindings in help-mode.
;;
;;   Slightly different from the default, but I find them much nicer.
;;
;; * Avoiding view-mode shadowing of help-mode key bindings.
;;
;;   Help buffers use view-minor mode, which has the effect of shadowing
;;   any conflicting keybindings set in help-mode-hook. The help
;;   system uses `minor-mode-overriding-map-alist' to prevent that,
;;   but the way this is done does not allow overriding this map
;;   in the help-mode-hook. (Short reason: `help-make-xrefs' is
;;   called *after* the help mode hook in setting up the buffer,
;;   resetting `minor-mode-overriding-map-alist'.) The functions
;;   `my/help-make-override-map' and `my/help-override-view-map'
;;   and the configuration below prevent this from happening.
;;
;; * Help Buttons that open code in read-only mode
;;
;; * Easy Mechanism for defining commands that act on help buffer
;;   See `my/define-remote-help-command' and examples below.
;;

;;; Code:


;; Easy navigation to and from *Help* buffer/window
;;ATTN: note view-return-to-alist is obsolete in >= 24
;;      use `quit-restore window-parameter in help buffer

(defun my/help-return-from-help (&optional bury-help)
  "From *Help* buffer, return to previous buffer.
This intentionally does not delete the help buffer or window, but
the prefix arg (when called interactively) or BURY-HELP can
modify the fate of the help buffer/window. With a single prefix
arg (or BURY-HELP eq 4), the help buffer is buried; with two
prefix args (or BURY-HELP eq 16), the help window is deleted.
This function uses the window given in `view-return-to-alist' if
available but ignores the buffer information in those entries.
See also `View-leave' and `View-kill-and-leave' for
alternatives."
  (interactive "p")
  (let* ((win-info
          (and (boundp 'view-return-to-alist)
               view-return-to-alist
               (assq (selected-window) view-return-to-alist)))
         (old-window (and win-info (car (cdr win-info))))
         (cbuf (current-buffer))
         (cwin (selected-window)))
    (if (and old-window (window-live-p old-window))
        (select-window old-window)
      (pop-to-buffer (other-buffer cbuf t)))
    (cond
     ((and (eq bury-help 16)
           (not (one-window-p t)))
      (delete-window cwin))
     ((eq bury-help 4)
      (with-selected-window cwin (bury-buffer))))))

(defun my/help-goto-help ()
  "If a buffer named *Help* exists, move to it."
  (interactive)
  (let* ((help-buf (get-buffer "*Help*")) ; ATTN: use (help-buffer) instead??
         (help-win (get-buffer-window help-buf)))
    (when help-buf
      (if (and help-win (window-live-p help-win))
          (select-window help-win t)
        (switch-to-buffer-other-window help-buf t)))))


;; Adjusted keybindings in help-mode.

(defun my/help-make-override-map (&optional event-list)
  "Create keymap used for `minor-mode-overriding-map-alist' to
prevent parent mode `view-mode' from shadowing several keys we
need. EVENT-LIST is a list of event specifiers, in either vector
or string form (as for local-set-key), to unshadow. If EVENT-LIST
is nil, `\r' is unshadowed as is done by default in help-mode. If
not EVENT-LIST is not nil, the `\r' is not included. Returns the
constructed keymap."
  (let ((map (make-sparse-keymap)))
    (when (null event-list)
      (setq event-list (list "\r")))
    (when (not (featurep 'view)) 
      (require 'view))               ; make sure view-mode-map is loaded
    (set-keymap-parent map view-mode-map)
    (dolist (key event-list)
      (define-key map key nil))
    map))

;; ATTN: Does view-mode still shadow help-mode in 24+?
(defun my/help-override-view-map (unshadow-map)
  "Prevent parent mode `view-mode' from shadowing several keys we
need. Resets `minor-mode-overriding-map-alist', adding an entry
for `view-mode' to do this. UNSHADOW-MAP is a keymap to unshadow,
as constructed by `my/help-make-override-map'."
  (set (make-local-variable 'minor-mode-overriding-map-alist)
       (list (cons 'view-mode unshadow-map))))

;; Configuration
;;
;; The main changes, as mentioned above, are some different keybindings,
;; easy navigation bound to g, and overriding of view-mode shadowing.
;; The latter is, in my view, a design bug in the current help-mode.el
;; and help.el, which will hopefully be rectified. So it is subject
;; to change in the future.
;; 

(defun beginning-of-help-buffer ()
  "Move to the beginning of the help buffer from any window."
  (interactive)
  (let ((window (get-buffer-window "*Help*" 0)))
    (when window
      (save-selected-window
        (select-window window)
        (goto-char (point-min))))))

(defun end-of-help-buffer ()
  "Move to the end of the help buffer from any window."
  (interactive)
  (let ((window (get-buffer-window "*Help*" 0)))
    (when window
      (save-selected-window
        (select-window window)
        (goto-char (point-max))))))

(defun scroll-help-window-forward (&optional arg)
  (interactive "P")
  (let* ((buf (get-buffer "*Help*"))
         (other-window-scroll-buffer buf))
    (when other-window-scroll-buffer
      (if (equal buf (current-buffer))
          (scroll-up arg)
       (scroll-other-window arg)))))

(defun scroll-help-window-backward (&optional arg)
  (interactive "P")
  (let* ((buf (get-buffer "*Help*"))
         (other-window-scroll-buffer buf))
    (when other-window-scroll-buffer
      (if (equal buf (current-buffer))
          (scroll-down arg)
        (scroll-other-window
         (case arg
           ('- nil)
           ((nil) '-)
           (t (- (prefix-numeric-value arg)))))))))


;; Make help-map and help-mode-map more consistent
;;  ATTN: Is the view mode shadowing still a problem?
(defvar my/help-keybindings
  '((","        . rename-uniquely)
    ("a"        . command-apropos)
    ("\M-a"     . apropos-documentation)
    ("b"        . describe-bindings)
    ("c"        . describe-key-briefly)
    ("C"        . describe-coding-system)
    ("d"        . apropos-documentation)
    ("e"        . view-echo-area-messages)
    ("E"        . view-emacs-FAQ)
    ("f"        . describe-function)
    ("F"        . describe-face)
    ("g"        . my/help-return-from-help)
    ("G"        . describe-gnu-project)
    ("i"        . info)
    ("I"        . Info-goto-emacs-command-node)
    ("k"        . describe-key)
    ("K"        . Info-goto-emacs-key-command-node)
    ("l"        . help-go-back)   ; like Info-mode, muscle memory
    ("L"        . view-lossage)   ; minor inconsistency with help-map
    ("\M-l"     . locate-library)
    ("m"        . describe-mode)
    ("n"        . help-go-forward)
    ("p"        . finder-by-keyword)
    ("q"        . quit-window)
    ("\M-q"     . View-kill-and-leave)
    ("r"        . info-emacs-manual)
    ("s"        . describe-syntax)
    ("S"        . info-lookup-symbol)
    ("t"        . describe-theme)
    ("T"        . help-with-tutorial)
    ("u"        . manual-entry)
    ("v"        . describe-variable)
    ("V"        . describe-language-environment) ; another minor inconsistency
    ("w"        . where-is)
    ("y"        . View-leave)           ; could do bury-buffer instead
    ("\C-c\C-c" . help-follow-symbol)
    ("\C-c\C-b" . help-go-back)
    ("\C-c\C-f" . help-go-forward)
    ([\M-left]  . help-go-back)
    ([\M-right] . help-go-forward))
  "Keybindings used in help-mode as well as keys to override so
that they are not shadowed by view-mode. This changes a few keys
and adds some useful functionality to the default help-mode map.
It is mostly consistent with the help bindings, although some
minor inconsistencies with help-map keys are allowed if they make
commonly used operations easier at the expense of shadowing less
commonly used functions in this mode.")


;; Avoiding view-mode shadowing of help-mode key bindings.

(defvar my/help-unshadow-map
  (my/help-make-override-map (mapcar #'car my/help-keybindings))
  "Keymap setting keys to be visible despite the bindings of
view-mode, which usually shadow various help keys. This
unshadowing is done by setting `minor-mode-overrriding-map-alist';
see `my/help-override-view-map'.")

;; Uncomment this if something needs to be added to my/help-mode-hook
;(add-my-hook help-mode-hook
;    ;(dolist (binding my/help-keybindings)
;    ;  (define-key help-mode-map (car binding) (cdr binding)))
;    ;(my/help-override-view-map my/help-unshadow-map)
;    (ignore "Nothing needed yet given the remaining config."))

;; Bind the keys here instead of in help-mode hook because
;; there are so many of them.  ??
(eval-after-load 'help-mode
  '(dolist (binding my/help-keybindings)
     (define-key help-mode-map (car binding) (cdr binding))))

(eval-after-load 'help
  '(progn
     (define-key help-map "g" 'my/help-goto-help)
     (define-key help-map "\C-v" 'scroll-help-window-forward)
     (define-key help-map "\C-w" 'scroll-help-window-backward)
     (define-key help-map "\M-v" 'end-of-help-buffer)
     (define-key help-map "\M-w" 'beginning-of-help-buffer)))

;;ATTN:24.4
(defadvice help-make-xrefs (after unshadow-view-keys last activate)
  "The variable `minor-mode-overriding-map-alist' is reset in
`help-make-xrefs' which is used for temporary buffer creation
and so undoes any attempt to adjust the map in the help-mode-hook."
  (my/help-override-view-map my/help-unshadow-map))

(setq help-window-select nil) ;; only go to help window on request
;; ATTN:idea : overriding mode for remote help window navigation and link following


;;; Make Help file buttons open code in read-only buffer

(define-button-type 'help-function-def
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (require 'find-func)
		   (when (eq file 'C-source)
		     (setq file
			   (help-C-file-name (indirect-function fun) 'fun)))
		   ;; Don't use find-function-noselect because it follows
		   ;; aliases (which fails for built-in functions).
		   (let ((location
			  (find-function-search-for-symbol fun nil file)))
		     (pop-to-buffer (car location))
                     (read-only-mode 1)
		     (if (cdr location)
			 (goto-char (cdr location))
		       (message "Unable to find location in file"))))
  'help-echo (purecopy "mouse-2, RET: find function's definition"))


;;;; Remote Help commands

(defmacro my/define-remote-help-command (name &rest body)
  "Define a command to operate on the help buffer from another window.
If NAME is a symbol, it is used as the name of the command
function; otherwise it is considered the first part of BODY and
an anonymous function is returned. BODY (with NAME prepended if
not a symbol) should consist an 1. an optional docstring, 2. an optional
sequence (in property-list style) of alternating keywords and values that
specify options (see below), and 3. a sequence of lisp forms to be executed in the help
buffer as an interactive command.

Currently allowed options are
       :key <KEY-SEQUENCE>   -- key sequence bound via `bind-key' to this command
       :map <KEY-MAP-SYMBOL> -- key map in which the command will be bound
                                (default: help-map)
       :buffer <STRING>      -- the name of the help buffer
                                (default: \"*Help*\")

Returns the command function symbol or anonymous lambda form.
This function takes a single optional argument for the prefix arg,
and although the body forms cannot access this argument by name,
they can use `current-prefix-arg' as needed.

Examples:
    (my/define-remote-help-command :key \"TAB\"
                            (forward-button
                             (prefix-numeric-value current-prefix-arg)))
    (my/define-remote-help-command :key \"RET\" (help-follow-symbol))
    (my/define-remote-help-command help-back    (help-go-back))
    (my/define-remote-help-command help-forward (help-go-forward))
"
  (let* ((anonymous? (or (keywordp name) (not (symbolp name))))
         (body (if anonymous? (cons name body) body))
         (defn (if anonymous? `(lambda) `(defun ,name))))
    (cl-destructuring-bind (options docstring forms)
        (macro-body-with-kw-options body)
      (let* ((arg (cl-gensym "arg"))
             (win (cl-gensym "win"))
             (map (gethash :map options 'help-map))
             (key (gethash :key options))
             (buf (gethash :buffer options "*Help*"))
             (fun `(,@defn (&optional ,arg)
                     ,docstring
                     (interactive "p")
                     (-when-let (,win (->> ,buf
                                        get-buffer 
                                        get-buffer-window-list
                                        (-first #'window-live-p)))
                       (with-selected-window ,win
                         (with-current-buffer (window-buffer ,win)
                           ,@forms))))))
        (cond ((not key)
               fun)
              (anonymous?
               (let ((f (cl-gensym "fn")))
                 `(let ((,f ,fun))
                    (bind-key ,key ,f ,map)
                    ,f)))
              (t
               `(progn
                  ,fun
                  (bind-key ,key ',name ,map)
                  ',name)))))))

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
(my/define-remote-help-command :key "q"   (quit-window))


;;; mods/help.el ends here
