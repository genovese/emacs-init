(define-prefix-command 'my/text-prefix 'my/text-map
  "Commonly used text-manipulation commands")
(define-key my/text-map "c" 'capitalize-word)
(define-key my/text-map "C" 'capitalize-region)
(define-key my/text-map "l" 'downcase-word)
(define-key my/text-map "L" 'downcase-region)
(define-key my/text-map "u" 'upcase-word)
(define-key my/text-map "U" 'upcase-region)
(define-key my/text-map "f" 'fill-individual-paragraphs)
(define-key my/text-map "F" 'fill-region)
(define-key my/text-map "x" 'unfill-region)
(global-set-key (kbd "C-x 9") 'my/text-prefix)


;;; String Inflection modifications

(require 'string-inflection)
(setq string-inflection-skip-backward-when-done t)

(defun my/string-inflection-lower-kebab-case-p (str)
  "if foo-bar => t"
  (let ((case-fold-search nil))
    (and (string-inflection-kebab-case-p str)
         (not (string-match "[A-Z]" str)))))

(defun my/string-inflection-upcase-p (str)
  "if FOO-BAR | FOO_BAR => t"
  (let ((case-fold-search nil))
    (string-match "\\`[-A-Z0-9_]+\\'" str)))

(defun my/string-inflection-underscore-function (str)
  "Like string-inflection-underscore-function but preserve case"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))))

(defun my/string-inflection-pc-pascal-case-function (str)
  "foo_bar => FooBar"
  (setq str (my/string-inflection-underscore-function str))
  (mapconcat 'capitalize (split-string str "_") ""))



(defun my/string-inflection-clojure-style-cycle ()
  "foo-bar => FOO-BAR => FooBar => foo-bar"
  (interactive)
  (string-inflection-insert
   (my/string-inflection-clojure-style-cycle-function (string-inflection-get-current-word))))

(defun my/string-inflection-clojure-style-cycle-function (str)
  "foo-bar => FOO-BAR => FooBar => foo-bar"
  (cond
   ((my/string-inflection-lower-kebab-case-p str)
    (upcase str))
   ((string-inflection-kebab-case-p str)
    (my/string-inflection-pc-pascal-case-function str))
   ((my/string-inflection-upcase-p str)
    (my/string-inflection-pc-pascal-case-function str))
   (t
    (string-inflection-kebab-case-function str))))


(defun my/string-inflection-js-style-cycle ()
  "foo_bar => fooBar => FooBar => fooBar
   FOO_BAR =>           FooBar => fooBar"
  (interactive)
  (string-inflection-insert
   (my/string-inflection-js-style-cycle-function (string-inflection-get-current-word))))

(defun my/string-inflection-js-style-cycle-function (str)
  "foo_bar => fooBar => FooBar => fooBar
   FOO_BAR =>           FooBar => fooBar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-camelcase-p str)
    (string-inflection-pascal-case-function str))
   (t
    (string-inflection-lower-camelcase-function str))))

;;; Application

(defvar my/string-inflection-dispatch
  '((clojure-mode . my/string-inflection-clojure-style-cycle)
    (clojurescript-mode . my/string-inflection-clojure-style-cycle)
    (clojurec-mode . my/string-inflection-clojure-style-cycle)
    (cider-repl-mode . my/string-inflection-clojure-style-cycle)
    (emacs-lisp-mode . my/string-inflection-clojure-style-cycle)
    (javascript-mode . my/string-inflection-js-style-cycle)
    (js2-mode . my/string-inflection-js-style-cycle)
    (python-mode  . string-inflection-python-style-cycle)))

(defun my/string-inflection-cycle ()
  (interactive)
  (if-let (mode-specific (assoc major-mode my/string-inflection-dispatch))
      (call-interactively (cdr mode-specific))
    (call-interactively #'string-inflection-cycle)))

(define-key my/text-map "9" #'my/string-inflection-cycle)
(define-key my/text-map [return] #'my/string-inflection-cycle)


