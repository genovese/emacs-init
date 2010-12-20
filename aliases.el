;; (@* "Aliases")
;;
;; Aliases to existing functions or macros that
;; I find useful or elegant or that I find myself
;; wanting to do frequently.
;;

(defalias 'filter           'remove-if-not)
(defalias 'foreach          'dolist)

(defalias '==               'equal)
(defalias '===              'eq)   
(defalias 'num-equal        '=)   ; goes with 'string-equal
(defalias 'num-not-equal    '/=) 

(defalias 'to-string        'prin1-to-string)
(defalias 'stringify        'prin1-to-string)

(defalias 'symbol-string    'symbol-name)
(defalias 'string-symbol    'intern)
(defalias 'symbol-to-string 'symbol-name)
(defalias 'string-to-symbol 'intern)

;; Scheme/Clojure style predicates read nicely.
;; It's probably not a good idea to use these
;; aliases in portable emacs-lisp code because
;; it imposes the aliases on anyone who loads it.
;; But for casual use and in the .emacs file,
;; it might make sense. I've recently added these,
;; so much of the code below does not use them.
;; We'll see how things develop....

(defalias 'buffer-alive?    'buffer-live-p)
(defalias 'feature-loaded?  'featurep)
(defalias 'frame-alive?     'frame-live-p)
(defalias 'nil?             'null)
(defalias 'num-equal?       '=) 
(defalias 'num-not-equal?   '/=) 
(defalias 'string-equal?    'string-equal)
(defalias 'whole-number?    'wholenump)
(defalias 'window-alive?    'window-live-p)

(dolist
    (sym '(arrayp atom bobp bolp booleanp boundp bound-and-true-p 
		  bufferp buffer-modified-p char-or-string-p char-table-p
		  commandp consp fboundp floatp framep functionp
		  integer-or-marker-p integerp keymapp keywordp
		  listp markerp nlistp number-or-marker-p numberp
		  overlayp processp sequencep stringp string-lessp
		  string-match-p string-or-null-p string-prefix-p
		  subrp symbolp vectorp windowp zerop))
  (defalias
    (string-symbol (replace-regexp-in-string "\\(?:-?p\\)?$" "?" (symbol-string sym)))
    sym))

