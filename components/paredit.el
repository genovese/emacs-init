;;; paredit.el -- paredit customizations -*- lexical-binding: t; -*-

(defun paredit-reset-commands (&optional commands)
  "Reset paredit command keys and annotation with examples and documentation.
If COMMANDS is not nil, `paredit-commands' is reset to COMMANDS
before redefining the key map. In this case, COMMANDS should have
the format as documented in `paredit-commands'. This command respects
`minor-mode-map-alist' and so can be called before or after loading
paredit.

   Note: This is needed because paredit makes it somewhat painful
to substantially change its key commands. Specifically,
`paredit-commands' is set with `setq' so simply resetting
`paredit-commands' and calling `paredit-define-keys' does not
eliminate the default settings. The solution is to clear out
`paredit-mode-map', but it cannot be just redefined because it is
linked into `minor-mode-map-alist'."
  (when commands
    (setq paredit-commands commands))
  (setcdr paredit-mode-map nil) ;; reset also in minor-mode-map-alist
  (paredit-define-keys)
  (paredit-annotate-mode-with-examples)
  (paredit-annotate-functions-with-examples))

(use-package paredit  
  :init
  (setq my/paredit-commands ;; changes from default marked with ; CRG
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
          ("M-h" ; CRG  -- was ,(concat "M-" paredit-backward-delete-key)
           paredit-backward-kill-word
           ("(foo bar)    ; baz\n(quux)|"
            "(foo bar)    ; baz\n(|)"
            "(foo bar)    ; |\n()"
            "(foo |)    ; \n()"
            "(|)    ; \n()"))

          "Movement & Navigation"
          ("A-f"     paredit-forward
           ("(foo |(bar baz) quux)"
            "(foo (bar baz)| quux)")
           ("(foo (bar)|)"
            "(foo (bar))|"))
          ("A-b"     paredit-backward
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
          ("M-i w"     paredit-wrap-sexp) ; CRG
          ("M-q"       paredit-reindent-defun)))
  :config (progn
            (paredit-reset-commands my/paredit-commands)
            (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode t)
            (add-hook 'lisp-mode-hook #'enable-paredit-mode t)
            (add-hook 'clojure-mode-hook #'enable-paredit-mode)))


;;; paredit.el ends here
