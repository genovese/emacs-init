;;; my-env.el -- local environment and application settings
;;
;; This version of the file is used on my Macbook Pro, running OS X 10.12.6.
;;
;; Date: 29 Oct 2017


;; Browser
(setq browse-url-firefox-program "open")
(setq browse-url-firefox-arguments '("-a" "Firefox"))


;; Paths
(setenv "PATH"    ".:./bin:../bin:/Users/genovese/bin:/usr/local/priority/bin:/usr/local/bin:/Library/TeX/texbin:/Users/genovese/Library/Python/Current/bin:/Users/genovese/.local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin")
(setenv "MANPATH" "/Users/genovese/man:/usr/local/priority/man:/Library/TeX/Distributions/.DefaultTeX/Contents/Man:/usr/local/man:/usr/local/share/man:/usr/share/man:/usr/local/share/man")

(setq exec-path
      (-as-> (getenv "PATH") path?
             (s-split ":" path?)
             (nthcdr 3 path?)        ; remove relative components
             (append path? exec-path)
             (cl-delete-duplicates path? :test 'string-equal :from-end t)))

(setq emacs-app-base-dir     # currently "/usr/local/Cellar/emacs/HEAD-3ad8ca4/"
      (thread-first "emacs"
        executable-find
        file-truename
        f-parent
        f-parent
        file-name-as-directory))

(setenv "INFOPATH" (s-join ":"
                           (list (concat emacs-app-base-dir "share/info/emacs") 
                                 "/usr/local/share/emacs/info"
                                 "/usr/local/share/info"
                                 "/usr/share/info"
                                 "/usr/local/info")))

(setenv "TEXINPUTS"   ":./tex:../tex:../../tex:/Users/genovese/tex/ProbabilityExplained:/Users/genovese/tex")


;; Emacs App locations
(setenv "EMACS_APP" (concat emacs-app-base-dir "Emacs.app/Contents/MacOS/Emacs"))
(setenv "EMACS_BIN" (concat emacs-app-base-dir "bin/"))
(setenv "EMACS_DIR" emacs-app-base-dir)
(setenv "EMACS_SHARE" "/usr/local/share/emacs/")


;; Special directories
(setenv "s750" (f-join (getenv "HOME") "class" "s750"))  ; class directories


;; General Emacs-useful environment settings
(setenv "PAGER" "cat")

;;; end my-env.el
