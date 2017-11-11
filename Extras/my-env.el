;;; my-env.el -- local environment and application settings
;;
;; This version of the file is used on my Macbook Pro, running OS X 10.12.6.
;; This file should work properly across platforms. It assumes that the
;; file is loaded during init as in dot-emacs.el.
;;
;; The PATH environment value should be set properly, especially on Mac OS X.
;; On linux or windows, emacs inherits the user's path, so the setenv line
;; for PATH can be removed or commented out.
;;
;; Date: 11 Nov 2017


;; Browser
(when (eq my-platform 'macosx)
  (setq browse-url-firefox-program "open")
  (setq browse-url-firefox-arguments '("-a" "Firefox")))


;; Paths
(setenv "PATH" ".:./bin:../bin:/Users/genovese/bin:/usr/local/priority/bin:/usr/local/bin:/Library/TeX/texbin:/Users/genovese/Library/Python/Current/bin:/Users/genovese/.local/bin:/usr/bin:/bin:/usr/X11R6/bin:/usr/sbin:/sbin")

(setq exec-path
      (-as-> (getenv "PATH") path?
             (s-split ":" path?)
             (-remove #'f-relative-p path?)
             (append path? exec-path)
             (cl-delete-duplicates path? :test 'string-equal :from-end t)))

(setq emacs-app-base-dir     ; currently "/usr/local/Cellar/emacs/HEAD-3ad8ca4/"
      (thread-first "emacs"
        executable-find
        file-truename
        f-parent
        f-parent             ; assumes emacs is in a bin subdirectory
        file-name-as-directory))

(setenv "MANPATH" (s-join ":"
                          (list (f-join user-home-directory "man")
                                "/usr/local/priority/man"
                                "/Library/TeX/Distributions/.DefaultTeX/Contents/Man"
                                "/usr/local/man"
                                "/usr/local/share/man"
                                "/usr/share/man")))

(setenv "INFOPATH" (s-join ":"
                           (list (concat emacs-app-base-dir "share/info/emacs") 
                                 "/usr/local/share/emacs/info"
                                 "/usr/local/share/info"
                                 "/usr/share/info"
                                 "/usr/local/info")))

(setenv "TEXINPUTS" (s-join ":"
                            (list ""
                                  "./tex"
                                  "../tex"
                                  "../../tex"
                                  "/Users/genovese/tex/ProbabilityExplained"
                                  (f-join user-home-directory "tex"))))


;; Emacs App locations
(setenv "EMACS_BIN" (executable-find "emacs"))
(setenv "EMACS_DIR" emacs-app-base-dir)
(setenv "EMACS_SHARE" "/usr/local/share/emacs/")
(when (eq my-platform 'macosx)
  (setenv "EMACS_APP"
          (concat emacs-app-base-dir "Emacs.app/Contents/MacOS/Emacs")))


;; General Emacs-useful environment settings
(setenv "PAGER" "cat")

;;; end my-env.el
