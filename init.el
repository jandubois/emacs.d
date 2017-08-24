; -*-Emacs-Lisp-*-


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if (<= emacs-major-version 23)
    (progn
      (setq custom-file "~/Dropbox/emacs.d/emacs23-custom.el")
      (load custom-file)
      (setq load-path (append load-path (list "~/Documents/Windows/emacs/site-lisp")))
      (load "~/Dropbox/emacs.d/emacs23.el"))
  (setq custom-file "~/Dropbox/emacs.d/emacs24-custom.el")
  (load custom-file)
  (setq load-path (append load-path (list "~/Dropbox/emacs.d/site-lisp")))
  (load "~/Dropbox/emacs.d/emacs24.el"))
