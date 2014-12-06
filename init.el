; -*-Emacs-Lisp-*-

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
