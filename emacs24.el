;;; -*-Emacs-Lisp-*-

(setq inhibit-startup-message t)

(load "my-buffer")

;;;;; Key redefinitions, marking mode etc.

(defun my-save-and-kill-buffer ()
  "Save buffer if modified; then kill buffer"
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;; If cursor is at the beginning of a line then kill the
;; whole line including the newline character.
;(setq kill-whole-line t)
(defun my-kill-whole-line ()
  "Kill the whole line including newline character."
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)))

;(global-set-key [(control prior)]       'move-to-window-home-nomark)
(global-set-key [(control prior)]       'my-previous-file-buffer)
;(global-set-key [(shift control prior)] 'move-to-window-home-mark)
(global-set-key [(shift control prior)] 'my-previous-buffer)
;(global-set-key [(control next)]        'move-to-window-end-nomark)
(global-set-key [(control next)]        'my-next-file-buffer)
;(global-set-key [(shift control next)]  'move-to-window-end-mark)
(global-set-key [(shift control next)]  'my-next-buffer)
(global-set-key [(home)]                'move-beginning-of-line)
(global-set-key [(end)]                 'move-end-of-line)
(global-set-key [(meta home)]           'scroll-to-window-top)
(global-set-key [(meta end)]            'scroll-to-window-bottom)
(global-set-key [delete]    'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [(meta delete)]    'my-kill-whole-line)
(global-set-key [(meta kp-delete)] 'my-kill-whole-line)

; OS X: Cmd-{ and Cmd-} to switch tabs/buffers
(global-set-key (read-kbd-macro "s-{")     'my-previous-file-buffer)
(global-set-key (read-kbd-macro "s-}")     'my-next-file-buffer)


(global-set-key [f2]          'save-buffer)
(global-set-key [f3]          'kill-this-buffer)
(global-set-key [f4]          'my-save-and-kill-buffer)
(global-set-key [f5]          'shell)
(global-set-key [f6]          'other-window)
(global-set-key [(shift f6)]  'delete-other-windows)
(global-set-key [f7]          'dired)
(global-set-key [(shift f7)]  'buffer-menu)
(global-set-key [f11]         'my-previous-file-buffer)
(global-set-key [f12]         'my-next-file-buffer)
(global-set-key [(shift f11)] 'my-previous-buffer)
(global-set-key [(shift f12)] 'my-next-buffer)
(global-set-key [(meta f11)]  '(lambda () (interactive) (other-frame -1)))
(global-set-key [(meta f12)]  'other-frame)

;(global-set-key [tab]    'tab-to-tab-stop)
(global-set-key [(control tab)]  'indent-for-tab-command)

;; Experimental shift left/right (they deactive the mark currently)
(global-set-key [(control f7)]   '(lambda (start end) (interactive "r") (indent-rigidly start end -4)))
(global-set-key [(control f8)]   '(lambda (start end) (interactive "r") (indent-rigidly start end 4)))

(global-set-key (read-kbd-macro "M-n")     'next-error)
(global-set-key (read-kbd-macro "M-p")     'previous-error)
(global-set-key (read-kbd-macro "C-x C-g") 'goto-line)

;; Make RET and C-m act as positive replace ack keys
(define-key query-replace-map [return] 'act)
(define-key query-replace-map (read-kbd-macro "C-m") 'act)
;; Use C-return as an LFD alias
(define-key function-key-map [(control return)] (read-kbd-macro "LFD"))

; Windows
(define-key cua--cua-keys-keymap [(shift delete)]   'kill-region)
(define-key cua--cua-keys-keymap [(control insert)] 'copy-region-as-kill)
(define-key cua--cua-keys-keymap [(shift insert)]   'yank)
; OS X
(define-key cua--cua-keys-keymap [(shift kp-delete)]    'kill-region)
(define-key cua--cua-keys-keymap [(hyper control help)] 'copy-region-as-kill)
(define-key cua--cua-keys-keymap [(hyper shift help)]   'yank)

(define-key cua--cua-keys-keymap [(hyper meta control  help)]   'overwrite-mode)

(setq mark-even-if-inactive nil)

(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 1)

(server-start)

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

(setq paren-match-face 'bold)
(setq paren-sexp-mode t)
;; http://www.gnuvola.org/software/j/mic-paren/
(load "mic-paren")
(paren-activate)

;;; CPerl-mode
(defalias 'perl-mode 'cperl-mode)
;(setq cperl-hairy t)
;(setq cperl-indent-parens-as-block t)
;(setq cperl-close-paren-offset -4)

(setq auto-mode-alist (cons '("\\.t\\'" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi\\'" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("_PL\\'" . perl-mode) auto-mode-alist))

;;; dired-x

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

(setq p4-executable "/usr/local/bin/p4")

(load-library "p4")

;; (setq backup-enable-predicate
;;       (lambda (name)
;;         (and (normal-backup-enable-predicate name)
;;              (not (string-match "/Dropbox/" name)))))

;; (setq auto-save-file-name-transforms
;;       '((".*/Dropbox/.*" ,(concat temporary-file-directory "\\1") t)))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; go mode
(if (getenv "GOROOT") ; XXX doesn't work because GOROOT is always defined by the shared .bash_profile
    (progn
      (setq load-path (cons (concat (getenv "GOROOT") "/misc/emacs") load-path))
      (require 'go-mode-load)))

;; (setq ispell-program-name "hunspell")
;; (setq
;;  ispell-local-dictionary-alist
;;  '(("en_CA"
;;     "[[:alpha:]]"
;;     "[^[:alpha:]]"
;;     "[']" nil ("-r") nil utf-8)))
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(load "color-theme-solarized")
(color-theme-solarized-light)
