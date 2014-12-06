;;; -*-Emacs-Lisp-*-

;;; ~/.emacs

; Stephen Eglen's Emacs Lisp List
; http://www.anc.ed.ac.uk/~stephen/emacs/ellcover.html

; http://www.emacs.org/

; GNU Emacs on Windows NT and Windows 95/98
; http://www.gnu.org/software/emacs/windows/ntemacs.html

; Many modules for PIM, Notes, Todo, Scheduling etc:
; http://www.emacs.org/~johnw/emacs.html

;(setq debug-on-error t)
(setq inhibit-startup-message t)
(setq message-log-max 250)
(setq max-specpdl-size 1000)

;; Use the long filename to uniquely identify a buffer
(setq find-file-visit-truename t)

(require 'cl)

(setq Info-directory-list
      (append (parse-colon-path (getenv "INFOPATH"))
	      (list (expand-file-name "../info/" invocation-directory)
		    (expand-file-name "../site-info/" invocation-directory))))

;; allow local customization
(if (locate-library "~/.emacs.local")
    (load "~/.emacs.local"))

(mouse-wheel-mode 1)
(tool-bar-mode 0)

;; Activate menu bar using ALT key
(setq w32-pass-alt-to-system t)

;; Enable italic font support; *must* be done before font settings!
(setq w32-enable-synthesized-fonts t)

;; Font sizes: 13-97 is 10 pts, 15-112 is 11 pts
;(set-default-font "-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
(case (x-display-pixel-width)
  (11024
   (setq initial-frame-alist '((top . 26) (left . 108))
	 default-frame-alist '((top . 26) (left . 108) (width . 112) (height . 40)
			       (font . "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1"))))
  ((11280 2560)
   (setq initial-frame-alist '((top . 26) (left . 112))
	 default-frame-alist '((top . 26) (left . 112) (width . 134) (height . 59)
			       (font . "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1"))))
  (11600
   (setq initial-frame-alist '((top . 26) (left . 8))
	 default-frame-alist '((top . 26) (left . 8) (width . 120) (height . 62)
			       (font . "-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-1"))))
  (t))
(setq frame-title-format "Emacs - %f [%b]")

; (server-start)

(defvar my-modeline-background-color "CornFlowerBlue")
(defvar my-modeline-foreground-color "White")

(when window-system
  (set-face-background 'modeline my-modeline-background-color)
  (set-face-foreground 'modeline my-modeline-foreground-color))

;; Use 'bar cursor in insert and 'box cursor in overwrite mode
(defadvice force-mode-line-update
  (after my-cursor-type activate compile)
  "Forces box-cursor in overwrite and bar-cursor in insert mode."
  (modify-frame-parameters
   (selected-frame)
   (if overwrite-mode
       '((cursor-type . box))
     '((cursor-type . bar)))))

(set-cursor-color "black")

;; Blinking cursor
;; (autoload 'blinking-cursor-mode "blinking-cursor"
;;   "Toggle Blinking Cursor mode." t)

;; Display characters 128 through 254 as-is.  Without this
;; statement, octal notation is used for these characters.
(standard-display-8bit 128 254)

;; umlaute.el --- support for german umlaute
;; http://www.deja.com/getdoc.xp?AN=504410439&fmt=raw
(require 'umlaute)
(umlaute-keys umlaute-7bit t)
(add-hook 'text-mode-hook '(lambda () (umlaute-keys umlaute-8bit)))
(add-hook 'latex-mode-hook '(lambda () (umlaute-keys umlaute-tex)))
(add-hook 'html-helper-mode-hook '(lambda () (umlaute-keys umlaute-html)))

;; Don't let cursor movement extend the buffer
(setq next-line-add-newlines nil)
;; Let Emacs ask about missing newline on last line
(setq require-final-newline 'ask)

;; print setup
(setq lpr-command "print")
(setq lpr-destination '("/D:lpt1")) ;; for normal printer
(setq ps-lpr-destination '("/D:lpt1")) ;; for postscript printer

(setq ps-print-use-gs t) ; t - use ghostscript, nil - do not
;(setq ps-print-use-faces t) ; t - use color and fonts

; e.g.: GSLIB=m:\apps\gstools\gs5.10;m:\apps\gstools\gs5.10\fonts
(setq ghostscript-path (car (parse-colon-path (getenv "GS_LIB"))))
(setq ghostview-path (expand-file-name (concat ghostscript-path "../gsview/")))

(setq gs-print-command (concat ghostscript-path "gswin32.exe"))
;(setq gs-print-switches (list (concat "@" ghostscript-path "cdj550.upp")))
(setq gs-view-command (concat ghostview-path "gsview32.exe"))
(setq ps-paper-type 'letter)

(setq ps-psnup-command (concat (getenv "EMACSPATH") "/psnup.exe"))
(setq ps-psnup-switches '(" -l -2 -m.25in "))

(require  'print-nt)

;; Define buffer ring functions for F11/F12
(load "my-buffer")
(cua-mode t)

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

;; (load "pager")

;; (defun my-scroll-up (&optional arg)
;;   "Like scroll-up, but moves a fixed amount of lines (fixed relative the
;; window-height) so that pg-dn moves back to the same line."
;;   (interactive)
;;   (if (not (pos-visible-in-window-p (point-max)))
;;       (scroll-screen (- (1- (window-height)) next-screen-context-lines))))

;; (defun my-scroll-down (&optional arg)
;;   "Like scroll-down, but moves a fixed amount of lines (fixed relative the
;; window-height) so that pg-up moves back to the same line."
;;   (interactive)
;;   (if (not (pos-visible-in-window-p (point-min)))
;;       (scroll-screen (- next-screen-context-lines (1- (window-height))))))

;; (setq pc-select-scroll-up-mark     'my-scroll-up
;;       pc-select-scroll-up-nomark   'my-scroll-up
;;       pc-select-scroll-down-mark   'my-scroll-down
;;       pc-select-scroll-down-nomark 'my-scroll-down)
(setq scroll-preserve-screen-position t)
(setq scroll-step 1)

;; Display line and column numbers in the mode line
(column-number-mode 1)
(setq line-number-display-limit 100000000) ; 100M

;; highlight incremental search string
(setq search-highlight t)
(setq query-replace-highlight t)

;; If cursor is at the beginning of a line then kill the
;; whole line including the newline character.
;(setq kill-whole-line t)
(defun my-kill-whole-line ()
  "Kill the whole line including newline character."
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)))

;; Keep cursor at EOL during vertical cursor movement
(setq track-eol t)

(set-default 'truncate-lines t)

(defun my-toggle-truncate ()
  "Toggle Line Trunctation"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))				; use (redraw-display) instead ???

;;;;; Key redefinitions, marking mode etc.

(defun my-save-and-kill-buffer ()
  "Save buffer if modified; then kill buffer"
  (interactive)
  (save-buffer)
  (kill-this-buffer))

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

;;;; parentheses matching mode

(setq paren-match-face 'bold)
(setq paren-sexp-mode t)
;; http://www.gnuvola.org/software/j/mic-paren/
(load "mic-paren")
(paren-activate)

;;;; align equal signs and other stuff
(autoload 'align-regexp "align-regexp" "Aligns regular expressions within lines of text." t)

;; read abbreviations file from ~/.abbrev_defs if it exists
(condition-case nil
    (quietly-read-abbrev-file)
  (error nil))
; (abbrev-mode 1) ;;;; doesn't work, must use file-visit-hook

;;;;; backup stuff

;; Create backup files in a separate subdirectory
;(setq backup-directory-alist `(("." . ,(expand-file-name "~/.backups"))))
(setq backup-directory-alist `(("." . ".backup~")))

(require 'jka-compr)

;;;;; imenu stuff

(setq cc-imenu-c-prototype-macro-regexp "\\(_P\\|_PROTO\\)")

(defun my-add-imenu-to-menu-bar ()
  "Add IMENU menu item to menu bar."
  (interactive)
  (if (not (current-local-map))		    ; Imenu needs a local keymap!
      (use-local-map (make-sparse-keymap))) ; Create an empty map (eg. for ini-generic-mode)
  (imenu-add-to-menubar "Imenu"))

;; Change C++ destructor names, so that they will be sorted directly behind the constructors
(defun my-imenu-name (name)
  (if (and (stringp name) (char-equal ?~ (string-to-char name)))
      (concat (substring name 1) "~")
    name))

(defun my-imenu--sort-by-name (item1 item2)
  (string-lessp (my-imenu-name (car item1)) (my-imenu-name (car item2))))

(setq imenu-sort-function 'my-imenu--sort-by-name)
(setq imenu-max-items 35)

(require 'which-func)
(nconc which-func-modes (list 'ini-generic-mode))
(which-func-mode 1)

; Ilya's imenu-go package
(autoload 'imenu-go-find-at-position "imenu-go" "Go to the definition of the current word." t)
(autoload 'imenu-go--back "imenu-go" "Return back to a position saved during `imenu-go-find-at-position'." t)
(global-set-key [(meta shift mouse-1)] 'imenu-go-find-at-position)
(global-set-key [(meta shift mouse-3)] 'imenu-go--back)

;;;;; font-lock stuff

(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

;; Display ALL defined colors (on win32), not just those in x-colors
(defun my-list-all-colors-display ()
  (interactive)
  (list-colors-display (mapcar (function car) w32-color-map)))

(setq font-lock-face-attributes
      '((font-lock-comment-face       "Magenta")
	(font-lock-string-face        "ForestGreen")
	(font-lock-keyword-face       "CornflowerBlue")
	(font-lock-function-name-face "Blue")
	(font-lock-variable-name-face "Goldenrod")
	(font-lock-type-face          "Brown")
	;(font-lock-reference-face     "CadetBlue")
	))

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size (* 2048 1024))

;(require 'font-lock)
;(global-font-lock-mode t)

(defface my-url-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :underline t))
    (((class color) (background light)) (:foreground "Blue" :bold nil :underline t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t :underline t))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight comments."
  :group 'font-lock-highlighting-faces)

;; Additional font-lock stuff
(mapcar (function
	 (lambda (mode)
	   (font-lock-add-keywords
	    mode
	    ;; Highlight trailing whitespace
	    '(("[ \t]+$" 0 'underline t)
	      ;; Hightlight various URLs
	      ("\\(mailto\\|http\\|ftp\\):[^])}> \t\r\n\'\"]+" 0 'my-url-face t)
	      ("[-A-Za-z0-9_.]+@[-A-Za-z0-9_.]+" 0 'my-url-face t)
	      ;; Highlight "to be done" comments
	      ("\\(// *\\|/\\* *\\|[;#]+ *\\)?\\(XXX\\|TBD\\|\\?\\?\\?\\).*$"
	       0 'font-lock-warning-face t)))))
	'(c-mode c++-mode csharp-mode cperl-mode emacs-lisp-mode ini-generic-mode))

;; Ilyas font-mode hacks
;(eval-after-load "font-lock" '(require 'font-lock-extra))
;; hack follows, because font-lock-maximum-decoration doesn't know about -3 and -4 modes
;(fset 'c-font-lock-keywords 'c-font-lock-keywords-4)

(autoload 'yaml-mode "yaml-mode" "YAML mode." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; VB editing
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vbs\\)$" .
				 visual-basic-mode)) auto-mode-alist))
(setq visual-basic-ide-pathname "C:/Program Files/DevStudio/VB/VB5.EXE")

(autoload 'vbp-mode "vbp-mode" "VBP mode." t)
(setq auto-mode-alist (append '(("\\.vbp$" . vbp-mode)) auto-mode-alist))

;;; --- generic font lock support (for .INI, .BAT, .RC, .INF files etc)
;; (if (or xemacs-p (= emacs-major-version 19))
;;     (progn
;;       (require 'generic-mode)
;;       (require 'generic-extras)
;;       (alter-generic-mode-functions 'ini-generic-mode '(my-add-imenu-to-menu-bar)))
;;   (require 'generic)
;;   (require 'generic-x)
;;   ; ugly hack, but alter-generic-mode-* functions are gone from 20.3 version of generic-mode
;;   (let* ((mode-list (assoc 'ini-generic-mode generic-mode-alist))
;;  	 (current-list (nthcdr 5 mode-list))
;;  	 (current-elt  (nth    5 mode-list)))
;;     (setcar current-list (append current-elt '(my-add-imenu-to-menu-bar)))))

;; (setq auto-mode-alist (cons '("\\.[Ll][Aa][Yy]\\'"  . ini-generic-mode) auto-mode-alist))

;;; --- revert buffer without queries
(setq revert-without-query (list "\\.[dD][bB][gG]\\'"))

(setq-default indent-tabs-mode nil)

;;; redefine TAB to tab-to-tab-stop locally
;;; also make tab-stop-list buffer-local and compute equidistant tab stops

(defun my-tab-to-tab-mode (tab-width max-tab)
  (let ((local-map (current-local-map))
	(tab-stop tab-width))
    (define-key local-map [tab] 'tab-to-tab-stop)
    (make-local-variable 'tab-stop-list)
    (setq tab-stop-list nil)
    (while (<= tab-stop max-tab)
      (setq tab-stop-list (append tab-stop-list (list tab-stop))
	    tab-stop (+ tab-stop tab-width)))))

;;; --- CC mode
(defconst my-c-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist . ((statement-block-intro . +)
			(knr-argdecl-intro . +)
			(substatement-open . 0)
			(label . 0)
			(statement-cont . +)
			)))
  "My C Programming Style")

;;   '((c-tab-always-indent        . t)
;;     (c-comment-only-line-offset . 4)
;;     (c-hanging-braces-alist     . ((substatement-open after)
;;                                    (brace-list-open)))
;;     (c-hanging-colons-alist     . ((member-init-intro before)
;;                                    (inher-intro)
;;                                    (case-label after)
;;                                    (label after)
;;                                    (access-label after)))
;;     (c-cleanup-list             . (scope-operator
;;                                    empty-defun-braces
;;                                    defun-close-semi))
;;     (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
;;                                    (substatement-open . 0)
;;                                    (case-label        . 4)
;;                                    (block-open        . 0)
;;                                    (knr-argdecl-intro . -)))
;;     (c-echo-syntactic-information-p . t))
;;   "My C Programming Style")

(c-add-style "jdb" my-c-style)
;(setq c-style-variables-are-local-p t)
(defvar my-c-mode-tab-stops nil)
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-style	"jdb")  ; default style is "gnu"
  ;(setq c-tab-always-indent nil)
  (if my-c-mode-tab-stops
      (my-tab-to-tab-mode my-c-mode-tab-stops 120))
  (setq indent-tabs-mode nil) ; always use spaces for indentation
  (my-add-imenu-to-menu-bar)
  (make-local-variable 'dabbrev-case-fold-search)
  (make-local-variable 'dabbrev-case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil)
  (abbrev-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;(setq c-echo-syntactic-information-p t) ; for debugging only

;; XXX Why doesn't this work:
;;(put 'c-indentation-style 'safe-local-variable 'stringp)
(setq safe-local-variable-values '((c-indentation-style . bsd)))

(setq auto-mode-alist (cons '("\\.cs\\'" . csharp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp\\'" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xs\\'"  . c-mode) auto-mode-alist))

(autoload 'csharp-mode "cc-mode")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

;;; --- Makefile mode: additional regexp to allow ANY extension on makefiles
(setq auto-mode-alist (cons '("\\(m\\|M\\)akefile\\." . makefile-mode) auto-mode-alist))

;;; --- Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'my-add-imenu-to-menu-bar)

;; enable ElDoc minor mode
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; --- Ilyas CPerl mode
;(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-hairy t)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)

(setq auto-mode-alist (cons '("\\.[pP][LlMm]\\'" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.t\\'" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.psgi\\'" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.\\(pod\\|pls\\)\\'" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("_PL\\'" . perl-mode) auto-mode-alist))
(setq interpreter-mode-alist (append interpreter-mode-alist '(("miniperl" . perl-mode))))

(defun my-cperl-mode-hook ()
  "Setup CPerl mode the way I want it"
  (cperl-set-style "PerlStyle")
  (my-add-imenu-to-menu-bar))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

;;; --- mode-compile stuff
(setq mode-compile-exe-file-ext ".exe")
(autoload 'mode-compile      "mode-compile" "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile" "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (read-kbd-macro "C-c c") 'mode-compile)
(global-set-key (read-kbd-macro "C-c k") 'mode-compile-kill)

;;; --- perl debugger support
;(autoload 'perldb "perldb" "Major mode for running the Perl debugger." t)
;(add-hook 'gud-mode-hook '(lambda () (load "perldb" t t)))
; (eval-after-load
;  "gud"
;  '(defun gud-perldb-massage-args (file args)
;     (cons "-dw" (cons (car args) (cons "-emacs" (cdr args))))))

;;; --- Python stuff

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
					interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)

;;; --- MATLAB mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(add-hook 'matlab-mode-hook
          '(lambda ()
	     ;; (setq matlab-function-indent t)	; if you want function bodies indented
	     (setq fill-column 76)		; where auto-fill should wrap
	     (font-lock-mode 1)
	     (turn-on-auto-fill)))
(setq matlab-indent-level 4)

;;; --- PostScript mode
(autoload 'ps-mode "ps-mode" "PostScript mode" t)
(setq auto-mode-alist (cons '("\\.[eE]?[pP][sS]\\'" . ps-mode) auto-mode-alist))
(add-hook 'ps-mode-hook 'my-add-imenu-to-menu-bar)

;;; --- ChangeLog stuff
(setq change-log-default-name "ChangeLog")
(if (not add-log-full-name)
    (setq add-log-full-name "Jan Dubois"))
(if (not add-log-mailing-address)
    (setq add-log-mailing-address "jand@ActiveState.com"))

;;; outline-view-mode
;;; http://www.emacs.org/~johnw/Emacs/outline-view.el
(autoload 'outline-view-mode "outline-view"
  "Toggle Outline-View mode, a minor mode for viewing outlines." t)

;;; numbered-outline-mode
;;; http://www.deja.com/getdoc.xp?AN=581599788&fmt=raw
(autoload 'numbered-outline-mode "numbered-outline" nil t)

;;; --- *compilation* buffer stuff

(setq-default compile-command "nmake")

;; Better font-lock for compilation-mode from <Toby.Speight@digitivity.com>
;; (defun build-compilation-mode-font-lock-keywords ()
;;   "Return a set of regexp based on the current compilation rules."
;;   (mapcar
;;    (function
;;     (lambda (x)
;;       (nconc
;;        (list
;;         (car x)
;;         (list (nth 1 x) font-lock-function-name-face nil t)
;;         (list (nth 2 x) font-lock-variable-name-face nil t))
;;        (and (nth 3 x)
;;             (list (list (nth 3 x) font-lock-reference-face nil t))))))
;;    compilation-error-regexp-alist))

;; (eval-after-load
;;  "compile"
;;  '(setq compilation-error-regexp-alist
;; 	(append compilation-error-regexp-alist
;; 		'(("\\<in file \\(\\S-+\\) at line \\([0-9]+\\)" 1 2)
;; 		  ;; xiv.cs(25,7): error SC0103: The name 'XNVIV' does not exist in the class or namespace 'PerlScript.XIV'
;; 		  ("\\([^( \n\t]+\\)(\\([0-9]+\\),\\([0-9]+\\)): " 1 2 3)))
;; 	compilation-mode-font-lock-keywords
;; 	(build-compilation-mode-font-lock-keywords)))



(setq compilation-window-height 12)
;; Automatically scroll compilation buffer <jaalto@tre.tele.nokia.fi>
(eval-after-load
 "compile"
 '(defadvice compile-internal
    (after my-scroll act comp)
    "Forces compile buffer to scroll. See around line 363 in compile.el"
    (let* ((ob (current-buffer)))
      (save-excursion
	(select-window (get-buffer-window ad-return-value))
	(goto-char (point-max))
	(select-window (get-buffer-window ob))))))

;;; --- dired-x stuff
(add-hook 'dired-load-hook
	  '(lambda ()
	     (load "dired-x")
	     ;; Set dired-x global variables here.  For example:
	     ;; (setq dired-guess-shell-gnutar "gtar")
	     ;; (setq dired-x-hands-off-my-keys nil)
	     ))
(add-hook 'dired-mode-hook
	  '(lambda ()
	     ;; Set dired-x buffer-local variables here.  For example:
	     ;; (setq dired-omit-files-p t)
	     ))

(autoload 'dired-jump "dired-x" "Jump to dired buffer corresponding to current buffer." t)

(global-set-key (read-kbd-macro "C-x C-j") 'dired-jump)
(global-set-key (read-kbd-macro "C-x 4 C-j") 'dired-jump-other-window)

;;; --- Calendar and Diary settings
; (setq calendar-location-name "Hamburg"
;       calendar-latitude  [53 34 north]
;       calendar-longitude [10  0 east])

; Getty Thesaurus of Geographic Names:
; http://shiva.pub.getty.edu/tgn_browser/
; MapBlast
; http://www.mapblast.com/mblast/index.mb
(setq calendar-location-name "Vancouver"
      calendar-latitude  [ 49 17 north]
      calendar-longitude [123  7 west])

(setq number-of-diary-entries 14)
(add-hook 'diary-display-hook 'fancy-diary-display)

;;; --- CALC: the mother of all calculators

;;;; Commands added by calc-private-autoloads on Mon Jan 20 21:14:02 1997.
(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
(autoload 'defmath		   "calc" nil t t)
(autoload 'calc			   "calc" "Calculator Mode" t)
(autoload 'quick-calc		   "calc" "Quick Calculator" t)
(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
(global-set-key (read-kbd-macro "M-#") 'calc-dispatch)
;;; End of Calc autoloads.

;;; browse-url.el --- Pass a URL to a WWW browser
(global-set-key "\C-c\C-z." 'browse-url-at-point)
(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
(global-set-key "\C-c\C-zr" 'browse-url-of-region)
(global-set-key "\C-c\C-zu" 'browse-url)
(global-set-key "\C-c\C-zv" 'browse-url-of-file)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))

(load "simple-web")

(simple-web-define
 web-whois
 "Lookup domain: "
 "http://www.geektools.com/cgi-bin/proxy.cgi?query=%s&targetnic=auto"
 "Look up address or domain name")

(simple-web-define
 web-google
 "Search for: "
 "http://www.google.com/search?&q=%s"
 "Look up a term on google")
(global-set-key "\C-c\C-g" 'web-google)

;;; Emacs/W3 is a full-featured web browser, written entirely in Emacs-Lisp
;;; http://www.cs.indiana.edu/elisp/w3/docs.html
;(require 'w3-auto)
(autoload 'w3 "w3" "WWW Browser" t)
(autoload 'w3-fetch "w3" "WWW Browser" t)
;(setq w3-default-homepage "file:i:/Ntemacs.htm")

;;; slashdot.el -- View headlines from Slashdot.org.
;;; http://www.tardis.ed.ac.uk/~skx/win/slashdot.el
(autoload 'slashdot "slashdot" "Retrieve /. headlines" t)
;(setq slashdot-browser "w3")

;;; --- bibl-mode settings
(autoload 'bibl-visit-bibliography "bibl-mode" "Autoload bibliography mode." t)
(autoload 'bibl-mode "bibl-mode" "Autoload bibliography mode." t)
(setq bibl-file-name "~/bibliography")
;(global-set-key "\C-cb" 'bibl-global-map)
(global-set-key (read-kbd-macro "C-c b") 'bibl-global-map)

;;; --- edb
(autoload 'db-find-file "database" "EDB database package" t)

;;; --- Lisp Code Directory
; (when (not xemacs-p)
;   (setq lisp-code-directory (concat my-emacs-base-dir "site/LCD-datafile"))
;   (autoload 'format-lisp-code-directory "lispdir" nil t)
;   (autoload 'lisp-dir-apropos "lispdir" nil t))
;(autoload 'lisp-dir-retrieve "lispdir" nil t)
;(autoload 'lisp-dir-verify "lispdir" nil t)

;;;; reenable some disabled commands
(put 'eval-expression  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Misc packages
(autoload 'setnu-mode "setnu" "Toggle setnu-mode." t)

;; Author: Bob Glickstein <bobg@zanshin.com>
(autoload 'live-mode "live-mode" "View/edit a growing or changing file." t)
(autoload 'wcount-mode "wcount" "Count words in buffer." t)

;; woman
;; Francis J. Wright: http://www.maths.qmw.ac.uk/~fjw/
;; http://centaur.maths.qmw.ac.uk/Emacs/files/woman.el
(autoload 'woman "woman" "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman" "Find, decode and browse a specific UN*X man-page file." t)
(autoload 'woman-dired-find-file "woman" "In dired, run the WoMan man-page browser on this file." t)
(autoload 'woman-decode-buffer "woman" "Decode a buffer in UN*X man-page source format." t)
(add-hook 'dired-mode-hook
	  (function
 	   (lambda ()
 	     (define-key dired-mode-map "W" 'woman-dired-find-file))))

;; word-help
(autoload 'set-help-file "word-help"
  "Sets the set of Texinfo files used for `word-help' to HELPFILE."  t nil)
(autoload 'word-help "word-help"
  "Find documentation on the KEYWORD under the cursor (or passed)."  t nil)
(autoload 'word-help-complete "word-help"
  "Perform completion on the symbol preceding the point." t nil)
(autoload 'word-help-add-keywords "word-help"
  "Add user keywords to a certain Info file." nil nil)
(define-key help-map [(control i)] 'word-help)
(global-set-key [(control tab)] 'word-help-complete)

;; ;; html-helper-mode
;; ;; http://www.santafe.edu/~nelson/tools/
;; ;; Version 3.xxx and later are found here (not yet installed):
;; ;; http://www.gest.unipd.it/~saint/hth.html
;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (setq html-helper-use-expert-menu t)
;; (setq html-helper-never-indent t)

;; (add-hook 'html-helper-load-hook
;; 	  (function (lambda () (load "tables.el"))))

;; (setq auto-mode-alist
;;       (append '(("\\.s?html?\\'" . html-helper-mode)
;; 		("\\.aspx?\\'" . html-helper-mode)
;; ;		("\\.asmx\\'" . html-helper-mode)
;; 		) auto-mode-alist ))


;; major mode for editing cascading stylesheets
;; http://www.stud.ifi.uio.no/~larsga/download/css-mode.html
;; (autoload 'css-mode "css-mode")
;; (setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; htmlize.el -- HTML-ize font-lock buffers
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
(autoload 'htmlize-buffer "htmlize" "HTML-ize BUFFER." t)
(autoload 'htmlize-file "htmlize" "HTML-ize FILE, and save the result." t)
(autoload 'htmlize-buffer "htmlize" "HTML-ize BUFFER." t)

;; ~/.emacs.local can define a function to override some of the settings made so far
(if (fboundp 'my-local-dot-emacs)
    (my-local-dot-emacs))

;;; ~/.emacs ends here
(require 'custom)
;(custom-set-variables
; '(c-echo-syntactic-information-p t))
;(custom-set-faces)

(defun perl-renumber-tests ()
  "Renumber Perl tests"
  (interactive)
  (shell-command-on-region
   (point-min) (point-max) "perl -pe \"++$t if !$t || s/^# \\d+\\./# $t./\"" t))

;(require 'recentf)

(load "recent-files")
(setq recent-files-menu-title "Recent-Files")
(setq recent-files-add-menu-before 'buffer)
(setq recent-files-actions-on-top t)
(setq recent-files-permanent-first nil)
(setq recent-files-non-permanent-submenu nil)
(setq recent-files-commands-submenu t)
(setq recent-files-dont-include '("/\\'"))
(setq recent-files-number-of-entries 30)
(recent-files-initialize)

(autoload 'desktop-menu "desktop-menu" "Make menu of desktops" t)

;; dictionary.el -- an interface to RFC 2229 dictionary server
;; http://me.in-berlin.de/~myrkr/dictionary/index.html

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)

(global-set-key [(control c) ?s] 'dictionary-search)
(global-set-key [(control c) ?m] 'dictionary-match-words)
(global-set-key [(shift mouse-3)] 'dictionary-mouse-popup-matching-words)
(setq dictionary-server "dict.org")
(add-hook 'dictionary-mode-hook
	  '(lambda ()
	     (define-key dictionary-mode-map "\C-?" 'dictionary-previous)))

;;; babel.el --- interface to web translation services such as Babelfish
;;; http://www.chez.com/emarsden/downloads/babel.el
(autoload 'babel "babel" "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"   "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel" "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel" "Use a web translation service to translate the current buffer." t)

;;; watson.el --- query web search engines and aggregate results
;;; http://www.chez.com/emarsden/downloads/watson.el
(autoload 'watson "watson" "Lookup a word or phrase on various search engines." t)
(autoload 'watson-referers "watson" "List web pages which link to URL" t)


;;; p4.el --- Perforce-Emacs Integration
;;; http://www.dsmit.com/p4/
;;; Unofficial patches sometimes found here: http://w1.894.telia.com/~u89404340/

(defun my-query-registry (hkey key value)
  (let ((string
	 (shell-command-to-string
	  (format (concat "perl -MWin32::Registry -e\""
			  "$%s->Open('%s', my $hkey) or exit;"
			  "$hkey->QueryValueEx('%s', my $type, my $value) or exit;"
			  "print $value\"")
		  hkey key value))))
    (if (string= string "") nil string)))

(defun my-set-p4-env (name)
  (unless (getenv name)
    (setenv name
	    (my-query-registry "HKEY_CURRENT_USER"
			       "Software\\Perforce\\environment"
			       name))))

;(mapc 'my-set-p4-env '("P4PORT" "P4USER" "P4CLIENT"))

(setq p4-executable "/usr/local/bin/p4")
(setq p4-use-p4config-exclusively t)
(load-library "p4")

(setq diff-added-face 'p4-depot-added-face)
(setq diff-removed-face 'p4-depot-deleted-face)

;; Experimental IRC log file mode
;; ??? need some work on unbalanced string quotes ???
(define-generic-mode 'irc
  nil
  nil
  '(("^\\(<.*>\\)"        1 'font-lock-reference-face)
    ("^\\(\\*\\*\\*.*\\)" 1 'font-lock-variable-name-face)
    ("^\\(\\*.*\\)"       1 'font-lock-comment-face))
  (list "\\.[iI][rR][cC]$")
  nil
  "Generic mode for IRC log files")
(custom-set-faces)

(defun my-journal-write (packet)
1  (let ((socket (condition-case err
		    (open-network-stream "journal" nil "localhost" 4711)
		  (file-error nil))))
    (cond (socket
	   (process-send-string socket (format "emacs: %s" packet))
	   (delete-process socket)))))

(setq my-current-buffer (buffer-name))

(defun my-buffer-switch-hook ()
  (let ((buffername (buffer-name)))
    (if (not (equal buffername my-current-buffer))
	(progn
	  (setq my-current-buffer buffername)
	  (my-journal-write
	   (format "file='%s' buffer='%s'" (buffer-file-name) buffername))))))

; XXX Files loaded via gnuclient don't trigger post-command-hook
; XXX Don't enable this hook; it really slows down buffer switching
; XXX if the socket doesn't exist!
;(add-hook 'post-command-hook 'my-buffer-switch-hook)

(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))


;; (setq ack-command "ack --nogroup ")
;; (setq ack-history nil)
;; (defun ack ()
;;   "Like grep, but using ack-command as the default"
;;   (interactive)
;;   ; Make sure that compile.el is loaded and grep has been initialized
;;   (require 'compile)
;;   (unless grep-command
;;     (grep-compute-defaults))
;;   ; Close STDIN to avoid ack from going into filter mode
;;   (let ((null-device (format "< %s" null-device))
;; 	(grep-command ack-command)
;; 	(grep-history ack-history))
;;     (call-interactively 'grep)
;;     (setq ack-history grep-history)))

; (require 'ack)

(defun my-shutdown ()
  (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
	 (server-file (expand-file-name server-name server-dir)))
    (ignore-errors (delete-file server-file)))
  nil)

(add-hook 'kill-emacs-hook (function my-shutdown))

(eval-after-load
 "tramp"
 '(add-to-list 'tramp-methods '("stackato"
				(tramp-login-program        "stackato")
				(tramp-login-args           (("ssh") ("--app") ("%h")))
				(tramp-remote-sh            "/bin/sh")
				(tramp-copy-program         nil)
				(tramp-copy-args            nil)
				(tramp-copy-keep-date       nil)
				(tramp-password-end-of-line nil)
				(tramp-default-port         22))))

;; http://support.markedapp.com/kb/how-to-tips-and-tricks/marked-bonus-pack-scripts-commands-and-bundles
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

