;;; my-buffer.el

(defun my-change-buffer (forward files-only)
  "Documentation required"
  (let ((names (cdr (mapcar (function buffer-name) (buffer-list))))
	;; names contains all buffernames except for the current one
	(skip-list (if forward nil (list (buffer-name (current-buffer)))))
	;; skip-list will hold all buffers to be cycled through to keep
	;; he buffer list in the correct sequence
	(result nil))

    ;; change to next buffer: start with the least recently used one
    (if forward (setq names (reverse names)))
    (while names
      (let* ((case-fold-search nil)
             (this-name  (car names))
	     (first-char (string-to-char this-name))
	     (mode (cdr (assoc 'major-mode (buffer-local-variables (get-buffer this-name))))))
	(cond ((or (= ?\  first-char) 
		   (and files-only (or (= ?\* first-char) (eq mode 'dired-mode)
				       (string-match "\\`TAGS" this-name))))
	       ;; ignore all buffernames starting with a space
	       ;; also ignore all names starting with an asterisk (or TAGS) and dired buffers
	       ;; if files-only is t
	       (nconc skip-list (list this-name))
	       (setq names (cdr names)))
	      (t
	       ;; otherwise we found the new buffer
	       (setq result this-name) 
	       (setq names nil)))))
    (cond ( result
	    ;; select or bury all buffers in the skip-list
	    (mapcar (if forward (function switch-to-buffer) (function bury-buffer)) skip-list)
	    (switch-to-buffer result)))))

(defun my-next-file-buffer ()
  "Switch to next file buffer in current window"
  (interactive)
  (my-change-buffer t t))

(defun my-previous-file-buffer ()
  "Switch to previous file buffer in current window"
  (interactive)
  (my-change-buffer nil t))

(defun my-next-buffer ()
  "Switch to next buffer in current window"
  (interactive)
  (my-change-buffer t nil))

(defun my-previous-buffer ()
  "Switch to previous buffer in current window"
  (interactive)
  (my-change-buffer nil nil))

;;; my-buffer.el ends here
