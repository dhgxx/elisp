;;; mi-misc.el --- various stuff in elisp

;;; Commentary:
;; miscellaneous things in elisp.

;;; Code:

(defvar mi-time-stamp-filename nil "File name to make time stamps.")
(defvar mi-time-stamp nil "Time stamp string.")

(defun mi-update-time-stamp ()
  "Update time stamps in unfiled buffers."
  (interactive)
  (set-buffer (current-buffer))
  (goto-char (point-min))
  (when (re-search-forward "[$]time-stamp[$]" (point-max) t 1)
	(setq mi-time-stamp (concat "$LastUpdated: "
								(format-time-string "%Y/%m/%d %T ") (user-login-name) " $"))
	(replace-match mi-time-stamp)))

(defun mi-update-time-stamp-for-file ()
  "Update time stams in filed buffers."
  (interactive)
  (set-buffer (current-buffer))
  (setq mi-time-stamp-filename (file-name-nondirectory (buffer-file-name)))
  (goto-char (point-min))
  (when (re-search-forward "[$]time-stamp[$]" (point-max) t 1)
	(setq mi-time-stamp
		  (concat "$xbsd: "
				  mi-time-stamp-filename ", "
				  (format-time-string "%Y/%m/%d %T ")
				  (user-login-name) " $"))
	(replace-match mi-time-stamp)))

(defun mi-fill-whole-buffer ()
  "Fill the current buffer."
  (interactive)
  (set-buffer (current-buffer))
  (fill-region (point-min) (point-max)))

(global-set-key "\C-cm" nil)
(global-set-key "\C-cmt" 'mi-update-time-stamp)
(global-set-key "\C-cmf" 'mi-update-time-stamp-for-file)

(global-set-key "\C-cf" 'mi-fill-whole-buffer)

(global-set-key "\C-cc" 'emacs-lisp-byte-compile)
(global-set-key "\C-cl" 'emacs-lisp-byte-compile-and-load)

;;; mi-misc.el ends here
