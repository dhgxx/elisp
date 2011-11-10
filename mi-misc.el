;;; mi-misc.el --- various stuff in elisp

;;; Commentary:
;; miscellaneous things in elisp.

;;; Code:

(defun mi-insert-time-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %H:%M")))

(defvar mi-time-stamp-filename
  nil
  "File name to make time stamps.")

(defun mi-update-time-stamp ()
  "Update time stamps in unfiled buffers."
  (interactive)
  (set-buffer (current-buffer))
  (goto-char (point-min))
  (when (re-search-forward "\\(\\([$]time-stamp[$]\\)\\|\\([$]LastUpdated:.*[$]\\)\\)" (point-max) t 1)
    (replace-match (concat "$LastUpdated: "
			   (format-time-string "%Y/%m/%d %T ")
			   (user-login-name)
			   " $"))))

(defun mi-update-time-stamp-for-file ()
  "Update time stams in filed buffers."
  (interactive)
  (set-buffer (current-buffer))
  (setq mi-time-stamp-filename
	(file-name-nondirectory (buffer-file-name)))
  (goto-char (point-min))
  (when (re-search-forward "\\(\\([$]time-stamp[$]\\)\\|\\([$]xbsd:.*[$]\\)\\|\\([$]LastUpdated:.*[$]\\)\\)" (point-max) t 1)
    (replace-match (concat "$xbsd: "
			   mi-time-stamp-filename
			   ", "
			   (format-time-string "%Y/%m/%d %T ")
			   (user-login-name)
			   " $"))))

(defun mi-fill-whole-buffer ()
  "Fill the current buffer."
  (interactive)
  (set-buffer (current-buffer))
  (fill-region (point-min)
	       (point-max)))

(global-set-key "\C-ct" nil)
(global-set-key "\C-ctd" 'mi-insert-time-date)
(global-set-key "\C-ctt" 'mi-update-time-stamp)
(global-set-key "\C-ctf" 'mi-update-time-stamp-for-file)

(global-set-key "\C-cf" 'mi-fill-whole-buffer)

(global-set-key "\C-cc" nil)
(global-set-key "\C-ccc" 'emacs-lisp-byte-compile)
(global-set-key "\C-ccl" 'emacs-lisp-byte-compile-and-load)

(global-set-key "\C-ck" 'copy-region-as-kill)

;;; mi-misc.el ends here
