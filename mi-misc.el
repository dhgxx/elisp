;;; mi-misc.el --- various stuff in elisp

;;; Commentary:
;; miscellaneous things in elisp.

;;; Code:

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

(defvar mi-html-elisp-defs-list
  '("defvar" "defcustom")
  "Elisp defs list.")

(defvar mi-html-elisp-defuns-list
  '("defun")
  "Elisp defuns list.")

(defvar mi-html-elisp-keywords-list
  '("if" "when" "while" "progn")
  "Elisp keywords.")

(defun mi-html-make-code-elisp ()
  "Make Elisp code listing for HTML."
  (interactive)

  ;; match quoted strings
  (goto-char (point-min))
  (while (search-forward-regexp "\\(\".*\"\\)" (point-max) t)
    (replace-match "<span style=\"color:#e66c2c\">\\&</span>"))

  ;; match comments
  (goto-char (point-min))
  (while (search-forward-regexp "^[ ]*\\([;]+.*\\)" (point-max) t)
    (replace-match "<span style=\"color:#ab2300\">\\&</span>"))

  ;; match `def*' in elisp code
  (setq tmp-html-elisp-defs-list mi-html-elisp-defs-list
	current-keyword (car tmp-html-elisp-defs-list))
  (while current-keyword
    (goto-char (point-min))
    (while (search-forward-regexp (concat "\\(" current-keyword "\\)[ ]"
					  "\\(\\w+[-]*\\)+") (point-max) t)
      (replace-match "<span style=\"color:#3bb9ff\">\\1</span>&nbsp;<span style=\"color:#eac117\">\\2</span>"))
    (setq tmp-html-elisp-defs-list (cdr tmp-html-elisp-defs-list)
	  current-keyword (car tmp-html-elisp-defs-list)))

 ;; match `defun' in elisp code
  (setq tmp-html-elisp-defuns-list mi-html-elisp-defuns-list
	current-keyword (car tmp-html-elisp-defuns-list))
  (while current-keyword
    (goto-char (point-min))
    (while (search-forward-regexp (concat "\\(" current-keyword "\\)[ ]"
					  "\\(\\w+[-]*\\)+") (point-max) t)
      (replace-match "<span style=\"color:#3bb9ff\">\\1</span>&nbsp;<span style=\"color:#2b60de\">\\2</span>"))
    (setq tmp-html-elisp-defuns-list (cdr tmp-html-elisp-defuns-list)
	  current-keyword (car tmp-html-elisp-defuns-list)))

  ;;  match elisp key words
  (setq tmp-html-elisp-keywords-list mi-html-elisp-keywords-list
	current-keyword (car tmp-html-elisp-keywords-list))
  (while current-keyword
    (goto-char (point-min))
    (while (search-forward-regexp current-keyword  (point-max) t)
      (replace-match "<span style=\"color:#3bb9ff\">\\&</span>"))
    (setq tmp-html-elisp-keywords-list (cdr tmp-html-elisp-keywords-list)
	  current-keyword (car tmp-html-elisp-keywords-list)))

  ;; match leading tab spaces
  (progn
    (goto-char (point-min))
    (setq buffer-lines (line-number-at-pos (point-max))
	  current-line (line-number-at-pos (point)))
    (while (< current-line buffer-lines)
      (move-to-column 0)
      (while (looking-at "[\t]")
	(replace-match "&nbsp;&nbsp;&nbsp;&nbsp;")
	(forward-char))
      (forward-line 1)
      (setq current-line (+ 1 current-line))))

  ;; match leading spaces
  (progn
    (goto-char (point-min))
    (setq buffer-lines (line-number-at-pos (point-max))
	  current-line (line-number-at-pos (point)))
    (while (< current-line buffer-lines)
      (move-to-column 0)
      (while (looking-at "[ ]")
	(replace-match "&nbsp;")
	(forward-char))
      (forward-line 1)
      (setq current-line (+ 1 current-line))))

  ;; match newline
  (goto-char (+ 1 (point-min)))
  (while (search-forward-regexp "\n" (point-max) t)
    (replace-match "<br />\n")))

(global-set-key "\C-ct" nil)
(global-set-key "\C-ctt" 'mi-update-time-stamp)
(global-set-key "\C-ctf" 'mi-update-time-stamp-for-file)

(global-set-key "\C-cf" 'mi-fill-whole-buffer)

(global-set-key "\C-cc" 'emacs-lisp-byte-compile)
(global-set-key "\C-cl" 'emacs-lisp-byte-compile-and-load)

(global-set-key "\C-ch" nil)
(global-set-key "\C-che" 'mi-html-make-code-elisp)

;;; mi-misc.el ends here
