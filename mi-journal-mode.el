;;; mi-journal-mode.el --- by Denise H. G. (darcsis@gmail.com)

(require 'generic-x)

;;; Commentary:
;; A major mode for writing journals.

;;; Code:

(define-generic-mode 'journal-mode
  ()
  '("Date")
  '(("\\(#.*\\)" 1 'font-lock-comment-face))
  '("\\.[Jj][Nn][Ll]\\'")
  (list (lambda () (setq comment-start "#")))
  "Major mode for writing journals.")

(defvar mi-journal-version "0.1" "Journal mode version.")
(defvar mi-journal-default-emacs-directory "~/emacs" "Default Emacs data directory.")
(defvar mi-journal-default-directory nil "Default journal directory.")
(defvar mi-journal-date-year nil "Current year in digit.")
(defvar mi-journal-date-month nil "Current month in digit.")
(defvar mi-journal-date-current-date nil "Current date.")

(defvar mi-journal-default-file-suffix ".jnl" "Default journal file suffix.")
(defvar mi-journal-current-journal nil "Current journal file.")
(defvar mi-journal-current-journal-fullpath nil "Current journal file in full path.")
(defvar mi-journal-current-buffer nil "Buffer for journal.")

(defvar journal-start nil "Marker for the start of a journal.")
(defvar journal-end nil "Marker for the end of a journal.")

;; functions
(defun mi-journal-kick-off ()
  "Journal mode kick off."
  (interactive)
  (setq journal-start (make-marker)
	journal-end (make-marker)
	mi-journal-default-directory (concat mi-journal-default-emacs-directory "/journal")
	mi-journal-date-year (format-time-string "%Y")
	mi-journal-date-month (format-time-string "%m")
	mi-journal-date-current-date (format-time-string "%A, %Y-%m-%d")
	mi-journal-current-journal (concat mi-journal-date-year "/" mi-journal-date-month mi-journal-default-file-suffix)
	mi-journal-current-journal-fullpath (concat mi-journal-default-directory "/" mi-journal-current-journal))
  ;; make default journal directory
  (make-directory mi-journal-default-directory mi-journal-default-emacs-directory)
  ;; make year directory
  (make-directory (concat mi-journal-default-directory "/" mi-journal-date-year) mi-journal-default-directory)

  (progn
    (setq mi-journal-current-buffer (create-file-buffer (concat mi-journal-date-month mi-journal-default-file-suffix)))
    (set-buffer mi-journal-current-buffer)
    (if (file-exists-p mi-journal-current-journal-fullpath)
	(insert-file-contents mi-journal-current-journal-fullpath))
    (write-file mi-journal-current-journal-fullpath)
    (switch-to-buffer mi-journal-current-buffer)))

(defun mi-journal-quit-journal ()
  "Quit journal."
  (interactive)
  (mi-journal-save-journal)
  (bury-buffer mi-journal-current-buffer)
  (kill-buffer mi-journal-current-buffer))

(defun mi-journal-before-save ()
  "Before saving a journal, we can do something like checking if tags are standing alone."
  (interactive)
  (set-buffer (current-buffer))
  ;;(setq i (line-number-at-pos (point-min))
  (setq i 0
	j (line-number-at-pos (point-max)))
  (progn
    (goto-char (point-min))
    (forward-line (1- i)))
  (while (< i j)
    (beginning-of-line)
    (setq line-begin (point))
    (end-of-line)
    (setq line-end (point))
    (setq mi-string (buffer-substring line-begin line-end))
    (setq mi-string-new (replace-regexp-in-string
			 "\\(.+\\)\\(Date:.*[-]*[0-9][0-9][-]*\\)\\(.+\\)\\'"
			 " \\1\n\n\\2\n\n\\3 " mi-string))
    (if (not (equal mi-string mi-string-new))
	(progn
	  (delete-char (- 0 (string-width mi-string)))
	  (insert mi-string-new)
	  (setq mi-string nil
		mi-string-new nil)))
    (forward-line 1)
    (setq i (+ 1 i))))

(defun mi-journal-save-journal ()
  "Save current journal."
  (interactive)
  (if (and
       mi-journal-current-buffer
       mi-journal-current-journal)
      (if (buffer-modified-p mi-journal-current-buffer)
	  (progn
	    (set-buffer mi-journal-current-buffer)
	    (mi-journal-before-save)
	    (write-file mi-journal-current-journal-fullpath))
	(message "No changes need to be saved!"))
    (message "No journal to save!")))

(defun mi-journal-ready-to-compose ()
  "Set up something before composing a journal."
  ;;(interactive)
  (if mi-journal-current-buffer
      (progn
	(set-buffer mi-journal-current-buffer)
	(goto-char (point-min))
	(if (null (re-search-forward (concat "Date: " mi-journal-date-current-date "\n") nil t))
	    (progn
	      (insert (concat "Date: " mi-journal-date-current-date "\n\n\n\n"))
	      (goto-char (point-min))
	      (forward-line 2)))))
  (set-marker journal-start (point)))

(defun mi-journal-jump-to-previous ()
  "Jump to previous journal entry."
  (interactive)
  (setq current-point (point))
  (forward-line -2)
  (unhighlight-regexp (match-string 0))
  (if (null (re-search-backward "Date:.*" nil t 1))
      (progn
	(goto-char current-point)
	(message "No more entries."))
    (progn
      (forward-line 1)
      (highlight-regexp (match-string 0)))))

(defun mi-journal-jump-to-next ()
  "Jump to next journal entry."
  (interactive)
  (setq current-point (point))
  (unhighlight-regexp (match-string 0))
  (if (null (re-search-forward "Date:.*" nil t 1))
      (progn
	(goto-char current-point)
	(message "No more entries"))
    (progn
      (forward-line 1)
      (highlight-regexp (match-string 0)))))

(defun mi-journal-jump-to-specific ()
  "Jump to a specific day."
  (interactive)
  (setq day
	(read-from-minibuffer "Which day to go to? "))
  (if (>= 1 (string-width day))
      (setq day (concat "0" day)))
  (setq current-point (point))
  (setq target-day (concat mi-journal-date-month "-" day))
  (unhighlight-regexp (match-string 0))
  (goto-char (point-min))
  (if (null (re-search-forward (concat "Date:[ ].*" target-day ".*") nil t))
      (message "No journal entry on %s" target-day)
    (progn
      (forward-line 1)
      (highlight-regexp (match-string 0)))))

(defun mi-journal-toggle-read-only (&optional val)
  "Toggle buffer read only or not.
Optional argument VAL whether the current buffer is read only or not."
  (interactive)
  (if val
      (progn
	(setq buffer-read-only t)
	(message "Journal read only."))
    (progn
      (if buffer-read-only
	  (progn
	    (setq buffer-read-only nil)
	    (message "Insert mode."))
	(progn
	  (setq buffer-read-only t)
	  (message "Journal read only.")))))
  (mi-journal-toggle-local-key))

(defun mi-journal-toggle-local-key ()
  "Toggle local key bindings."
  (if buffer-read-only
      (progn
	(local-set-key "s" 'mi-journal-save-journal)
	(local-set-key "n" 'mi-journal-jump-to-next)
	(local-set-key "p" 'mi-journal-jump-to-previous)
	(local-set-key "j" 'mi-journal-jump-to-specific)
	(local-set-key "q" 'mi-journal-quit-journal)
	(local-set-key "i" 'mi-journal-toggle-read-only))
    (progn
      (local-unset-key "s")
      (local-unset-key "n")
      (local-unset-key "p")
      (local-unset-key "j")
      (local-unset-key "q")
      (local-unset-key "i"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mi-journal-startup ()
  "Startup journal mode."
  (interactive)
  (mi-journal-kick-off)
  (mi-journal-ready-to-compose)
  (local-set-key "Js" 'mi-journal-save-journal)
  (local-set-key "Jn" 'mi-journal-jump-to-next)
  (local-set-key "Jp" 'mi-journal-jump-to-previous)
  (local-set-key "Jj" 'mi-journal-jump-to-specific)
  (local-set-key "Jq" 'mi-journal-quit-journal)
  (local-set-key "Ji" 'mi-journal-toggle-read-only)
  (mi-journal-toggle-read-only t)
  (turn-on-auto-fill))

(global-set-key "\C-cj" 'mi-journal-startup)

(provide 'journal-mode)

;;; mi-journal-mode.el ends here
