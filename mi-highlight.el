;;; elisp code highlighting for HTML.

(defvar mi-html-elisp-defvars-list
  '("defvar" "defcustom")
  "Elisp defs list.")

(defvar mi-html-elisp-defuns-list
  '("defun" "defalias")
  "Elisp defuns list.")

(defvar mi-html-elisp-keywords-list
  '("eval-after-load" "if" "lambda" "let" "progn" "save-excursion" "when" "while" "with-current-buffer")
  "Elisp keywords.")

;; a temporary list to hold search results.
(defvar mi-html-elisp-tmp-list nil)

;; functions
(defun mi-html-make-code-elisp ()
  "Make Elisp code listing for HTML."
  (interactive)

  (save-excursion

    ;; make a new buffer for highlighting
    (let ((tmp-buffer (create-file-buffer (concat (buffer-name (current-buffer))
						 " *Highlighted*"))))
      (copy-to-buffer tmp-buffer (point-min) (point-max))
      (set-buffer tmp-buffer)
      (make-frame '((width . 80) (height . 40)))

      ;; the matching order here matters!

      ;; match double quoted strings
      (progn
	(goto-char (point-min))
	(while (search-forward-regexp "\\([^\\]\\)\\(\\(\"+.*\"\\)+\\)" (point-max) t)
	  (replace-match "\\1<span style=\"color:#e66c2c\">\\3</span>")))

      ;; match single quoted string
      (progn
	(goto-char (point-min))
	(while (search-forward-regexp "`\\(\\b\\(\\w?+-?\\)+\\b\\)'" (point-max) t)
	  (replace-match "`<span style=\"color:#4cc417\">\\1</span>'")))

      ;; match comments
      (progn
	(goto-char (point-min))
	(while (search-forward-regexp "\\([;]+[ ]*\\(\\w+[ ]*\\)+.*\\)" (point-max) t)
	  (replace-match "<span style=\"color:#ab2300\">\\&</span>")))

      ;; match type defs
      (progn
	(goto-char (point-min))
	(while (search-forward-regexp "\\(^[ ]*\\)\\(:\\w+\\)" (point-max) t)
	  (replace-match "\\1<span style=\"color:#9172ec\">\\2</span>")))

            ;; match `def*' in elisp code
      (progn
	(setq tmp-html-elisp-defvars-list mi-html-elisp-defvars-list
	      current-keyword (car tmp-html-elisp-defvars-list))
	(while current-keyword
	  (goto-char (point-min))
	  (while (search-forward-regexp (concat "\\(\\b" current-keyword "\\b\\)[ ]+"
						"\\(\\b\\(\\w?+-?\\)+\\b\\)") (point-max) t)
	    (replace-match "<span style=\"color:#3bb9ff\">\\1</span>&nbsp;<span style=\"color:#eac117\">\\2</span>"))
	  (setq tmp-html-elisp-defvars-list (cdr tmp-html-elisp-defvars-list)
		current-keyword (car tmp-html-elisp-defvars-list))))

      ;; match `defun' in elisp code
      (progn
	(setq tmp-html-elisp-defuns-list mi-html-elisp-defuns-list
	      current-keyword (car tmp-html-elisp-defuns-list))
	(while current-keyword
	  (goto-char (point-min))
	  (while (search-forward-regexp (concat "\\(\\b" current-keyword "\\b\\)[ ]+"
						"\\(\\b\\(\\w?+-?\\)+\\b\\)") (point-max) t)
	    (replace-match "<span style=\"color:#3bb9ff\">\\1</span>&nbsp;<span style=\"color:#2b60de\">\\2</span>"))
	  (setq tmp-html-elisp-defuns-list (cdr tmp-html-elisp-defuns-list)
		current-keyword (car tmp-html-elisp-defuns-list))))
  
      ;; match elisp key words
      (progn
	(setq tmp-html-elisp-keywords-list mi-html-elisp-keywords-list
	      current-keyword (car tmp-html-elisp-keywords-list))
	(while current-keyword
	  (goto-char (point-min))
	  (while (search-forward-regexp (concat "\(\\(\\b"
						current-keyword
						"\\b\\)")
					(point-max) t)
	    (replace-match "&#40;<span style=\"color:#3bb9ff\">\\1</span>"))
	  (setq tmp-html-elisp-keywords-list (cdr tmp-html-elisp-keywords-list)
		current-keyword (car tmp-html-elisp-keywords-list))))

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
      (progn
	(goto-char (+ 1 (point-min)))
	(while (search-forward-regexp "\n" (point-max) t)
	  (replace-match "<br />\n")))
      )
    )
  )

(global-set-key "\C-ch" nil)
(global-set-key "\C-che" 'mi-html-make-code-elisp)
