;;; elisp code highlighting for HTML.

(defvar mi-html-elisp-defs-list
  '("defvar" "defcustom")
  "Elisp defs list.")

(defvar mi-html-elisp-defuns-list
  '("defun" "defalias")
  "Elisp defuns list.")

(defvar mi-html-elisp-keywords-list
  '("eval-after-load" "if" "lambda" "let" "progn" "save-excursion" "when" "while")
  "Elisp keywords.")

;; a temporary list to hold search results.
(defvar mi-html-elisp-tmp-list nil)

;; custom variables
(defcustom mi-html-color-elisp-quoted
  "#e66c2c"
  "HTML color code for quoted string in Elisp."
  :type 'string
  :group 'mi-highlight)

(defcustom mi-html-color-elisp-comments
  "#ab2300"
  "HTML color code for comments in Elisp."
  :type 'string
  :group 'mi-highlight)

(defcustom mi-html-color-elisp-defs
  "#3bb9ff"
  "HTML color code for def* in Elisp."
  :type 'string
  :group 'mi-highlight)

(defcustom mi-html-color-elisp-vars
  "#eac117"
  "HTML color code for variables in Elisp."
  :type 'string
  :group 'mi-highlight)

(defcustom mi-html-color-elisp-funcname
  "#2b60de"
  "HTML color code for function names in Elisp."
  :type 'string
  :group 'mi-highlight)

;; functions
(defun mi-html-make-code-elisp ()
  "Make Elisp code listing for HTML."
  (interactive)

  ;; the matching order here matters!
  ;; match quoted strings
  (goto-char (point-min))
  (while (search-forward-regexp "\\(\\(\".+[ ]*\"\\)+\\)" (point-max) t)
    (replace-match (concat "<span style=\"color:"
			   mi-html-color-elisp-quoted
			   "\">\\&</span>")))

  ;; match comments
  (goto-char (point-min))
  (while (search-forward-regexp "^[ ]*\\([;]+.*\\)" (point-max) t)
    (replace-match (concat "<span style=\"color:"
			   mi-html-color-elisp-comments
			   "\">\\&</span>")))

  ;; match `def*' in elisp code
  (setq tmp-html-elisp-defs-list mi-html-elisp-defs-list
	current-keyword (car tmp-html-elisp-defs-list))
  (while current-keyword
    (goto-char (point-min))
    (while (search-forward-regexp (concat "\\(" current-keyword "\\)[ ]"
					  "\\(\\**\\(\\w?-?\\)+\\**\\)") (point-max) t)
      (replace-match "<span style=\"color:#3bb9ff\">\\1</span>&nbsp;<span style=\"color:#eac117\">\\2</span>"))
    (setq tmp-html-elisp-defs-list (cdr tmp-html-elisp-defs-list)
	  current-keyword (car tmp-html-elisp-defs-list)))

  ;; match `defun' in elisp code
  (setq tmp-html-elisp-defuns-list mi-html-elisp-defuns-list
	current-keyword (car tmp-html-elisp-defuns-list))
  (while current-keyword
    (goto-char (point-min))
    (while (search-forward-regexp (concat "\\(" current-keyword "\\)[ ]"
					  "\\(\\(\\w?[-]?\\)+\\)") (point-max) t)
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

(global-set-key "\C-ch" nil)
(global-set-key "\C-che" 'mi-html-make-code-elisp)
