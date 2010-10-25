;; mi-psvn.el

(require 'psvn)

(defcustom mi-svn-keyword-01 "Rev"
  "SVN Keyword."
  :type 'string
  :group 'mi-psvn)
(defcustom mi-svn-keyword-02 "Date"
  "SVN Keyword."
  :type 'string
  :group 'mi-psvn)

(defun mi-regex-search (stringA)
  (set-buffer (current-buffer))
  (goto-char (point-min))
  (re-search-forward stringA (point-max) t (count-lines (point-min) (point-max))))

(defun mi-regex-replace (stringA)
  (setq stringB (concat "$" stringA "$"))
  (setq stringA (concat "[$]" stringA ".*[$]$"))
  (mi-regex-search stringA)
  (setq string-matched (match-string 0))
  (if (null string-matched)
      (message "mi-regex-replace:%s: not found." stringA)
    (replace-match stringB)))

(defun mi-svn-before-commit ()
  (mi-regex-replace mi-svn-keyword-01)
  (mi-regex-replace mi-svn-keyword-02))

(defun mi-svn-save-buffer ()
  (interactive)
  (setq before-save-hook
	(cons 'mi-svn-before-commit before-save-hook))
  (set-buffer (current-buffer))
  (basic-save-buffer)
  (setq before-save-hook
	(cdr before-save-hook)))

(defun mi-svn-add-keywords ()
  (interactive)
  (set-buffer (current-buffer))
  (setq this-buffer-file-name (buffer-file-name (current-buffer)))
  (setq svn-keywords (concat " '"
			     mi-svn-keyword-01 " " mi-svn-keyword-02
			     "' "))
  (setq svn-command-string
	(concat
	 "env LANG=POSIX svn propset svn:keywords" svn-keywords this-buffer-file-name))
  (shell-command svn-command-string)
  (message "%s" svn-command-string))

(defun mi-svn-merge-file ()
  (interactive)
  (set-buffer (current-buffer))
  (setq this-buffer-file-name (buffer-file-name (current-buffer)))
  (mi-regex-search "[$]Path.*[$]$")
  (setq string-matched (match-string 0))
  (when string-matched
    (setq string-to-cook
	  (replace-regexp-in-string "Path\\|[$]\\|[:]\\|[ ]" "" string-matched))
    (setq file-to-diff-new this-buffer-file-name)
    (if (null (string-match "^/.*" string-matched))
	(setq real-path-name "~/")
      (setq real-path-name ""))
    (setq file-to-diff-old (concat real-path-name string-to-cook "/"
				   (file-name-nondirectory this-buffer-file-name)))
    (message "-OLD:%s\n+NEW:%s" file-to-diff-old file-to-diff-new)
    (diff file-to-diff-old file-to-diff-new)
    (select-window (next-window))
    (set-buffer (current-buffer))
    (diff-mode)))

(global-set-key "\C-cs" nil)
(global-set-key "\C-csb" 'mi-svn-save-buffer)
(global-set-key "\C-csk" 'mi-svn-add-keywords)
(global-set-key "\C-csn" 'mi-svn-merge-file)
(global-set-key "\C-css" 'svn-status)

