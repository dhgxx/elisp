;; mi-files.el

(defun mi-save-file-with-eol ()
  (setq point-current-position (point))
  (goto-char (- (point-max) 1))
  (if
	  (looking-at "\n$")
	  ()
	(progn
	  (goto-char (point-max))
	  (insert "\n")))
  (goto-char point-current-position))

(add-hook 'before-save-hook 'mi-save-file-with-eol)
