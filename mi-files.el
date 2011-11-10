;; mi-files.el

(setq require-final-newline t)

(defun mi-file-save-with-eol ()
  (unless require-final-newline
    (progn
      (let ((point-current-position (point)))
	(goto-char (- (point-max) 1))
	(if (looking-at "\n$")
	    ()
	  (progn
	    (goto-char (point-max))
	    (insert "\n")))
	(goto-char point-current-position)))))

;;(add-hook 'before-save-hook 'mi-update-time-stamp-for-file)
(add-hook 'before-save-hook 'mi-file-save-with-eol)
