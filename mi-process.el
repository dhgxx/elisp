;;; check dead buffer with exited process
;;; and kill them all.

(defvar mi-process-list
  nil
  "List of all running processes within Emacs.")

(defvar mi-process-buffer-list
  nil
  "Alist of buffer names of all running processes within Emacs.")

(defun mi-process-poll-status ()
  (setq mi-process-list (process-list)
	tmp-process-list mi-process-list
	current-process (car mi-process-list)
	current-process-buffer (process-buffer current-process))  
  (while current-process
    (if (buffer-live-p (process-buffer current-process))
	(add-to-list 'mi-process-buffer-list
		     current-process))
    (setq tmp-process-list (cdr tmp-process-list)
	  current-process (car tmp-process-list))))

(defun mi-process-kill-buffer ()
  (setq tmp-process-buffer-list mi-process-buffer-list
	current-process (car tmp-process-buffer-list))
  (while current-process
    (let ((status (process-status current-process)))
      (if (or (eq 'exit status)
	      (eq 'signal status)
	      (eq 'nil status))
	  (progn
	    (bury-buffer (process-buffer current-process))
	    (kill-buffer (process-buffer current-process))))
      (setq tmp-process-buffer-list (cdr tmp-process-buffer-list)
	    current-process (car tmp-process-buffer-list)))))

(when mi-startup-first-time
  (run-with-timer 1 1 '(lambda ()
			 (mi-process-poll-status)
			 (mi-process-kill-buffer))))

