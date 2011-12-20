;; mi-shells.el

(defvar mi-ansi-idle-timer nil "Anti idle timer in a terminal environment")

;; ansiterm
(defun mi-ansi-term ()
  "An ANSI term that exuctes /bin/csh"
  (interactive)
  (save-excursion
    (ansi-term "/bin/csh")
    (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
      (when process
	(set-process-sentinel process
			      (lambda (proc change)
				(when (string-match "\\(finished\\|exited\\|terminated\\)" change)
				  (kill-buffer (process-buffer proc)))))))))

;; anti idle
(defun mi-ansi-anti-idle-start ()
  "Anti idle while execued"
  (interactive)
  (setq mi-ansi-idle-timer
	(run-with-timer 0 180 '(lambda () (term-send-up) (term-send-down))))
  (message "Anti idle started."))

(defun mi-ansi-anti-idle-stop ()
  "Stop anti idle"
  (interactive)
  (cancel-timer mi-ansi-idle-timer)
  (message "Anti idle stopped."))

(defun mi-eshell ()
  "Start eshell"
  (interactive)
  (require 'eshell)
  (save-excursion
    (eshell)))

(global-set-key "\C-cta" 'mi-ansi-term)
(global-set-key "\C-cti" 'mi-ansi-anti-idle-start)
(global-set-key "\C-cto" 'mi-ansi-anti-idle-stop)
(global-set-key "\C-cte" 'mi-eshell)
