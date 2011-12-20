;;; mi-send-pr.el --- My own send-pr implementation in elisp.
;; a mojor mode to highlight problem reports


;;; Commentary:
;; succeffully sent a PR on 2010/10/10 for ports/151361.

;;; Code:

(define-generic-mode 'send-pr-mode
  ()
  '("To" "From" "Subject" "Reply-To" "Cc" "X-send-pr-version" "X-GNATS-Notify"
    ">Submitter-Id" ">Originator" ">Organization" ">Confidential" ">Synopsis"
    ">Severity" ">Priority" ">Category" ">Class" ">Release" ">Environment"
    ">Description" ">How-To-Repeat" ">Fix")
  '(("\\(\\[.*\\]\\)" 1 'font-lock-comment-face))
  '("\\.[Pp][Rr]\\'")
  (list (lambda () (setq comment-start "[")))
  "Major mode for very simple problem reports highlighting.")

(defvar mi-pr-version "MiSendPr 0.3")
(defvar mi-pr-working-file nil "Default working file for sending pr.")
(defvar mi-pr-working-buffer nil "Default working buffer for sending pr.")
(defvar mi-pr-fill-list '() "A list for lines where empty fields shoule be filled.")
(defvar mi-pr-reserve-word "TO BE FILLED" "Reserve word for problem reports.")
(defvar mi-pr-attachment-filename nil "Attachment file name.")
(defvar mi-pr-attachment-prettyname nil "Better file name handling.")
(defvar mi-pr-filename-suffix ".pr" "Default filename suffix for a problem report.")
(defvar mi-pr-key-input nil "Capture key input sequence while filling out a problem report.")

(defcustom mi-pr-tmp-directory "/var/tmp"
  "Default send-pr tmp directory. e.g. '/var/tmp'"
  :type 'string
  :group 'mi-send-pr)

(defcustom mi-pr-template-path "~/emacs/template/pr.template"
  "Absolute file path to PR Template. e.g. '~/emacs/template/pr.template'"
  :type 'string
  :group 'mi-send-pr)

(defcustom mi-pr-subject-string "Problem report"
  "Fail safe subject for problem report."
  :type 'string
  :group 'mi-send-pr)

(defcustom mi-pr-default-to-address "freebsd-gnats-submit@freebsd.org"
  "Default gnats server address."
  :type 'string
  :group 'mi-send-pr)

(defcustom mi-pr-organization-name "XBSD Networks"
  "Default organization name for mi-send-pr."
  :type 'string
  :group 'mi-send-pr)

(defun mi-pr-fill-out-pr ()
  "Fill out a problem report."
  (interactive)
  (unless (null mi-pr-fill-list)
    (progn
      (goto-char (point-min))
      (forward-line (- (pop mi-pr-fill-list) 1))
      (end-of-line)
      (setq mi-pr-key-input
	    (read-key-sequence nil))
      (while (not (string-equal " " mi-pr-key-input))
	(setq mi-pr-key-input (read-key-sequence nil)))
      (delete-char (- 0 (+ 2 (string-width mi-pr-reserve-word)))))))

(defun mi-pr-post-fill ()
  "Post fill any unfilled items."
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "^>Synopsis:[ ]+.*$" (point-max) t))
  (unless (null (match-string 0))
    (setq mi-pr-subject-string
	  (replace-regexp-in-string "^>Synopsis:[ ]+[\t]+" "" (match-string 0))))
  (goto-char (point-min))
  (insert (concat "Subject: " mi-pr-subject-string "\n")))

(defun mi-pr-send-pr ()
  "Send a problem report via smtp."
  (interactive)
  (save-buffer)
  (if (yes-or-no-p "Really send the problem report? ")
      (progn
	(message-send-mail-with-sendmail)
	(bury-buffer mi-pr-working-buffer)
	(kill-buffer mi-pr-working-buffer))
    (message "Message sending cancelled!")))

(defun mi-pr-build-fill-list ()
  "Build a fill list."
  (interactive)
  (progn
    (goto-char (point-min))
    (while (re-search-forward (concat "\\[" mi-pr-reserve-word "\\]") nil t)
      (add-to-list 'mi-pr-fill-list (line-number-at-pos nil)))
    (setq mi-pr-fill-list (nreverse mi-pr-fill-list))))

(defun mi-pr-attach-file ()
  "Attach a file to a problem report."
  (interactive)
  (goto-char (point-max))
  (setq mi-pr-attachment-filename (read-file-name "File to attach: "))
  (if (< 0 (string-width mi-pr-attachment-filename))
      (progn
	(setq
	 mi-pr-attachment-prettyname (replace-regexp-in-string ".*\\/" "" mi-pr-attachment-filename)
	 mi-pr-attachment-prettyname (replace-regexp-in-string "^\\." "dot." mi-pr-attachment-prettyname))
	;; (newline)
	(insert (concat "--- " mi-pr-attachment-prettyname " begins here ---\n"))
	(insert-file-contents mi-pr-attachment-filename)
	(goto-char (point-max))
	(insert (concat "--- " mi-pr-attachment-prettyname " ends here ---")))
    (message "OK, no attachments.")))

(defun mi-pr-prepare-pr ()
  "Prepare a problem report with needed info."
  (interactive)
  (set-buffer mi-pr-working-buffer)
  (goto-char (point-min))
  (while (re-search-forward "[<]+.*[>]+" nil t)
    (replace-match (concat "\t[" mi-pr-reserve-word "]") t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>To:.*$" nil t)
    (replace-match (concat "To: " mi-pr-default-to-address) t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>From:.*$" nil t)
    (replace-match (concat "From: " user-full-name " <" user-mail-address ">") t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>Reply-To:.*$" nil t)
    (replace-match (concat "Reply-To: " user-full-name " <" user-mail-address ">") t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>X-send-pr-version:.*$" nil t)
    (replace-match (concat "X-send-pr-version: " mi-pr-version) t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>>Submitter-Id:.*$" nil t)
    (replace-match ">Submitter-Id: \tcurrent-users" t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>>Originator:.*$" nil t)
    (replace-match (concat ">Originator: \t" user-full-name) t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>>Organization:.*$" nil t)
    (replace-match (concat ">Organization: \t" mi-pr-organization-name) t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>>Confidential:.*$" nil t)
    (replace-match ">Confidential: \tno" t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>>Release:.*$" nil t)
    (replace-match (concat ">Release: \t" (shell-command-to-string "echo -n `uname -smr`")) t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>System:.*$" nil t)
    (replace-match (concat "System: " (shell-command-to-string "echo -n `uname -a`")) t))
  (goto-char (point-min))
  (while (re-search-forward "^MI-SEND-PR>" nil t)
    (replace-match "" t)))

(defun mi-pr-make-pr ()
  "Make a problem report."
  (interactive)
  (make-directory (concat mi-pr-tmp-directory "/mi-send-pr") "/var/tmp")
  (setq mi-pr-working-file
	(make-temp-file
	 (concat mi-pr-tmp-directory "/mi-send-pr/" user-login-name "-")
	 nil
	 mi-pr-filename-suffix))
  (setq mi-pr-working-buffer (create-file-buffer mi-pr-working-file))
  (set-buffer mi-pr-working-buffer)
  (insert-file-contents-literally mi-pr-template-path)
  (switch-to-buffer mi-pr-working-buffer)
  (write-file mi-pr-working-file)
  (local-set-key [tab] 'mi-pr-fill-out-pr)
  (local-set-key [C-tab] 'mi-pr-post-fill)
  (local-set-key [M-return] 'mi-pr-attach-file)
  (local-set-key [C-return] 'mi-pr-send-pr))

(defun mi-pr-kick-off ()
  "Start a new problem report."
  (interactive)
  (mi-pr-make-pr)
  (mi-pr-prepare-pr)
  (mi-pr-build-fill-list)
  (save-buffer))

(global-set-key "\C-cp" 'mi-pr-kick-off)

(provide 'mi-send-pr)

;;; mi-send-pr.el ends here
