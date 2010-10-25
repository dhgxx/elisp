;;; mi-mail.el --- mail stuff

;;; Commentary:
;; compose & send mail with gnus-user-agent
;; even when we are not within gnus

;;; Code:

(require 'smtpmail)

;; my own mail user agent message
(setq gnus-user-agent '(emacs gnus type codename))
(setq mail-user-agent 'gnus-user-agent)

;; common smtp configuration
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(eval-after-load "mi-user"
  '(setq user-mail-address mi-message-user-mail-address))

;; pop3 configuration
(setq mail-sources '((file :path "/var/mail/dhg")))

(defun mi-message-smtpmail-tls (server port user passwd key cert auth-file)
  "Send mail using tls method.
Argument SERVER server name or ip address.
Argument PORT server port number.
Argument USER user name.
Argument PASSWD password.
Argument KEY key.
Argument CERT certificate.
Argument AUTH-FILE authentication file."
  (setq smtpmail-smtp-server server
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service port
	starttls-gnutls-program "gnutls-cli"
	starttls-extra-arguments nil
	smtpmail-auth-credentials auth-file
	smtpmail-starttls-credentials (list (list server port user passwd key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'."
   server port user))

;; we have opensmtpd to deal different
;; smtp severs.
(defun mi-message-smtp-send ()
  "Send mail directly to smtp service provided by OpenSMTPD."
  (save-excursion
    (mi-message-smtpmail-tls
     "smtp.gmail.com"
     587
     "tamgya@gmail.com"
     nil
     nil
     nil
     "~/.authinfo")))

;; mail envelopes
;;
;; initialize my own vars
(defvar mi-message-header-to nil)
(defvar mi-message-header-subject nil)
(defvar mi-message-header-cc nil)
(defvar mi-message-header-bcc nil)
(defvar mi-message-header-envelope nil)

(defun mi-message-header-setup-hook ()
  "Set up mail headers before posting."
  (setq
   subject-content (gnus-fetch-field "Subject")
   cc-content (gnus-fetch-field "Cc")
   gcc-content (gnus-fetch-field "Gcc")
   newsgroup-content (gnus-fetch-field "Newsgroups")
   envelope-content (or (gnus-fetch-field "To")
			newsgroup-content))
  (if (null envelope-content)
      (progn
	(setq envelope-content
	      (read-from-minibuffer "Send a mail to: "))
	(if (not (null envelope-content))
	    (setq mi-message-header-to envelope-content)
	  (setq mi-message-header-to nil))))

  (if (null subject-content)
      (progn
	(setq subject-content
	      (read-from-minibuffer "Subject: "))
	(if (not (null subject-content))
	    (setq mi-message-header-subject
		  (concat "\"" subject-content "\""))
	  (setq mi-message-header-subject nil))))

  (if (not (or cc-content newsgroup-content))
      (progn
	(setq cc-content
	      (read-from-minibuffer "Cc: "))
	(if (= 0 (string-width cc-content))
	    (setq mi-message-header-cc nil)
	  (setq mi-message-header-cc cc-content))))

  (if (string-equal "nnml:mail.sent.mail" gcc-content)
      (setq mi-message-header-bcc
	    (concat "Bcc: " mi-message-user-mail-address "\n"))
    (setq mi-message-header-bcc nil))

  (setq
   mi-message-header-envelope
   (concat "X-Envelope-To: "
	   (replace-regexp-in-string
	    "\(.*\)" ""
	    (replace-regexp-in-string
	     ">.*" ""
	     (replace-regexp-in-string ".*<" "" envelope-content)))
	   "\n"))

  (insert mi-message-header-envelope)

  (if (not (null mi-message-header-bcc))
      (insert mi-message-header-bcc))

  (if (not (null mi-message-header-to))
      (progn
	(goto-char (point-min))
	(while (search-forward-regexp "^To: $" (point-max) t)
	  (replace-match (concat "To: " mi-message-header-to)))))

  (if (not (null mi-message-header-subject))
      (progn
	(goto-char (point-min))
	(while (search-forward-regexp "^Subject: $" (point-max) t)
	  (replace-match (concat "Subject: " mi-message-header-subject)))))

  (if (not (null mi-message-header-cc))
      (progn
	(if (or (null (gnus-fetch-field "Cc"))
		(eq "" (gnus-fetch-field "Cc")))
		(progn
		  (insert (concat "\nCc: " mi-message-header-cc))
		  (goto-char (point-min))
		  (delete-blank-lines)))))

  (setq
   mi-message-header-envelope nil
   mi-message-header-to nil
   mi-message-header-subject nil
   mi-message-header-cc nil
   mi-message-header-bcc nil))

;; hooks
(add-hook 'message-header-setup-hook
	  'mi-message-header-setup-hook)
(add-hook 'message-send-hook 'mi-message-smtp-send)

(provide 'mi-mail)

;;; mi-mail.el ends here
