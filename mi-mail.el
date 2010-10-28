;;; mi-mail.el --- mail stuff

;;; Commentary:
;; compose & send mail with gnus-user-agent
;; even when we are not within gnus

;;; Code:

(require 'smtpmail)

(defvar mi-mail-smtp-server nil
  "Default SMTP server name.")
(defvar mi-mail-smtp-port nil
  "Default SMTP server port.")
(defvar mi-mail-smtp-user-name nil
  "Default SMTP login user name.")

;; my own mail user agent message
(setq gnus-user-agent '(emacs gnus type codename))
(setq mail-user-agent 'gnus-user-agent)

;; custom smtp server
(eval-after-load "mi-user"
  '(if (string-match "^pluton.xbsd.name$"
		     (shell-command-to-string "echo -n `uname -n`"))
       (setq mi-mail-smtp-server "smtp.xbsd.name"
	     mi-mail-smtp-port 25
	     mi-mail-smtp-user-name user-login-name)
     (setq mi-mail-smtp-server "smtp.gmail.com"
	   mi-mail-smtp-port 587
	   mi-mail-smtp-user-name mi-message-user-mail-address)))

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
	smtpmail-default-smtp-server mi-mail-smtp-server
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
     mi-mail-smtp-server
     mi-mail-smtp-port
     mi-mail-smtp-user-name
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
  (unless envelope-content
    (progn
      (setq envelope-content
	    (read-from-minibuffer "Send a mail to: "))
      (if (not (null envelope-content))
	  (setq mi-message-header-to envelope-content)
	(setq mi-message-header-to nil))))

  (unless subject-content
    (progn
      (setq subject-content
	    (read-from-minibuffer "Subject: "))
      (if (not (null subject-content))
	  (if (= (string-width subject-content)
		 (length subject-content))
	      (setq mi-message-header-subject
		    subject-content)
	    (setq mi-message-header-subject
		  (concat "\"" subject-content "\"")))
	(setq mi-message-header-subject nil))))

  (unless (or cc-content newsgroup-content)
    (progn
      (setq cc-content
	    (read-from-minibuffer "Cc: "))
      (if (or (= 0 (string-width cc-content))
	      (string-match "^[ ]+$" cc-content))
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

  (unless (null mi-message-header-bcc)
    (insert mi-message-header-bcc))

  (unless (null mi-message-header-to)
    (progn
      (goto-char (point-min))
      (while (search-forward-regexp "^To: $" (point-max) t)
	(replace-match (concat "To: " mi-message-header-to)))))

  (unless (null mi-message-header-subject)
    (progn
      (goto-char (point-min))
      (while (search-forward-regexp "^Subject: $" (point-max) t)
	(replace-match (concat "Subject: " mi-message-header-subject)))))

  (unless (null mi-message-header-cc)
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
