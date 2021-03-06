;;; mi-user.el --- information about me

;;; Commentary:
;;  general information about me.

;;; Code:


(defcustom mi-message-user-full-name (user-full-name)
  "User's full name, which defaults to user's full name from local system."
  :type 'string
  :group 'mi-user)

(defcustom mi-message-user-mail-address nil
  "User's default mail address."
  :type 'string
  :group 'mi-user)

(defcustom mi-message-user-nickname (user-login-name)
  "User's nickname, which defaults to user's login name."
  :type 'string
  :group 'mi-user)

(if (string-match "^pluton.xbsd.name$"
		  (shell-command-to-string "echo -n `uname -n`"))
    (setq mi-message-user-mail-address
	  "darcsis@gmail.com")
  (setq mi-message-user-mail-address
	"tamgya@gmail.com"))

(provide 'mi-user)

;;; mi-user.el ends here
