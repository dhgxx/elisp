;; mi-tramp.el

(require 'tramp)

(setq
 tramp-default-method "ssh"
 tramp-default-host "sol.xbsd.name"
 tramp-remote-sh "/bin/ksh"
 tramp-backup-directory-alist '(("" . "~/.emacs.d/rescure/tramp"))
 tramp-default-method-alist '(("" "\\'dhg\\'" "ssh")
			      ("" "\\'ftp\\'" "sftp")
			      ("" "\\'root\\'" "sudo")))
