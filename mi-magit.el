;;; mi-magit.el -- emacs interface to git.

(require 'magit)

(setq magit-repo-dirs '("~/repos/codaset"
			"~/repos/git"
			"~/repos/github"))

(global-set-key "\C-cm" nil)
(global-set-key "\C-cms" 'magit-status)

;;; mi-magit.el ends here
