;; mi-magit.el -- emacs interface to git.

(require 'magit)

(setq magit-repo-dirs '("~/repos/git/scripts"
			"~/repos/git/www"
			"~/repos/github/bsd"
			"~/repos/github/elisp"
			"~/repos/github/misc"))

(global-set-key "\C-cm" nil)
(global-set-key "\C-cms" 'magit-status)
