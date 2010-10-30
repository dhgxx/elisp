;; mi-backup.el

;; backup policies
(setq make-backup-files t
      version-control t
      kept-old-versions 1
      kept-new-versions 1
      delete-old-versions t
      delete-auto-save-files t
      backup-directory-alist '(("" . "~/.emacs.d/rescue")))

