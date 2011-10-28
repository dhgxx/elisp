;; dot.emacs.el

;; no startup message
;; changes since 23.2
;;(setq inhibit-startup-message t)
(setq inhibit-startup-screen t
      initial-scratch-message nil)

(eval-after-load "gnus.el"
  '(setq gnus-init-file "~/emacs/elisp/mi-gnus-init.elc"
	 gnus-directory "~/emacs/gnus"))

;; override default key bindings for set-mark-command
(global-unset-key "\M-sm")
(global-set-key "\M-sm" 'set-mark-command)
