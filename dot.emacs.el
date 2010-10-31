;; dot.emacs.el

;; no startup message
;; changes since 23.2
;;(setq inhibit-startup-message t)
(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq gnus-init-file "~/emacs/elisp/mi-gnus-init.elc"
      gnus-directory "~/emacs/gnus")

;; auto fill in text mode
;;(if mi-startup-first-time
;;    (toggle-text-mode-auto-fill))

;; override default key bindings for set-mark-command
(global-set-key "\C-[space]" nil)
(global-set-key "\M-sm" nil)
(global-set-key "\M-sm" 'set-mark-command)
