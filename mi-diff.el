;; mi-diff.el

;; diff
(setq ediff-custom-diff-progam "/usr/bin/diff"
      ediff-custom-diff-options "-anu"
      ediff-window-setup-function 'ediff-setup-windows-multiframe)

(global-set-key "\C-ce" nil)
(global-set-key "\C-ced" 'ediff)
(global-set-key "\C-ceb" 'ediff-backup)

