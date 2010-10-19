;; mi-diff.el

;; diff
(setq
 diff-command "/usr/bin/diff"
 diff-switches "-anu"
 ediff-window-setup-function 'ediff-setup-windows-plain)

(global-set-key "\C-cd" 'diff)

