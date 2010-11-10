;;; abbrev configuration

(setq abbrev-mode t)
(quietly-read-abbrev-file)
(setq save-abbrevs t)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
