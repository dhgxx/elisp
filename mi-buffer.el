;; mi-buffer.el

(require 'bs)

(setq bs-must-always-show-regexp
      (concat ".*[Tt][Ee][Rr][Mm].*\\|"
	      ".*[Ss][Hh][Ee][Ll].*\\|"
	      ".*[Ss][Cc][Rr][Aa][Tt][Cc][Hh].*\\|"
	      ".*[Ii][Nn][Ff][Oo].*\\|"
	      ".*[Gg]roup.*\\|"
	      ".*[Aa]rticle.*\\|"
	      ".*[Ss]ummary.*"))

(global-set-key "\C-cb" 'bs-show)
(global-set-key (kbd "M-<up>") 'bs-cycle-previous)
(global-set-key (kbd "M-<down>") 'bs-cycle-next)
(global-set-key "\C-c\C-p" 'bs-cycle-previous)
(global-set-key "\C-c\C-n" 'bs-cycle-next)
