;; mi-ibus.el

;; ibus mode for emacs
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)
;; Use C-/ for Undo command
(ibus-define-common-key ?\C-/ nil)
;; Change cursor color depending on IBus status
(setq ibus-cursor-color '("red" "yellow" "limegreen"))
