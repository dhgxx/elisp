;; mi-yas.el

(require 'yasnippet)
(yas/initialize)
;; for the default snippets:
(yas/load-directory "/usr/local/share/yasnippet/snippets")

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))
