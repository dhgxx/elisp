;; mi-xml.el

;; html
(if mi-startup-firsttime-run
    (setq auto-mode-alist
	  (append '(("\\.[Hh][Tt][Mm].*" . html-mode))
		  auto-mode-alist)))

;; sgml
(if mi-startup-firsttime-run
    (setq auto-mode-alist
	  (append '(("\\.[Ss][Gg][Mm][Ll]$" . psgml-mode))
		  auto-mode-alist)))
