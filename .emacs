;; .emacs

;; first time startup
(setq mi-startup-firsttime-run t)

;; load path
(add-to-list 'load-path "~/emacs/local-lisp")

;; test wether it is under xwindow
(if (string-match "x" (prin1-to-string window-system))
    (setq mi-use-xwindow 1)
  (setq mi-use-xwindow nil))

;; default configuration direcotry
(setq mi-default-conf-dir "~/emacs/elisp/")
(setq mi-default-conf-elisp-dir "~/emacs/elisp/")

(setq mi-startup-el-files (directory-files mi-default-conf-elisp-dir t ".+\\.el$"))
(setq mi-startup-elc-files (directory-files mi-default-conf-dir t ".+\\.elc$"))

(while (not (null mi-startup-el-files))
  (setq startup-el-file (car mi-startup-el-files))
  (setq mi-startup-el-files (cdr mi-startup-el-files))
  (setq startup-el-file-bytecoded (concat startup-el-file "c"))
  (setq startup-elc-file (concat (concat mi-default-conf-dir
					 (file-name-nondirectory startup-el-file)) "c"))
  (when (file-newer-than-file-p startup-el-file startup-elc-file)
    (byte-compile-file startup-el-file)
    (rename-file startup-el-file-bytecoded startup-elc-file t)))

;; at last, suck in the configurations.
(mapc 'load-file mi-startup-elc-files)

;; first time startup completed
(setq mi-startup-firsttime-run nil)

;; end of dot.emacs
