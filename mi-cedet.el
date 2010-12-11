;;; emacs cedet configuration

(require 'cedet)
(require 'semantic-ia)
(require 'semantic-c)
(require 'semantic-gcc)
(require 'semantic-make)

(defconst opt-include-dirs
  (list "/opt/local/include" "/opt/local/include/xbsd"))

(let ((include-dirs opt-include-dirs))
  (mapc (lambda (dir)
	  (semantic-add-system-include dir 'c++-mode)
	  (semantic-add-system-include dir 'c-mode))
	include-dirs))

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(setq semantic-load-turn-useful-things-on t)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
 
;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

