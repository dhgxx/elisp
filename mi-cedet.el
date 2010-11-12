;;; emacs cedet configuration

(require 'cedet)
(require 'semantic-ia)
(require 'semantic-gcc)
 
;; Enable EDE (Project Management) features
(global-ede-mode 1)
 
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
 
;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)
