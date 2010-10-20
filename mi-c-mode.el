;; mi-c-mode.el

(require 'cc-mode)

;; stolen from http://www.bloomington.in.us/~brutt/emacs-c-dev.html
(c-set-offset 'substatement-open 0)
(c-set-offset 'case-label '+)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'topmost-intro-cont '+)

(defun mi-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
	(counter 1)
	(ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

;; c mode hook
(defun mi-c-mode-common-hook ()
  (setq tab-width 4)
  (mi-build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil)
  (c-toggle-hungry-state 1))

;; mode hooking, always last
(add-hook 'c-mode-common-hook 'mi-c-mode-common-hook)

