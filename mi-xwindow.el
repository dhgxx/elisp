;;; mi-xwindow.el --- Window manager state changer.

;;; Commentary:
;; toggle different window manager modes.

;;; Code:


(defun mi-xwindow-toggle-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun mi-xwindow-maximized-horz ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defun mi-xwindow-maximized-vert ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(when mi-use-xwindow
  (global-set-key "\C-z" nil)
  (global-set-key "\C-zf" 'mi-xwindow-toggle-fullscreen)
  (global-set-key "\C-zm" '(lambda ()
			     (interactive)
			     (progn
			       (mi-xwindow-maximized-vert)
			       (mi-xwindow-maximized-horz)))))

;;; mi-xwindow.el ends here
