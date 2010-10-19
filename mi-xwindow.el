;;; mi-xwindow.el --- Window manager state changer.

;;; Commentary:
;; toggle different window manager modes.

;;; Code:

(when mi-use-xwindow
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

  (defun mi-xwindow-toggle-maximize ()
    (interactive)
    (progn
      (mi-xwindow-maximized-vert)
      (mi-xwindow-maximized-horz)))

  (global-set-key "\C-z" nil)
  (global-set-key "\C-zf" 'mi-xwindow-toggle-fullscreen)
  (global-set-key "\C-zm" 'mi-xwindow-toggle-maximize))

;;; mi-xwindow.el ends here
