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

(defun mi-setup-window-resolutions ()
  (interactive)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
	(add-to-list 'default-frame-alist (cons 'width 120))
      (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
		 (cons 'height (/ (- (x-display-pixel-height) 50)
				  (frame-char-height))))))

(if mi-use-xwindow
    (progn
      (mi-setup-window-resolutions)
      (global-set-key "\C-z" nil)
      (global-set-key "\C-zf" 'mi-xwindow-toggle-fullscreen)
      (global-set-key "\C-zm" '(lambda ()
				 (interactive)
				 (progn
				   (mi-xwindow-maximized-vert)
				   (mi-xwindow-maximized-horz))))))

;;; mi-xwindow.el ends here
