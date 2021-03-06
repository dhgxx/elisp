;; mi-gui.el

;; use color for bg and fg under windows
(set-foreground-color "#eeeeee")
(set-background-color "#101010")

;; visual appearance
(tool-bar-mode 0)

;; display line number
(line-number-mode t)
(setq column-number-mode t)

;; display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq system-time-locale "C")
(display-time)

;; no visible bell
(setq visible-bell t)

;; mouse yank at point!
(setq mouse-yank-at-point t)

;; large kill ring
(setq kill-ring-max 200)

;; fill column
(setq fill-column 72)

;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; default major mode
;(setq major-mode 'text-mode)

;; matching parentheses with colors
(show-paren-mode t)
;; show-paren-style takes threes arguments as follow:
;; `parenthesis' - show matching paren.
;; `expression' - show entire expression enclosed by the paren
;; `mixed' - show matching paren if visible, and the expression otherwise.
(setq show-paren-style 'parenthesis)

;; emacs frame title
(setq frame-title-format "emacs@%b - [%f]")

;; image display
(auto-image-file-mode)

;; default tab width
(setq tab-width 4)
(setq tab-always-indent t)

;; mouse scrolling
(setq mouse-wheel-scroll-amount `(2))
(setq mouse-wheel-progressive-speed nil)
