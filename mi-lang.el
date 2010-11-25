;; mi-lang.el

;; chinese encoding et al.
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
;;(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(modify-coding-system-alist 'file "*" 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq-default pathname-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq locale-coding-system 'utf-8)
(setq x-select-enable-clipboard t)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; emacs23 xft fonts
(when mi-use-xwindow
  (add-to-list 'default-frame-alist '(font . "Monaco-11"))
  (set-fontset-font "fontset-default" 'ascii '("Monaco" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'latin '("Monaco" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'cyrillic-iso8859-5 '("Monaco" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-gb2312 '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-gbk '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'gb18030 '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-big5-1 '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-big5-2 '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-cns11643-1 '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-cns11643-2 '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'unicode '("Droid Sans Fallback" . "unicode-bmp"))
  (set-fontset-font "fontset-default" '(#x20ac . #x20ac) '("Droid Sans Mono" . "unicode-bmp"))
  ;; chinese fill
  (put-charset-property 'chinese-cns11643-5 'nospace-between-words t)
  (put-charset-property 'chinese-cns11643-6 'nospace-between-words t)
  (put-charset-property 'chinese-cns11643-7 'nospace-between-words t)
  ;; chinese punctuations
  (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
	sentence-end-double-space nil))
