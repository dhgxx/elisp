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
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
  (set-fontset-font "fontset-default" 'ascii '("DejaVu Sans Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'latin '("DejaVu Sans Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'cyrillic-iso8859-5 '("DejaVu Sans Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-gb2312 '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-gbk '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'gb18030 '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-big5-1 '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-big5-2 '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-cns11643-1 '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'chinese-cns11643-2 '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'unicode '("WenQuanYi Micro Hei Mono" . "unicode-bmp"))
  (set-fontset-font "fontset-default" '(#x20ac . #x20ac) '("DejaVu Sans Mono" . "unicode-bmp"))
  ;; chinese fill
  (put-charset-property 'chinese-cns11643-5 'nospace-between-words t)
  (put-charset-property 'chinese-cns11643-6 'nospace-between-words t)
  (put-charset-property 'chinese-cns11643-7 'nospace-between-words t)
  ;; chinese punctuations
  (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
	sentence-end-double-space nil))
