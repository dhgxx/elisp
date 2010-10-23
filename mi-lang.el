;; mi-lang.el

;; chinese encoding et al.
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-enable-clipboard t)
(set-clipboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(modify-coding-system-alist 'file "*" 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)

;; Xwidow related
;;(when mi-use-xwindow
;;  (setq font-encoding-alist
;;	(append '(("UTF8"          (utf-8 . 0))
;;		  ("GB18030"       (gb18030 . 0))
;;		  ("GB2312"        (chinese-gb2312 . 0))
;;		  ("HZ-GB-2312"    (chinese-gb2312 . 0))
;;		  ("GBK"           (chinese-gbk . 0))
;;		  ("BIG5"          (chinese-big5 . 0))
;;		  ("ISO-2022-CN"   (chinese-cns11643-1 . 0)))
;;		font-encoding-alist)))
;  (create-fontset-from-fontset-spec
;   (concat
;    "-misc-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-chinese,"
; 	"chinese-gb2312:-*-gb2312.1980-*,"
;	"chinese-big5-1:-*-big5-1,"
;	"chinese-big5-2:-*-big5-2,"
;	"chinese-cns11643-1:-*-cns11643-1,"
;	"chinese-cns11643-2:-*-cns11643-2,"
;	"chinese-cns11643-3:-*-cns11643-3,"
;	"ipa:-*-ipapannew-*-iso10646-*,"
;	"utf-8:-*-iso10646-*"))
;  (set-default-font "fontset-chinese")
;  (setq default-frame-alist
;		(append '((font . "fontset-chinese"))
;				default-frame-alist)))

;; emacs23 xft fonts
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-11"))
(set-fontset-font "fontset-default" 'ascii '("Droid Sans Mono" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'latin '("Droid Sans Mono" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'cyrillic-iso8859-5 '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'chinese-gb2312 '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'chinese-gbk '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'gb18030 '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'chinese-big5-1 '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'chinese-big5-2 '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'chinese-cns11643-15 '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" 'unicode '("Droid Sans Fallback" . "unicode-bmp"))
(set-fontset-font "fontset-default" '(#x20ac . #x20ac) '("Droid Sans Mono" . "unicode-bmp"))

;; chinese fill
(put-charset-property 'chinese-cns11643-5 'nospace-between-words t)
(put-charset-property 'chinese-cns11643-6 'nospace-between-words t)
(put-charset-property 'chinese-cns11643-7 'nospace-between-words t)

;; chinese punctuations
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
