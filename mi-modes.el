;; mi-modes.el

;;
;; stolen from: http://www.dotemacs.de/dotfiles/MichaelAWolf.emacs.html
;;
;; These tell emacs to associate certain filename extensions with 
;; certain modes.  I use cc-mode.el (c++-mode) for C as well as C++
;; code.  It is fairly all-encompassing, also working with other C-like
;; languages, such as Objective C and Java. 
;;

(setq
 auto-mode-alist (cons '("\\.text$" . text-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.doc$" . text-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.awk$" . awk-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.perl$" . perl-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.plx$" . perl-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.pl$" . perl-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.C$" . c++-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.cc$" . c++-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.c$" . c-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.h$" . c-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.cpp$" . c++-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.tcl$" . tcl-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.sh$" . shell-script-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.zsh$" . shell-script-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.cs$" . csharp-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.[Pp][Rr]$" . send-pr-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.[Jj][Nn][Ll]$" . journal-mode) auto-mode-alist))

(setq completion-ignored-extensions;; Filename completion ignores these.
      (append completion-ignored-extensions 
			  '(".CKP" ".u" ".press" ".imp" ".BAK")))

(put 'eval-expression 'disabled nil)


