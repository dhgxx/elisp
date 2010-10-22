;; mi-gnus-init.el

;; startup file
(setq
 gnus-home-directory "~/emacs/gnus"
 gnus-default-directory gnus-home-directory
 gnus-startup-file (concat gnus-home-directory "/newsrc")
 gnus-article-save-directory (concat "/news/articles")
 gnus-kill-files-directory (concat gnus-home-directory "/news/articles")
 message-directory (concat gnus-home-directory "/messages")
 message-auto-save-directory (concat gnus-home-directory "/messages/autosave")
 nndraft-directory (concat gnus-home-directory "/messages/drafts")
 mail-source-directory message-directory)


;; primary select method
(setq
 gnus-select-method '(nntp "news.cn99.com")
 gnus-secondary-select-methods 
 '((nnfolder "archive"
	     (nnfolder-directory "~/emacs/gnus/archive/")
	     (nnfolder-active-file "~/emacs/gnus/archive/ative")
	     (nnfolder-newsgroups-file "~/emacs/gnus/archive/newsgroups")
	     (nnfolder-get-new-mail nil))))

;; more news group
(add-to-list 'gnus-secondary-select-methods
             '(nnml ""))
;;(add-to-list 'gnus-secondary-select-methods
;;             '(nntp "news.gmane.org"))
;;(add-to-list 'gnus-secondary-select-methods
;;			 '(nnslashdot ""))

;; expiring rules
(setq gnus-auto-expirable-newsgroups
      "mail\\.freebsd\\..*\\|mail.freedesktop\\..*\\|mail.opensolaris\\..*\\|mail\\.openbsd\\..*\\|mail\\.x11\\..*\\|mail\\.news.*")

;; misc
(setq
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil)

;; coding system
;;
;; gnus default
(setq
 gnus-default-charset 'utf-8
 gnus-group-name-charset-group-alist '((".*" . utf-8)
				       ("^cn\\.*" . chinese-iso-8bit)
				       ("^tw\\.*" . big5))
 gnus-summary-show-article-charset-alist
 '((1 . ascii)
   (2 . iso-8859-1)
   (3 . gb2312)
   (4 . gb18030)
   (5 . big5)
   (6 . utf-8))
 gnus-newsgroup-ignored-charsets
 '(unknown-8bit x-unknown x-gbk))

;; coding alias
;; maybe now we can handle gb18030
;;(define-coding-system-alias 'gb18030 'gb2312)
(define-coding-system-alias 'gbk 'gb18030)

;; visual appearance
;;
;; summary buffer sorting
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
	(not gnus-thread-sort-by-number)))

;; frame resizing
(gnus-add-configuration '(article
                          (vertical 1.0
				    (summary .4 point)
				    (article 1.0))))

;; view html
(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; more headers
(add-hook 'gnus-startup-hook
          '(lambda ()
             (setq gnus-visible-headers
                   (concat "^From:\\|^User-Agent:\\|^Content-Type:\\|"
                           "^Content-Transfer-Encoding:\\|"
			   "^Message-ID:\\|"
                           "^X-Mailer:\\|^X-Newsreader:\\|^Xref:\\|^X-Sender:\\|"
                           gnus-visible-headers))))

;; group buffer line format
(setq
 gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%l %O\n"
 ;;gnus-summary-line-format ":%U%R %B %s %-70=|%3L|%-20,20n|%&user-date; \n")
 gnus-summary-line-format ":%U%R %B %s %-80=|%3L|%-20,20n|%d| \n")

;; process ansi colors
(autoload 'ansi-color-apply-on-region "ansi-color")
(defun article-treat-ansi-sequences ()
  "Translate ANSI SGR control sequences into overlays or extents."
  (interactive)
  (save-excursion
    (when (article-goto-body)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point) (point-max))))))
(add-hook 'gnus-part-display-hook 'article-treat-ansi-sequences)

;; use rfc2047
(require 'rfc2047)
(defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)
(setq rfc2047-allow-incomplete-encoded-text nil)
(progn
  (add-to-list 'rfc2047-header-encoding-alist '("Subject"))
  (add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
  (add-to-list 'rfc2047-charset-encoding-alist '(gb2312 . B))
  (add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))
  (add-to-list 'rfc2047-charset-encoding-alist '(big5 . B))
  (add-to-list 'rfc2047-charset-encoding-alist '(utf-8 . B)))

;; encoding priorities
(setq mm-coding-system-priorities '(iso-8859-1 gb2312 gb18030 big5 utf-8))

;; decide encoding according group names
(defvar mi-organization "Pluto The Planet"
  "My default organization name.")
(defvar mi-signature-file "~/.signature"
  "My default signature file name.")
(defvar mi-chinese-nickname "dhg"
  "My default Chinese nick name.")

;; defaults to using only utf-8
(setq gnus-posting-styles
      '((".*"
         (name user-full-name)
         (address "tamgya@gmail.com")
         (signature-file mi-signature-file)
         (organization mi-organization)
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 utf-8))))
	("^cn\\..*"
         (name mi-chinese-nickname)
         (address "tamgya@gmail.com")
         (signature-file mi-signature-file)
         (organization mi-organization)
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 gb2312 gb18030 utf-8))))
        ("^tw\\..*"
         (name mi-chinese-nickname)
         (address "tamgya@gmail.com")
         (signature-file mi-signature-file)
         (organization mi-organization)
         (eval (setq mm-coding-system-priorities
                     '(iso-8859-1 big5 utf-8))))
	(".*\\.bbs\\..*"
	 (name mi-chinese-nickname)
	 (signature-file "~/.signature-rotated")
	 (organization-file mi-orgazation)
	 (eval (setq mm-coding-system-priorities
		     '(iso-8859-1 gb2312 gb18030 big5 utf-8))))
	("^\\(\\(nnfolder\\)\\|\\(nnml\\)\\):.*mail\\..*"
	 (name user-full-name)
	 (address "tamgya@gmail.com")
	 (signature-file mi-signature-file)
	 (organization mi-organization)
	 (eval (setq mm-coding-system-priorities
		     '(iso-8859-1 gb2312 gb18303 big5 utf-8))))))

;; confirm sending mail to newsgroups
(setq gnus-confirm-mail-reply-to-news t)

;; auto fill long lines
(add-hook 'message-mode-hook
          (lambda ()
	    (setq fill-column 72)
            (turn-on-auto-fill)))

;; auto article washing
(setq mm-text-html-renderer 'lynx)

;; generate all headers
(setq message-generate-headers-first t)

;; confirm sending mail/news
(defadvice message-send (around my-confirm-message-send)
  (if (yes-or-no-p "Really send message? ")
      ad-do-it))
(ad-activate 'message-send)

;; local posting time

(add-hook 'gnus-article-prepare-hook
	  'gnus-article-date-local)
;;(add-hook 'gnus-article-prepare-hook
;;	  'gnus-article-fill-long-lines)

;; MFT things
(setq message-subscribed-regexps
      '("\\(\\(.*@[Oo][Pp][Ee][Nn][Ss][Uu][Ss][Ee]\\)\\|\\(.*@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\)\\|\\(.*@[Oo][Pp][Ee][Nn][Ss][Oo][Ll][Aa][Rr][Ii][Ss]\\)\\|\\(.*@[Oo][Pp][Ee][Nn][Bb][Ss][Dd]\\)\\|\\(.*@[Ll][Ii][Ss][Tt][Ss]\\.[Ff][Rr][Ee][Ee][Dd][Ee][Ss][Kk][Tt][Oo][Pp]\\)\\|\\(.*@[Ll][Ii][Ss][Tt][Ss]\\.[Xx]\\)\\)\\.[Oo][Rr][Gg]"))

;; citation style
(require 'supercite)

(defvar mi-is-reply-to-bbs nil "Whether we are replying to a BBS")
(defvar mi-safe-time-val nil "Nil if date string is invalid")

(defun mi-header-on-wrote ()
  "Similar to `sc-header-on-said', but using a shorter date string."
  (setq mi-safe-time-val (safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert (sc-hdr "\nOn " (format-time-string "%Y/%m/%d at %R" mi-safe-time-val) ", ")
		whofrom " wrote:\n"))))

(defun mi-header-on-wrote-cn ()
  "Similar to `mi-header-on-said', but using Chinese."
  (defvar mi-whofrom-id nil
    "The ID portion of the complete string of a BBS author.")
  (defvar mi-whofrom-nick nil
    "The nickname portion of the complete string of a BBS author")
  (setq mi-safe-time-val (safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(progn
	  (setq
	   mi-whofrom-id (replace-regexp-in-string "\\(\\.bbs\\|@.*\\|.*[<]\\|[>].*\\)" "" whofrom)
	   mi-whofrom-nick (replace-regexp-in-string "\\(.*\(\\|\).*\\|\"\\|[ ]*<.*>\\)" "" whofrom))
	  (if (and
	       (equal mi-whofrom-id mi-whofrom-nick)
	       (= (string-width mi-whofrom-id)
		  (string-width mi-whofrom-nick)))
	      (insert (sc-hdr "\n【 在 " (format-time-string "%Y年%m月%d日 %H点%M分%S秒" mi-safe-time-val) ", ")
		      mi-whofrom-id " 说道: 】\n")
	    (insert (sc-hdr "\n【 在 " (format-time-string "%Y年%m月%d日 %H点%M分%S秒" mi-safe-time-val) ", ")
		    mi-whofrom-id " (" mi-whofrom-nick ") 说道: 】\n"))))))

;; super citation style
(setq sc-citation-leader ""
      sc-citation-leader-regexp ""
      sc-reference-tag-string ""
      sc-citation-separator " "
      sc-citation-separator-regexp "[ ]*")

(defun mi-message-citation-style-normal ()
  "Normal citation prefix and regex"
  (interactive)
  (setq sc-cite-blank-lines-p t
	sc-preferred-header-style 2 ; use newsgroup style citation header
	sc-citation-delimiter ">"
	sc-citation-delimiter-regexp "[>]+"
	message-yank-prefix "> "
	message-yank-cited-prefix ">"
	message-yank-empty-prefix ">"
	message-cite-prefix-regexp "\\(\\([:word:]\\|[_.]\\)+>+\\|[ ]*[]>|}]\\)+"))

(defun mi-message-citation-style-bbs ()
  "Citation prefix and regex for bbs"
  (interactive)
  (setq sc-cite-blank-lines-p t
	sc-preferred-header-style 1 ; use bbs style citation header
	sc-citation-delimiter ":"
	sc-citation-delimiter-regexp "\\([:word:]\\|[_.]\\)+[:]+"
	message-yank-prefix ": "
	message-yank-cited-prefix ":"
	message-yank-empty-prefix ":"
	message-cite-prefix-regexp "\\(\\([:word:]\\|[_.]\\)*:+\\|[ ]*[]:+|}]\\)+"))
(defvar mi-is-or-not-newsgroup nil "To see whether or not we are in a newsgroup browsing.")
(defun mi-message-citation-style ()
  "We are replying to a BBS"
  (interactive)
  (setq mi-is-or-not-newsgroup (gnus-fetch-field "Xref"))
  (if (not (null (string-match ".*\\.[Bb][Bb][Ss]\\..*" mi-is-or-not-newsgroup)))
      (setq mi-is-reply-to-bbs t))
  (save-excursion
    (if mi-is-reply-to-bbs
	(mi-message-citation-style-bbs)
      (mi-message-citation-style-normal))))

;;  citation post hook
;; post functions
(defun mi-citation-post-hooks ()
  "My own post hooks for citation"
  (interactive)
  (mi-citation-delete-signature)
  (mi-message-header-subject-rewrite)
  (mi-citation-ready-to-compose)
  (setq mi-is-reply-to-bbs nil))

(defvar mi-signature-current-line nil "Current working line within in signature region.")
(defun mi-citation-ready-to-compose ()
  "Do something after citation completes"
  (interactive)
  (goto-char (point-max))
  (while (search-backward-regexp "^\\([-]\\|[_]\\)+[ ]*$" (point-min) t)
    (progn
      (setq mi-signature-current-line (line-number-at-pos (point)))
      (beginning-of-line)
      (newline 3)
      (goto-char (point-min))
      (forward-line mi-signature-current-line))))

(defvar mi-signature-region-start nil "A generic marker for the start point of a region.")
(defvar mi-signature-region-end nil "A generic marker for the end point of a region.")

(defun mi-citation-delete-signature ()
  "Delete signatures in citation"
  (interactive)
  (progn
    (goto-char (point-min))
    (setq mi-signature-region-start
	  (search-forward-regexp "^\\([ ]*[>][ ]*\\)+\\([-][-]+\\|[_][_]+\\)[ ]*$" (point-max) t))
    (if (null mi-signature-region-start)
	(progn
	  (goto-char (point-min))
	  (setq mi-signature-region-start
		(search-forward-regexp "^\\([ ]*[:][ ]*\\)+\\([-][-]+\\|[_][_]+\\)[ ]*$" (point-max) t)))))
  
  (if (not (null mi-signature-region-start))
      (progn
	(beginning-of-line)
	(setq mi-signature-region-start (point))))
  
  (progn
    (goto-char (point-max))
    (setq mi-signature-region-end (search-backward-regexp "^[-][-]+[ ]*$" (point-min) t)))
  
  (if (and mi-signature-region-start mi-signature-region-end)
      (if (and
	   (> mi-signature-region-start (point-min))
	   (< mi-signature-region-end (point-max)))
	  (progn
	    (delete-region mi-signature-region-start mi-signature-region-end)
	    (insert (concat
		     sc-citation-delimiter " ................ \n"))
	    (setq mi-signature-region-start nil
		  mi-signature-region-end nil)
	    (goto-char (point-max))
	    (search-backward-regexp "^[-][-]+[ ]*" (point-min) t)))))

;; citation funcion
(setq message-cite-function
      'message-cite-original-without-signature)
;; blank lines should be cited, too
(setq sc-cite-blank-lines-p t)
;; nesting citation
(setq sc-nested-citation-p t)
;; don't fill citation
(setq sc-auto-fill-region-p nil)
(add-hook 'sc-pre-hook 'mi-message-citation-style)
(add-hook 'sc-post-hook 'mi-citation-post-hooks)
(add-hook 'mail-citation-hook 'sc-cite-original)

;; mi-header-on-wrote now follows `sc-no-header' in
;; `sc-rewrite-header-list'.
(setq mi-tmp-rewrite-header-list (cdr sc-rewrite-header-list))
(add-to-list 'mi-tmp-rewrite-header-list '(mi-header-on-wrote))
(add-to-list 'mi-tmp-rewrite-header-list '(mi-header-on-wrote-cn))
;; now the first one is `mi-header-on-wrote-cn'.
(add-to-list 'mi-tmp-rewrite-header-list '(sc-no-header))
(setq sc-rewrite-header-list mi-tmp-rewrite-header-list)
(setq mi-tmp-rewrite-header-list nil)

;; misc
;;

;; mail header subjecr rewrite
(defvar mi-message-header-subject nil "Rewritten mail header subject, include it with double quotation markers.")
(defun mi-message-header-subject-rewrite ()
  "Rewrite mail header subject, include it with double quotation markers."
  (setq mi-message-header-subject
	(gnus-fetch-field "Subject"))
  (if (and
       (not (null mi-message-header-subject))
       (not (eq (string-bytes mi-message-header-subject)
		(length mi-message-header-subject))))
      (progn
	(setq mi-message-header-subject
	      (replace-regexp-in-string "\\\\" "" mi-message-header-subject nil nil)
	      mi-message-header-subject
	      (replace-regexp-in-string "^[ ]*[\"]" "" mi-message-header-subject nil nil)
	      mi-message-header-subject
	      (replace-regexp-in-string "[\"][ ]*$" "" mi-message-header-subject nil nil)
	      mi-message-header-subject
	      (replace-regexp-in-string "[\"]" "'" mi-message-header-subject nil nil)
	      mi-message-header-subject
	      (concat "Subject: \"" mi-message-header-subject "\""))
	(goto-char (point-min))
	(while (search-forward-regexp "Subject: .*$" (point-max) t)
	  (replace-match mi-message-header-subject nil nil))
	(setq mi-message-header-subject nil))))

;; enable caching
(setq gnus-use-cache 'passive)

;; mail archiving
(setq gnus-message-archive-group
      '((if (message-news-p)
            "nnml:mail.sent.news"
          "nnml:mail.sent.mail")))

;; mail splitting
(setq
 nnmail-split-methods
 '(("mail.openbsd.misc"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Mm][Ii][Ss][Cc]@\\(\\([Cc][Vv][Ss]\\.[Oo][Pp][Ee][Nn][Bb][Ss][Dd]\\)\\|\\([Oo][Pp][Ee][Nn][Bb][Ss][Dd]\\)\\)\\.[Oo][Rr][Gg].*")
   ("mail.openbsd.tech"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Tt][Ee][Cc][Hh]@[Oo][Pp][Ee][Nn][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.security"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\([Ff][Rr][Ee][Ee][Bb][Ss][Dd]-[Ss][Ee][Cc][Uu][Rr][Ii][Tt][Yy]\\)\\|\\([Ss][Ee][Cc][Uu][Rr][Ii][Tt][Yy]-[Aa][Dd][Vv][Ii][Ss][Oo][Rr][Ii][Ee][Ss]\\)\\)@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.bugs"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Bb][Ss][Dd]-[Bb][Uu][Gg][Ss]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.fs"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\([Bb][Ss][Dd]-[Ff][Ss]\\)\\|\\([Ff][Ss]\\)\\).*@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.gnats"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Gg][Nn][Aa][Tt][Ss].*@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.amd64"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Aa][Mm][Dd]64@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.x11"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Xx]11@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.multimedia"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Mm][Uu][Ll][Tt][Ii][Mm][Ee][Dd][Ii][Aa]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.hardware"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Hh][Aa][Rr][Dd][Ww][Aa][Rr][Ee]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.gnome"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Gg][Nn][Oo][Mm][Ee]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.cvs-all"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\([Ss][Vv][Nn]-[Ss][Rr][Cc]-[Aa][Ll][Ll]\\)\\|\\([Cc][Vv][Ss]-[Aa][Ll][Ll]\\)\\)@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.cvs-doc"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Cc][Vv][Ss]-[Dd][Oo][Cc]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.cvs-ports"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Cc][Vv][Ss]-[Pp][Oo][Rr][Tt][Ss]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.cvs-src"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\([Ss][Vv][Nn]\\)\\|\\([Cc][Vv][Ss]\\)\\)-[Ss][Rr][Cc]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.ports"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):\\(\\(.*[Bb][Ss][Dd]-[Pp][Oo][Rr][Tt][Ss]\\)\\|\\(.*[^-][Pp][Oo][Rr][Tt][Ss]\\)\\)@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.ports-bugs"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Pp][Oo][Rr][Tt][Ss]-[Bb][Uu][Gg][Ss]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.bug-followup"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Bb][Uu][Gg]-[Ff][Oo][Ll][Ll][Oo][Ww][Uu][Pp]@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.stable"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Ss][Tt][Aa][Bb][Ll][Ee].*@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.hackers"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Hh][Aa][Cc][Kk][Ee][Rr].*@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freebsd.current"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Cc][Uu][Rr][Rr][Ee][Nn][Tt].*@[Ff][Rr][Ee][Ee][Bb][Ss][Dd]\\.[Oo][Rr][Gg].*")
   ("mail.freedekstop.xorg"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Xx][Oo][Rr][Gg]@.*\\(\\([Ff][Rr][Ee][Ee][Dd][Ee][Ss][Kk][Tt][Oo][Pp]\\)\\|\\([Bb][Uu][Gg][Ss]\\.[Dd][Ee][Bb][Ii][Aa][Nn]\\)\\)\\.[Oo][Rr][Gg].*")
   ("mail.opensolaris.amd64"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Aa][Mm][Dd]-[Pp][Ll][Aa][Tt][Ff][Oo][Rr][Mm]-.*@[Oo][Pp][Ee][Nn][Ss][Oo][Ll][Aa][Rr][Ii][Ss]\\.[Oo][Rr][Gg].*")
   ("mail.opensolaris.desktop"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Dd][Ee][Ss][Kk][Tt][Oo][Pp]-.*@[Oo][Pp][Ee][Nn][Ss][Oo][Ll][Aa][Rr][Ii][Ss]\\.[Oo][Rr][Gg].*")
   ("mail.opensolaris.sysadmin"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Ss][Yy][Ss][Aa][Dd][Mm][Ii][Nn]-.*@[Oo][Pp][Ee][Nn][Ss][Oo][Ll][Aa][Rr][Ii][Ss]\\.[Oo][Rr][Gg].*")
   ("mail.newsletters"
    "^From:\\(\\(.*\\)\\|\\(.*[Nn][Oo]-[Rr][Ee][Pp][Ll][Yy].*\\)\\)@.*\\(\\([Nn][Oo][Rr][Ee][Pp][Ll][Yy]\\)\\|\\([Mm][Aa][Ii][Ll]\\.[Cc][Oo][Mm][Mm][Uu][Nn][Ii][Cc][Aa][Tt][Ii][Oo][Nn][Ss]\\.[Ss][Uu][Nn]\\)\\|\\([Cc][Oo][Mm][Mm][Uu][Nn][Ii][Cc][Aa][Tt][Ii][Oo][Nn][Ss]2\\)\\|\\([Gg][Ee][Oo][Cc][Aa][Cc][Hh][Ii][Nn][Gg]\\)\\|\\([Aa][Pp][Pp][Ll][Ee]\\)\\|\\([Ee][Cc][Oo][Nn][Oo][Mm][Ii][Ss][Tt]\\)\\|\\([Ff][Rr][Ee][Ee][Ss][Oo][Tt][Ww][Aa][Rr][Ee][Mm][Aa][Gg][Aa][Zz][Ii][Nn][Ee]\\)\\|\\([Nn][Yy][Tt][Ii][Mm][Ee][Ss]\\)\\|\\([Pp][Hh][Oo][Rr][Oo][Nn][Ii][Xx]\\)\\|\\([Ss][Ll][Aa][Ss][Hh][Dd][Oo][Tt]\\)\\|\\([Oo][Ss][Nn][Ee][Ww][Ss]\\)\\|\\([Yy][Ee][Ee][Yy][Aa][Nn]\\)\\|\\([Zz][Ii][Kk][Ii]\\)\\|\\([Ss][Oo][Uu][Rr][Cc][Ee][Ff][Oo][Rr][Gg][Ee]\\)\\|\\([Mm][Yy][Ff][Oo][Nn][Tt][Ss]\\)\\)\\.\\(\\([Oo][Rr][Gg]\\)\\|\\([Cc][Oo][Mm]\\)\\|\\([Nn][Ee][Tt]\\)\\).*")
   ("mail.x11.ati"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Xx][Oo][Rr][Gg]-[Dd][Rr][Ii][Vv][Ee][Rr]-[Aa][Tt][Ii].*@[Ll][Ii][Ss][Tt][Ss]\\.[Xx]\\.[Oo][Rr][Gg].*")
   ("mail.x11.radeonhd"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\|\\(Subject\\)\\):\\(\\(.*[Rr][Aa][Dd][Ee][Oo][Nn][Hh][Dd].*\\)\\|\\(.*[Rr][Aa][Dd][Ee][Oo][Nn][Hh][Dd].*@[Oo][Pp][Ee][Nn][Ss][Uu][Ss][Ee]\\.[Oo][Rr][Gg].*\\)\\)")
   ("mail.x11.nouveau"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Nn][Oo][Uu][Vv][Ee][Aa][Uu].*@[Ll][Ii][Ss][Tt][Ss]\\.[Ff][Rr][Ee][Ee][Dd][Ee][Ss][Kk][Tt][Oo][Pp]\\.[Oo][Rr][Gg].*")
   ("mail.x11.kde"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*@[Kk][Dd][Ee]\\.[Oo][Rr][Gg].*")
   ("mail.x11.gnome-cn"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*[Gg][Nn][Oo][Mm][Ee]-[Cc][Nn]-[Ll][Ii][Ss][Tt].*@[Gg][Nn][Oo][Mm][Ee]\\.[Oo][Rr][Gg].*")
   ("mail.local"
    "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*@\\(\\(sol\\)\\|\\(pluton\\)\\)\\.xbsd\\.name$")
   ("mail.old"
    "^From:.*\\(\\(kapo\\)\\|\\(stoneboy\\)\\)@.*")
   ("mail.misc" "")))

;; gnus daemon
(gnus-demon-init)
(gnus-demon-add-handler 'gnus-demon-scan-mail 10 t)
(gnus-demon-add-handler 'gnus-demon-scan-news 10 t)

;; my own gnus defun's
(defvar mi-gnus-default-article-number 200
  "Default number of articles to fetch in Gnus")
(defun mi-gnus-select-group ()
  "To fetch a pre-set number of articles from a group in Gnus.
See `mi-gnus-default-article-number' for more information."
  (interactive)
  (set-buffer (current-buffer))
  (gnus-group-select-group mi-gnus-default-article-number))

(global-set-key "\C-cg" nil)
(global-set-key "\C-cgg" 'gnus)
(global-set-key "\C-cgf" 'mi-gnus-select-group)
