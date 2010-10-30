;; mi-gnus-init.el -- gnus configuration

(require 'supercite)

;; startup file
(setq gnus-home-directory "~/emacs/gnus"
      gnus-default-directory gnus-home-directory
      gnus-startup-file (concat gnus-home-directory "/newsrc")
      gnus-article-save-directory (concat "/news/articles")
      gnus-kill-files-directory (concat gnus-home-directory "/news/articles")
      message-directory (concat gnus-home-directory "/messages")
      message-auto-save-directory (concat gnus-home-directory "/messages/autosave")
      nndraft-directory (concat gnus-home-directory "/messages/drafts")
      mail-source-directory message-directory)

;; misc
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

;; defaults to using only utf-8
(setq gnus-default-charset 'utf-8)

;; confirm sending mail to newsgroups
(setq gnus-confirm-mail-reply-to-news t)

;; local posting time
(add-hook 'gnus-article-prepare-hook
	  'gnus-article-date-local)

;; gnus select method
(setq gnus-select-method '(nntp "news.cn99.com")
      gnus-auto-expirable-newsgroups "mail\\.freebsd\\..*\\|mail.freedesktop\\..*\\|mail.opensolaris\\..*\\|mail\\.openbsd\\..*\\|mail\\.x11\\..*\\|mail\\.news.*")
(setq gnus-secondary-select-methods
      (nconc '((nnml "")
	       (nnfolder "archive"
			 (nnfolder-directory "~/emacs/gnus/archive/")
			 (nnfolder-active-file "~/emacs/gnus/archive/ative")
			 (nnfolder-newsgroups-file "~/emacs/gnus/archive/newsgroups")
			 (nnfolder-get-new-mail nil)))))

;; gnus default
(eval-after-load "gnus-group"
  '(setq gnus-group-name-charset-group-alist
	 (nconc '(("^tw\\..*" . chinese-big5)
		  ("^cn\\..*" . chinese-iso-8bit))
		gnus-group-name-charset-group-alist)))

(eval-after-load "gnus-sum"
  '(progn
     (setq gnus-newsgroup-ignored-charsets
	   '(unknown-8bit x-unknown))
     (define-coding-system-alias 'x-gbk 'gbk)
     (setq gnus-summary-show-article-charset-alist
	   (nconc '((1 . us-ascii)
		    (2 . iso-8859-1)
		    (3 . gb2312)
		    (4 . gbk)
		    (5 . gb18030)
		    (6 . big5)
		    (7 . big5-hkscs)
		    (8 . utf-8))
		  gnus-summary-show-article-charset-alist))
     (add-to-list 'gnus-newsgroup-variables
		  'mm-coding-system-priorities)
     (setq gnus-parameters
	   (nconc '((".*"
		     (posting-style
		      (name mi-message-user-full-name)
		      (address mi-message-user-mail-address)
		      (signature-file mi-message-signature-file)
		      (organization mi-message-header-organization))
		     (mm-coding-system-priorities
		      '(us-ascii iso-8859-1 gb2312 gbk gb18030 big5 big5-hkscs utf-8)))
		    ("^tw\\..*"
		     (charset . chinese-big5)
		     (posting-style
		      (name mi-message-user-full-name))
		     (mm-coding-system-priorities
		      '(us-ascii iso-8859-1 big5 big5-hkscs)))
		    ("^cn\\..*"
		     (charset . chinese-iso-8bit)
		     (posting-style
		      (name mi-message-user-full-name))
		     (mm-coding-system-priorities
		      '(us-ascii iso-8859-1 gb2312 gbk gb18030)))
		    ("^mailing\\..*"
		     (mm-coding-system-priorities
		      '(us-ascii iso-8859-1 utf-8)))
		    ("^nnml:.*"
		     (visible . t)
		     (mm-coding-system-priorities
		      '(us-ascii iso-8859-1 utf-8)))
		    ("^nndraft:.*"
		     (visible . t)))		    
		  gnus-parameters))))

;; visual appearance
;; summary buffer sorting
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
	(not gnus-thread-sort-by-number)))

;; frame resizing
(gnus-add-configuration '(article
			  (vertical 1.0
				    (summary .45 point)
				    (article 1.0))))

;; view html
(eval-after-load "mm-decode"
  '(setq mm-discouraged-alternatives
	 (nconc '("text/html" "text/richtext")
		mm-discouraged-alternatives)))

;; topic mode
(add-hook 'gnus-group-mode-hook
	  'gnus-topic-mode)

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
(setq gnus-group-line-format
      "%M%S%p%P%5y:%B%(%g%)%l %O\n"
      gnus-summary-line-format
      ":%U%R %B %s %-70=|%3L|%-20,20n|%&user-date; \n")

;; process ansi colors
;; `ansi-color-apply-on-region': in mi-ansi-color.el
(add-hook 'gnus-part-display-hook
	  '(lambda ()
	     (save-excursion
	       (when (article-goto-body)
		 (let ((inhibit-read-only t))
		   (ansi-color-apply-on-region (point)
					       (point-max)))))))

;; use rfc2047
(eval-after-load "rfc2047"
  '(progn
     (defalias 'mail-header-encode-parameter
       'rfc2047-encode-parameter)
     (add-to-list 'rfc2047-header-encoding-alist
		  '("Subject"))))

;; encoding priorities
(eval-after-load "mm-decode"
  '(setq mm-body-charset-encoding-alist
	 (nconc '((big5 . 8bit))
		'((gb2312 . 8bit))
		'((gbk . 8bit))
		'((gb18030 . 8bit))
		'((utf8 . base64))
		mm-body-charset-encoding-alist)))

;; decide encoding according group names
(defcustom mi-message-header-organization
  "Pluto The Planet"
  "My default organization name."
  :type 'string
  :group 'mi-gnus)
(defcustom mi-message-signature-file
  "~/.signature-rotated"
  "My default signature file name."
  :type 'string
  :group 'mi-gnus)
(defcustom mi-message-header-chinese-nickname
  "dhg"
  "My default Chinese nick name."
  :type 'string
  :group 'mi-gnus)

;; auto fill long lines
(add-hook 'message-mode-hook
          '(lambda ()
	     (setq fill-column 72)
	     (turn-on-auto-fill)))

;; auto article washing
(setq mm-text-html-renderer 'lynx)
;; generate all headers
(setq message-generate-headers-first t)

;; confirm sending mail/news
(defadvice message-send
  (around my-confirm-message-send)
  (if (yes-or-no-p "Really send message? ")
      ad-do-it))
(ad-activate 'message-send)

;; MFT things
(setq message-subscribed-regexps
      '(".*@\\(\\(opensuse\\)\\|\\(freebsd\\)\\|\\(opensolaris\\)\\|\\(openbsd\\)\\|\\(lists\\.freedesktop\\)\\|\\(lists\\.x\\)\\)\\.org.*"))

;; citation style
(defvar mi-message-safe-time-val nil
  "Nil if date string is invalid")

(defun mi-message-header-on-wrote ()
  "Similar to `sc-header-on-said', but using a shorter date string."
  (setq mi-message-safe-time-val
	(safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert (sc-hdr "\nOn "
			(format-time-string "%Y/%m/%d at %R" mi-message-safe-time-val) ", ")
		whofrom " wrote:\n"))))

(defun mi-message-header-on-wrote-cn ()
  "Similar to `mi-header-on-said', but using Chinese."
  (defvar mi-whofrom-id nil
    "The ID portion of the complete string of a BBS author.")
  (defvar mi-whofrom-nick nil
    "The nickname portion of the complete string of a BBS author")
  (setq mi-message-safe-time-val
	(safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(progn
	  (setq mi-whofrom-id
		(replace-regexp-in-string "\\(\\.bbs\\|@.*\\|.*[<]\\|[>].*\\)" "" whofrom)
		mi-whofrom-nick
		(replace-regexp-in-string "\\(.*\(\\|\).*\\|\"\\|[ ]*<.*>\\)" "" whofrom))
	  (if (and (equal mi-whofrom-id mi-whofrom-nick)
		   (= (string-width mi-whofrom-id)
		      (string-width mi-whofrom-nick)))
	      (insert (sc-hdr "\n【 在 "
			      (format-time-string "%Y年%m月%d日 %H点%M分%S秒" mi-message-safe-time-val)
			      ", ")
		      mi-whofrom-id
		      " 说道: 】\n")
	    (insert (sc-hdr "\n【 在 "
			    (format-time-string "%Y年%m月%d日 %H点%M分%S秒" mi-message-safe-time-val)
			    ", ")
		    mi-whofrom-id
		    " ("
		    mi-whofrom-nick
		    ") 说道: 】\n"))))))

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
	sc-citation-delimiter-regexp "\\([:word:]\\|[_.]\\)+:+"
	message-yank-prefix ": "
	message-yank-cited-prefix ":"
	message-yank-empty-prefix ":"
	message-cite-prefix-regexp "\\(\\([:word:]\\|[_.]\\)*:+\\|[ ]*[]:+|}]\\)+"))

(defvar mi-message-bbs-p nil
  "To see whether or not we are replying messages in a newsgroup")

(defun mi-message-citation-style ()
  "We are replying to a BBS"
  (interactive)
  (if (and (message-news-p)
	   (string-match ".*\\.[Bb][Bb][Ss]\\..*" (gnus-fetch-field "Xref")))
      (setq mi-message-bbs-p t))
  (save-excursion
    (if mi-message-bbs-p
	(mi-message-citation-style-bbs)
      (mi-message-citation-style-normal)))
  (setq mi-message-bbs-p nil))

;;  citation post hook
;; post functions
(defun mi-message-citation-post-hooks ()
  "My own post hooks for citation"
  (interactive)
  (mi-message-citation-delete-signature)
  (mi-message-header-subject-rewrite)
  (mi-message-citation-ready-to-compose))

(defvar mi-message-signature-current-line
  -1
  "Current working line within in signature region.")

(defun mi-message-citation-ready-to-compose ()
  "Do something after citation completes"
  (interactive)
  (goto-char (point-max))
  (while (search-backward-regexp "^\\([-]\\|[_]\\)+[ ]*$" (point-min) t)
    (setq mi-message-signature-current-line
	  (line-number-at-pos (point))))
  
  (if (< 0 mi-message-signature-current-line)
      (progn
	(beginning-of-line)
	(newline 3)
	(goto-char (point-min))
	(forward-line mi-message-signature-current-line)))
  (setq mi-message-signature-current-line
	-1))

(defvar mi-message-signature-region-start
  -1
  "Start position of a region.")
(defvar mi-message-signature-region-end
  -1
  "End position of a region.")

(defun mi-message-citation-delete-signature ()
  "Delete signatures in citation"
  (interactive)
  (progn
    (goto-char (point-min))
    (while (search-forward-regexp "^\\([ ]*[>|:][ ]*\\)+\\([-][-]+\\|[_][_]+\\)[ ]*$" (point-max) t)
      (setq mi-message-signature-region-start
	    (point))))
  
  (if (not (= -1 mi-message-signature-region-start))
      (progn
	(beginning-of-line)
	(setq mi-message-signature-region-start
	      (point))))
  
  (progn
    (goto-char (point-max))
    (while (search-backward-regexp "^[-][-]+[ ]*$" (point-min) t)
      (setq mi-message-signature-region-end
	    (point)))
    
    (if (and (< 0 mi-message-signature-region-start)
	     (< 0  mi-message-signature-region-end)
	     (>= mi-message-signature-region-start (point-min))
	     (<= mi-message-signature-region-end (point-max)))
	(progn
	  (delete-region mi-message-signature-region-start
			 mi-message-signature-region-end)
	  (insert (concat sc-citation-delimiter
			  " ................ \n"))
	  (setq mi-message-signature-region-start
		-1
		mi-message-signature-region-end
		-1)
	  (goto-char (point-max))
	  (search-backward-regexp "^[-][-]+[ ]*" (point-min) t)))))

;; citation funcion
;; blank lines should be cited, too
(setq sc-cite-blank-lines-p t
      ;; nesting citation
      sc-nested-citation-p t
      ;; don't fill citation
      sc-auto-fill-region-p nil)

(add-hook 'sc-pre-hook
	  'mi-message-citation-style)
(add-hook 'sc-post-hook
	  'mi-message-citation-post-hooks)
(add-hook 'mail-citation-hook
	  'sc-cite-original)

;; mi-message-header-on-wrote now follows `sc-no-header' in
;; `sc-rewrite-header-list'.
(setq sc-rewrite-header-list
      (nconc '((sc-no-header))
	     '((mi-message-header-on-wrote-cn))
	     '((mi-message-header-on-wrote))
	     (cdr sc-rewrite-header-list)))

;; misc
;;

;; mail header subjecr rewrite
(defvar mi-message-header-subject nil
  "Rewritten mail header subject, include it with double quotation markers.")
(defun mi-message-header-subject-rewrite ()
  "Rewrite mail header subject, include it with double quotation markers."
  (setq mi-message-header-subject
	(gnus-fetch-field "Subject"))
  (if (and (not (null mi-message-header-subject))
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
	      (concat "Subject: \""
		      mi-message-header-subject
		      "\""))
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
(setq nnmail-split-methods
      '(("mail.openbsd.misc"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*misc@\\(\\(cvs\\.openbsd\\)\\|\\(openbsd\\)\\).org.*")
	("mail.openbsd.tech"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*tech@openbsd\\.org.*")
	("mail.freebsd.security"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\(freebsd-security\\)\\|\\(security-advisories\\)\\)@freebsd\\.org.*")
	("mail.freebsd.bugs"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*bsd-bugs@freebsd\\.org.*")
	("mail.freebsd.fs"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\(bsd-fs\\)\\|\\(fs\\)\\).*@freebsd\\.org.*")
	("mail.freebsd.gnats"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*gnats.*@freebsd\\.org.*")
	("mail.freebsd.amd64"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*amd64@freebsd\\.org.*")
	("mail.freebsd.x11"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*x11@freebsd\\.org.*")
	("mail.freebsd.multimedia"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*multimedia@freebsd\\.org.*")
	("mail.freebsd.hardware"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*hardware@freebsd\\.org.*")
	("mail.freebsd.gnome"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*gnome@freebsd\\.org.*")
	("mail.freebsd.cvs-all"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\(svn-src-all\\)\\|\\(cvs-all\\)\\)@freebsd\\.org.*")
	("mail.freebsd.cvs-doc"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*cvs-doc@freebsd\\.org.*")
	("mail.freebsd.cvs-ports"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*cvs-ports@freebsd\\.org.*")
	("mail.freebsd.cvs-src"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\(svn\\)\\|\\(cvs\\)\\)-src@freebsd\\.org.*")
	("mail.freebsd.ports"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(\\(bsd-ports\\)\\|\\([^-]ports\\)\\)@freebsd\\.org.*")
	("mail.freebsd.ports-bugs"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*ports-bugs@freebsd\\.org.*")
	("mail.freebsd.bug-followup"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*bugs-followup@freebsd\\.org.*")
	("mail.freebsd.stable"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*\\(freebsd-\\)*stable@freebsd\\.org.*")
	("mail.freebsd.hackers"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*hacker.*@freebsd\\.org.*")
	("mail.freebsd.current"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*current.*@freebsd\\.org.*")
	("mail.freedesktop.xorg"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*xorg@\\(\\(lists\\.freedesktop\\)\\|\\(freedesktop\\)\\|\\(bugs\\.debian\\)\\)\\.org.*")
	("mail.opensolaris.amd64"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*amd-platform-.*@opensolaris\\.org.*")
	("mail.opensolaris.desktop"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*desktop-.*@opensolaris\\.org.*")
	("mail.opensolaris.sysadmin"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*sysadmin-.*@opensolaris\\.org.*")
	("mail.newsletters"
	 "^From:\\(\\(.*\\)\\|\\(.*no-reply.*\\)\\)@.*\\(\\(noreply\\)\\|\\(mail\\.communications\\.sun\\)\\|\\(communications2\\)\\|\\(geocaching\\)\\|\\(apple\\)\\|\\(economist\\)\\|\\(freesoftwaremagazine\\)\\|\\(nytimes\\)\\|\\(phoronix\\)\\|\\(slashdot\\)\\|\\(osnews\\)\\|\\(yeeyan\\)\\|\\(ziki\\)\\|\\(sourceforge\\)\\|\\(myfonts\\)\\|\\(oracle-mail\\)\\)\\.\\(\\(org\\)\\|\\(com\\)\\|\\(net\\)\\).*")
	("mail.x11.ati"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):\\(\\(.*xorg-driver-ati.*\\)\\|\\(.*\\)\\)@\\(\\(lists\\.x\\)\\|\\(bugs\\.debian\\)\\)\\.org.*")
	("mail.x11.radeonhd"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\|\\(Subject\\)\\):.*radeonhd.*@opensuse\\.org.*")
	("mail.x11.nouveau"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*nouveau.*@lists\\.freedesktop\\.org.*")
	("mail.x11.kde"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*@kde\\.org.*")
	("mail.x11.gnome-cn"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*gnome-cn-list@gnome\\.org.*")
	("mail.local"
	 "^\\(\\(From\\)\\|\\(To\\)\\|\\(Cc\\)\\):.*@\\(\\(sol\\)\\|\\(pluton\\)\\|\\(mars\\)\\)\\.xbsd\\.name$")
	("mail.old"
	 "^From:.*\\(\\(kapo\\)\\|\\(stoneboy\\)\\)@.*")
	("mail.misc" "")))

;; gnus daemon
(gnus-demon-init)
(gnus-demon-add-handler 'gnus-demon-scan-mail 10 t)
(gnus-demon-add-handler 'gnus-demon-scan-news 10 t)

;; my own gnus defun's
(defcustom mi-gnus-default-article-number
  200
  "Default number of articles to fetch in Gnus"
  :type 'integer
  :group 'mi-gnus)

(global-set-key "\C-cg" nil)
(global-set-key "\C-cgg" 'gnus)
(global-set-key "\C-cgf" '(lambda ()
			    (interactive)
			    (gnus-group-select-group mi-gnus-default-article-number)))
