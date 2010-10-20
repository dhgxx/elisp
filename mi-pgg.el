;; mi-pgg.el

(require 'pgg)

(setq
 pgg-default-scheme 'gpg
 pgg-scheme 'gpg
 pgg-cache-passphrase t
 pgg-passphrase-cache-expiry 3600
 pgg-default-user-id "tamgya@gmail.com"
 pgg-gpg-user-id "tamgya@gmail.com")

(global-set-key "\M-[" nil)
(global-set-key "\M-[i" 'pgg-insert-key)
(global-set-key "\M-[s" 'pgg-sign)
(global-set-key "\M-[t" 'pgg-sign-region)
(global-set-key "\M-[v" 'pgg-verify)
(global-set-key "\M-[w" 'pgg-verify-region)
