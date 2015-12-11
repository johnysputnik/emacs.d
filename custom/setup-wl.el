;;; wanderlust config

(autoload 'wl' "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;; IMAP

(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "john@jsolutions.co.uk")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq wl-from "John Cumming <john@jsolutions.co.uk>")

;; SMTP

(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "john@jsolutions.co.uk")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq smtp-local-domain "jsolutions.co.uk")

;; Folders

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder ".drafts")
(setq wl-trash-folder "%[Gmail]/Trash")
(setq wl-fcc "%[Gmail]/Sent Mail")

(setq wl-fcc-force-as-read t)

(setq wl-folder-check-async t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))

(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; more customisation

(setq wl-summary-line-format "%T%P %D/%M (%W) %h:%m %t$[%25(%c %f%) %] %s")
(setq wl-summary-width 150)

(setq wl-message-ignored-field-list '("^.*:"))
(setq wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^Message-Id:"
    "^User-Agent:"
    "^\\(Posted\\|Date\\):"))
(setq wl-message-sort-field-list
      '("^From"
    "^Organization"
    "^X-Attribution"
    "^Subject"
    "^Date"
    "^To"
    "Cc"))

;; BBDB stuff

(setq bbdb-file "~/.emacs.d/bbdb")
(require 'bbdb)
(bbdb-initialize)

(require 'bbdbV3-wl)

(define-key wl-draft-mode-map (kbd "<C-tab>") 'bbdb-complete-name)

(setq mm-w3m-safe-url-regexp nil)

(provide 'setup-wl)
