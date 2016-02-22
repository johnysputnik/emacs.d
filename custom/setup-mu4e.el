(require 'mu4e)
(require 'sauron)

;; default
;; (setq mu4e-maildir "~/.mail/jsolutions")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Trash"       . ?t)
       ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
;;(setq mu4e-get-mail-command "offlineimap -q")

;; something about ourselves
(setq
   user-mail-address "john@jsolutions.co.uk"
   user-full-name  "John Cumming")

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "john@jsolutions.co.uk" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; This messes with org mode status
;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(defun jc-mu4e-read-maildir (prompt maildirs predicate requre-match initial-input)
  (helm-comp-read prompt maildirs
                  :name prompt
                  :must-match t))
(setq mu4e-completing-read-function 'jc-mu4e-read-maildir)


(setq mu4e-view-show-images t)
(setq mu4e-html2text-command "w3m -dump -T text/html")
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-update-interval 60)
(setq message-kill-buffer-on-exit t)
(setq mu4e-hide-index-messages t)

(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(add-to-list 'mu4e-header-info-custom
             '(:recipnum .
                         ( :name "Number of recipients"  ;; long name, as seen in the message-view
                                 :shortname "Recip#"           ;; short name, as seen in the headers view
                                 :help "Number of recipients for this message" ;; tooltip
                                 :function
                                 (lambda (msg)
                                   (format "%d"
                                           (+ (length (mu4e-message-field msg :to))
                                              (length (mu4e-message-field msg :cc))))))))

(add-to-list 'mu4e-marks
             '(tag
               :char       "g"
               :prompt     "gtag"
               :ask-target (lambda () (read-string "What tag do you want to add?"))
               :action      (lambda (docid msg target)
                              (mu4e-action-retag-message msg (concat "+" target)))))

(setq mu4e-headers-fields
      '((:human-date . 10)
        (:flags . 6)
        (:tags . 10)
        (:recipnum . 5)
        (:from . 22)
        (:subject . nil)))

(provide 'setup-mu4e)
