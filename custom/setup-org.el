;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode stuff

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(require 'org)
(add-to-list 'org-modules 'org-mac-iCal)

;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; org mode key bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

;; org-mode agenda files

(setq org-agenda-files (quote (
                               "~/dev/org/jsolutions"
                               "~/dev/org/blog")))

(setq org-default-notes-file (concat "~/dev/org/notes.org"))

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))

(setq org-agenda-custom-commands
      '(
        ("1" "Priority A"
         ((agenda "" ((org-agenda-ndays 1)))
          (tags-todo "+PRIORITY=\"A\"")))
        ("2" "Priority B"
         ((agenda "" ((org-agenda-ndays 1)))
          (tags-todo "+PRIORITY=\"B\"")))
        ("3" "Priority C"
         ((agenda "" ((org-agenda-ndays 1)))
          (tags-todo "+PRIORITY=\"C\"")))
    ("4" "Priority D"
         ((agenda "" ((org-agenda-ndays 1)))
          (tags-todo "+PRIORITY=\"D\"")))
    ("5" "Priority E"
         ((agenda "" ((org-agenda-ndays 1)))
          (tags-todo "+PRIORITY=\"E\"")))
        )

)

(add-hook 'org-mode-hook
      (lambda ()
        (setq company-mode nil)))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 1)
                                 (org-agenda-files :maxlevel . 1))))


;; Set to the location of your Org files on your local system
(setq org-directory "~/dev/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/dev/org/notes.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; turn on spell checking for org mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; fontify code blocks
(setq org-src-fontify-natively t)

(require 'ox-latex)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf %f"))

(setq org-latex-create-formula-image-program 'imagemagick)

(setq org-pretty-entities nil)

(defun org-clocktable-indent-string (level)
  (if (= level 1) ""
    (let ((str ""))
      (dotimes (k (1- level) str)
    (setq str (concat "……" str))))))

;; code execution
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (C . t)
   (js . t)
   (octave . t)
   (sh . t)
   (gnuplot .t)))

;;; Extra calendar stuff

(setq org-agenda-include-diary t)


;;; Quick functions to get to home list

(defun my-notes ()
  (interactive)
  (make-frame '((name . "NOTES")))
  (set-background-color "gray13")
  (switch-to-buffer (find-file "~/dev/org/jsolutions/home.org"))
  (org-mode))


;; show images
(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images nil nil))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)

;; show and hide code blocks
(defvar org-blocks-hidden nil)

(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

(add-hook 'org-mode-hook 'org-toggle-blocks)

(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

(provide 'setup-org)
