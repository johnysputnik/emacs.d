
;;; paths

(setenv "PATH" (concat ":~/bin:/usr/local/bin:/Library/TeX/texbin" (getenv "PATH")))
(setq exec-path (append exec-path '("~/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

;;; packages

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(auctex
    magit
    flycheck
    flycheck-clojure
    flycheck-pos-tip
    anzu
    sauron
    mu4e-maildirs-extension
    mu4e-alert
    helm-mu
    company
    clojure-mode
    cider
    color-theme
    sublime-themes
    color-theme-solarized
    bbdb
    desktop
    slime
    slime-company
    common-lisp-snippets
    powerline
    gnuplot
    gnuplot-mode
    org-mac-iCal
    rainbow-mode
    tronesque-theme
    wanderlust
    weather-metno
    moe-theme
    duplicate-thing
    ggtags
    helm
    helm-company
    helm-gtags
    helm-projectile
    helm-swoop
    htmlize
    ;; function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    twittering-mode
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/dev/org/blog/elisp")


(require 'setup-helm)
(require 'setup-helm-gtags)
;;(require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)
;;(require 'setup-wl)
(require 'setup-mu4e)
(require 'setup-org)
(require 'setup-slime)

(require 'jsolutions-web)

(windmove-default-keybindings)

;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; (delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
;; (add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)


;;; My customisation

(tool-bar-mode -1)
(global-linum-mode)
;; (desktop-save-mode 1)
(electric-pair-mode 1)
(scroll-bar-mode -1)

;;; Some weather stuff
(setq weather-metno-location-name "Gloucester, UK"
      weather-metno-location-latitude 51.87
      weather-metno-location-longitude 2.24)

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "c7fb35ba0e1e7f2e4b48ba1508ce5ee309192c6e5e671dba296dc259844426e6" "3625c04fa4b8a802e96922d2db3f48c9cb2f93526e1dc24ba0b400e4ee4ccd8a" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" default)))
 '(tool-bar-mode nil))

;;; THEME

(require 'powerline)
;;(require 'moe-theme)
;; (powerline-moe-theme)
;; (load-theme 'moe-dark)

(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-solarized)
(customize-set-variable 'frame-background-mode 'dark)
(load-theme 'solarized t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar lunaryorn-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name) " ")))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'lunaryorn-projectile-mode-line 'risky-local-variable t)

(defvar lunaryorn-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'lunaryorn-vc-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                ;; mode-line-mule-info
                ;;mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                ;; Some specific information about the current buffer:
                lunaryorn-projectile-mode-line ; Project information
                (vc-mode lunaryorn-vc-mode-line) ; VC information
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; Misc information, notably battery state and function name
                " "
                mode-line-misc-info))
                ;; And the modes, which I don't really care for anyway
                ;; " " mode-line-modes mode-line-end-spaces))


(setq locate-command "mdfind")

(require 'fortune)
(setq fortune-dir "/usr/local/share/games/fortunes"
      fortune-file "/usr/local/share/games/fortunes/fortunes")


;; Sauron

(setq sauron-modules '(sauron-erc sauron-org sauron-twittering))
(setq sauron-watch-patterns
      '("pdf" "page" "text"))


;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; ERC

(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-keywords '("\\bpdf\\b"))




(eval-after-load 'flycheck '(flycheck-clojure-setup))
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
