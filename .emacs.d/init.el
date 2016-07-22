(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;;;; Terminal
(defun zsh ()
  "Start a terminal and rename buffer."
  (interactive)
  (term "/usr/local//bin/zsh"))

(global-set-key (kbd "M-z") 'zsh)

(add-hook 'term-mode-hook (lambda ()
			    (setq show-trailing-whitespace nil)
			    (linum-mode 0)))


;;;; Ledger
(require-package 'ledger-mode)

;;;; General Modes
(require-package 'yaml-mode)
(require-package 'json-mode)
(require-package 'web-mode)

;;;; Evil
(require-package 'evil)
(require-package 'evil-surround)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(evil-mode t)

;;;; exec-path ( for ag )
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;;; Backup files
(setq make-backup-files nil)


;;;; Config management
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
 
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(defun init-imenu (p)
  (interactive "P")
  (find-file-existing "~/.emacs.d/init.el")
  (widen)
  (helm-imenu)
  (if p (init-narrow-to-section)))


(global-set-key (kbd "M-i") 'init-imenu)


;;;; Helm
(require-package 'helm)
(require-package 'ac-helm)
(require-package 'helm-ag)
(require-package 'helm-projectile)
(setq helm-mode-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x) 
(global-set-key (kbd "C-x C-f") 'helm-find-files) 

(require 'helm-config)
(helm-mode 1)


;;;; Projectile
(require-package 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)


;;;; Magit
(require-package 'magit)
(require-package 'magit-gh-pulls)
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)


;;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)


;;;; Swagger
(defun swaggervpn ()
  (interactive)
  (start-process "SwaggerVPN" "swaggervpn" "~/bin/swaggervpn")
  )


(defun swaggerhub ()
  (interactive)
  (start-process "SwaggerHubFrontend" "swaggerhub" "~/projects/swaggerhub-frontend/.bin/start")
  )


;;;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a164837cd2821475e1099911f356ed0d7bd730f13fa36907895f96a719e5ac3e" default))))
;;;; Custom faces ( fonts )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "misc" :family "Droid Sans Mono"))))
 '(term-color-white ((t (:background "dim gray" :foreground "dim gray")))))
