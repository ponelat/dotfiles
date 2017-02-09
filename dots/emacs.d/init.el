(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;;; Clipboard

;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
p(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
	(insert text)
	;; I prefer using the "clipboard" selection (the one the
	;; typically is used by c-c/c-v) before the primary selection
	;; (that uses mouse-select/middle-button-click)
	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	(unless (string= (car kill-ring) xsel-output)
	  xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))


;;;; Themes
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
	       (load-theme 'ample-flat t t)
	       (load-theme 'ample-light t t)
	       (enable-theme 'ample))
  :defer t
  :ensure t)

;;;; Autosave
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;; Clojure
(use-package cider
  :ensure t
  :pin melpa)

;;;; Autocomplete, ido
(use-package ido
  :ensure t
  :config
  (ido-mode)
  :pin melpa)

;;;; Projects
(use-package projectile
  :ensure t
  :config
  (projectile-mode))

;;;; Git
(use-package magit
  :ensure t)


;; ;;;; Get package function
;; ;;; from purcell/emacs.d
;; (defun require-package (package &optional min-version no-refresh)
;;   "Install given PACKAGE, optionally requiring MIN-VERSION.
;; If NO-REFRESH is non-nil, the available package lists will not be
;; re-downloaded in order to locate PACKAGE."
;;   (if (package-installed-p package min-version)
;;       t
;;     (if (or (assoc package package-archive-contents) no-refresh)
;;         (package-install package)
;;       (progn
;;         (package-refresh-contents)
;;         (require-package package min-version t)))))

;; ;;;; General Functions
;; (defun what-face (pos)
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;;                   (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; ;;;; Terminal
;; (defun zsh ()
;;   "Start a terminal and rename buffer."
;;   (interactive)
;;   (term "/usr/local/bin/zsh"))

;; (global-set-key (kbd "M-z") 'zsh)
;; (defun my-term-hooks ()
;;   (setq show-trailing-whitespace nil)
;;   (linum-mode 0))
;; (add-hook 'term-mode-hook 'my-term-hooks)

;; ;;;; Autocomplete
;; (require-package 'auto-complete)
;; (ac-config-default)


;; ;;;; Snippets
;; (require-package 'yasnippet)

;; ;;;; Generic Key Bindings
;; (global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; ;;;; Ledger
;; (require-package 'ledger-mode)

;; ;;;; Org mode
;; (require-package 'org)
;; (require-package 'evil-org)


;; ;;;; Clojure
;; ;(require-package 'clojure)
;; ;(require-package 'parinfer-mode)

		
;; ;;;; General Modes
;; (require-package 'json-mode)
;; (require-package 'yaml-mode)

;; ;;;; Editorconfig
;; (require-package 'editorconfig)
;; (defun my-editor-config-hooks (hash)
;;   (setq web-mode-block-padding 0))
  
;; (add-hook 'editorconfig-custom-hooks 'my-editor-config-hooks)

;; (editorconfig-mode 1)

;; ;;;; Javascript
;; (require-package 'npm-mode)

;; (npm-global-mode)


;; ;;;; Web
;; (require-package 'emmet-mode)
;; (require-package 'web-mode)
;; (require-package 'less-css-mode)

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode. Adjust indents"
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)

;; ;; Don't auto-quote attribute values
;;   (setq-local web-mode-enable-auto-quoting nil)
;;   (setq web-mode-code-indent-offset 2))

;; (add-hook 'web-mode-hook  'emmet-mode)
;; (add-hook 'web-mode-hook  'npm-mode)
;; (add-hook 'web-mode-hook  'my-web-mode-hook)

;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; ;; Add better syntax highlighting of jsx
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;     (let ((web-mode-enable-part-face nil))
;;       ad-do-it)
;;     ad-do-it))


;; ;;;; Flycheck
;; ; (require 'flycheck)
;; ; (require-package 'flycheck)
;; ; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (comment defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; ;(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; ;(flycheck-add-mode 'javascript-eslint 'web-mode)
;; ;(setq-default flycheck-temp-prefix ".flycheck")
;; (comment setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;;     '(javascript-jshint)))

;; ;;;; Evil
;; (require-package 'evil)
;; (require-package 'evil-surround)
;; (require-package 'evil-commentary)
;; (require-package 'evil-leader)

;; (setq evil-search-module 'evil-search
;;       evil-want-C-u-scroll t
;;       evil-want-C-w-in-emacs-state t)

;; (global-evil-leader-mode)

;; (defun my-evil-hooks ()
;;   (evil-set-initial-state 'term-mode 'emacs)
;;   (evil-leader/set-leader "<SPC>" )
;;   (evil-leader/set-key
;;     "e" 'eval-buffer
;;     "j" 'helm-projectile
;;     "s" 'helm-projectile-switch-project
;;     "b" 'helm-projectile-switch-to-buffer
;;     "n" 'linum-mode
;;     "g" 'magit-status
;;     "q" 'kill-this-buffer
;;     "a" 'helm-projectile-ag
;;     "w" 'save-buffer)
;;   )

  
;; (add-hook 'evil-mode-hook 'my-evil-hooks)

;; (add-hook 'evil-mode-hook 'evil-surround-mode)
;; (add-hook 'evil-mode-hook 'evil-commentary-mode)

;; (evil-mode t)

;; ;;;; exec-path ( for ag )
;; (require-package 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

;; ;;;; Backup files
;; (defconst emacs-tmp-dir (format "%s%s" default-directory "tmp"))
;; (setq make-backup-files nil)
;; (setq backup-directory-alist
;;       `((".*" . ,emacs-tmp-dir)))

;; ;;;; Autosave
;; ;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
;; (setq auto-save-file-name-transforms
;;       `((".*" ,emacs-tmp-dir t)))
;; (setq auto-save-list-file-prefix
;;       emacs-tmp-dir)


;; ;;;; Config management
;; (defun imenu-elisp-sections ()
;;   (setq imenu-prev-index-position-function nil)
;;   (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))
 
;; (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


;; (defun init-imenu (p)
;;   (interactive "P")
;;   (find-file-existing "~/.emacs.d/init.el")
;;   (widen)
;;   (helm-imenu)
;;   (if p (init-narrow-to-section)))

;; (defun init-narrow-to-section ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (unless (looking-at "^;;;;")
;;       (re-search-backward "^;;;;" nil t))
;;     (push-mark)
;;     (next-line)
;;     (re-search-forward "^;;;;" nil t)
;;     (previous-line)
;;     (narrow-to-region (region-beginning) (region-end))))

;; (global-set-key (kbd "M-i") 'init-imenu)
;; (global-set-key (kbd "M-S-I") 'init-narrow-to-section)


;; ;;;; Helm
;; (require-package 'helm)
;; (require-package 'ac-helm)
;; (require-package 'helm-ag)
;; (require-package 'helm-projectile)
;; (setq helm-mode-fuzzy-match t)
;; (setq helm-M-x-fuzzy-match t)

;; (global-set-key (kbd "M-x") 'helm-M-x) 
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) 

;; (require 'helm-config)
;; (helm-mode 1)


;; ;;;; Projectile
;; (require-package 'projectile)
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)


;; ;;;; Magit
;; (require-package 'magit)
;; (require-package 'magit-gh-pulls)
;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;; (global-set-key (kbd "C-x g") 'magit-status)

;; ;;;; Git gutter
;; (require-package 'git-gutter)
;; (require-package 'git-gutter-fringe+)
;; (global-git-gutter+-mode +1)
;; ;; (global-git-gutter-mode +1)
;; ;; (add-hook 'linum-mode-hook 'git-gutter:linum-setup)




(provide 'init)
;;; init.el ends here
