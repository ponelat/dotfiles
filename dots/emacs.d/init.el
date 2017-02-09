(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;;; init.el helpers
(defmacro comment (&rest body)
  "Comment out sexp"
  nil)


;;;; Dirs
(setq today-dir "~/projects/today")


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

(use-package flx-ido
  :ensure t)
  
(use-package auto-complete-config
  :ensure auto-complete
  :init
  (ac-config-default))

;;;; Projects

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

;;;; Git
(use-package magit
  :ensure t)

;;;; Org-mode
(setq org-default-notes-file (concat today-dir "/notes.org"))
(define-key global-map "\M-n" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Todos")
	 "** TODO %?\n %i\n %a")
	("n" "Notes" entry (file+headline org-default-notes-file "Notes")
	 "** %? \nEntered on %U\n %i\n %a")))
	 
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

(defun init-narrow-to-section ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^;;;;")
      (re-search-backward "^;;;;" nil t))
    (push-mark)
    (next-line)
    (re-search-forward "^;;;;" nil t)
    (previous-line)
    (narrow-to-region (region-beginning) (region-end))))

(global-set-key (kbd "M-i") 'init-imenu)
(global-set-key (kbd "M-S-I") 'init-narrow-to-section)


(provide 'init)
;;; init.el ends here
