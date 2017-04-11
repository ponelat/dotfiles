
(setq package-archives
      (quote
       (("gnu"          . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa"        . "http://melpa.org/packages/")
	("marmalade"    . "http://marmalade-repo.org/packages/"))))

(require 'package)
(package-initialize)

;;;; init.el helpers
(defmacro comment (&rest body)
  "Comment out sexp"
  nil)

(use-package macrostep
  :ensure t)

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

;;;; Term, bash, zsh
(defun ponelat/term ()
  "Create or jump to an ansi-term, running zsh"
  (interactive)
  (if (get-buffer "*terminal*")
      (switch-to-buffer "*terminal*")
      (ansi-term "/bin/zsh" "terminal")))

(global-set-key (kbd "M-z") #'ponelat/term)

;;;; Dirs
(setq today-dir "~/Dropbox/org")

;;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;;; General, editor, config
(windmove-default-keybindings)
(auto-image-file-mode 1)
(setq vc-follow-symlinks t)

(comment use-package dot-mode
  :ensure t
  :config
  (global-dot-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;;; Markdown
(use-package markdown-mode
  :ensure t)

(use-package evil
   :ensure t
   :config (evil-mode))

(use-package evil-replace-with-register
  :ensure t
  :config
  (progn
    (setq evil-replace-with-register-key "gr")
    (evil-replace-with-register-install)))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

;;;; Global Bindings, keys
(bind-key "C-x C-k" 'kill-this-buffer)

;;;; Lisp, paredit
(show-paren-mode 1)

(comment use-package paredit
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'json-mode-hook 'enable-paredit-mode)
)

(comment use-package lispy
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'lispy-mode)
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'lisp-mode-hook 'lispy-mode)
  (define-key lispy-mode-map (kbd "M-n") nil))
  
;;(setq show-paren-style 'expression)

;;;; Ag
;; use the_silver_searcher when available
(use-package ag
  :ensure t
  :if (executable-find "ag"))


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
(unless window-system
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

;; (load-theme 'wombat t)

;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;; 	       (load-theme 'ample-flat t t)
;; 	       (load-theme 'ample-light t t)
;; 	       (enable-theme 'ample))
;;   :defer t
;;   :ensure t)

(if window-system
  (use-package sublime-themes
      :ensure t
      :config
      (load-theme 'graham t))
  (use-package badwolf-theme
    :ensure t
    :config (load-theme 'badwolf t)))


;;;; Autosave
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;; Javascript
(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-jsx-mode))
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-mode-in)))

(use-package rjsx-mode
  :ensure t)

;;;; Flycheck, syntax, lint
(use-package flycheck
  :ensure t
  :config
  (progn
    (global-flycheck-mode)
    (set-face-attribute 'flycheck-warning nil
			:foreground "black"
			:background "yellow")
    (set-face-attribute 'flycheck-error nil
			:foreground "black"
			:background "pink")))

(use-package pos-tip
  :ensure t)
  
(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;; From http://www.cyrusinnovation.com/initial-emacs-setup-for-reactreactnative/

;; (defun ponelat/setup-local-eslint ()
;;     "If ESLint found in node_modules directory - use that for flycheck.
;; Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
;;     (interactive)
;;     (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
;;       (setq flycheck-javascript-eslint-executable
;;             (and (file-exists-p local-eslint) local-eslint))))

;; So that we can access `./node_modules/.bin/eslint` mostly
(use-package add-node-modules-path
  :ensure t
  :init
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path))) 
    (eval-after-load 'js2-jsx-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path))) 

;;;; Jq
(use-package jq-mode
  :ensure t)

;;;; Clojure
(use-package cider
  :ensure t
  :config
  (progn 
    (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))
  (global-company-mode))

(use-package clojure-mode
  :ensure t
  :config
 ;  (setq clojure-defun-style-default-indent t)
  )

;;;; Autocomplete, company
(use-package company
  :bind (("TAB"  . company-indent-or-complete-common))
  :ensure t
  :config
  (setq company-idle-delay 0.1))

;;;; Projects

(use-package projectile
  :ensure t
  :config
  (projectile-mode))


;;;; Fuzzy, ido, helm
(use-package helm
  :ensure t
  :config
  (helm-mode))

(use-package helm-projectile
  :bind (("M-x" . helm-M-x))
  :ensure t
  :defer 2
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure t)
  
(use-package evil-leader
  :ensure t
  :config
  (progn 
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "q" 'kill-buffer-and-window
      "Q" 'save-buffers-kill-terminal
      "p" 'helm-projectile-switch-project
      "j" 'helm-projectile-find-file
      "a" 'helm-ag-project-root
      "w" 'save-buffer
      )))

;; (use-package helm-flx
;;   :ensure t
;;   :config
;;   (helm-flx-mode))

(use-package helm-fuzzier
  :ensure t
  :config
  (helm-fuzzier-mode 1))

;; (use-package ido
;;   :ensure t
;;   :config
;;   (ido-mode)
;;   :pin melpa)

;; (use-package flx-ido
;;   :ensure t)

;;;; Git, magit
(use-package magit
  :bind (("C-c g" . magit-status))
  :ensure t)

;;;; Ledger
(use-package ledger-mode
  :ensure t)


;;;; Org-mode
(use-package org
  :ensure t
  :bind (("M-n" . org-capture)
	 ("C-c C-a" . org-agenda))
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file "notes.org")
  (setq org-capture-templates
	'(("t" "Todo" entry (file org-default-notes-file)
	   "* TODO %?\n  SCHEDULED: %t\n  %i\n  %a")
	  ("d" "Today" entry (file org-default-notes-file)
	   "* TODO %?\n  SCHEDULED: %t\n  %i\n  %a")
	  ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
	   "** %? \nEntered on %U\n %i\n %a"))))



(setq org-agenda-files (list today-dir))

(use-package org-projectile
  :bind (("M-p" . org-projectile:project-todo-completing-read))
  :config
  (progn
    (setq org-projectile:projects-file 
          (concat today-dir "/projects.org"))
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))
  :ensure t)
	 

;;;; Run current file

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2017-02-10"
  (interactive)
  (let (
        (-suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("js" . "node") ; node.js
           ("ts" . "tsc --alwaysStrict --lib DOM,ES2015,DOM.Iterable,ScriptHost --target ES5") ; TypeScript
           ("sh" . "bash")
           ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        -fname
        -fSuffix
        -prog-name
        -cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))
    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (shell-command -cmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (shell-command -cmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/notes.org")))
 '(org-export-backends (quote (ascii html icalendar latex md deck))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
