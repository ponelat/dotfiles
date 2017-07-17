;;; init.el --- Just my dot file.
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq package-archives
  (quote (("gnu"          . "http://elpa.gnu.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("melpa"        . "http://melpa.org/packages/")
           ("marmalade"    . "http://marmalade-repo.org/packages/"))))

(require 'package)
(package-initialize)

;; Allows use to create closures for functions ( in my case, for sentinel callbacks )
(setq lexical-binding t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; init.el helpers
(defmacro comment (&rest body)
  "Comment out sexp (BODY)."
  nil)

;;;; Macrostep
(use-package macrostep
  :ensure t)

;;;; Config management
(defun imenu-elisp-sections ()
  "Create a list of sections from config file."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("use" "^ *( *use-package *\\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(defun ponelat/emacs-lisp-imenu-init (p)
  "Jump to section in init.el file.  Or straight to P."
  (interactive "P")
  (find-file-existing "~/.emacs.d/init.el")
  (widen)
  (helm-imenu)
  (if p (init-narrow-to-section)))

(defun init-imenu (p)
  "Jump to section in init.el file.  Or straight to P."
  (interactive "P")
  (find-file-existing "~/.emacs.d/init.el")
  (widen)
  (helm-imenu)
  (if p (init-narrow-to-section)))

(defun init-narrow-to-section ()
  "Narrow to section within config file."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^;;;;")
      (re-search-backward "^;;;;" nil t))
    (push-mark)
    (forward-line)
    (re-search-forward "^;;;;" nil t)
    (forward-line -1)
    (narrow-to-region (region-beginning) (region-end))))

(global-set-key (kbd "C-c l e") 'ponelat/emacs-lisp-imenu-init)
(global-set-key (kbd "C-c l o") 'helm-org-agenda-files-headings)
(define-key emacs-lisp-mode-map (kbd "C-c n") 'init-narrow-to-section)
(define-key emacs-lisp-mode-map (kbd "C-c w") 'widen)

;;;; Term, bash, zsh
(defun ponelat/term ()
  "Create or jump to an 'ansi-term', running zsh."
  (interactive)
  (if (get-buffer "*terminal*")
      (switch-to-buffer "*terminal*")
    (ansi-term "/bin/zsh" "terminal")))

(use-package shell-pop
  :init
  (progn
    (setq shell-pop-default-directory "~/projects")
    (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
    (setq shell-pop-term-shell "/bin/zsh")
    (setq shell-pop-universal-key "C-t")
    (setq shell-pop-window-size 30)
    (setq shell-pop-full-span t)
    (setq shell-pop-window-position "bottom"))
  :ensure t)

(global-set-key (kbd "M-C-z") #'projectile-run-async-shell-command-in-root)
(global-set-key (kbd "M-z") #'ponelat/term)

;;;; Dirs
(defvar ponelat/today-dir "~/Dropbox/org")

;;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq-default truncate-lines t)

;;;; General, editor, config
(defun browse-url-chrome-unstable (url &optional new-window)
  "Open URL in Chrome unstable, possibly in NEW-WINDOW."
  (shell-command (concat "google-chrome-unstable" " " "\"" url "\"")))
(setq x-selection-timeout 300)

(windmove-default-keybindings)
(auto-image-file-mode 1)
(setq vc-follow-symlinks t)
(setq browse-url-browser-function #'browse-url-chrome-unstable)
(electric-pair-mode t)

;; (set-face-attribute 'default t :font "isoveska-13" )

(use-package avy
  :config
  (setq avy-timeout-seconds 0.4)
  :ensure t)

;;;; Strings
(use-package string-inflection
  :ensure t)

;;;; Hydra, menus
(use-package hydra
  :ensure t)

(defhydra hydra-zoom (global-map "C-x =")
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out"))

(defhydra hydra-string-case (global-map "C-c C-s")
  "string case"
  ("c" string-inflection-all-cycle "all cycle"))

(use-package diminish
  :ensure t)

(use-package ace-window
  :disabled
  :bind (("M-p" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  :ensure t)

(use-package ranger
  :disabled t
  :ensure t)

(comment use-package dot-mode
  :ensure t
  :config
  (global-dot-mode))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;;; Autoindent
(use-package auto-indent-mode
  :config
  (add-hook 'rjsx-mode 'auto-indent-mode)
  :ensure t)

;;;; Markdown

(use-package markdown-mode
  :ensure t)

;;;; Evil, vim

(use-package evil-leader
  :ensure t
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
    (setq evil-motion-state-modes nil)
    (evil-leader/set-key
      "q" #'kill-buffer-and-window
      "Q" #'save-buffers-kill-terminal
      "p" #'helm-projectile-switch-project
      "j" #'helm-browse-project
      "a" #'helm-do-ag-project-root
      "b" #'helm-buffers-list
      "w" #'save-buffer
      "s" #'avy-goto-char-timer
      "k" #'avy-goto-char-2
      ":" #'delete-other-windows
      "l" #'find-library
      "i" #'helm-imenu)))

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (define-key evil-normal-state-map "\C-d" nil))

;; Make sure words are treated correctly in evil mode
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(use-package evil-replace-with-register
  :ensure t
  :config
  (progn
    (setq evil-replace-with-register-key "gr")
    (evil-replace-with-register-install)))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(use-package evil-surround
  :config (global-evil-surround-mode t)
  :ensure t)

;;;; Hard core escape, super powerful keywords
(defun ed/escape-normal-mode ()
  "Stop any recursive edit and go into normal mode."
  (interactive)
  (keyboard-escape-quit)
  (evil-normal-state))
(global-set-key (kbd "C-g") #'ed/escape-normal-mode)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;;;; Global Bindings, keys
(bind-key "C-x C-k" 'kill-this-buffer)
(bind-key "C-h l" #'find-library)
(bind-key "C-x q" #'eval-buffer)

;;;; Lisp, paredit
(show-paren-mode 1)

;;;; Pretty symbols, lambda
(add-hook 'emacs-lisp-mode-hook (lambda () (setq prettify-symbols-alist '(("lambda" . 955)))))
(add-hook 'clojure-mode-hook (lambda () (setq prettify-symbols-alist '(("fn" . 955)))))
(global-prettify-symbols-mode 1)

(use-package highlight-sexp
  :disabled
  :ensure t)
;; (defun ponelat/non-lisp-paredit()
;;   "Turn on paredit mode for non-lisps."
;;   (interactive)
;;   (set (make-local-variable 'paredit-space-for-delimiter-predicates)
;;        '((lambda (endp delimiter) nil)))
;;   (paredit-mode 1))

;; (use-package paredit
;;   :ensure t
;;   :init
;;   (add-hook 'rjsx-mode-hook 'ponelat/non-lisp-paredit))

;; (use-package evil-paredit
;;   :init
;;   (add-hook 'cider-repl-mode-hook 'evil-paredit-mode)
;;   (add-hook 'ielm-mode-hook 'evil-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
;;   (add-hook 'json-mode-hook 'enable-paredit-mode)
;;   :ensure t)

;;;; html,xml, markup
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (progn
    (setq emmet-expand-jsx-className? t)
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'rjsx-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.
  :ensure t)

(use-package web-mode
  :ensure t)

;; (comment use-package lispy
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook 'lispy-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
;;   (add-hook 'lisp-mode-hook 'lispy-mode)
;;   (define-key lispy-mode-map (kbd "M-n") nil))

(use-package evil-lispy
  :init
  (add-hook 'clojure-mode-hook 'evil-lispy-mode)
  (add-hook 'emacs-lisp-mode-hook 'evil-lispy-mode)
  (add-hook 'lisp-mode-hook 'evil-lispy-mode)
  :config
  (evil-define-key 'insert evil-lispy-mode-map "[" nil)
  (evil-define-key 'insert evil-lispy-mode-map "]" nil)
  :ensure t)

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
(setq select-enable-clipboard t)

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


;;;; Autosave
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;; Cuccumber, test, geherkin
(use-package feature-mode
  :ensure t)

;;;; Yaml
(use-package yaml-mode
  :ensure t)

;;;; HTTP, REST, Swagger
(use-package restclient
  :ensure t)

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  :ensure t)

;;;; Typescript
(defun ponelat/setup-tide-mode ()
  "Setup the typescript IDE mode ( tide )."
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :config
    (add-hook 'typescript-mode-hook #'ponelat/setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook
    (lambda ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (ponelat/setup-tide-mode))))
  :ensure t)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
;; (setq flycheck-check-syntax-automatically '(save mode-enabled))

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;;;; CSV
(use-package csv-mode
  :disabled t
  :ensure t)

;;;; Javascript, js-mode, js2-mode
(use-package js2-mode
  :ensure t
  :diminish js2-mode
  :config
  (progn
    (setq js2-mode-show-parse-errors t)
    (setq js2-mode-show-strict-warnings nil)))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  :ensure t)

(use-package indium
  :disabled t
  :ensure t)

;;;; Scaffolding, scaffolds
;;  Writing CPS style code

;; (defmacro with-process-shell-command (name buffer command &rest sentinel-forms)
;;   "Run a process, with the given sentinel.\nProccess args are NAME BUFFER COMMAND and SENTINEL-FORMS."
;;   `(let ((proc (start-process-shell-command ,name ,buffer ,command)))
;;      (let ((sentinel-cb (lambda (process signal)
;;                           ,@sentinel-forms)))
;;        (set-process-sentinel proc sentinel-cb))))

(defun create-react-app ()
  "Create a react app, by unzipping a .tar.gz into ~/projects/NAME, firing up the server and opening src/App.js."
  (interactive)
  (let* ((name (read-from-minibuffer "App name: "))
          (project-path (format "~/projects/%s" name)))
    (shell-command (format "mkdir -p %s" project-path))
    (shell-command (format "tar zvxf ~/projects/scaffolds/create-react-app.tar.gz -C %s" project-path))
    (find-file-other-window (format "%s/src/App.js" project-path))
    (async-shell-command "npm start")))

;;;; Less/Css

(use-package less-css-mode
  :ensure t)

;;;; Flycheck, syntax, lint
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
    (setq flycheck-highlighting-mode 'lines)
    (global-flycheck-mode))
    ;; (set-face-attribute 'flycheck-warning nil
		;; 	:foreground "black"
		;; 	:background "yellow")
    ;; (set-face-attribute 'flycheck-error nil
		;; 	:foreground "black"
		;; 	:background "pink"))

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
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package clj-refactor
  :ensure t)

; Auto load buffer, when in jacked-in
(add-hook 'cider-mode-hook
  (lambda ()
    (add-hook 'after-save-hook 'cider-load-buffer nil 'make-it-local)))

;;;; Autocomplete, company, snippets
(use-package company
  :bind (("TAB"  . company-indent-or-complete-common))
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1))

(use-package yasnippet
  :init (setq yas-snippet-dirs
	      '("~/.emacs.d/snippets"))

  :config (yas-global-mode 1)
  :ensure t)

(use-package react-snippets
  :ensure t)

;;;; npm
(require 'json)
(defun alist-keys (alist)
  "Return the keys of ALIST."
  (mapcar 'car alist))

(defun ponelat/last-dir (path) "
get the last directory from PATH.
eg: /one/two => two
    C:\One\Two => Two
."
  (file-name-nondirectory
    (directory-file-name
      (file-name-directory path))))

(defun ponelat/npm-run (project-dir)
  "Fetch a list of npm scripts from PROJECT-DIR/package.json and async execute it."
  (let* ((file-path (concat project-dir "package.json"))
          (json-data (json-read-file file-path))
          (scripts (alist-get 'scripts json-data))
          (script-keys (alist-keys scripts))
          (choice (completing-read "Npm: " script-keys))
          (project-name (ponelat/last-dir project-dir)))
    (async-shell-command (format "cd %s && npm run %s" project-dir choice) (format "*npm* - %s - %s" choice project-name))))

(defun ponelat/helm-npm-run ()
  "Run npm-run, from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'ponelat/npm-run))

(defun ponelat/projectile-npm-run ()
  "Run an npm command in the current project."
  (interactive)
  (ponelat/npm-run (projectile-project-root)))

(defun ponelat/helm-ag-do ()
  "Run npm-run, from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'helm-do-ag))

;;;; Projects

(use-package projectile
  :ensure t
  :diminish projectile
  :config
  (progn
    (projectile-mode)
    (define-key projectile-command-map (kbd "n") #'ponelat/projectile-npm-run)))

;;;; Fuzzy, ido, helm
(use-package helm
  :defines helm-mode-fuzzy-match helm-completion-in-region-fuzzy-match helm-M-x-fuzzy-match
  :diminish helm-mode
  :config
  (progn
    (setq helm-mode-fuzzy-match t)
    (setq helm-completion-in-region-fuzzy-match t)
    (helm-mode))
  :ensure t)

(use-package helm-ls-git
  :ensure t)

(use-package helm-projectile
  :bind (("M-x" . helm-M-x))
  :ensure t
  :defer 1
  :config
  (helm-projectile-on)
  (define-key helm-projectile-projects-map (kbd "C-l") #'ponelat/helm-npm-run)
  (define-key helm-projectile-projects-map (kbd "C-a") #'ponelat/helm-ag-do))

(use-package helm-ag
  :ensure t)

(use-package flx
  :ensure t)

(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode +1))

(use-package helm-fuzzier
  :ensure t
  :config
  (progn
    (setq helm-flx-for-helm-find-files t)
    (setq helm-flx-for-helm-locate t)
    (helm-fuzzier-mode 1)))

(defun helm-buffer-switch-to-new-window (_candidate)
  "Display buffers in new windows."
  ;; Select the bottom right window
  (require 'winner)
  (select-window (car (last (winner-sorted-window-list))))
  ;; Display buffers in new windows
  (dolist (buf (helm-marked-candidates))
    (select-window (split-window-right))
    (switch-to-buffer buf))
  ;; Adjust size of windows
  (balance-windows))

;; (use-package ido
;;   :ensure t
;;   :config
;;   (ido-mode)
;;   :pin melpa)

;; (use-package flx-ido
;;   :ensure t)

;;;; Git, magit
(setq smerge-command-prefix "\C-cv")

(use-package magit
  :bind (("C-c g" . magit-status))
  :ensure t)

(use-package evil-magit
  :ensure t)

;;;; GitHub
(use-package magit-gh-pulls
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  :ensure t)

(use-package github-browse-file
  :ensure t)

;;;; Ledger
(use-package ledger-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  :ensure t)


(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :diminish (git-gutter+-mode . "+="))


;;;; Org-mode
(use-package org
  :ensure t
  :bind
  (("M-n" . org-capture)
    ("C-c a" . org-agenda)
    ("C-c n" . ponelat/open-notes))
  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file "notes.org")
  (setq org-src-fontify-natively t)
  (setq org-capture-templates
    '(("t" "Todo" entry (file org-default-notes-file)
        "* TODO %?\n  SCHEDULED: %t\n  %i\n  %a")
       ("d" "Today" entry (file+headline org-default-notes-file "Today")
         "** TODO %?\n  SCHEDULED: %t\n  %i\n  %a")
       ("i" "Ireland" entry (file org-default-notes-file)
         "*%?\n  ")
       ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
         "** %? \nEntered on %U\n %i\n %a")
       ("s" "Dream Stack" entry (file+headline org-default-notes-file "Dream Stack")
         "** %? \nEntered on %U\n %i\n %a")
       ("m" "Meetup Notes" entry (file+headline org-default-notes-file "Meetup Notes")
         "** %? \nEntered on %U\n %i\n %a"))))

(defun ponelat/open-notes ()
  "Open the default notes (org-mode) file."
  (interactive)
  (find-file (concat org-directory "/" org-default-notes-file)))

(use-package evil-org
  :config
  (evil-define-key 'normal evil-org-mode-map
    "J" nil
    "K" nil)
  :ensure t)

;; Create a code block in org mode

(defun d12-org/insert-block-template ()
  "Insert block template at point."
  (interactive)
  (if (org-at-table-p)
    (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                       ("e" . "EXAMPLE")
                       ("h" . "HTML")
                       ("q" . "QUOTE")
                       ("c" . "CENTER")))
            (key
              (key-description
                (vector
                  (read-key
                    (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                      (mapconcat (lambda (choice)
                                   (concat (propertize (car choice) 'face 'font-lock-type-face)
                                     ": "
                                     (cdr choice)))
                        choices
                        ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
              ((region-active-p)
                (let ((start (region-beginning))
                       (end (region-end)))
                  (goto-char end)
                  (insert "\n#+END_" choice)
                  (goto-char start)
                  (insert "#+BEGIN_" choice "\n")))
              (t
                (insert "#+BEGIN_" choice "\n")
                (save-excursion (insert "\n#+END_" choice))))))))))
;; (use-package redtick
;;   :disabled t
;;   :ensure t)
;; (use-package org-alert
;;   :config
;;   (setq alert-default-style 'libnotify)
;;   (org-alert-enable)
;;   :ensure t)

(defun my-org-archive-done-tasks ()
  "Archive all TODOs with DONE."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

(setq org-agenda-files (list ponelat/today-dir))

(use-package org-projectile
  :config
  (progn
    (setq org-projectile:projects-file
          (concat ponelat/today-dir "/projects.org"))
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

;;;; Themes
;; Disable previous theme, before enabling new one. Not full proof.
;; Themes have a lot of power, and some of it cannot be reversed here

(defadvice load-theme (before theme-dont-propagate activate)
  "Try to completely revert a theme, befor applying a new one."
  (mapc #'disable-theme custom-enabled-themes))
(use-package svg-mode-line-themes
  :ensure t)

(use-package ocodo-svg-modelines
  :ensure t)

;; (use-package badwolf-theme
;;   :ensure t)

;; (use-package zenburn-theme
;;   :ensure t)

(use-package sublime-themes
  :ensure t)

;; (with-eval-after-load 'zerodark-theme ())
(use-package org-beautify-theme
  :disabled t
  :ensure t)


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package zerodark-theme
  :config
  (progn
    (load-theme 'zerodark t)
    (zerodark-setup-modeline-format))
  :ensure t)


;; This will probably break terminal theme
;; Great for outdoors

;; (if (window-system)
;;     (load-theme "leuven t)
;;     ;; (load-theme 'graham t)
;;     (load-theme 'badwolf t))

;; (set-face-attribute 'default  nil :height 100)

;; (set-face-foreground 'mode-line "black")
;; (set-face-background 'mode-line "#B1CC6F")

;; (set-face-foreground 'mode-line-inactive "#474747")
;; (set-face-background 'mode-line-inactive "#161A1F")


(provide 'init)

;;; init.el ends here
;;;; Custom variables stored here...
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
