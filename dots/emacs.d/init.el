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

;; Add custom files
;; Be sure to run "rcup" for stuff you've added to projects/dotfiles to show up here
(add-to-list 'load-path "~/.emacs.d/custom")

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

;;;; Copy filename helper
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;; Config management
(defun imenu-elisp-sections ()
  "Create a list of sections from config file."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("use" "^ *( *use-package *\\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;; Firsts, macro
(defmacro ponelat/first-macro (&rest body)
  "The first macro (it use BODY)!!!"
  `(progn ,@(mapcar (lambda (form) `(message (format "%s" ,form))) body)))

;; Emacs, Lisp
(defun ponelat/emacs-lisp-imenu-init (p)
  "Jump to section in init.el file.  Or straight to P."
  (interactive "P")
  (find-file-existing "~/projects/dotfiles/dots/emacs.d/init.el")
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

(use-package avy
  :config
  (setq avy-timeout-seconds 0.3)
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

(defhydra ponelat/hydra/open-notes (:idle 1.0 :color blue)
  "Org files"
  ("o" (ponelat/open-notes "office.org") "office")
  ("f" (ponelat/open-notes "money.org") "money")
  ("m" (ponelat/open-notes "meetups.org") "meetups")
  ("p" (ponelat/open-notes "projects.org") "projects")
  ("t" (ponelat/open-notes "thoughts.org") "thoughts")
  ("n" (ponelat/open-notes "notes.org") "notes")
  ("j" (ponelat/open-notes "jokes.org") "jokes")
  ("d" (ponelat/open-notes "docs.org") "docs")
  ("e" (ponelat/open-notes "personal.org") "personal"))

  (defhydra hydra-string-case (global-map "C-c C-s")
  "string case"
  ("c" string-inflection-all-cycle "all cycle"))

(global-set-key (kbd "C-c o") 'ponelat/hydra/open-notes/body)

(defhydra hydra-helm (:hint nil :color pink)
        "
                                                                          ╭──────┐
   Navigation   Other  Sources     Mark             Do             Help   │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
         ^_k_^         _K_        _p_    [_m_] mark         [_v_] view         [_H_] helm help
        ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
    _h_ ←   → _l_    _c_        ^ ^    [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
        ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
         ^_j_^         _J_        _n_      ^ ^               [_w_] toggle windows
  --------------------------------------------------------------------------------
        "
        ("<tab>" helm-keyboard-quit "back" :exit t)
        ("<escape>" nil "quit")
        ("\\" (insert "\\") "\\" :color blue)
        ("h" helm-beginning-of-buffer)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-end-of-buffer)
        ("g" helm-beginning-of-buffer)
        ("G" helm-end-of-buffer)
        ("n" helm-next-source)
        ("p" helm-previous-source)
        ("K" helm-scroll-other-window-down)
        ("J" helm-scroll-other-window)
        ("c" helm-recenter-top-bottom-other-window)
        ("m" helm-toggle-visible-mark)
        ("t" helm-toggle-all-marks)
        ("u" helm-unmark-all)
        ("H" helm-help)
        ("s" helm-buffer-help)
        ("v" helm-execute-persistent-action)
        ("d" helm-persistent-delete-marked)
        ("y" helm-yank-selection)
        ("w" helm-toggle-resplit-and-swap-windows)
  ("f" helm-follow-mode))



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

;;;; Sudo, root, sudowrite, dired, tramp
(require 'tramp)
(setq dired-dwim-target t)
(defun ponelat/sudo-dired ()
  "Opens a Dired buffer with sudo priviledges."
  (interactive)
  (dired "/sudo::/"))

;;;; Markdown

(use-package markdown-mode
  :ensure t)

;;;; Evil, vim
 (defun ponelat/expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
           '(try-expand-line-all-buffers)))
      (call-interactively 'hippie-expand)))



(use-package evil-leader
  :ensure t
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
    (setq evil-motion-state-modes nil)
    (evil-set-initial-state 'Info-mode 'normal)
    (define-key evil-insert-state-map (kbd "C-x C-l") 'ponelat/expand-lines)
    (evil-leader/set-key
      "q" #'kill-buffer-and-window
      "Q" #'save-buffers-kill-terminal
      "p" #'helm-projectile-switch-project
      "j" #'helm-M-x
      "a" #'helm-do-ag-project-root
      "b" #'helm-buffers-list
      "w" #'save-buffer
      "l" #'avy-goto-line
      "s" #'avy-goto-char-2
      ;; "s" #'avy-goto-char-timer
      ";" #'delete-other-windows
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

;;;; Evil Rebellion
(require 'evil-org-rebellion)

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
(bind-key "C-x Q" 'save-buffers-kill-emacs)
(bind-key "C-x y" #'eval-buffer)

(bind-key "C-c l e" 'ponelat/emacs-lisp-imenu-init)
(bind-key "C-c l o" (lambda () (interactive) (helm-org-agenda-files-headings)))

(bind-key "C-c ;" 'delete-other-windows)
(bind-key "C-c :" 'delete-window)
(bind-key "C-c C-a" 'helm-do-ag-project-root)

(bind-key "C-h l" #'find-library)

;;;; Lisp, paredit
(show-paren-mode 1)

;;;; Pretty symbols, lambda

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq prettify-symbols-alist '(("lambda" . 955)))
    (prettify-symbols-mode)))

(add-hook 'clojure-mode-hook
  (lambda ()
    (setq prettify-symbols-alist '(("fn" . 955)
                                    (";;" . 955)))
    (prettify-symbols-mode)))

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
  (progn
    (evil-define-key 'insert evil-lispy-mode-map "[" nil)
    (evil-define-key 'insert evil-lispy-mode-map "]" nil))
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

;;;; Mermaid
;; It should be added to .emacs.d/custom: see 'load-path

;;;; CSV
(use-package csv-mode
  :disabled t
  :ensure t)

;;;; Ruby, rspec
(use-package ruby-mode
  :ensure t)

(use-package rspec-mode
  :ensure t)


;;;; Javascript, js-mode, js2-mode
(use-package js2-mode
  :ensure t
  :diminish js2-mode
  :config
  (progn
    (setq js2-mode-show-parse-errors t)
    (setq js2-mode-show-strict-warnings nil)))


;; ag projects
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

;; TODO finish this
(comment defun ponelat/ag (filename folder)
  "Search for uses of FILENAME, within FOLDER."
  (ag-regexp (format "import .* from *['\"]%s(.js|.jsx)?['\"]" filename) folder))

(comment defun ponelat/get-babel-aliases (project-dir)
(let* ((file-path (concat project-dir "package.json"))
          (json-data (json-read-file file-path))
          (scripts (assoc-recursive json-data 'babel ))
          (script-keys (alist-keys scripts))
          (choice (completing-read "Npm: " script-keys))
          (project-name (ponelat/last-dir project-dir)))
    (async-shell-command (format "cd %s && npm run %s" project-dir choice) (format "*npm* - %s - %s" choice project-name))))


;; (ponelat/ag "actions/user" "/home/josh/projects/swaggerhub-frontend/")

(use-package web-beautify
  :ensure t)

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  :ensure t)

(use-package indium
  :disabled t
  :ensure t)

(defun ponelat/find-incoming-js-imports (project filename)
  "Find all files within PROJECT, that import the FILENAME, and present a helm buffer to jump to."
  (interactive)
  ;; get regexp results ( from within project )
  ;; (Combine multiple results?)
  ;; Display in helm buffer
  ;; TODO Sunday-funday
  t)

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

(defun creat-project ()
  "Create a simple project folder with .git/."
  (interactive)
  (let* ((name (read-from-minibuffer "Project name: "))
          (project-path (format "~/projects/%s" name)))
    (shell-command (format "mkdir -p %s" project-path))
    (shell-command (format "git init" project-path))
    (shell-command (read-from-minibuffer "Your next command? "))))

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
  :defer 1
  :config
  (progn
    (global-company-mode)
    (setq company-idle-delay 0.1)))

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
          (script-keys-with-base (append script-keys '((install . install) (build . build))))
          (choice (completing-read "Npm: " script-keys-with-base))
          (project-name (ponelat/last-dir project-dir))
          (run-prefix (cond ((equal choice "install") "")
                        ((equal choice "test") "")
                        (t "run"))))
    (async-shell-command (format "cd %s && npm %s %s" project-dir run-prefix choice) (format "*npm* - %s - %s" choice project-name))))

(defun ponelat/npm-link ()
  "It'll link some project into some other ( defauling to current )"
  (interactive)
  (async-shell-command (format "cd %s && npm link && cd %s && npm link %s" project-target-dir project-base-path project-target)))


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
    (setq helm-autoresize-mode t)
    (setq helm-buffer-max-length 40)
    (define-key helm-map (kbd "<escape>") 'hydra-helm/body)
    (defvar helm-source-file-not-found
      (helm-build-dummy-source
        "Create file"
        :action 'find-file))
    (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t)
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

(defun ponelat/reset-commit-soft ()
  "Reset to the last commit, softly."
  (interactive)
  (magit-reset-soft "HEAD^"))

(global-set-key (kbd "M-o s") #'ponelat/reset-commit-soft)

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (progn
    (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent")
    (setq magit-list-refs-sortby "-creatordate"))
  :ensure t)

(use-package evil-magit
  :ensure t)

;;;; GhostText, browser, live
(use-package atomic-chrome
  :ensure t)
;;;; Jira
(use-package jira-markup-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.confluence$" . jira-markup-mode))
  (add-to-list 'auto-mode-alist '("\\.jira" . jira-markup-mode))
  (add-to-list 'auto-mode-alist '("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode))
  :ensure t)

;;;; GitHub
(use-package magit-gh-pulls
  :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  :ensure t)

(use-package gist
  :ensure t)

(use-package github-clone
  :ensure t)

(use-package git-link
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


;;;; org-mode
(use-package org
  :ensure t
  :bind
  (("M-n" . org-capture)
    ("C-c a" . org-agenda)
    ("C-c i" . org-narrow-to-subtree)
    ("C-c w" . org-agenda-refile)
    ("C-c I" . widen)
    ("C-c j" . ponelat/open-journal))
  :config
  (progn
    (define-key org-mode-map (kbd "C-c ;") nil)
    (add-hook 'org-open-link-functions #'ponelat/org-open-link-shub)

    (setq org-directory ponelat/today-dir)
    (setq org-agenda-files (list ponelat/today-dir))
    (setq org-default-notes-file "notes.org")
    (setq org-confirm-elisp-link-function nil)
    (setq org-src-fontify-natively t)
    (setq org-insert-heading-respect-content t)
    (setq org-agenda-start-day "1d")
    (setq org-agenda-span 5)
    (setq org-agenda-start-on-weekday nil)
    (setq org-deadline-warning-days 1)

    ;;;; Org refile
    (setq org-refile-use-outline-path 'file)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 3)
             (org-agenda-files :maxlevel . 3)))

;;;; TODOs labels
    (setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d!)")
         (sequence "LOOSE(l)" "SOMEDAY(s)" "|" "HABIT(h)")
         (sequence "BLOCKED(b@)" "|" "CANCELLED(c@)")
         (sequence "DISCUSS(i/@)" "|" "DONE(d!)")))
;;;; org templates
    (setq org-capture-templates
      '(("t" "Todo" entry (file (lambda () (concat org-directory "/notes.org")))
          "* TODO %?\n  %i\n  %a")
         ("b" "Blank Point" entry (file (lambda () (concat org-directory "/notes.org")))
           "* %?")
         ("j" "Jokes" entry (file (lambda () (concat org-directory "/jokes.org")))
           "* %?")
         ("n" "Notes" entry (file (lambda () (concat org-directory "/notes.org")))
           "* %?\n  %i\n  %a")
         ("h" "Thought" entry (file (lambda () (concat org-directory "/thoughts.org")))
           "* LOOSE %?\n  %i\n  %a")))))

(use-package helm-org-rifle
  :ensure t)

;;;; External Org mode, Office, Gmail
(defun ponelat/get-office-data ()
  (interactive)
  "Get Office evnets for the week"
  (async-shell-command "n use 8.9.1 ~/projects/microsoft-graph-client/get-event-data.js" "*Import Office365 Events*"))

(defun ponelat/get-all-data ()
  "Gets all external org data."
  (interactive)
  (ponelat/get-gmail-data)
  (ponelat/get-office-data))

(defun ponelat/get-gmail-data ()
  "Download ical from Gmail."
  (interactive)
  (async-shell-command "source ~/.env && ical2org-gmail.awk <(curl -Ls -o - $ICAL_GMAIL) > ~/Dropbox/org/gmail.org" "*Importing Gmail Calendar*"))

(defun ponelat/get-PTO-calendar ()
  "The iCal for the PTO calendar at work"
  (interactive)
  (async-shell-command "source ~/.env && ical2org-pto.awk <(curl -Ls -o - $ICAL_PTO) > ~/Dropbox/org/ical-pto.org" "*Importing Confluence PTO Calendar*"))

;;;; Org mode helpers
(defun ponelat/org-insert-child-headline ()
  "It inserts a child headline ( ie: Lower than the current."
  (interactive)
  (org-insert-heading-respect-content)
  (org-demote)
  (call-interactively 'evil-insert))

(use-package ox-jira
  :ensure t)

(defun ponelat/open-notes (filename)
  "Open the default FILENAME from default org dir."
  (interactive)
  (find-file (concat org-directory "/" filename)))

(defun ponelat/open-journal ()
  "Open the journal file."
  (interactive)
  (find-file (concat org-directory "/journal.org")))

(use-package htmlize
  :ensure t)

(defun ponelat/open-project-org ()
  "Open the default notes (org-mode) file."
  (interactive)
  (find-file (concat ponelat/today-dir "/projects.org")))

(defun ponelat/org-open-link-shub (link)
  "Open LINK as JIRA issue, if it matches shub-xxxx."
  (cond ((string-match "\\(shub-[0-9]\\{4\\}\\)" link) ; [[shub-xxxx]]
         (let* ((shub (match-string 1 link))
                (url (concat "https://smartbear.atlassian.net/browse/" (url-encode-url (upcase shub)))))
           (browse-url url)))))


(use-package evil-org
  :config
  (add-hook 'org-mode-hook (lambda () (evil-org-mode t)))
  (progn
    (evil-define-key 'normal evil-org-mode-map
      "t" 'org-todo)
    (evil-define-key 'normal evil-org-mode-map
      (kbd "C-S-<return>") 'ponelat/org-insert-child-headline)
    (evil-define-key 'insert evil-org-mode-map
      (kbd "C-S-<return>") 'ponelat/org-insert-child-headline))
  :ensure t)

;; TODO: fix this
(defun my-org-archive-done-tasks ()
  "Archive all TODOs with DONE."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(comment use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

(use-package org-projectile
  :bind
  (("C-c p t" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file
      (concat ponelat/today-dir "/projects.org"))
    (comment setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)
    :ensure t))

;;;; Package stuff
(defun package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

;; Only in Emacs 25.1+
(defun package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by status."
  (interactive
   (list (completing-read
          "Status: " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter (concat "status:" status)))

(define-key package-menu-mode-map "s" #'package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'package-menu-find-marks)
;;;; Run current file

(defun ponelat/run-this-file ()
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

;; Provides leuven which is good for daylight coding
(use-package zerodark-theme
  :ensure)
(use-package solarized-theme
  :disabled t
  :ensure t)
;; (with-eval-after-load 'zerodark-theme ())
;; This can only run in window mode...
(comment use-package org-beautify-theme
  :ensure t)

(use-package sublime-themes
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ;;;; Load themes
;; (defadvice load-theme (before theme-dont-propagate activate)
;;   "Try to completely revert a theme, befor applying a new one."
;;   (mapc #'disable-theme custom-enabled-themes))

 (defun load-theme-only ()
  "Load a theme, and disable all others."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
   (command-execute 'load-theme))

;; Load theme on first frame ( only once )
(defvar ponelat:theme-window-loaded nil "A flag used to indicate that the GUI theme got loaded.")
(defvar ponelat:theme-terminal-loaded nil "A flag used to indicate that the Terminal theme got loaded.")

;; Due to starting a daemon at the same time as our client
;; The follow code exists to ensure that the theme is loaded at the right time.
(defun ponelat/setup-theme ()
  "Enable or load gui/window theme."
  (if ponelat:theme-window-loaded
    (progn
      (enable-theme 'zerodark)
      (zerodark-setup-modeline-format))
    (progn
      (load-theme 'zerodark)
      (setq org-beautify-theme-use-box-hack nil)
      (load-theme 'org-beautify)
      (zerodark-setup-modeline-format)
      (setq ponelat:theme-window-loaded t))))

(defun ponelat/setup-theme-terminal ()
  "Enable or load terminal theme."
  (if ponelat:theme-terminal-loaded
    (enable-theme 'solarized-light)
    (progn
      (load-theme 'solarized-light)
      (setq ponelat:theme-terminal-loaded t))))

(defun ponelat/after-make-frame-functions-setup-theme (frame)
  "To be called from 'after-make-frame-functions which will provide FRAME.  This will setup theme."
  (with-selected-frame frame
    (if (window-system frame)
      (ponelat/setup-theme)
      (ponelat/setup-theme-terminal))))

;; Either defer loading theme if this is a server
;; or load the apropriate theme
(if (daemonp)
  (add-hook 'after-make-frame-functions #'ponelat/after-make-frame-functions-setup-theme)
  (progn
    (if (display-graphic-p)
      (ponelat/setup-theme)
      (ponelat/setup-theme-terminal))))

;;;; Custom variables stored here...
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;; Font,face
(defvar ponelat/fonts
  '(    ("Big" (:family "Ubuntu Mono" :height 160 :weight normal))
     ("Normal" (:family "Ubuntu Mono" :height 120 :weight normal))))

(defun ponelat/default-font (font-name)
  "
Set the font.  FONT-NAME is the key found in ponelat/fonts.
Interactively you can choose the FONT-NAME"
  (interactive
    (list
      (completing-read "Choose font: " (alist-keys ponelat/fonts))))
  (let ((font-props (car (assoc-default font-name ponelat/fonts))))
    (apply 'set-face-attribute (append '(default nil) font-props))))

;; What is this???...
;; (put 'narrow-to-region 'disabled nil)

;;; init.el ends here
(provide 'init)
