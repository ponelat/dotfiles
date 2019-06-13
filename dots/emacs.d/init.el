;;; init.el --- Just my dot file.
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Custom variables stored here...
(setq emacs-dir "~/.emacs.d")
(setq custom-file (concat emacs-dir "/custom.el"))

;; Add custom files
;; Be sure to run "rcup" for stuff you've added to projects/dotfiles to show up here
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Allows use to create closures for functions ( in my case, for sentinel callbacks )
(setq lexical-binding t)
(windmove-default-keybindings)
(auto-image-file-mode 1)
(electric-pair-mode t)

;;;; init.el helpers
(defmacro comment (&rest body)
  "Comment out sexp (BODY)."
  nil)

;;;;;;;;;;;;; Packages ...
;; Install `use-package'
(straight-use-package 'use-package)

;; Tell straight.el to overwrite use-package, such it uses straight.el instead of package.el by default.
;; ...to NOT use straight.el, add `:straigh nil` to `use-package'
(setq straight-use-package-by-default t)

;; Load org quickly! Before it could be loaded by some nafarious package, breaking it from straight.el
(straight-use-package 'org)

;; Disable
(defun ponelat/toggle-trace-request ()
  "It does something"
  (interactive)
  (if (eq request-log-level -1)
    (progn
      (custom-set-variables '(request-log-level 'blather)
        '(request-message-level 'blather))
      (message "Tracing Requests enabled"))
    (progn
      (custom-set-variables '(request-log-level -1)
        '(request-message-level -1))
      (message "Tracing Requests disabled"))))


(defun ponelat/sum-numbers-in-region (start end)
  "Sum the numbers in the current buffer, from START to END."
  (interactive "r")
  (message "%s"
    (cl-reduce #'+
      (split-string
        (replace-regexp-in-string "[^0-9]+" " "
          (buffer-substring start end)))
      :key #'string-to-number)))

;; ;;;; SSH mode
;; (use-package ssh-mode
;;   )

;;;; Macrostep
(use-package macrostep)


;;;; Auth info, secrets, passwords
(defun ponelat/get-secret (host)
  "Return list of user and password for HOST in ~/.authinfo."
  (require 'auth-source)
  (let* ((auth (nth 0 (auth-source-search :host host
                       :requires '(user secret))))
         (password (funcall (plist-get auth :secret)))
         (user (plist-get auth :user)))
    (list user password)))

(defun ponelat/get-secret-basic (host)
  "Return base64 encoded login:password from HOST in .authinfo."
  (let* ((auth (ponelat/get-secret host))
         (user (nth 0 auth))
         (password (nth 1 auth)))
    (ponelat/basic-auth user password)))

(defun ponelat/basic-auth (username pass)
  "Return the basic auth for USERNAME and PASS."
  (base64-encode-string (format "%s:%s" username pass)))

;;;; Copy filename helper
(defun ponelat/kill-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;; Revert buffer/file reload
(global-set-key  (kbd "C-x RET RET") (lambda () (interactive) (revert-buffer t t nil)))

(defun ponelat/copy-file-from-downloads ()
  "It copies a file from ~/Downloads, using Helm."
  (interactive)
  (let* ((file-to-copy (read-file-name "File to copy: " "~/Downloads/"))
          (directory default-directory)
          (filename (file-name-nondirectory file-to-copy))
          (dest-file
            (expand-file-name
              (read-file-name "Dest: " directory nil nil filename)
              directory)))
    (copy-file file-to-copy dest-file)))

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

;;;; Term, bash, zsh, shell
(defun ponelat/term ()
  "Create or jump to an 'ansi-term', running zsh."
  (interactive)
  (if (get-buffer "*terminal*")
      (switch-to-buffer "*terminal*")
    (ansi-term "/bin/zsh" "terminal")))

;; Used to source .zshrc and .profile ( for $PATH )
;; This causes RGB pain
;; (setq shell-command-switch "-ic")
(global-set-key (kbd "M-C-z") #'projectile-run-async-shell-command-in-root)
(global-set-key (kbd "M-z") #'ponelat/term)

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;;;; Dirs
(defvar ponelat/today-dir "~/Dropbox/org" "My base ORG-MODE folder.")
(defvar ponelat/projects-dir "~/projects" "My base projects folder, used with PROJECTILE and others.")

;;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq-default truncate-lines t)

;;;; General, editor, config
(defun browse-url-chrome-unstable (url &optional new-window)
  "Open URL in Chrome unstable, possibly in NEW-WINDOW."
  (shell-command (concat "google-chrome-unstable" " " "\"" url "\"")))
(setq x-selection-timeout 300)

(defun browse-url-x-www-browser (url &optional new-window)
  "Open URL in x-www-browser, possibly in NEW-WINDOW."
  (shell-command (concat "x-www-browser" " " "\"" url "\"")))

(defun browse-url-firefox (url &optional new-window)
  "Open URL in firefox, possibly in NEW-WINDOW."
  (shell-command (concat "firefox" " " "\"" url "\"")))

(setq vc-follow-symlinks t)
(setq browse-url-browser-function #'browse-url-firefox)

(use-package avy
  :config
  (setq avy-timeout-seconds 0.3))

;;;; Strings
(use-package string-inflection)

(defun ponelat/string-kebab-case-function (str)
  "It converts STR to be in kebab case. Handles spaces as well.

eg: \"Hello over there\" => \"hello-over-there\"
"
  (downcase (replace-regexp-in-string "[ \t]" "-" str)))

;;;; Hydra, menus
(use-package hydra
  )

(defhydra hydra-zoom (global-map "C-x =")
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("0" (lambda () (interactive) (text-scale-decrease 0)) "reset"))

(progn
  (defhydra hydra-window-resize (global-map "C-x {")
    "zoom"
    ("{" shrink-window-horizontally "shrink")
    ("}" enlarge-window-horizontally "enlarge"))
  (global-set-key (kbd "C-x }") 'hydra-window-resize/body))

(defhydra ponelat/hydra/open-notes (:idle 1.0 :color blue)
  "Org files"
  ("o" (ponelat/open-notes "office.org") "office")
  ("t" (ponelat/open-notes "travel.org") "travel")
  ("g" (ponelat/open-notes "get-well-onprem.org") "get-well")
  ("f" (ponelat/open-notes "money.org") "money")
  ("b" (find-file (concat ponelat/projects-dir "/api-book/book/api-book.org")) "api-book")
  ("m" (ponelat/open-notes "meetups.org") "meetups")
  ("p" (ponelat/open-notes "projects.org") "projects")
  ("n" (ponelat/open-notes "notes.org") "notes")
  ("j" (ponelat/open-notes "jokes.org") "jokes")
  ("s" (ponelat/open-notes "shopping.org") "shopping")
  ("a" (ponelat/open-notes "pain.org") "pain points")
  ("d" (ponelat/open-notes "docs.org") "docs")
  ("l" (org-capture-goto-last-stored) "(last)")
  ("e" (ponelat/open-notes "personal.org") "personal")
  ("x" (ponelat/open-notes "phoenix.org") "phoenix coffee"))

(defhydra hydra-string-case (global-map "C-c C-s")
  "string case"
  ("s" string-inflection-all-cycle "all cycle"))

(global-set-key (kbd "C-c o") 'ponelat/hydra/open-notes/body)
(global-set-key (kbd "C-c s") 'hydra-string-case/body)

(use-package diminish
  )

;(comment use-package ace-window
;  :bind (("M-p" . ace-window))
;  :config
;  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;  (setq aw-dispatch-always t)
;  )

;;;; Mode discovery
(use-package discover-my-major
  :config
  (progn
    (global-set-key (kbd "C-h C-m") 'discover-my-major)
    (global-set-key (kbd "C-h M-m") 'discover-my-mode))
  )

(use-package ranger
  :disabled t
  )

(comment use-package dot-mode

  :config
  (global-dot-mode))

(use-package editorconfig

  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;;; Autoindent
(use-package auto-indent-mode
  :config
  (add-hook 'rjsx-mode 'auto-indent-mode)
  )

;;;; Writeroom, writing, book
(defun ponelat/write ()
  "Set up writing mode."
  (interactive)
  (writeroom-mode t)
  (git-gutter+-mode -1)
  (visual-line-mode t)
  (flyspell-mode-on)
  (ponelat/add-frame-padding))


;; Disable git-gutter mode in writeroom-mode ( as it clashes with margin/fringe )
(defun ponelat/inhibit-git-gutter+-mode ()
  "Disable `git-gutter+-mode' in local buffer."
  (add-hook 'after-change-major-mode-hook
    (lambda () (git-gutter+-mode 0))
    :append :local))

(use-package writeroom-mode
  :config
  (comment progn                        ; Doesn't seem to work. Want to disable git-gutter+-mode when in writeroom-mode
    (add-hook 'writeroom-mode-hook #'ponelat/inhibit-git-gutter+-mode))
  )

;;;; Sudo, root, sudowrite, dired, tramp
(require 'tramp)
(setq dired-dwim-target t)
(setq tramp-default-method "ssh")
(defun ponelat/sudo-this ()
  "Opens a Dired buffer with sudo priviledges."
  (interactive)
  (dired (format "/sudo::%s" (buffer-file-name))))

(use-package dired-narrow
  :bind (:map dired-mode-map
          ("C-c s" . dired-narrow)))

(use-package docker-tramp
  )

;;;; Markdown

(use-package markdown-mode
  )

 (defun ponelat/expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
           '(try-expand-line-all-buffers)))
      (call-interactively 'hippie-expand)))

;;;; Evil, vim
(use-package evil

  :init
  (setq
    evil-want-keybinding nil
    evil-want-integration t)
  :config
  (progn
    (evil-mode 1)
    (evil-set-initial-state 'Info-mode 'normal)
    (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
    (define-key evil-normal-state-map "\C-d" nil)
    (define-key evil-normal-state-map "\M-." nil)
    (define-key evil-normal-state-map "go" 'org-open-at-point-global)))

;; (use-package evil-collection
;;   :after evil
;;   :custom (evil-collection-setup-minibuffer t)
;;   :config
;;   (progn
;;     (evil-collection-init pdf)))

(use-package evil-leader
  :after evil
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
    (setq evil-motion-state-modes nil)
    (define-key evil-insert-state-map (kbd "C-x C-l") 'ponelat/expand-lines)
    (global-set-key (kbd "C-S-l") #'evil-window-right)
    (global-set-key (kbd "C-S-h") #'evil-window-left)
    (global-set-key (kbd "C-S-k") #'evil-window-up)
    (global-set-key (kbd "C-S-j") #'evil-window-down)
    (evil-leader/set-key
      ;; "q" #'kill-buffer-and-window
      "q" #'quit-window
      "Q" #'save-buffers-kill-terminal
      "p" #'helm-projectile-switch-project
      "hp" #'helm-projectile
      "j" #'helm-M-x
      "o" #'xah-open-in-external-app
      "a" #'helm-do-ag-project-root
      "b" #'helm-buffers-list
      "w" #'save-buffer
      "l" #'avy-goto-line
      "s" #'avy-goto-char-2
      "f" #'flycheck-list-errors
      ;; "s" #'avy-goto-char-timer
      ";" #'delete-other-windows
      "i" #'helm-imenu)))

(use-package evil-matchit
  :config
  (progn
    (global-evil-matchit-mode 1))
  )

(evil-define-text-object rsb/textobj-inner-c-defun (count &optional beg end type)
  (save-excursion
    (mark-defun)
    (re-search-forward "{")
    (exchange-point-and-mark)
    (re-search-backward "}")
    (evil-range (region-beginning) (region-end) type :expanded t)))

(evil-define-text-object rsb/textobj-outer-c-defun (count &optional beg end type)
  :type line
  (save-excursion
    (mark-defun)
    (if (looking-at "[:space:]*$")
        (forward-line))
    (exchange-point-and-mark)
    (unless (save-excursion
              (forward-line)
              (looking-at "[:space:]*$"))
      (forward-line))
    (evil-range (region-beginning) (region-end) type :expanded t)))


(define-key evil-inner-text-objects-map "f" 'rsb/textobj-inner-c-defun)
(define-key evil-outer-text-objects-map "f" 'rsb/textobj-outer-c-defun)

(use-package ace-link
  :config
  (progn
    (ace-link-setup-default))
  )

;; Make sure words are treated correctly in evil mode
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(use-package evil-replace-with-register

  :config
  (progn
    (setq evil-replace-with-register-key "gr")
    (evil-replace-with-register-install)))

(use-package evil-commentary

  :config (evil-commentary-mode))

(use-package evil-surround
  :config (global-evil-surround-mode t)
  )

;;;; Hard core escape, super powerful keywords
(defun ed/escape-normal-mode ()
  "Stop any recursive edit and go into normal mode."
  (interactive)
  (keyboard-escape-quit)
  (evil-normal-state))
(global-set-key (kbd "C-g") #'ed/escape-normal-mode)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;;;; Lisp, paredit
(show-paren-mode 1)
(use-package parinfer
  :disabled t

  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
      '(defaults       ; should be included.
         pretty-parens  ; different paren styles for different modes.
         evil           ; If you use Evil.
         lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
         paredit        ; Introduce some paredit commands.
         smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
         smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))
;;;; Pretty symbols, lambda

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq prettify-symbols-alist '(("lambda" . 955)))
    (prettify-symbols-mode)))

;; ┌ ┬ ┐ ├ ┼ ┤ └ ┴ ┘ ─ │

(add-hook 'clojure-mode-hook
  (lambda ()
    (setq prettify-symbols-alist '(("fn" . 955)
                                    (";;" . 955)))
    (prettify-symbols-mode)))

(use-package highlight-sexp
  :config
  (highlight-sexp-mode 1)
  )
;; (defun ponelat/non-lisp-paredit()
;;   "Turn on paredit mode for non-lisps."
;;   (interactive)
;;   (set (make-local-variable 'paredit-space-for-delimiter-predicates)
;;        '((lambda (endp delimiter) nil)))
;;   (paredit-mode 1))

;; (use-package paredit
;;
;;   :init
;;   (add-hook 'rjsx-mode-hook 'ponelat/non-lisp-paredit))

;; (use-package evil-paredit
;;   :init
;;   (add-hook 'cider-repl-mode-hook 'evil-paredit-mode)
;;   (add-hook 'ielm-mode-hook 'evil-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode)
;;   (add-hook 'json-mode-hook 'enable-paredit-mode)
;;   )

;;;; Color
(use-package rainbow-mode
  :config
  (rainbow-mode 1)
  )

;;;; html,xml, markup
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (progn
    (setq emmet-expand-jsx-className? t)
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'rjsx-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (evil-define-key 'visual emmet-mode-keymap (kbd "C-j") #'emmet-wrap-with-markup))
  )

(progn
  (defun ponelat/nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                   (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                     (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))

  (defun xml-find-file-hook ()
    (when (derived-mode-p 'nxml-mode)
      (which-function-mode t)
      (setq which-func-mode t)))

  (add-hook 'which-func-functions 'nxml-where t t))

(add-hook 'find-file-hook 'xml-find-file-hook t)
(use-package web-mode
  )

;; (comment use-package lispy
;;
;;   :config
;;   (add-hook 'clojure-mode-hook 'lispy-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
;;   (add-hook 'lisp-mode-hook 'lispy-mode)
;;   (define-key lispy-mode-map (kbd "M-n") nil))

(defun ponelat/lispy-kill-sexp ()
  "It'll kill the next balanced sexp and then jump back into lispy mode."
  (interactive)
  (kill-sexp)
  (delete-blank-lines)
  (next-line)
  (evil-lispy/enter-state-left))

(defun ponelat/lispy-clone ()
  "It'll clone the sexp, then run."
  (interactive)
  (special-lispy-clone)
  (special-lispy-down)
  (special-lispy-ace-symbol-replace))

(defun ponelat/slurp-forward ()
  "It tries to slurp forward in different langs, starting with Lisp."
  (interactive)
  (lispy-forward-slurp-sexp 1))

(defun ponelat/barf-forward ()
  "It tries to barf forward in different langs, starting with Lisp."
  (interactive)
  (lispy-forward-barf-sexp 1))

(use-package evil-lispy
  :init
  (add-hook 'clojure-mode-hook 'evil-lispy-mode)
  (add-hook 'emacs-lisp-mode-hook 'evil-lispy-mode)
  (add-hook 'lisp-mode-hook 'evil-lispy-mode)
  :config
  (progn
    (evil-define-key 'insert evil-lispy-mode-map "[" nil)
    (evil-define-key 'insert evil-lispy-mode-map ")" nil)
    (define-key lispy-mode-map (kbd "C-d") 'ponelat/lispy-kill-sexp)
    (define-key lispy-mode-map (kbd "c") 'ponelat/lispy-clone)
    (define-key lispy-mode-map (kbd "\"") nil)
    (evil-define-key 'insert evil-lispy-mode-map "]" nil)
    (evil-define-key 'insert evil-lispy-mode-map (kbd "C-.") 'ponelat/slurp-forward)
    (evil-define-key 'insert evil-lispy-mode-map (kbd "C-,") 'ponelat/barf-forward))
  )

;;(setq show-paren-style 'expression)

;;;; Ag
;; use the_silver_searcher when available
(use-package ag

  :if (executable-find "ag"))

(use-package rg
  )

(use-package helm-rg
  :after helm
  )


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
  )

;;;; Yaml
(use-package yaml-mode
  )

(use-package yaml-imenu
  :config
  (yaml-imenu-enable)
  )

;;;; HTTP, REST, Swagger
(use-package restclient
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))))

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  )

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
  )

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
  )

;;;; Ruby, rspec
(use-package ruby-mode
  )

(use-package rspec-mode
  )

;;;; Rust, cargo
(use-package rust-mode
  :config
  (progn
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)
    (setq rust-cargo-bin "~/.cargo/bin/cargo"))
  )

(use-package toml-mode
  )

(use-package racer
  :config
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
  )

;;;; Docker, Dockerfile
(use-package dockerfile-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
  )

(use-package docker-compose-mode
  )

;; LDAP, LDIF, Active Directory
(use-package ldap
  )

;;;; Java
(use-package meghanada
  :config
  (progn (add-hook 'java-mode-hook
           (lambda ()
             ;; meghanada-mode on
             (meghanada-mode t)
             (flycheck-mode +1)
             (setq c-basic-offset 2)))

    (cond
      ((eq system-type 'windows-nt)
        (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
        (setq meghanada-maven-path "mvn.cmd"))
      (t
        (setq meghanada-java-path "java")
        (setq meghanada-maven-path "mvn")))
    ))

;; (use-package jdee
;;   :config
;;   (setq jdee-server-dir "~/jars")
;;   )

;;;; Java - automation

;; mvn verify -Dmaven.test.failure.ignore=true -Denv=dev -Dselenium=browser.chrome
(defun shub/test-features (&rest extra)
  (interactive)
  "Run features, with EXTRA flags."
  (let*
    ((task "verify")
      (directory (projectile-project-root))
      (flags '("-Dmaven.test.failure.ignore=true"
                "-Denv.url=http://localhost:3200"
                "-Dagent=browser.chrome"
                "-DdriverPath=/home/josh/bin/chromedriver"))
      (cmd
        (format "cd %s && %s %s %s %s"
          directory
          jdee-maven-program
          task
          (string-join flags " ")
          (string-join extra " "))))
    (compilation-start cmd)))

(defun shub/test-current-feature (&rest extra)
  "Run all feature tests."
  (interactive)
  (shub/test-features (format "-Dcucumber.options=%s" buffer-file-name)))

(defun shub/test-current-feature-staging (&rest extra)
  "Run all feature tests."
  (interactive)
  (shub/test-features (format "-Dcucumber.options=%s" buffer-file-name) "-Denv=staging"))

;;;; Javascript, js-mode, js2-mode
(use-package js2-mode

  :diminish js2-mode
  :config
  (progn
    (setq js2-mode-show-parse-errors t)
    (setq js2-mode-show-strict-warnings nil)
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

(comment use-package mocha
  (progn
    (define-key js2-mode-map  (kbd "C-c C-t f") 'mocha-test-file)
    (define-key js2-mode-map  (kbd "C-c C-t p") 'mocha-test-at-point)
    (define-key js2-mode-map  (kbd "C-c C-t a") 'mocha-test-project))
  )

(use-package nodejs-repl
  )

(use-package company-tern
  :defer 1
  :config
  (progn
    (add-to-list 'company-backends 'company-tern))
  )

(defun ponelat/shell-command-on-region-to-string (begin end command)
  "It wraps `shell-command-on-region' using BEGIN, END and COMMAND, so that it returnes a string."
  (let ((buf (generate-new-buffer "*Shell Output*")))
    (shell-command-on-region begin end command buf)
    (let ((result (with-current-buffer buf (buffer-string))))
      (progn
        (kill-buffer buf)
        result))))

(defun ponelat/yaml-to-json (begin end)
  "Convert the BEGIN END region into JSON.  Putting the result into the kill ring."
  (interactive "r")
  (kill-new
    (ponelat/shell-command-on-region-to-string begin end "yaml json write -")))

(defun ponelat/json-to-yaml (begin end)
  "Convert the BEGIN END region into YAML  Putting the result into the kill ring."
  (interactive "r")
  (kill-new
    (ponelat/shell-command-on-region-to-string begin end "yaml json read -")))

(defun ponelat/beautify-json ()
  "Will prettify JSON."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
      "python -mjson.tool" (current-buffer) t)))

(use-package xref-js2
  :config
  (progn
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
      (lambda ()
        (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  )

(use-package js2-refactor
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'js2-mode-hook #'js2-refactor-mode))
  )

;;;; Apps, spotify
(defun ponelat/spotify (command)
  "Runs the ~/bin/spot spotify client (DBus) with COMMAND as argument.
See: https://gist.githubusercontent.com/wandernauta/6800547/raw/2c2ad0f3849b1b1cd1116b80718d986f1c1e7966/sp"
  (call-process-shell-command (format "~/bin/spot %s" command)))

(defun ponelat/spotify-next ()
  "Skips next spotify song."
  (interactive)
  (ponelat/spotify "next"))
(defun ponelat/spotify-previous ()
  "Visit previous spotify song."
  (interactive)
  (ponelat/spotify "prev"))
(defun ponelat/spotify-play-toggle ()
  "Play/pause spotify."
  (interactive)
  (ponelat/spotify "play"))


;;;; ag projects
(defun assoc-recursive (alist &rest keys)
  "Recursively search ALIST for KEYS."
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
  )

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map (kbd "C-c C-j") nil)
  (define-key rjsx-mode-map (kbd "C-c r") #'rjsx-rename-tag-at-point)
  )

(use-package indium
  )

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
          (typescript? (y-or-n-p "Typescript? "))
          (project-path (format "~/projects/%s" name))
          (tarball
            (cond
              (typescript? "create-react-app-typescript.tgz")
              (t "create-react-app.tgz"))))
    (progn
      (shell-command (format "mkdir -p %s" project-path))
      (shell-command (format "tar zxf ~/projects/scripts/scaffolds/%s -C %s" tarball project-path))
      (find-file-other-window (format "%s/src/%s" project-path (if typescript? "App.tsx" "App.js")))
      (async-shell-command "npm run dev"))))

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
  )

;;;; Flycheck, syntax, lint
(use-package flycheck

  :diminish flycheck-mode
  :config
    (setq flycheck-highlighting-mode 'lines)
    (global-flycheck-mode))

(use-package pos-tip
  )

(use-package flycheck-pos-tip

  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

;;;; $PATH environment variable
;; (setenv "PATH" (concat (getenv "PATH") ":/home/josh/.nix-profile/bin"))
(setq exec-path (append exec-path '("~/.nix-profile/bin" exec-directory)))

;; So that we can access `./node_modules/.bin/eslint` mostly
(use-package add-node-modules-path

  :init
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path)))
    (eval-after-load 'js2-jsx-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path)))

;;;; Jq
(use-package jq-mode
  )

;;;; Go lang
 (use-package go-mode
   :config
   (setq gofmt-command "goimports")
   (add-hook 'before-save-hook 'gofmt-before-save)
   (evil-define-key 'normal go-mode-map "gd" #'godef-jump)
   )

(use-package company-go
  :after company
  :init
  (add-to-list 'company-backends 'company-go))


(use-package go-eldoc

  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-guru

  :init (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))

;;;; Haskell, FP
(use-package haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
  )
;;;; Clojure
(use-package cider

  :config
  (progn
    (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
    (setq cljr-suppress-no-project-warning t)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)))

(use-package flycheck-joker
  )

(use-package clojure-mode

  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package clj-refactor
  )

; Auto load buffer, when in jacked-in
(add-hook 'cider-mode-hook
  (lambda ()
    (add-hook 'after-save-hook 'cider-load-buffer nil 'make-it-local)))

;;;; Autocomplete, company, snippets
(use-package company

  :config
  (progn
    (setq
      evil-complete-next-func 'company-complete-common-or-cycle
      evil-complete-previous-func 'company-complete-common-or-cycle
      company-global-modes '(not magit-mode))
    (global-company-mode)
    (define-key company-mode-map (kbd "TAB") 'company-complete)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-s") 'company-search-mode)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

    (define-key company-active-map (kbd "C-w") 'nil)

    (define-key company-search-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-search-map (kbd "C-p") 'company-select-previous-or-abort)

    (setq company-idle-delay nil)))

(use-package yasnippet
  :init (setq yas-snippet-dirs
	      '("~/.emacs.d/snippets"))

  :config (yas-global-mode 1)
  )

(use-package react-snippets
  )

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

(defun ponelat/npm-project-p (project-dir)
  "Test if PROJECT-DIR is an npm project, ie: it has a package.json file."
  (interactive)
  (let* ((file-path (concat project-dir "package.json")))
    (file-exists-p file-path)))

(defun ponelat/shell-file-p (file-str)
  "Test if FILE-STR is a shell file."
  (string-match-p "\\.sh$" file-str))

(defun ponelat/shell-run (prefix project-dir)
  "With PREFIX add arguments, then search for shell commands in PROJECT-DIR and execute it."
  (interactive)
  (projectile-with-default-dir project-dir
    (let* ((args (if prefix (buffer-file-name) ""))
            (files (projectile-current-project-files))
            (shell-files (seq-filter #'ponelat/shell-file-p files))
            (shell-file-rel (completing-read "Run: " shell-files))
            (shell-file-abs (concat project-dir shell-file-rel " " args)))
      (async-shell-command shell-file-abs))))


(defun ponelat/project-run (prefix project-dir)
  "Guess at and execute a build step, possibly with PREFIX (current file) in PROJECT-DIR."
  (cond
    ((ponelat/npm-project-p project-dir)
      (ponelat/npm-run project-dir))
    (t
      (ponelat/shell-run prefix project-dir))))

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

;; TODO make more generic
(defun ponelat/npm-install (project-dir)
  "Install a npm module into PROJECT-DIR with --save-dev and --exact."
  (let ((dep (read-from-minibuffer "Npm module: ")))
    (async-shell-command (format "cd %s && npm i -DE %s" project-dir dep))))

(defun ponelat/get-links (directory)
  "Show links in DIRECTORY.  Default to `projectile-project-root'."
  (interactive (list (projectile-project-root)))
  (split-string
    (shell-command-to-string
      (format "find %s -type l -not -ipath '*/.bin/*'" directory))))

(defun ponelat/npm-clone-and-link (project-dir)
  "Fetch a list of npm scripts from PROJECT-DIR/package.json and async execute it."
  (interactive (list (projectile-project-root)))
  (let* ((node-modules (concat project-dir "node_modules/"))
          (choice (completing-read "Npm Dep: " (directory-files node-modules)))
          (choice-full-path (concat project-dir "node_modules/" choice)))
    (async-shell-command (format "cd %s && ~/projects/scripts/node-install-dep.sh %s" project-dir choice-full-path) (format "*Installing Dependency* - %s" choice))))

(defun ponelat/clone-npm-module ()
  "Clone an npm module into projects."
  (interactive)
  (let* ((npm-module (read-string "Npm module: "))
          (project-dir ponelat/projects-dir))
    (async-shell-command (format "cd %s && ~/projects/scripts/npm-clone-module.sh %s" project-dir npm-module) (format "*Cloning Dependency* - %s" npm-module))))

(defun ponelat/npm-link (base-dir target-dir)
  "It'll link TARGET-DIR to BASE-DIR project."
  (let ((relative-path-to-target (file-relative-name target-dir base-dir)))
    (async-shell-command (format "cd %s && npm link %s" base-dir relative-path-to-target))))

(defun ponelat/parent-dir (path)
  "Return parent directory of PATH."
  (file-name-directory (directory-file-name path)))

(defun ponelat/npm-unlink (base-dir target-dir)
  "It'll unlink TARGET-DIR from BASE-DIR project."
  (let ((filename (file-name-nondirectory target-dir)))
    (async-shell-command (format "cd %s && npm unlink %s" base-dir filename))))

(defun ponelat/helm-npm-run ()
  "Run npm-run, from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'ponelat/npm-run))

(defun ponelat/helm-project-run ()
  "Run `ponelat/project-run', from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'ponelat/project-run))

(defun ponelat/helm-npm-install ()
  "Run npm-install-save, from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'ponelat/npm-install))

(defun ponelat/helm-npm-clone-and-link ()
  "Run npm-clone-and-link, from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'ponelat/npm-clone-and-link))

(comment defun ponelat/helm-get-env ()
  "Get a secret from .env file and yank into clipboard."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.env")
    (keep-lines "(:alphanum:)+=(:alphanum:)")))

(defun ponelat/projectile-npm-run ()
  "Run an npm command in the current project."
  (interactive)
  (ponelat/npm-run (projectile-project-root)))

(defun ponelat/projectile-project-run (prefix)
  "Run build command in the current project."
  (interactive "P")
  (ponelat/project-run prefix (projectile-project-root)))

;; (defun ponelat/test-universal (prefix)
;;   "It does something"
;;   (interactive "P")
;;   (if prefix
;;     (message "Prefix!")
;;     (message "Not prefix.")))


(defun ponelat/projectile-npm-link (link-path)
  "Run an npm link against LINK-PATH."
  (interactive
    (list (read-directory-name "Project to Link: "
            (ponelat/parent-dir
              (projectile-project-root)))))
  (ponelat/npm-link (projectile-project-root) link-path))

(defun ponelat/projectile-npm-unlink (link-path)
  "Run an npm link against LINK-PATH."
  (interactive
    (list (completing-read "Project to Unlink: "
            (ponelat/get-links (projectile-project-root)))))
  (ponelat/npm-unlink (projectile-project-root) link-path))

(defun ponelat/helm-ag-do ()
  "Run npm-run, from the helm projectile buffer."
  (interactive)
  (helm-exit-and-execute-action #'helm-do-ag))

;;;; Projects

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))

  :diminish projectile
  :config
  (progn
    (projectile-mode)
    (define-key projectile-command-map (kbd "n") #'ponelat/projectile-project-run)))
;;;; Fuzzy, ido, helm

;; (put 'helm-ff-run-open-file-externally 'helm-only t)

(defun ponelat/helm-execute-file ()
  "Run open file externally command action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'xah/run-this-file-fn)))

;; From https://www.reddit.com/r/emacs/comments/bxt0cz/helm_and_the_tab_key/
(progn
  (defun ponelat/double-flash-mode-line ()
    "Flash the modeline"
    (let ((flash-sec (/ 1.0 20)))
      (invert-face 'mode-line)
      (run-with-timer flash-sec nil #'invert-face 'mode-line)
      (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
      (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))
  (defun ponelat/helm-execute-if-single-persistent-action (&optional attr split-onewindow)
    "Execute persistent action if the candidate list is less than 2"
    (interactive)
    (with-helm-alive-p
      (if (> (helm-get-candidate-number) 2)
        (ponelat/double-flash-mode-line)
        (helm-execute-persistent-action)))))

(use-package helm
  :defines helm-mode-fuzzy-match helm-completion-in-region-fuzzy-match helm-M-x-fuzzy-match
  :diminish helm-mode
  :bind (("M-x" . helm-M-x))
  ;; :bind* (:map helm-map
  ;;          ([tab] . ponelat/helm-execute-if-single-persistent-action)
  ;;          ("C-i" . ponelat/helm-execute-if-single-persistent-action))

  :config
  (progn
    (setq helm-adaptive-history-file (concat emacs-dir "/helm-adaptive-history.el"))
    (helm-adaptive-mode)
    (setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-buffer-max-length 40)
    (define-key helm-map [(control ?w)] 'backward-kill-word)

    (helm-mode)))

(use-package helm-ls-git
  )

(use-package helm-projectile

  :defer 1
  :config
  (progn
    (helm-projectile-on)
    (helm-projectile-define-key helm-projectile-projects-map (kbd "C-c g") #'helm-projectile-vc)
    (define-key helm-projectile-projects-map (kbd "C-l") #'ponelat/helm-npm-run)
    (define-key helm-projectile-projects-map (kbd "C-p") #'ponelat/helm-project-run)
    (define-key helm-projectile-projects-map (kbd "C-c n") #'ponelat/helm-npm-install)
    (define-key helm-projectile-projects-map (kbd "C-c l") #'ponelat/helm-npm-clone-and-link)
    (define-key helm-projectile-find-file-map (kbd "C-x C-x") #'ponelat/helm-execute-file)
    (define-key helm-projectile-projects-map (kbd "C-a") #'ponelat/helm-ag-do)))


(use-package helm-ag
  )

(use-package helm-fuzzier

  :config
  (progn
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

;;;; fasd, files, recent
(use-package fasd
  :config
  (global-fasd-mode 1)
  )
;;;; Ivy, counsel, swiper
(setq enable-recursive-minibuffers t)
(use-package ivy
  :bind (("C-s" . swiper))
  :config
  (progn
    (define-key swiper-map [(control ?w)] 'backward-kill-word))
  )

;;;; Git, magit

(setq smerge-command-prefix "\C-cv")

(defun ponelat/reset-head-soft ()
  "Reset to the last commit, softly."
  (interactive)
  (magit-reset-soft "HEAD^"))

(defun ponelat/wipe-file ()
  "Checkout the current file from HEAD, effectively wiping out the changes."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to wipe out your changes in %s? " buffer-file-name))
    (magit-file-checkout "HEAD" buffer-file-name)))

(setq auth-source '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (progn
    (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent")
    (setq magit-list-refs-sortby "-creatordate"))
  )
(use-package evil-magit
  )

;;;; GhostText, browser, live
(use-package atomic-chrome
  )
;;;; Jira
(use-package jira-markup-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.confluence$" . jira-markup-mode))
  (add-to-list 'auto-mode-alist '("\\.jira" . jira-markup-mode))
  (add-to-list 'auto-mode-alist '("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode)))
;;;; GitHub
;; Required for magithub until they move to `transient' https://github.com/magit/magit/issues/3749
(use-package forge
 :requires magit)

; (use-package magit-popup)
; (use-package magithub
;   :requires magit-popup
;   :config
;   (magithub-feature-autoinject t)
;   (setq magithub-clone-default-directory "~/projects")
;   )

(use-package gist)

(use-package git-link)

(defun ponelat/git-link-develop ()
  "Call `git-link' and set `git-link-default-branch' to develop."
  (interactive)
  (setq git-link-default-branch "develop")
  (call-interactively #'git-link)
  (setq git-link-default-branch nil))

(defun ponelat/git-link-master ()
  "Call `git-link' and set `git-link-default-branch' to master."
  (interactive)
  (setq git-link-default-branch "master")
  (call-interactively #'git-link)
  (setq git-link-default-branch nil))

(defun ponelat/ponelat-copy-branch ()
  "It kills the current git branch."
  (interactive)
  (kill-new (git-link--branch)))


;;;; Ledger
(use-package ledger-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
    (setq ledger-report-use-native-highlighting t))
  )


(use-package git-gutter+

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
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)
            (custom-set-variables
              '(git-gutter+-window-width 1)
              '(git-gutter+-added-sign "+")
              '(git-gutter+-deleted-sign "-")
              '(git-gutter+-modified-sign "=")
              '(git-gutter:visual-line t)
              '(git-gutter+-modified  "yellow")))
  :diminish (git-gutter+-mode . "+="))

;; "^\\(=====[ 	]+\\)\\([^\t\n].*?\\)\\(\\([ \t]+=====\\)?[ \t]*\\(?:\n\\|\\'\\)\\)"
(use-package adoc-mode
  :config
  (progn

    (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
    (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))

    (defun ponelat/adoc-imenu-create-index ()
      (let* ((index-alist)
              (re-all-titles-core
                (mapconcat
                  (lambda (level) (adoc-re-one-line-title level))
                  '(0 1 2 3 4)
                  "\\)\\|\\(?:"))
              (re-all-titles
                (concat "\\(?:" re-all-titles-core "\\)")))
        (save-restriction
          (widen)
          (goto-char 0)
          (while (re-search-forward re-all-titles nil t)
            (backward-char) ; skip backwards the trailing \n of a title
            (let* ((descriptor (adoc-title-descriptor))
                    (title-level (nth 2 descriptor))
                    (title-text (nth 3 descriptor))
                    (title-pos (nth 4 descriptor)))
              (setq
                index-alist
                (cons (cons (format "%s%s" (make-string title-level ?\ ) title-text) title-pos) index-alist)))))
        (list (cons "Title" (nreverse index-alist)))))

    (defun ponelat/adoc-imenu-expresssions ()
      (interactive)
      "Create only the top level titles. With proper titles"
      (setq-local imenu-create-index-function #'ponelat/adoc-imenu-create-index)
      ;; (setq imenu-create-index-function #'imenu-default-create-index-function)
      (setq-local imenu-prev-index-position-function nil)
      ;; (add-to-list 'imenu-generic-expression '("Title" "^\\(=+[ \t]*[^ \t].+\\)\n$" 1 t))
      ;; (add-to-list 'imenu-generic-expression '("Images" "^[ \t]*image::?\\(\\)\\[.*\\]\n?" 1 t))
      )
    (add-hook 'adoc-mode-hook #'ponelat/adoc-imenu-expresssions)

    (defun ponelat/adoc-imenu-to-org-headings ()
      "Captures the imenu into the kill ring."
      (interactive)
      (kill-new (pp (imenu-create-index-function))))

    ))

;;;; IMB Box symbols
;; ┌ ┬ ┐ ├ ┼ ┤ └ ┴ ┘ ─ │

;;;; org-mode pre
(defun ponelat/org-mode-styles ()
  "It styles org mode."
  (interactive)
  ;; Headline sizes
  (progn
    (let* ((variable-tuple
             (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
               ((x-list-fonts "Noto Sans")         '(:font "Noto Sans"))
               ((x-list-fonts "Verdana")         '(:font "Verdana"))
               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
            (fixed-tuple
              (cond ((x-list-fonts "Noto Mono") '(:font "Noto Mono"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Noto Sans"))))
            (base-font-color     (face-foreground 'default nil 'default))
            (headline           `(:inherit default :foreground ,base-font-color)))
      (progn
        (custom-theme-set-faces
          'user
          `(variable-pitch ((t ( ,@variable-tuple :height ,(face-attribute 'default :height) :weight light))))
          `(fixed-pitch    ((t ( ,@fixed-tuple :slant normal :weight normal :height ,(face-attribute 'default :height) :width normal)))))

        (custom-theme-set-faces
          'user
          `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.0))))
          `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.0))))
          `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.1))))
          `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.1))))
          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
          `(org-level-1 ((t (,@headline :height 1.3))))
          `(org-document-title ((t (,@headline ,@variable-tuple :height 3.0 :weight bold :underline nil))))
          '(org-block                 ((t (:inherit fixed-pitch))))
          '(org-table                 ((t (:inherit fixed-pitch))))
          '(org-todo                  ((t (:inherit fixed-pitch))))
          '(org-code                  ((t (:inherit fixed-pitch))))
          '(org-indent                ((t (:inherit org-hide fixed-pitch))))
          '(org-hide                  ((t (:inherit fixed-pitch))))
          '(org-document-info-keyword ((t (:inhert (shadow) :height 0.8))))
          '(org-document-info         ((t :height 1.0)))
          '(org-link                  ((t :underline nil)))
          '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
          '(org-property-value        ((t (:inherit fixed-pitch))) t)
          '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
          '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
          '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))))
    (ponelat/face-extras)))

;;;; org-mode
(use-package org
  :bind
  (("M-n" . org-capture)
    ("C-c a" . org-agenda)
    ("C-c i" . org-narrow-to-subtree)
    ("C-c t" . org-teleport)
    ("C-c w" . org-agenda-refile)
    ("C-c I" . widen)
    ("C-c j" . ponelat/open-journal)
    ("C-c d" . ponelat/open-today))
  :config
  (progn
    (define-key org-mode-map (kbd "C-c ;") nil)
    (define-key org-mode-map (kbd "C-c C-'") 'org-cycle-list-bullet)
    (global-set-key (kbd "C-c C-L") #'org-store-link)
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
    (setq org-confirm-babel-evaluate nil)

;;;; Babel
    (progn
      (setq org-babel-load-languages
        (append
          org-babel-load-languages
          '((js . t)
             (shell . t))))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
      (add-to-list 'org-babel-tangle-lang-exts '("js" . "js")))

;;;; Styles
    (setq
      org-hide-emphasis-markers t
      org-startup-indented t
      org-hide-leading-stars t)

    (font-lock-add-keywords 'org-mode
      '(("^ *\\([-]\\) "
          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "—"))))))
    (add-hook 'org-mode-hook 'variable-pitch-mode)
    (add-hook 'org-mode-hook #'ponelat/org-mode-styles)

;;;; Org refile
    (setq org-refile-use-outline-path 'file)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
      '((nil :maxlevel . 3)
         (org-agenda-files :maxlevel . 3)))

;;;; TODOs labels
    (setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "InProgress(p)" "|" "DONE(d!)")
         (sequence "MEET(m)" "BLOCKED(b@)" "|" "CANCELLED(c@)")
         (sequence "DISCUSS(i/@)" "|" "DONE(d!)")))
;;;; org templates
    (setq org-capture-templates
      '(("t" "Todo" entry (file (lambda () (concat org-directory "/notes.org")))
          "* TODO %?\n  %i\n  %a")
         ("b" "Blank Point" entry (file (lambda () (concat org-directory "/notes.org")))
           "* %?")
         ("d" "Today" entry (file+olp+datetree (lambda () (concat org-directory "/today.org")))
           "* TODO %? \nAdded: %T")
         ("s" "Shopping" entry (file (lambda () (concat org-directory "/shopping.org")))
           "* %?")
         ("j" "Jokes" entry (file (lambda () (concat org-directory "/jokes.org")))
           "* %?")
         ("n" "Notes" entry (file (lambda () (concat org-directory "/notes.org")))
           "* %?\n  %i\n  %a")
         ("h" "Thought" entry (file (lambda () (concat org-directory "/thoughts.org")))
           "* LOOSE %?\n  %i\n  %a")))))

;;;; Time world clock

(setq zoneinfo-style-world-list
  '(("America/New_York" "Boston")
    ("Europe/Dublin" "Galway")
    ("Europe/Stockholm" "Stockholm")
    ("Africa/Johannesburg" "Plett")))

 (defun ponelat/log-timestamp ()
   (interactive)
   (let ((log-str (format-time-string "%Y %b %d, %a")))
     (when (called-interactively-p 'any)
       (insert log-str))
     log-str))


(defun ponelat/refresh-agenda-buffers (fn)
  "Refresh all org agenda buffers, saving before.  Call FN inbetween."
  (interactive)
  (org-save-all-org-buffers)
  (apply fn '())
  (org-revert-all-org-buffers))

(defun ponelat/org-todo-keywords ()
  "Return list of org todo kewords."
  (mapcar (lambda (a) (replace-regexp-in-string "\\(\\w+\\).*" "\\1" a))
    (delete-dups
      (sort
        (seq-filter (lambda (item) (not (string= "|" item)))
          (apply 'seq-concatenate
            (cons 'list
              (mapcar 'cdr org-todo-keywords))))
        (lambda (a b) (string< a b))))))

(defun org-teleport (&optional arg)
  "Teleport the current heading to after a headline selected with avy.
With a prefix ARG move the headline to before the selected
headline. With a numeric prefix, set the headline level. If ARG
is positive, move after, and if negative, move before."
  (interactive "P")
  ;; Kill current headline
  (org-mark-subtree)
  (kill-region (region-beginning) (region-end))
  ;; Jump to a visible headline
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style))
  (cond
   ;; Move before  and change headline level
   ((and (numberp arg) (> 0 arg))
    (save-excursion
      (yank))
    ;; arg is what we want, second is what we have
    ;; if n is positive, we need to demote (increase level)
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
               do
               (if (> 0 n)
                   (org-promote-subtree)
                 (org-demote-subtree)))))
   ;; Move after and change level
   ((and (numberp arg) (< 0 arg))
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))
    ;; n is what we want and second is what we have
    ;; if n is positive, we need to demote
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
               do
               (if (> 0 n) (org-promote-subtree)
                 (org-demote-subtree)))))

   ;; move to before selection
   ((equal arg '(4))
    (save-excursion
      (yank)))
   ;; move to after selection
   (t
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))))
  (outline-hide-leaves))

;; Provides function to export current org buffer as JSON structure
;; to $file.org.json. Adapted from an org-mode mailing post by
;; Brett Viren: https://lists.gnu.org/archive/html/emacs-orgmode/2014-01/msg00338.html
(require 'json)
(defun org-export-json ()
  "Export 'org-mode' buffer into json."
  (interactive)
  (let* ((tree (org-element-parse-buffer 'object nil)))
    (org-element-map tree (append org-element-all-elements
                                  org-element-all-objects '(plain-text))
      (lambda (x)
        (if (org-element-property :parent x)
            (org-element-put-property x :parent "none"))
        (if (org-element-property :structure x)
            (org-element-put-property x :structure "none"))
        ))
    (write-region
     (json-encode tree)
      nil (concat (buffer-file-name) ".json"))))

(defun cli-org-export-json ()
  (let ((org-file-path (car command-line-args-left))
        (other-load-files (cdr command-line-args-left)))
    (mapc 'load-file other-load-files)
    (find-file org-file-path)
    (org-mode)
    (message "Exporting to JSON: %s" (car command-line-args-left))
    (org-export-json)))


(use-package helm-org-rifle
  :config
  (progn
    (setq helm-org-rifle-show-path t))
  )

(use-package org-journal
  :config
  (setq org-journal-dir (concat org-directory "/journal"))
  )


;;;; Helper functions
(defun ponelat/link-at-point ()
  "It uses org-mode functions to get link at point."
  (cond
   ((org-in-regexp org-plain-link-re)
    (buffer-substring
     (match-beginning 0)
     (match-end 0)))))

;;;; PDFs / doc view
 (progn
   (setq doc-view-continuous nil)
   (evil-define-key 'normal doc-view-mode-map "j" (lambda () (interactive) (doc-view-scroll-down-or-previous-page 1)))

   (use-package pdf-tools
     :config
     ;; initialise
     (progn
       ;; This forces the pdf-tools to use /usr/bin for pkg-config. As linuxbrew conflicts with that.
       (let ((process-environment (cons (concat "PATH=/usr/bin/:" (getenv "PATH")) process-environment)))
         (pdf-tools-install t t))
       (evil-set-initial-state 'pdf-view-mode 'normal)
       ;; open pdfs scaled to fit page
       (setq-default pdf-view-display-size 'fit-page)
       ;; automatically annotate highlights
       (setq pdf-annot-activate-created-annotations t)
       ;; use normal isearch
       (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))))

;;;; External Org mode, Office, Gmail
(defun ponelat/get-office-data ()
  (interactive)
  "Get Office evnets for the week"
  (shell-command "n use 8.9.1 ~/projects/scripts/microsoft-graph-client/get-event-data.js" "*Import Office365 Events*"))

(defun ponelat/get-all-data ()
  "Gets all external org data."
  (interactive)
  (ponelat/refresh-agenda-buffers
    (lambda ()
      (ponelat/get-gmail-data)
      (ponelat/get-pto-calendar)
      (ponelat/get-office-data))))

(defun ponelat/get-gmail-data ()
  "Download ical from Gmail."
  (interactive)
  (shell-command "source ~/.env && ical2org-gmail.awk <(curl -Ls -o - $ICAL_GMAIL) > ~/Dropbox/org/gmail.org" "*Importing Gmail Calendar*"))

(defun ponelat/get-pto-calendar ()
  "The iCal for the PTO calendar at work"
  (interactive)
  (shell-command "source ~/.env && ical2org-pto.awk <(curl -Ls -o - $ICAL_PTO) > ~/Dropbox/org/ical-pto.org" "*Importing Confluence PTO Calendar*"))

;;;; Org mode helpers
(defun ponelat/org-insert-child-headline ()
  "It inserts a child headline ( ie: Lower than the current."
  (interactive)
  (org-insert-heading-respect-content)
  (org-demote)
  (call-interactively 'evil-insert))

(use-package ox-jira
  )

;;;; Open with external tools
(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-01-18"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                       (start-process "" nil "xdg-open" $fpath))) $file-list))))))

 (use-package openwith
  :config
  (progn
     (setq openwith-associations
      (list
        (list (openwith-make-extension-regexp
                '("svg"))
          "inkscape"
          '(file))))
    (openwith-mode 1))
  )

(defun ponelat/open-notes (filename)
  "Open the default FILENAME from default org dir."
  (interactive)
  (find-file (concat org-directory "/" filename)))

(defun ponelat/open-journal ()
  "Open the journal file."
  (interactive)
  (find-file (concat org-directory "/journal.org")))

(defun ponelat/open-today ()
  "Open the journal file."
  (interactive)
  (find-file (concat org-directory "/today.org"))
  (org-datetree-find-date-create (calendar-current-date))
  (org-narrow-to-subtree))

(use-package htmlize
  )

(defun ponelat/open-project-org ()
  "Open the default notes (org-mode) file."
  (interactive)
  (find-file (concat ponelat/today-dir "/projects.org")))

(defun ponelat/org-open-link-shub (link)
  "Open LINK as JIRA issue, if it matches shub-xxxx."
  (cond ((string-match "\\(\\(shub|sdes|sonp|splat|steam|scons\\)-[0-9]\\{4\\}\\)" link) ; [[shub-xxxx]]
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
      (kbd "C-S-<return>") 'ponelat/org-insert-child-headline)
    (evil-define-key 'insert evil-org-mode-map
      (kbd "M-h") 'org-metaleft)
    (evil-define-key 'insert evil-org-mode-map
      (kbd "M-l") 'org-metaright))
  )

;; TODO: fix this
(defun ponelat/archive (type)
  "Archive all TODOs with TYPE."
  (interactive
    (list
      (completing-read "Choose type: " (ponelat/org-todo-keywords))))
  (org-map-entries 'org-archive-subtree (format "/%s" type) 'agenda))

(comment use-package org-pomodoro

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
    (push (org-projectile-project-todo-entry) org-capture-templates)
    ))

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


(defun xah/run-this-file-fn (filename)
  "Execute FILENAME
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

Derived from:
URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2017-12-27"
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
        -fdir
        -fSuffix
        -prog-name
         -cmd-str)
    (setq -fname filename)
    (setq -fdir (file-name-directory filename))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (format "cd %s && %s \"%s\"" -fdir -prog-name -fname))
    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (async-shell-command -cmd-str "*Run this*" )
        (async-shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (async-shell-command -cmd-str "*Run this*"))
          (message "No recognized program file suffix for this file."))))))

(defun xah/run-this-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

Derived from:
URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2017-12-27"
  (interactive)
  (progn
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq -fname (buffer-file-name))
    (xah/run-this-file-fn -fname)))

;;;; Themes
;; Disable previous theme, before enabling new one. Not fool-proof.
;; Themes have a lot of power, and some of it cannot be reversed here
;;; Theme hooks

;; "gh" is from http://www.greghendershott.com/2017/02/emacs-themes.html
(defvar gh/theme-hooks nil
  "((theme-id . function) ...)")

(defun gh/add-theme-hook (theme-id hook-func)
  "Add (THEME-ID . HOOK-FUNC) to 'gh/theme-hooks'."
  (add-to-list 'gh/theme-hooks (cons theme-id hook-func)))


(defun gh/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. ~~Disables enabled themes for a clean slate.~~
   Not doing that, since I want to layer some themes (ie: org-beautify )
2. Calls functions registered using `gh/add-theme-hook'."
  (prog1
    (apply f theme-id no-confirm no-enable args)
    (unless no-enable
       (progn
        (pcase (assq theme-id gh/theme-hooks)
          (`(,id . ,f) (funcall f id)))))))

(advice-add 'load-theme :around #'gh/load-theme-advice)

;;;; Faces, font, style
(defun ponelat/face-extras ()
  "Change some faces, regardless of theme.  To my liking."
  (interactive)
  (progn
    (let ((bg-color (face-attribute 'default :background)))
      (set-face-attribute 'org-hide nil :foreground bg-color :background bg-color)
      (set-face-attribute 'org-code nil :foreground (face-attribute 'org-formula :foreground))
      (set-face-attribute 'fringe nil :foreground bg-color :background bg-color))))

(defun ponelat/add-frame-padding ()
  "Add a padding to frame."
  (interactive)
  (set-frame-parameter nil 'internal-border-width 50))

(defun ponelat/theme-soothe-extras ()
  "Add extra theme settings to THEME-ID theme."
  (interactive)
  (let*  ((class '((class color) (min-colors 89)))
           ;; Palette
           (foam             "#E0E4CC")
           (snow-code        "#ECE5CE")
           (crem             "#F4EAD5")
           (dirty-crem       "#DBD2BF")
           (dirty-crem-bg    "#2B2A26")
           (gray-1           "#AAAAAA")
           (gray-2           "#828282")
           (gray-3           "#333333")
           (gray-4           "#2A2A2A")
           (gray-5           "#252525")
           (gray-6           "#202020")
           (gray-1bg         "#0A0A0A")
           (gray-2bg         "#111111")
           (gray-3bg         "#141414")
           (gray-4bg         "#171717")
           (gray-5bg         "#1A1A1A")
           (gray-6bg         "#1E1E1E")
           (red-1            "#B13120")
           (red-2            "#A23F1E")
           (red-3            "#AA1100")
           (red-4            "#660000")
           (red-1bg          "#1D1515")
           (red-2bg          "#251C1E")
           (brown-1          "#8F621D")
           (brown-1bg        "#2A1F1F")
           (orange-1         "#D94A05")
           (orange-2         "#FF5211")
           (orange-1bg       "#1F1710")
           (yellow-1         "#CEAE3E")
           (yellow-1bg       "#18140C")
           (green-1          "#719F34")
           (green-2          "#3E8F75")
           (green-3          "#839F5E")
           (green-1bg        "#1A2321")
           (green-2bg        "#1A2321")
           (turquoise-1      "#01535F")
           (turquoise-2      "#073E46")
           (turquoise-1bg    "#04181C")
           (turquoise-2bg    "#031316")
           (blue-1           "#7C9FC9")
           (blue-2           "#317598")
           (blue-3           "#009090")
           (blue-4           "#364E7A")
           (blue-1bg         "#1E252F")
           (blue-2bg         "#1B333E")
           (blue-3bg         "#132228")
           (blue-4bg         "#172028")
           (purple-1         "#7868B5")
           (purple-2         "#8A7FB5")
           (purple-3         "#483E6C")
           (purple-4         "#342B58")
           (purple-1bg       "#1D1B25")
           (purple-2bg       "#302948")
           (purple-3bg       "#241F36")
           (foreground       "#F4EAD5")
           (hl-line          "#11252A")
           (selection        "#11253A")
           (background       "#110F13")
           (background-dark  "#0F0D11")
           (alt-background   "#111013"))

  (custom-theme-set-faces
      'soothe
    `(company-echo-common ((,class (:foreground ,gray-1 :background ,gray-1bg))))
    `(company-preview ((,class (:foreground ,gray-1 :background ,gray-1bg))))
      `(company-preview-common ((,class (:foreground ,gray-1 :background ,gray-1bg))))
    `(company-preview-search ((,class (:foreground ,orange-1 :background ,gray-2bg))))
      `(company-scrollbar-bg ((,class (:foreground ,gray-1 :background ,gray-1bg))))
      `(company-scrollbar-fg ((,class (:foreground ,gray-1 :background ,gray-1bg))))
      `(company-template-field ((,class (:foreground ,gray-1 :background ,gray-1bg))))
      `(company-tooltip ((,class (:foreground ,gray-1 :background ,gray-1bg))))
      `(company-tooltip-annotation ((,class (:foreground ,gray-1))))
      `(company-tooltip-annotation-selection ((,class (:foreground ,gray-1 :weight bold))))
      `(company-tooltip-common ((,class (:foreground ,gray-1))))
    `(company-tooltip-common-selection ((,class (:foreground ,gray-1 :background ,gray-1bg))))
    `(company-tooltip-mouse ((,class (:inherit highlight))))
    `(company-tooltip-selection ((,class (:background ,gray-3bg :weight bold)))))))
;; Provides leuven which is good for daylight coding

(use-package zerodark-theme
  :defer t
  )

(use-package gruvbox-theme
  :defer t
  )

(use-package soothe-theme
  :disabled t
  :config
  (progn
    (gh/add-theme-hook
      'soothe
      (lambda (a) (ponelat/theme-soothe-extras))))
  )

(use-package badwolf-theme
  :disabled t
  :defer t
  :ensure)

(use-package solarized-theme
  :defer t
  )

;; (with-eval-after-load 'zerodark-theme ())
;; This can only run in window mode...
(use-package org-beautify-theme
  :defer t
  )

(use-package sublime-themes
  :defer t
  :disabled t
  )

(use-package org-bullets

  :config
  (setq
    org-bullets-bullet-list '("●" "○")
    org-bullets-face-name 'shadow)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun ponelat/setup-mode-line ()
  "Set up the modeline."
  (interactive)
  (progn
    (setq zerodark-use-paddings-in-mode-line nil)
    (zerodark-setup-modeline-format)
    (setq-default mode-line-format
      `("%e"
         " "
         ,zerodark-modeline-ro " "
         ,zerodark-buffer-coding
         mode-line-frame-identification " "
         " "
         ,zerodark-modeline-modified
         " "
         ,zerodark-modeline-buffer-identification
         ,zerodark-modeline-position
         ,zerodark-modeline-vc
         "  "
         (:eval (zerodark-modeline-flycheck-status))
         "  " mode-line-misc-info mode-line-end-spaces))
    (setq mode-line-format (default-value 'mode-line-format))))

 (defun ponelat/load-theme-only ()
  "Load a theme, and disable all others."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
   (command-execute 'load-theme))

(defun load-theme-only (theme &optional no-confirm no-enable)
  "Load a THEME, and disable all others.  While respecting NO-CONFIRM and NO-ENABLE from 'load-theme'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme no-confirm no-enable))

 (defun enable-theme-only (theme)
  "Enable a THEME, and disable all others."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
   (enable-theme theme))

;; Load theme on first frame ( only once )
(defvar ponelat:theme-window-loaded nil "A flag used to indicate that the GUI theme got loaded.")
(defvar ponelat:theme-terminal-loaded nil "A flag used to indicate that the Terminal theme got loaded.")
;; (defvar ponelat:theme 'gruvbox-dark-medium "The initial theme.")
(defvar ponelat:theme 'gruvbox "The initial theme.")

;; Due to starting a daemon at the same time as our client
;; The follow code exists to ensure that the theme is loaded at the right time.
(defun ponelat/setup-theme ()
  "Enable or load gui/window theme."
  (interactive)
  (unless ponelat:theme-window-loaded
    (progn
      (ponelat/setup-mode-line)
      (load-theme-only ponelat:theme)
      (setq org-beautify-theme-use-box-hack nil)
      (load-theme 'org-beautify 1)
      (setq ponelat:theme-window-loaded t)
      (ponelat/face-extras)
      (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
      (add-to-list 'default-frame-alist '( alpha . (100 . 100))))))

(defun ponelat/setup-theme-terminal ()
  "Enable or load terminal theme."
  (if ponelat:theme-terminal-loaded
    (progn
      (enable-theme 'solarized-light))
    (progn
      (customize-set-variable 'solarized-termcolors 256)
      (load-theme 'solarized-light t)
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


;;;; Font,face
(defvar ponelat/fonts
  '( ("Small"  (:family "Noto Mono"   :height 98  :weight normal))
     ("Normal" (:family "Noto Mono"   :height 140 :weight normal))))
(defun ponelat/default-font (font-name)
"Set the font.  FONT-NAME is the key found in ponelat/fonts.
Interactively you can choose the FONT-NAME"
  (interactive
    (list
      (completing-read "Choose font: " (alist-keys ponelat/fonts))))
  (let ((font-props (car (assoc-default font-name ponelat/fonts))))
    (apply 'set-face-attribute (append '(default nil) font-props))))

;;;; Set default font
(ponelat/default-font "Normal")

;;;; Cycle through fonts
(defvar ponelat/default-font-index 1)
(defun ponelat/cycle-default-font ()
  "It cycles through the default fonts."
  (interactive)
  (let* ((index ponelat/default-font-index)
          (next-index (mod (+ 1 index) (length ponelat/fonts))))
    (ponelat/default-font (first (nth next-index ponelat/fonts)))
    (setq ponelat/default-font-index next-index)))
(bind-key "C-x f" #'ponelat/cycle-default-font)

;;;; Scratch buffer, Emacs
(setq initial-scratch-message ";; Start...\n\n")

;;;; Eval, inline, Emacs lisp
(use-package eros
  :bind (("C-c C-c" . #'eval-defun))
  :config
  (progn (eros-mode 1))
  )

;; Kill all other buffers
(defun ponelat/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
            (remove-if-not 'buffer-file-name (buffer-list)))))

;;;; Toggle fullscreen, buffer
(defun ponelat/toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd "C-w f") #'ponelat/toggle-maximize-buffer))

;; Shows a helpful panel, if you forget which keys are on a prefix.
(use-package which-key
  :config
  (progn
    (which-key-mode)))

;;;; Global Bindings, keys
(bind-key "C-x C-k" 'kill-this-buffer)
(bind-key "C-x Q" 'save-buffers-kill-emacs)
(bind-key "C-x y" #'eval-buffer)

(bind-key "C-c l e" 'ponelat/emacs-lisp-imenu-init)
(bind-key "C-c l l" #'imenu)
(bind-key "C-c l a" 'helm-org-rifle-agenda-files)
(bind-key "C-c l o" 'helm-org-rifle)

(bind-key "C-c ;" 'delete-other-windows)
(bind-key "C-c :" 'delete-window)
(bind-key "C-c C-a" 'helm-do-ag-project-root)

(bind-key "C-h l" #'find-library)
(bind-key "C-x a n" #'ponelat/spotify-next)
(bind-key "C-x a p" #'ponelat/spotify-previous)
(bind-key "C-x a SPC" #'ponelat/spotify-play-toggle)

;;;; Keys, emojis

;; Can you see this face: 😬
(use-package emojify
  :config
  (emojify-set-emoji-styles '(unicode))
  (global-emojify-mode)
  (global-set-key (kbd "C-x 8 e") 'emojify-insert-emoji))

;;;; Custom.el file
(load custom-file 'noerror)
;;; init.el ends here
(provide 'init)
(put 'narrow-to-region 'disabled nil)
