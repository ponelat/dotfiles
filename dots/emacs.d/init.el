;;; init.el --- Just my dot file.
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defmacro comment (&rest body)
  "Comment out sexp (BODY)."
  nil)

;; Dir for eln-cache. Needed if the original one ends up in the /nix/store (which is readonly)
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name (convert-standard-filename ".local/eln-cache/")
                            user-emacs-directory)))

;;; Core flags
(setq lexical-binding t)

;;; Startup
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq-default truncate-lines t)

;;; Dirs and files
(defvar emacs-dir "~/.emacs.d" "The home Emacs directory." )
(setq custom-file (concat emacs-dir "/custom.el"))
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(defvar ponelat/org-dir "~/Dropbox/org" "My base ORG-MODE folder.")
(defvar ponelat/org-roam-dir "~/Dropbox/org/roam" "My base ORG-MODE Roam folder.")
(defvar ponelat/projects-dir "~/projects" "My base projects folder, used with PROJECTILE and others.")


(defun fdx/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting. https://kundeveloper.com/blog/buffer-files/#:~:text=File%3A%20%3Dfdx%2Frename%2D,(or%20move%20the%20file)."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

(defun fdx/delete-current-buffer-file ()
    "Removes file connected to current buffer and kills buffer. https://kundeveloper.com/blog/buffer-files"
    (interactive)
    (let ((filename (buffer-file-name))
          (buffer (current-buffer))
          (name (buffer-name)))
      (if (not (and filename (file-exists-p filename)))
          (ido-kill-buffer)
        (when (yes-or-no-p (format "Are you sure you want to remove \"%s\": " filename))
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))

;;; Package, Straight, use-package
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

;;; Straight.el configure
(progn
  ;; Install `use-package'
  (straight-use-package 'use-package)
  ;; Tell straight.el to overwrite use-package, such it uses straight.el instead of package.el by default.
  ;; ...to NOT use straight.el, add `:straigh nil` to `use-package'
  (setq straight-use-package-by-default t)
  ;; Load org quickly! Before it could be loaded by some nafarious package, breaking it from straight.el
  (straight-use-package 'org))

(use-package bug-hunter
  :straight '(bug-hunter :host github :repo "Malabarba/elisp-bug-hunter"))

;;; So long, large files
(global-so-long-mode 1) ; Disables major modes when files are minified/massive.

;;; Scratch buffer, Emacs
(setq initial-scratch-message "Emacs\n")
;; This breaks shit, not sure why??
(setq initial-major-mode 'fundamental-mode )

(windmove-default-keybindings)
(auto-image-file-mode 1)

(electric-pair-mode t)
; Gotten from https://emacs-lsp.github.io/lsp-mode/page/performance/ to help with LSP performance
;;; Garbage collection
(setq
 gc-cons-threshold (* 100 1024 1024)
 read-process-output-max (* 1024 1024))

;; Disable
(defun ponelat/toggle-trace-request ()
  "Toggles on/off tracing of HTTP requests."
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

;;; Utilities
(defun ponelat/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun pairs-to-cons (al)
  "Converts AL from ((one 1) (two 2)) => ((one . 1) (two . 2))."
  (mapcar (lambda (p) (cons (car p) (car (cdr p)))) al))

(defun with-num-at-point (&optional fn)
  "Apply FN to number at point. Using json rules."
  (when (save-excursion (skip-chars-backward "[0-9.]") (looking-at json-mode-number-re))
    (let ((num (apply fn (list (string-to-number (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))
          (pt (point)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (number-to-string num))
      (goto-char pt))))

(defun ponelat/sum-numbers-in-region (start end)
  "Sum the numbers in the current buffer, from START to END."
  (interactive "r")
  (message "%s"
    (cl-reduce #'+
      (split-string
        (replace-regexp-in-string "[^0-9]+" " "
          (buffer-substring start end)))
      :key #'string-to-number)))

;;; Authentication, ssh, gpg
(setq auth-source '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;;; Auth info, secrets, passwords
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

;;; Copy filename helper
(defun ponelat/kill-copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;; General for general-define-key with leaders
(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys t)
  (defconst ponelat/global-leader-key "SPC")
  (defconst ponelat/local-leader-key ","))

;;; Revert buffer/file reload
(global-set-key  (kbd "C-x RET RET") (lambda () (interactive) (revert-buffer t t nil)))

(defun ponelat/copy-file-from-downloads ()
  "It copies a file from ~/Downloads."
  (interactive)
  (let* ((file-to-copy (read-file-name "File to copy: " "~/Downloads/"))
          (directory default-directory)
          (filename (file-name-nondirectory file-to-copy))
          (dest-file
            (expand-file-name
              (read-file-name "Dest: " directory nil nil filename)
              directory)))
    (copy-file file-to-copy dest-file)))


;;; Config management
(defun imenu-elisp-sections ()
  "Create a list of sections from config file."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("use" "^ *( *use-package *\\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '("hydra" "^ *( *defhydra *\\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;; Firsts, macro
(defmacro ponelat/first-macro (&rest body)
  "The first macro (it use BODY)!!!"
  `(progn ,@(mapcar (lambda (form) `(message (format "%s" ,form))) body)))

;; Emacs, Lisp
(defun ponelat/emacs-lisp-imenu-init (p)
  "Jump to section in init.el file.  Or straight to P."
  (interactive "P")
  (find-file-existing "~/projects/dotfiles/dots/emacs.d/init.el")
  (widen)
  (consult-imenu)
  (if p (init-narrow-to-section)))

(defun init-narrow-to-section ()
  "Narrow to section within config file."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^;;;")
      (re-search-backward "^;;;" nil t))
    (push-mark)
    (forward-line)
    (re-search-forward "^;;;" nil t)
    (forward-line -1)
    (narrow-to-region (region-beginning) (region-end))))

;;; Scroll, smooth, pixel, bar
(progn
  (pixel-scroll-mode 1)
  (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
  (setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
  (setq mouse-wheel-progressive-speed nil)) ;; on a long mouse scroll keep scrolling by 1 line
;; Not using the `smooth-scrolling' package as its a little slow

;;; Eshell
(progn
  ;; https://www.emacswiki.org/emacs/EshellPrompt
  (defun ponelat/shortened-path (path max-len)

    "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
    (let* ((components (split-string (abbreviate-file-name path) "/"))
	   (len (+ (1- (length components))
		   (reduce '+ components :key 'length)))
	   (str ""))
      (while (and (> len max-len)
		  (cdr components))
	(setq str (concat str (if (= 0 (length (car components)))
				  "/"
				(string (elt (car components) 0) ?/)))
	      len (- len (1- (length (car components))))
	      components (cdr components)))
      (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

  (defun eshell/j (str)
    "Use fasd to change directory."
    (let ((dir
            (string-trim
              (shell-command-to-string
                (format "fasd -d -1 %s" str)))))
      (eshell/cd dir)))

  (defun eshell/f ()
    "Opens projectile-find-file."
    (interactive)
    (counsel-projectile-find-file))


  (defun ponelat/rjs-eshell-prompt-function ()
    (concat (ponelat/shortened-path (eshell/pwd) 40)
	    (if (= (user-uid) 0) " # " " $ ")))

  (setq eshell-prompt-function 'ponelat/rjs-eshell-prompt-function))


;;; Term, bash, zsh, shell
;; Set certain files to be in sh-mode automagically
;; (use-package vterm)

(progn
  (add-to-list 'auto-mode-alist '("\\.?zshrc\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.?profile\\'" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.?aliases\\'" . sh-mode)))

;; Colors, CSS
(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))


;;; shell commands, chmod
(defun ponelat/chmodx ()
  "Make this file executable."
  (interactive)
  (chmod (buffer-file-name) 509))

;;; Nix queries file paths
(defun nix-path (exeFile &rest joins)
  "It returns the path to the /nix/store of EXEFILE and joins JOINS together into a string."
  (let* ((target (string-trim (executable-find exeFile)))
	 (path (string-trim (shell-command-to-string (format "nix-store -q %s" target)))))
    (string-join (cons path joins))))

(comment defun ponelat/methodpath-to-badge ()
  "It takes an input string and pastes a URL badge into buffer."
  (interactive)
  (let* ((methodpath (read-string "method /path: "))
          (method (downcase (car (split-string methodpath " "))))
          (path (url-hexify-string (car (cdr (split-string methodpath)))))
          (color (cond
                 ((string= method "get") "1391FF") ;; #1391FF
                 ((string= method "post") "009D77") ;; #009D77
                 ((string= method "put") "E97500") ;; #E97500
                 ((string= method "delete") "CF3030") ;; #cf3030
                 "1391FF")))
    (insert (format "<img:https://raster.shields.io/static/v1?label=%s&message=%s&color=%s>  /' %s %s '/" method path color   (upcase method) (url-unhex-string path)))))

;;; Encode/decode
(defun xah-html-decode-percent-encoded-url ()
  "Decode percent encoded URL of current line or selection.

Example:
 %28D%C3%BCrer%29
becomes
 (Dürer)

Example:
 %E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
 文本编辑器

URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'
Version 2018-10-26"
  (interactive)
  (let ( $p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (url-unhex-string $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert (decode-coding-string $newStr 'utf-8))))))

(defun xah-html-encode-percent-encoded-url (&optional hexify)
  "Percent encode URL in current line or selection.

Example:
    http://example.org/(Dürer)
becomes
    http://example.org/(D%C3%BCrer)

Example:
    http://example.org/文本编辑器
becomes
    http://example.org/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8

URL `http://ergoemacs.org/emacs/emacs_url_percent_decode.html'
Version 2018-10-26"
  (interactive)
  (let ($p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (require 'url-util)
    (setq $newStr (funcall (if hexify #'url-hexify-string #'url-encode-url) $input-str))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert $newStr)))))

(defun ponelat/replace-region (fn &rest args)
  "Replaces the region with the result of calling (FN $input-str . ARGS)."
  (interactive)
  (let ($p1 $p2 $input-str $newStr)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (setq $newStr (apply fn (cons $input-str args)))
    (if (string-equal $newStr $input-str)
        (progn (message "no change" ))
      (progn
        (delete-region $p1 $p2)
        (insert $newStr)))))

(defun ponelat/split-lines-in-region ()
  "Splits the string (into newlines) within region/line based on regexp prompt."
  (interactive)
  (ponelat/replace-region
    (lambda (input-str) (string-join (split-string input-str (read-regexp "Split on regex:")) "\n"))))


(defun ponelat/collapse-lines-in-region ()
  "Collapses/joins region/line and prompt for separator"
  (interactive)
  (ponelat/replace-region
    (lambda (input-str)
      (string-join
        (split-string input-str "\n*")
        (read-string "Join lines with:")))))


(defun ponelat/pretty-cert-in-region ()
  "Wrap cert with BEGIN/END Certificate in region

Example:
 MIIEBTCCAu2gAwIBA...
becomes
-----BEGIN CERTIFICATE-----
 MIIEBTCCAu2gAwIBA..64
 MIIEBTCCAu2gAwIBA..64
 MIIEBTCCAu2IBA..
-----END CERTIFICATE----- "
  (interactive)
  (ponelat/replace-region #'ponelat/pretty-cert))

;;; URL stuff

(defun ponelat/explode-url (url)
  "Return an alist of URL components."
  (let* ((url-struct (url-generic-parse-url url))
          (host (url-host url-struct))
          (type (url-type url-struct))
          (port (url-portspec url-struct))
          (portstr (if port (format ":%s" port) ""))
          (base (format "%s://%s%s" type host portstr))
          (path-and-query (url-path-and-query url-struct))
          (path (car path-and-query))
          (query-list (cdr path-and-query))
          (query (if query-list
                   (pairs-to-cons
                     (url-parse-query-string
                       (cdr path-and-query))))))
    `(("base" . ,base)
       ("path" . ,path)
       ("query" . ,query))))

(defun ponelat/pretty-cert (cert)
  "Wrap CERT with BEGIN/END certificate and crop to 64 chars."
  (concat
   "-----BEGIN CERTIFICATE-----\n"
   (string-join (-partition-all 64 (string-to-list cert)) "\n")
    "\n-----END CERTIFICATE-----"))

;;; General, editor, config
(defun browse-url-chrome-unstable (url &optional new-window)
  "Open URL in Chrome unstable, possibly in NEW-WINDOW."
  (shell-command (concat "google-chrome-unstable" " " "\"" url "\"")))
(setq x-selection-timeout 300)

(defun browse-url-x-www-browser (url &optional new-window)
  "Open URL in x-www-browser, possibly in NEW-WINDOW."
  (shell-command (concat "google-chrome-stable" " " "\"" url "\"")))

(defun browse-url-firefox (url &optional new-window)
  "Open URL in firefox, possibly in NEW-WINDOW."
  (shell-command (concat "firefox" " " "\"" url "\"")))

(setq vc-follow-symlinks t)
(setq browse-url-browser-function #'browse-url-x-www-browser)

(use-package avy
  :config
  (progn
    (setq
      avy-timeout-seconds 0.3
      avy-single-candidate-jump nil)))

;;; Strings
(use-package string-inflection)

(defun ponelat/string-kebab-case-function (str)
  "It converts STR to be in kebab case. Handles spaces as well.

eg: \"Hello over there\" => \"hello-over-there\"
"
  (downcase (replace-regexp-in-string "[ \t]" "-" str)))


(defun xah-escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun xah-unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

;;; Special files to edit
(progn
  (defun ponelat/edit-hosts-file ()
    "Edit /etc/hosts"
    (interactive)
    (find-file "/sudo::/etc/hosts"))
  )

(defun ponelat/size-increase ()
  "Increase text/image size."
  (interactive)
  (call-interactively
    (cond
      ((equal major-mode 'image-mode) #'image-increase-size)
      ((equal major-mode 'pdf-view-mode) #'pdf-view-enlarge)
      (t #'text-scale-increase))))

(defun ponelat/size-decrease ()
  "Decrease text/image size."
  (interactive)
  (call-interactively
    (cond
      ((equal major-mode 'image-mode) #'image-decrease-size)
      ((equal major-mode 'pdf-view-mode) #'pdf-view-shrink)
      (t #'text-scale-decrease))))

(defun ponelat/size-reset ()
  "Reset text/image size."
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
    (setq visual-fill-column-center-text t
      visual-fill-column-width 120))
  (if (equal major-mode 'image-mode)
    (image-transform-reset)
    (text-scale-decrease 0))
  (if (equal major-mode 'pdf-view-mode)
    (pdf-view-scale-reset)))

(use-package hydra)

 ;;; Hydra, menus

; (defhydra hydra-zoom (global-map "C-x =")
 ;  "zoom"
  ; ("k" ponelat/size-increase "in")
  ; ("j" ponelat/size-decrease "out")
  ; ("h" ponelat/visual-fill-column-width-decrease "narrow")
  ; ("l" ponelat/visual-fill-column-width-increase "widen")
  ; ("0" ponelat/size-reset "reset")
  ; ("t" ponelat/toggle-default-font "toggle"))


;;; Sound, mixer volume

(defhydra hydra-volume (global-map "C-x -")
  "Sound / Volume\n"
  ("k" (lambda () (interactive) (shell-command "pamixer -i 5")) "up")
  ("j" (lambda () (interactive) (shell-command "pamixer -d 5")) "down")
  ("l" (lambda () (interactive) (shell-command "playerctl next")) "next")
  ("h" (lambda () (interactive) (shell-command "playerctl previous")) "previous"))

;; bindsym XF86AudioPlay exec playerctl play-pause
;; bindsym XF86AudioNext exec playerctl next
;; bindsym XF86AudioPrev exec playerctl previous

(defun ponelat/visual-fill-column-width-decrease ()
  "It decreases the `visual-fill-column-width' variable by 10."
  (interactive)
  (setq visual-fill-column-width (-max (list 0 (- (or visual-fill-column-width 0) 10)))))

(defun ponelat/visual-fill-column-width-increase ()
  "It increases the `visual-fill-column-width' variable by 10."
  (interactive)
  (setq visual-fill-column-width (+ (or visual-fill-column-width 0) 10)))


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
  ("a" (ponelat/open-notes "log.org") "log")
  ("w" (ponelat/open-notes "web-office.org") "web office")
  ("f" (ponelat/open-notes "money.org") "money")
  ("m" (ponelat/open-notes "meetups.org") "meetups")
  ("p" (ponelat/open-notes "projects.org") "projects")
  ("n" (ponelat/open-notes "notes.org") "notes")
  ("j" (ponelat/open-notes "jokes.org") "jokes")
  ("s" (ponelat/open-notes "shopping.org") "shopping")
  ("d" (ponelat/open-notes "docs.org") "docs")
  ("l" (org-capture-goto-last-stored) "(last)")
  ("e" (ponelat/open-notes "personal.org") "personal")
  ("x" (ponelat/open-notes "phoenix.org") "phoenix coffee"))

(defhydra hydra-string-case (global-map "C-c C-s")
  "string case"
  ("s" string-inflection-all-cycle "all cycle"))

;; (defmacro ponelat/first-macro (&rest body)
;;   "The first macro (it use BODY)!!!"
;;   `(progn ,@(mapcar (lambda (form) `(message (format "%s" ,form))) body)))

;; (defmacro lambda-file (FILE)
;;   "Return a lambda that opens a file."
;;   (lambda () (interactive) (find-file (format "%s/dotfiles/dots/config/i3/config" ponelat/projects-dir)))

(defhydra edit-file (:color blue
                      :exit t
                      :pre (which-key-mode nil)
                      :post (which-key-mode t))
  "Special File"
  ("h" ponelat/edit-hosts-file "/etc/hosts")
  ("e" ponelat/emacs-lisp-imenu-init "init.el")
  ("r" ponelat/jump-to-restclient "rest-scratch")
  ("i" (lambda () (interactive) (find-file (format "%s/dotfiles/dots/config/i3/config" ponelat/projects-dir))) "i3 config")
  ("z" (lambda () (interactive) (find-file (format "%s/dotfiles/dots/zshrc" ponelat/projects-dir))) ".zshrc")
  ("p" (lambda () (interactive) (find-file (format "%s/dotfiles/dots/profile" ponelat/projects-dir))) ".profile")
  ("s" (lambda () (interactive) (find-file "~/.ssh/config")) "ssh config")
  ("k" (lambda () (interactive) (find-file "~/.kube/config")) "kube config")
  ("k" (lambda () (interactive) (find-file "~/.kube/config")) "kubeconfig"))

;; Keyboard shortcuts for hydras
(progn
  (global-set-key (kbd "C-c l f") #'edit-file/body)
  (global-set-key (kbd "C-c s") 'hydra-string-case/body))

(use-package diminish
  )

;(comment use-package ace-window
;  :bind (("M-p" . ace-window))
;  :config
;  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;  (setq aw-dispatch-always t)
;  )

;;; Mode discovery
(use-package discover-my-major
  :config
  (progn
    (global-set-key (kbd "C-h C-m") 'discover-my-major)
    (global-set-key (kbd "C-h M-m") 'discover-my-mode))
  )

;; (use-package ranger)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; Autoindent
; (use-package auto-indent-mode
;   :config
;   (add-hook 'rjsx-mode 'auto-indent-mode))

;;; Math, calc

(use-package literate-calc-mode)


;;; Writeroom, writing, book
(defun ponelat/write ()
  "Set up writing mode."
  (interactive)
  ;; (writeroom-mode t)
  (visual-fill-column-mode t)
  (setq visual-fill-column-width 120
    visual-fill-column-center-text t)
  (git-gutter+-mode -1)
  (visual-line-mode t)
  (flyspell-mode)
  (ponelat/add-frame-padding))


;; Disable git-gutter mode in writeroom-mode ( as it clashes with margin/fringe )
(defun ponelat/inhibit-git-gutter+-mode ()
  "Disable `git-gutter+-mode' in local buffer."
  (add-hook 'after-change-major-mode-hook
    (lambda () (git-gutter+-mode 0))
    :append :local))

(use-package writeroom-mode
  :config
  (setq writeroom-bottom-divider-width 0)
  (comment progn                        ; Doesn't seem to work. Want to disable git-gutter+-mode when in writeroom-mode
    (add-hook 'writeroom-mode-hook #'ponelat/inhibit-git-gutter+-mode))
  )

;;; System, Linux, SSH, Sudo, root, sudowrite, dired, tramp
(progn
  (require 'tramp)
  (eval-after-load 'tramp '(setenv "SHELL" "/usr/bin/env bash")))

;; (use-package journalctl-mode)

;; (use-package docker-tramp)

(setq dired-dwim-target t)

(setq tramp-default-method "ssh")
(defun ponelat/sudo-this ()
  "Opens a Dired buffer with sudo priviledges."
  (interactive)
  (dired (format "/sudo::%s" (buffer-file-name))))

;; (defun ponelat/ssh (&optional PREFIX)
;;   "SSH into a server defined in ~/.ssh/config. With PREFIX it'll open _without_ sudo."
;;   (interactive "P")
;;   )

;;; Dired and Dired hacks (see: https://github.com/Fuco1/dired-hacks)

(setq dired-listing-switches "-alh"
  dired-recursive-copies "always")

(use-package dired-narrow
  :bind (:map dired-mode-map
          (("C-c s" . dired-narrow))))

(straight-use-package
  '(dired-gitignore :type git :host github :repo "johannes-mueller/dired-gitignore.el"))

;; (use-package dired-filter)
;; (use-package dired-collapse)
;; (use-package dired-rainbow
;;   :config
;;   (progn
;;     (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;;     (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;;     (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;;     (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;;     (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;;     (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;;     (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;;     (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;;     (dired-rainbow-define log "#c17d11" ("log"))
;;     (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;;     (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;;     (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;;     (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;     (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;     (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;;     (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;;     (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;;     (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;;     (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;;     (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

;; (defun xah-dired-sort ()
;;   "Sort dired dir listing in different ways.
;; Prompt for a choice.
;; URL `http://ergoemacs.org/emacs/dired_sort.html'
;; Version 2018-12-23"
;;   (interactive)
;;   (let ($sort-by $arg)
;;     (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" )))
;;     (cond
;;      ((equal $sort-by "name") (setq $arg "-Al "))
;;      ((equal $sort-by "date") (setq $arg "-Al -t"))
;;      ((equal $sort-by "size") (setq $arg "-Al -S"))
;;      ;; ((equal $sort-by "dir") (setq $arg "-Al --group-directories-first"))
;;      (t (error "logic error 09535" )))
;;     (dired-sort-other $arg )))


;;; Evil, vim
(use-package evil
  :init
  (setq
    evil-want-keybinding nil
    evil-want-integration t
    evil-undo-system 'undo-redo)
  :config
  (progn
    (evil-mode)
    (evil-set-initial-state 'Info-mode 'normal)
    (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
    (define-key evil-normal-state-map "\C-d" nil)
    (define-key evil-normal-state-map "\C-j" nil)
    (define-key evil-insert-state-map "\C-d" nil)
    (define-key evil-insert-state-map "\C-k" nil)
    (define-key evil-normal-state-map "\M-." nil)
    (define-key evil-normal-state-map "go" 'org-open-at-point-global)))


(use-package evil-numbers
  :config (progn (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
            (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)))

(comment use-package evil-multiedit
  :config (progn
            ;; Highlights all matches of the selection in the buffer.
            (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

            ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
            ;; incrementally add the next unmatched match.
            (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
            ;; Match selected region.
            (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
            ;; Insert marker at point
            (define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

            ;; Same as M-d but in reverse.
            (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
            (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

            ;; OPTIONAL: If you prefer to grab symbols rather than words, use
            ;; `evil-multiedit-match-symbol-and-next` (or prev).

            ;; Restore the last group of multiedit regions.
            (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

            ;; RET will toggle the region under the cursor
            ;; (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

            ;; ...and in visual mode, RET will disable all fields outside the selected region
            (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

            ;; For moving between edit regions
            ;; (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
            ;; (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
            ;; (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
            ;; (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

            ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
            ))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (progn
    (evil-collection-init)
    (evil-define-key 'normal image-mode-map "y" 'x11-yank-image-at-point-as-image)))


(use-package evil-matchit
  :config
  (progn
    (global-evil-matchit-mode 1)))

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

;;; Markdown
(use-package markdown-mode
  :config
  (setq auto-mode-alist (cons '("\\.mdx$" . markdown-mode) auto-mode-alist)))

 (defun ponelat/expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
           '(try-expand-line-all-buffers)))
      (call-interactively 'hippie-expand)))

;;; FOlding
(use-package origami
  :config
  (global-origami-mode t) )

(use-package ace-link
  :config
  (progn
    (ace-link-setup-default)))

;; Make sure words are treated correctly in evil mode
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(use-package evil-replace-with-register
  :config
  (progn
    (setq evil-replace-with-register-key "r")
    (evil-replace-with-register-install)))

(use-package evil-commentary

  :config (evil-commentary-mode))

(use-package evil-surround
  :config (global-evil-surround-mode t)
  )

;; This seems to work well, compared to `ed/escape-normal-mode', in that it doesn't collapse frames
(use-package evil-escape
  :config
  (global-set-key (kbd "C-g") 'evil-escape))

;; ;;; Hard core escape, super powerful keywords
;; (defun ed/escape-normal-mode ()
;;   "Stop any recursive edit and go into normal mode."
;;   (interactive)
;;   (keyboard-escape-quit)
;;   (evil-normal-state))
;; (global-set-key (kbd "C-g") #'ed/escape-normal-mode)
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;;; Lisp, paredit
(use-package elisp-slime-nav
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

(use-package racket-mode)
(use-package pollen-mode)

(use-package xmlgen)

(show-paren-mode 1)

;;; Pretty symbols, lambda
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

;; (defun ponelat/non-lisp-paredit()
;;   "Turn on paredit mode for non-lisps."
;;   (interactive)
;;   (set (make-local-variable 'paredit-space-for-delimiter-predicates)
;;        '((lambda (endp delimiter) nil)))
;;   (paredit-mode 1))

(use-package paredit
  :config
  ;; Duplicate sexp
  (defun paredit-duplicate-after-point
      ()
    "Duplicates the content of the line that is after the point."
    (interactive)
    ;; skips to the next sexp
    (while (looking-at " ")
      (forward-char))
    (set-mark-command nil)
    ;; while we find sexps we move forward on the line
    (while (and (<= (point) (car (bounds-of-thing-at-point 'sexp)))
		(not (= (point) (line-end-position))))
      (forward-sexp)
      (while (looking-at " ")
	(forward-char)))
    (kill-ring-save (mark) (point))
    ;; go to the next line and copy the sexprs we encountered
    (paredit-newline)
    (set-mark-command nil)
    (yank)
    (exchange-point-and-mark)
    ))


(use-package evil-paredit
  :init
  (add-hook 'cider-repl-mode-hook 'evil-paredit-mode)
  (add-hook 'ielm-mode-hook 'evil-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'evil-paredit-mode))

;;; html,xml, markup, css
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (progn
    (setq emmet-expand-jsx-className? t)
    (add-hook 'sgml-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'web-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'rjsx-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'typescript-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'rjsx-minor-mode-hook #'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook #'emmet-mode) ;; enable Emmet's css abbreviation.
    (add-hook 'markdown-mode #'emmet-mode) ;;
    (evil-define-key 'visual emmet-mode-keymap (kbd "C-l") #'emmet-wrap-with-markup))
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

  (add-hook 'which-func-functions 'nxml-where t t)
  (setq auto-mode-alist (cons '("\\.xml$" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.xsl$" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.xhtml$" . nxml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.page$" . nxml-mode) auto-mode-alist))

  (autoload 'xml-mode "nxml" "XML editing mode" t)

  (eval-after-load 'rng-loc
    '(add-to-list 'rng-schema-locating-files "~/.emacs.d/schema/schemas.xml")))

(add-hook 'find-file-hook 'xml-find-file-hook t)
;;; Ag, RipGrep
;; use the_silver_searcher when available
;; (use-package ag :if (executable-find "ag"))

(use-package rg)

;;; Clipboard

;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(comment unless window-system
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

;; From https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs
(defun x11-yank-image-at-point-as-image ()
  "Yank the image at point to the X11 clipboard as image/png."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq (car image) 'image)
        (let ((data (plist-get (cdr image) ':data))
              (file (plist-get (cdr image) ':file)))
          (cond (data
                 (with-temp-buffer
                   (insert data)
                   (call-shell-region
                    (point-min) (point-max)
                     "xclip -i -selection clipboard -t image/png")
                   (message "Copied image to clipboard!")))
                (file
                 (if (file-exists-p file)
                     (start-process
                      "xclip-proc" nil "xclip"
                      "-i" "-selection" "clipboard" "-t" "image/png"
                       "-quiet" (file-truename file))
                   (message "Copied image to clipboard!")))
                (t
                 (message "The image seems to be malformed."))))
      (message "Point is not at an image."))))

;;; Autosave
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Reloads buffer if file changes on disk. Won't overwrite modifications
(global-auto-revert-mode t)

;; These files have a pattern of \.#.* and are used to prevent multiple users editing the file.
;; I'm disabling it because it causes create-react-app to have a fuss.
(setq create-lockfiles nil)

;;; Cuccumber, test, geherkin
(use-package feature-mode)


;;; Yaml
(use-package yaml-mode)


(defun yaml-delete-backward-word-electric (arg)
  "Delete backward one word or back-dent the current line.
If invoked following only whitespace on a line, will back-dent to the
immediately previous multiple of `yaml-indent-offset' spaces."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column)) (bolp))
      (evil-delete-backward-word)
    (let ((ci (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to (* (/ (- ci (* arg yaml-indent-offset))
		       yaml-indent-offset)
		    yaml-indent-offset)))))


(use-package yaml-imenu
  :config
  (yaml-imenu-enable))

(straight-use-package
  '(yaml :type git :host github :repo "zkry/yaml.el"))

(defun xah-hash-to-list (@hash-table)
  "Return a list that represent the @HASH-TABLE
Each element is a list: '(key value).

http://ergoemacs.org/emacs/elisp_hash_table.html
Version 2019-06-11"
  (let ($result)
    (maphash
     (lambda (k v)
       (push (list k v) $result))
     @hash-table)
    $result))

(require 'yaml)
(defun ponelat/edit-url--normalize-query-item (val)
  "Changes t to \"true\". nil => \"\". Leaves all other values"
  (message (format "josh: %s" val))
  (cond
   ((equal val t) "true")
   ((equal val :false) "false")
   ((equal val :null) "")
   ((equal val nil) "")
   (t val)))

(defun ponelat/edit-url--normalize-query-list (l)
  "It changes t => \"true\" and nil => \"\""
  (mapcar (lambda (pair)
	          (list
              (car pair)
              (ponelat/edit-url--normalize-query-item (car (cdr pair)))))
	  l))

(defun ponelat/edit-url--build-url-from-httphash (hash)
  "HASH is a hashtable with (base \"https://localhost:3000\" path \"/one\" query <hash>). Turns into into a URL again."
  (let* ((base (gethash 'base hash))
          (path (gethash 'path hash))
          (query (gethash 'query hash))
          (query-str (if (hash-table-p query)
                       (url-build-query-string
                         (ponelat/edit-url--normalize-query-list (xah-hash-to-list query))))))
    (concat
      base ; http://localhost:3000
      path ; /one
      (if query-str (format "?%s" query-str)))))

(defun ponelat/edit-url ()
  "It opens a temp buffer with the exploded (via `ponelat/explode-url') URL."
  (interactive)
  (let* (($bounds (bounds-of-thing-at-point 'url))
          ($url (buffer-substring-no-properties (car $bounds) (cdr $bounds)))
          (xbuff (generate-new-buffer (format "*Editing: %s*" $url)))
          ($point (point))
          ($buffer (buffer-name)))
    (princ
      (yaml-encode (ponelat/explode-url $url))
      xbuff)
    (switch-to-buffer xbuff)
    (yaml-mode)
    (ponelat/edit-url-mode)
    (setq-local
      edit-url--position $point
      edit-url--buffer $buffer
      edit-url--bounds $bounds)))

(defun ponelat/implode-edit-url-buffer ()
  "Decodes the region from yaml to lisp."
  (interactive)
  (let* ((data (buffer-substring-no-properties (point-min) (point-max)))
	 (hash (yaml-parse-string data))
	 (url (ponelat/edit-url--build-url-from-httphash hash))
	 (pos edit-url--position)
	 (bounds edit-url--bounds)
	 (buf edit-url--buffer))
    (kill-buffer)
    (switch-to-buffer buf)
    (replace-bounds url bounds)
    (goto-char pos)))

(define-minor-mode ponelat/edit-url-mode
  "Edit a URL then use C-c to replace the URL or C-k to cancel."
  :lighter " Edit URL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'ponelat/implode-edit-url-buffer)
            (define-key map (kbd "C-c C-k") (lambda () (interactive) (kill-buffer)))
            map))

;;; HTTP, REST, Swagger
(use-package restclient
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))))

;; OpenAPI yaml mode
(straight-use-package
  '(openapi-yaml-mode :type git :host github :repo "magoyette/openapi-yaml-mode"))

;; Swagger UI(ish) in Emacs
(use-package swagg
  :straight (:host github :repo "isamert/swagg.el")
  :config
  (setq
   swagg-definitions
   '((:name "GitHub"
	    :json "https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json"
	    :base "https://api.github.com")
     (:name "Petstore3"
	    :json "https://petstore3.swagger.io/api/v3/openapi.json"
	    :base "https://petstore3.swagger.io/api/v3")
     (:name "SwaggerHub"
	    :json "https://api.swaggerhub.com/apis/swagger-hub/registry-api/1.0.67"
	    :base "https://api.swaggerhub.com")
     ))
  )


;; Add command to jump into a restclient scratch, stored by projectile project.
(defun ponelat/jump-to-restclient (&optional project)
  "Jump to the restclient buffer for making rest calls.
Will use `projectile-default-project-name' .rest as the file name."
  (interactive)
  (let* ((basedir (format "%s/restclient-scratch" ponelat/projects-dir))
          (project-name (cond
                          (project (projectile-default-project-name project))
                          ((projectile-project-root) (projectile-default-project-name (projectile-project-root)))
                          (t "global")))
          (scratch-file (format "%s/%s.rest" basedir project-name)))
    (find-file scratch-file)))

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

;; (use-package typescript-mode
;;   :after tree-sitter
;;   :config
;;   ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
;;   ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
;;   (progn
;;     (define-derived-mode typescriptreact-mode typescript-mode
;;       "TypeScript TSX"))

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter typescript parser
;;   ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.njk\\'" . web-mode))
  ;; :init
  ;; (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript-tsx")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)


;;; UML PlantUML
(use-package plantuml-mode
  :config
  (add-to-list 'auto-mode-alist '(".puml\\'" . plantuml-mode))
  (setq plantuml-output-type "png"
    plantuml-default-exec-mode 'jar
    plantuml-java-args '("-Djava.awt.headless=true" "-jar")))

;;; CSV
(use-package csv-mode)

;;; TOML
(use-package toml-mode)

;;; Docker, Dockerfile
(use-package dockerfile-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
  )

;;; JSON
(use-package json-mode)

;;; Nginx,HAproxy,Caddy Server
(use-package caddyfile-mode)

;;; LSP, language server protocol, eglot
(comment use-package lsp-mode
  :hook
  '((lsp-mode . lsp-enable-which-key-integration) 
    (typescript-mode . lsp-deferred))
  :bind ("C-x C-l" . lsp-command-map)
  :config
  ;; (add-to-list 'lsp-language-id-configuration '(".*\\.njk" . "html"))

  ;; Add Deno to the list of LSP clients
  (add-to-list 'lsp-language-id-configuration '(typescript-mode . "deno"))
  (add-to-list 'lsp-language-id-configuration '(js-mode . "deno"))
  (add-to-list 'lsp-language-id-configuration '(js2-mode . "deno"))
  (add-to-list 'lsp-language-id-configuration '(web-mode . "deno"))

  ;; Optionally disable ts-ls
  (setq lsp-disabled-clients '(ts-ls))


  (setq lsp-idle-delay 2.000
    lsp-eslint-enable nil
    lsp-clients-deno-server-command '("deno" "lsp")
    lsp-enable-suggest-server-download nil
    ;; lsp-eslint-package-manager "yarn"
    lsp-eslint-run "onSave")

  :commands (lsp lsp-deferred))


(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" my/format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(defun ponelat/setup-lsp-mode ()
  (message "my/setup-lsp-mode called")
  (flycheck-mode 1)
  (company-mode 1)
  (yas-minor-mode-on)
  (flyspell-prog-mode)
  (lsp-enable-which-key-integration)
  (lsp-diagnostics-mode 1)
  (lsp-completion-mode 1)
  ;; (when (lsp-feature? "textDocument/formatting")
  ;;   (setq my/format-buffer-function 'lsp-format-buffer)))
  )

(use-package format-all)
(defvar-local my/format-buffer-function 'format-all-buffer
  "Function to call in order to format the current buffer.")
(defun my/format-buffer ()
  "Run `my/format-buffer-function' to format the current buffer."
  (interactive)
  (funcall my/format-buffer-function))
(bind-key "C-c f f" 'my/format-buffer)

;; (comment use-package lsp-mode
;;   :commands lsp
;;   :config
;;   (add-to-list 'lsp-language-id-configuration '(typescriptreact-mode . "deno"))
;;   (setq lsp-disabled-clients '(ts-ls))
;;   :custom
;;   (lsp-log-io nil)
;;   (lsp-print-performance nil)
;;   (lsp-report-if-no-buffer nil)
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-enable-snippet t)
;;   (lsp-auto-guess-root t)
;;   (lsp-restart 'iteractive)
;;  ;(lsp-session-file)
;;   (lsp-auto-configure nil)
;;  ;(lsp-document-sync-method)
;;   (lsp-auto-execute-action nil)
;;   (lsp-eldoce-render-all nil)
;;   (lsp-enable-completion-at-point t)
;;   (lsp-enable-xref t)
;;   (lsp-diagnostics-provider :flycheck)
;;   (lsp-enable-indentation t)
;;   (lsp-enable-on-type-formatting nil)
;;   (lsp-before-save-edits nil)
;;   (lsp-imenu-show-container-name t)
;;   (lsp-imenu-container-name-separator "/")
;;   (lsp-imenu-sort-methods '(kind name))
;;   (lsp-response-timeout 5)
;;   (lsp-enable-file-watchers nil)
;;   (lsp-server-trace nil)
;;   (lsp-semantic-highlighting nil)
;;   (lsp-enable-imenu t)
;;   (lsp-signature-auto-activate t)
;;   (lsp-signature-render-documentation nil)
;;   (lsp-enable-text-document-color nil)
;;   (lsp-completion-provider :capf)
;;   (gc-cons-threshold 100000000)
;;   (read-process-output-max (* 3 1024 1024))
;;   :hook ((lsp-mode . ponelat/setup-lsp-mode))
;;   :bind (:map lsp-mode-map
;;          ("C-c f" . hydra-lsp/body)))

;;; Corfu completion

(use-package corfu
      :ensure t
      ;; Optional customizations
      :custom
      (corfu-cycle t)                 ; Allows cycling through candidates
      (corfu-auto t)                  ; Enable auto completion
      (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
      (corfu-auto-delay 0)            ; No delay for completion
      (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
      (corfu-preview-current 'insert) ; insert previewed candidate
      (corfu-preselect 'prompt)
      (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
      ;; Optionally use TAB for cycling, default is `corfu-complete'.
      :bind (:map corfu-map
                  ("M-SPC"      . corfu-insert-separator)
                  ("TAB"        . corfu-next)
                  ([tab]        . corfu-next)
                  ("S-TAB"      . corfu-previous)
                  ([backtab]    . corfu-previous)
                  ("S-<return>" . corfu-insert)
                  ("RET"        . corfu-insert))

      :init
      (global-corfu-mode)
      (corfu-history-mode)
      (corfu-popupinfo-mode) ; Popup completion info
      :config
      (add-hook 'eshell-mode-hook
                (lambda () (setq-local corfu-quit-at-boundary t
                                       corfu-quit-no-match t
                                       corfu-auto nil)
                  (corfu-mode))
                nil
                t))


(defun ponelat/typescript-language-server-bin-and-args ()
  "This returns a list of the tls bin path and its arguments.
e.g., (\"../path/bin\" \"--stdio\" \"--tserver-path\" \"../lib\" "

  (let ((path-to-typescript-lib-dir
	 (expand-file-name
	  "../../lib" ;; Get to the lib directory.
	  (file-truename ;; Handle symlinks
	   (executable-find "tsserver")))) ;; Find tsserver. 
	(path-to-typescript-language-server
	 (executable-find "typescript-language-server")))

     `(,path-to-typescript-language-server
	"--stdio"
	"--tsserver-path"
	,path-to-typescript-lib-dir)))

(ponelat/typescript-language-server-bin-and-args)


;;; LSP from ovistoica
  (use-package lsp-mode
    :ensure t
    :hook ((lsp-mode . lsp-diagnostics-mode)
	   (lsp-mode . lsp-enable-which-key-integration)
	   ((tsx-ts-mode
	     typescript-ts-mode
	     js-ts-mode) . lsp-deferred))
    :custom
    (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
    (lsp-completion-provider :none)       ; Using Corfu as the provider
    :config
    (comment progn ;; TS-LS
      (setq
       lsp-clients-typescript-tls-path (executable-find "typescript-language-server")
       )
      )
    (comment progn ;; Deno. God damn deno vs ts-ls nonsense
      (setq 
       lsp-disabled-clients '(ts-ls)
       lsp-clients-deno-server-command '("deno" "lsp")
       lsp-enable-suggest-server-download nil)))

    ;; (comment add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "deno")))

(use-package lsp-ui
      :ensure t
      :after lsp-mode
      :commands
      (lsp-ui-doc-show
       lsp-ui-doc-glance)
      :bind (:map lsp-mode-map
                  ("C-c C-d" . 'lsp-ui-doc-glance))
      :after (lsp-mode evil)
      :config (setq lsp-ui-doc-enable t
                    evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                    lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                    lsp-ui-doc-include-signature t       ; Show signature
                    lsp-ui-doc-position 'at-point))

;;; LSP - JavaScript / Typescript


;; Optional Flutter packages
;; Flutter dart
;; (use-package dart-mode)
;; (use-package lsp-dart
;;   :hook '((dart-mode . lsp))
;;   :config  (setq lsp-dart-sdk-dir "~/snap/flutter/common/flutter/bin/cache/dart-sdk"))

(comment use-package hover) ;; run app from desktop without emulator

;; (use-package lsp-ui )

;; (use-package company-tabnine
;;   :disabled
;;   :config
;;   (add-to-list 'company-backends #'company-tabnine))

(use-package gradle-mode)
(use-package groovy-mode)

(defun ponelat/shell-command-on-region-to-string (begin end command)
  "It wraps `shell-command-on-region' using BEGIN, END and COMMAND, so that it returnes a string."
  (let ((buf (generate-new-buffer "*Shell Output*")))
    (shell-command-on-region begin end command buf)
    (let ((result (with-current-buffer buf (buffer-string))))
      (progn
        (kill-buffer buf)
        result))))

(defun ponelat/kill-command-on-region (shell-cmd)
  "Convert the BEGIN END region into JSON.  Putting the result into the kill ring."
  (save-mark-and-excursion
    (let* ((begin (if mark-active (min (point) (mark)) (point-min)))
	   (end (if mark-active (max (point) (mark)) (point-max)))
	   (output (ponelat/shell-command-on-region-to-string begin end shell-cmd)))
      (kill-new output)
      (message output))))

(defun ponelat/yaml-to-json ()
  (interactive)
  (ponelat/kill-command-on-region
   "bb '(println (json/generate-string (clj-yaml.core/parse-stream *in*) {:pretty true}))'"))

(defun ponelat/json-to-yaml ()
  (interactive)
  (ponelat/kill-command-on-region
   "bb '(println (clj-yaml.core/generate-string (json/parse-stream *in*) {:flow-style :auto}))'"))

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
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(defun assoc-recursive (alist &rest keys)
  "Recursively search ALIST for KEYS."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(use-package web-beautify)

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map (kbd "C-c C-j") nil)
  (define-key rjsx-mode-map (kbd "C-c r") #'rjsx-rename-tag-at-point))

;;; Boostrap, template code
(defconst ponelat/nix-shell-nodejs
"{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs-16_x
    yarn

    bashInteractive
  ];

  shellHook = ''
    echo Execution environment for Nodejs projects
  '';
}")


(defconst ponelat/nix-shell-java
"{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    jdk_headless
    maven
    gradle

    bashInteractive
  ];

  shellHook = ''
    echo Execution environment for Java projects
  '';
}")

(defconst ponelat/postcss-config
"module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
}
")

(defconst ponelat/tailwindcss-deps '(tailwindcss postcss autoprefixer))

(defconst ponelat/tailwindcss-index-css "
@tailwind base;
@tailwind components;
@tailwind utilities;
")

(defconst ponelat/tailwind-config
  "

module.exports = {
  content: [
    \"./src/**/*.{js,jsx,ts,tsx}\",
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}

")


(defun ponelat/insert-shell-nix ()
  "Creates a shell.nix in project root and inserts template"
  (interactive)
  (let* (($templates `(("Nodejs" . ,ponelat/nix-shell-nodejs) ("Java" . ,ponelat/nix-shell-java)))
	 ($buffer (read-file-name "Shell.nix: " (projectile-project-root) nil nil "shell.nix"))
	 ($template
	  (cdr (assoc
		(completing-read "Template: " $templates)
		$templates))))
    (progn
      (find-file $buffer)
      (insert $template))))

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


;;; Flycheck, syntax, lint
(use-package flycheck
  :diminish flycheck-mode
  :config
    (setq flycheck-highlighting-mode 'lines)
    (global-flycheck-mode))

;; So that we can access `./node_modules/.bin/eslint` mostly
(use-package add-node-modules-path
  :init
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path)))
    (eval-after-load 'js2-jsx-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path)))

;;; Jq
(use-package jq-mode)

;;; Voice and Talon
(use-package talonscript-mode)


;;; Clojure
(use-package cider
  :config
  (progn
    ;; (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
    ;; (setq cljr-suppress-no-project-warning t)
    (general-define-key
     :states '(normal insert)
     :keymaps 'cider-mode-map
     "C-c C-S-c" 'cider-eval-buffer)

    (setq cider-clojure-cli-aliases ":cider"
	  cider-repl-display-help-banner nil)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook #'evil-local-mode)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)))

(defun ponelat/cider-babashka ()
  "Creates a babashka REPL and runs `cider-connect-clj'"
  (interactive)
  (async-shell-command "nix-shell -p babashka --run \"bb --nrepl-server\"" "*Babashka NREPL*")
  (run-with-timer 5 nil #'cider-connect-clj '(:host "localhost" :port "1667")))


;;; Clojure
(use-package clj-refactor)
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))
;;; Clojure, flycheck
(use-package flycheck-joker)


; Auto load buffer, when in jacked-in
;; (add-hook 'cider-mode-hook
;;   (lambda ()
;;     (add-hook 'after-save-hook 'cider-load-buffer nil 'make-it-local)))

;;; Autocomplete, company, snippets, ivy, helm, company, etc

;;; Git, magit
(use-package magit)

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

  :config (yas-global-mode 1))

(use-package react-snippets)

;;; npm
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
          (script-keys-with-base (append script-keys
                                   '((install . install)
                                      (ci . ci)
                                      (build . build))))
          (choice (completing-read "Npm: " script-keys-with-base))
          (project-name (ponelat/last-dir project-dir))
          (run-prefix
            (cond
              ((equal choice "install") "")
              ((equal choice "test") "")
              ((equal choice "ci") "")
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

(defun ponelat/shell-command-to-list (cmd)
  "Execute CMD and returns the results as a list."
  (split-string (shell-command-to-string cmd) "\n"))

(defun ponelat/read-directory-glob (glob &optional base-dir)
  "Returns a list of files from DIR."
  (ponelat/shell-command-to-list (format "cd %s && ls %s" (or base-dir ".") glob)))

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

(defun ponelat/get-hostnames-k8s ()
  "It'll unlink TARGET-DIR from BASE-DIR project."
  (interactive)
  (let ((ip-addr (read-from-minibuffer "IPv4 of VM: ")))
    (async-shell-command (format "~/bin/get-hostnames.sh %s" ip-addr))))

(defun ponelat/projectile-npm-run ()
  "Run an npm command in the current project."
  (interactive)
  (ponelat/npm-run (projectile-project-root)))

(defun ponelat/projectile-project-run (prefix)
  "Run build command in the current project."
  (interactive "P")
  (ponelat/project-run prefix (projectile-project-root)))

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

;;; fasd, files, recent
(use-package fasd
  :config
  (global-fasd-mode 1))


;;; Vertico
(progn
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Grow and shrink minibuffer
    (setq resize-mini-windows t)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist :init (savehist-mode))
  (use-package recentf :init (recentf-mode))

(use-package orderless
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

  ;; Enable vertico
  (use-package vertico
    :init
    (vertico-mode)
    :bind
    (:map vertico-map
      ("C-j" . #'vertico-next)
      ("C-k" . #'vertico-previous)
      ("C-y" . #'vertico-save)))

  ;; Example configuration for Consult
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
            ("C-*" . ponelat/swiper-region-or-symbol)
            ("C-&" . (lambda () (interactive) (rg (thing-at-point-or-mark 'symbol) "*.*" (projectile-project-root))))

            ("C-c h" . consult-history)
            ("C-c m" . consult-mode-command)
            ("C-c b" . consult-bookmark)
            ("C-c k" . consult-kmacro)
            ;; C-x bindings (ctl-x-map)
            ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
            ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
            ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
            ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
            ;; Custom M-# bindings for fast register access
            ("M-#" . consult-register-load)
            ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
            ("C-M-#" . consult-register)
            ;; Other custom bindings
            ("M-y" . consult-yank-pop)                ;; orig. yank-pop
            ("<help> a" . consult-apropos)            ;; orig. apropos-command
            ;; M-g bindings (goto-map)
            ("M-g e" . consult-compile-error)
            ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
            ("M-g g" . consult-goto-line)             ;; orig. goto-line
            ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
            ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
            ("M-g m" . consult-mark)
            ("M-g k" . consult-global-mark)
            ("M-g i" . consult-imenu)
            ("M-g I" . consult-imenu-multi)
            ;; M-s bindings (search-map)
            ("M-s f" . consult-find)
            ("M-s F" . consult-locate)
            ("M-s g" . consult-grep)
            ("M-s G" . consult-git-grep)
            ("M-s r" . consult-ripgrep)
            ("C-s" . consult-line)
            ("M-s L" . consult-line-multi)
            ("M-s m" . consult-multi-occur)
            ("M-s k" . consult-keep-lines)
            ("M-s u" . consult-focus-lines)
            ;; Isearch integration
            ("M-s e" . consult-isearch)
            :map isearch-mode-map
            ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
            ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
            ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
            ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch


    ;; Enable automatic preview at point in the *Completions* buffer.
    ;; This is relevant when you use the default completion UI,
    ;; and not necessary for Vertico, Selectrum, etc.
    ;; :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0
      register-preview-function #'consult-register-format)

    (setq consult-line-start-from-top t)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Optionally replace `completing-read-multiple' with an enhanced version.
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
      consult-theme
      :preview-key '(:debounce 0.2 any)
      consult-ripgrep consult-git-grep consult-grep
      consult-bookmark consult-recent-file consult-xref
      :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")


    (progn
      (defun define-minibuffer-key (key &rest defs)
	"Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
	(define-key minibuffer-local-map key
		    (list 'menu-item nil defs :filter
			  (lambda (d)
			    (plist-get d (completion-metadata-get
					  (completion-metadata (minibuffer-contents)
							       minibuffer-completion-table
							       minibuffer-completion-predicate)
					  'category))))))

      (define-minibuffer-key "\C-d" 'file #'find-file))

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; Optionally configure a function which returns the project root directory.
    ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
    (setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
    ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
    )

  ;; (use-package consult-dir
  ;;   :straight '(consult-dir :host github :repo "karthink/consult-dir"))
  ;;   :bind (("C-x C-d" . consult-dir)
  ;;           :map vertico-map
  ;;           ("C-x C-d" . consult-dir)
  ;;           ("C-x C-j" . consult-dir-jump-file)))

  )

;;; Swiper
(defun ponelat/swiper-region-or-symbol ()
  (interactive)
  (if (region-active-p)
    (let* (($beg (region-beginning))
            ($end (region-end))
            ($str (regexp-quote (buffer-substring-no-properties $beg $end))))
      (deactivate-mark)
      (consult-line $str))
    (consult-line (thing-at-point-or-mark 'symbol))))

;;; Projects
(use-package projectile
  :diminish projectile
  :config
  (progn
    (setq projectile-git-command "git ls-files -zco --exclude-standard")
    (define-key projectile-command-map (kbd "n") #'ponelat/projectile-project-run)
    (global-set-key (kbd "C-j") nil)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode 1)))

;;; Fuzzy, ido
(comment use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

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

(use-package git-timemachine)

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

;;; Whitespace, text, ascii

;; For Manning's style guide, around code snippet text lengths
(setq whitespace-line-column 55)
(setq indent-tabs-mode nil)
(use-package lorem-ipsum)


;;; GhostText, browser, live
(use-package atomic-chrome
  )
;;; Copy as format (for pasting into GitHub/Jira/Confluence)
(use-package copy-as-format)
;;; Jira
(comment use-package org-jira
  :init
  (setq jiralib-url "https://smartbear.atlassian.net")
  :config
  (setq org-jira-custom-jqls
    '(
       (:jql "project = CC AND component = SwaggerHub AND resolution = Unresolved order by lastViewed DESC"
         :limit 10
         :filename "swaggerhub-cc")
       (:jql "assignee = currentUser() and project = CC AND component = SwaggerHub AND resolution = Unresolved order by lastViewed DESC"
         :limit 10
         :filename "swaggerhub-cc-mine"))))


(comment use-package jira-markup-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.confluence$" . jira-markup-mode))
  (add-to-list 'auto-mode-alist '("\\.jira" . jira-markup-mode))
  (add-to-list 'auto-mode-alist '("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode)))

;;; GitHub
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :bind (("C-c C-g" . #'copilot-mode)
	 ("C-k" . #'copilot-accept-completion))
  :ensure t)


;; Simple fib function



;; you can utilize :map :hook and :config to customize copilot
(use-package forge
 :after magit)

;; (use-package gist)

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

(defun ponelat/ponelat-copy-pwd-path-file ()
  "It kills the current pwd"
  (interactive)
  (kill-new (pwd)))

;;; Ledger
(progn
  (use-package ledger-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
      (setq ledger-report-use-native-highlighting t))
    )

  ;; Adding price data into kill-ring
  (comment progn  ;; Need to fix the API call.
    (require 'url)
    (require 'json)

    (defun thing-at-point-date ()
      "Return the date at point as a string in YYYY-MM-DD format, or nil if none is found."
      (save-excursion
	(let ((line (thing-at-point 'line t))
	      (date-regex "\\(\\([0-9]\\{4\\}[-/]?\\)?[0-9]\\{2\\}[-/]?[0-9]\\{2\\}\\)"))
	  (when (and line (string-match date-regex line))
	    (let* ((matched-date (match-string 1 line))
		   (parts (split-string matched-date "[-/]"))
		   (year (if (= (length (car parts)) 4)
			     (car parts)
			   (format-time-string "%Y")))
		   (month (if (= (length (car parts)) 4)
			      (cadr parts)
			    (car parts)))
		   (day (if (= (length (car parts)) 4)
			    (caddr parts)
			  (cadr parts))))
	      (format "%s-%02d-%02d"
		      year
		      (string-to-number month)
		      (string-to-number day)))))))

    (put 'date 'thing-at-point 'thing-at-point-date)

    (defun ponelat/fetch-usd-zar-price-for-ledger (date)
      "Fetch USD to ZAR spot price for DATE and format it for ledger-cli.
   DATE should be in the format YYYY-MM-DD.
   If called interactively with no argument, use the date at point.
   The result is added to the kill-ring for pasting."
      (interactive (list (or (thing-at-point 'date)
                             (read-string "Enter date (YYYY-MM-DD): "
					  (format-time-string "%Y-%m-%d")))))
      (let* ((url (format "https://api.exchangerate.host/%s?base=USD&symbols=ZAR" date))
             (buffer (url-retrieve-synchronously url))
             (json-object-type 'alist)
             (json-array-type 'list)
             price 
             formatted-string )
	(message (format "Url: %s" url))
	(with-current-buffer buffer
	  (goto-char (point-min))
	  (re-search-forward "^$")
	  (delete-region (point-min) (point))
	  (setq price (cdr (assoc 'ZAR (cdr (assoc 'rates (json-read))))))
	  (message (format "Price? %s" price)))
	(kill-buffer buffer)
	(setq formatted-string 
              (format "P %s 00:00:00 USD R%s" date (format "%.6f" price)))
	(kill-new formatted-string)
	(message "Added to kill-ring: %s" formatted-string))))

  (defun ponelat/insert-ledger-account-into-string ()
    "Completes and inserts an account from ledger file"
    (interactive)
    (save-mark-and-excursion
      (when-let ((bounds 
		  (or (ponelat/use-region-bounds)
		      (bounds-of-thing-at-point 'filename)))
		 (account 
		  (completing-read "Account: " (read-lines "/home/josh/projects/accounts/accounts.txt"))))
	(replace-bounds account bounds))))

  (defun ponelat/insert-ledger-batch-rule ()
    "Grabs a rule from batch-rules.edn to add to rules.edn file."
    (interactive)
    (insert
      (completing-read "Rule: " (read-lines "/home/josh/projects/accounts/batch-rules.edn")))))

;; "^\\(=====[ 	]+\\)\\([^\t\n].*?\\)\\(\\([ \t]+=====\\)?[ \t]*\\(?:\n\\|\\'\\)\\)"
(defun ponelat/grab-imenu-of (filename)
  "Grabs the imenu-list of FILENAME"
  (save-current-buffer
    (set-buffer (find-file-noselect filename))
    (imenu--make-index-alist)))

;;; Asciidoc, adoc
(use-package adoc-mode
  :config
  (progn

    (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
    ;; (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
    (define-key adoc-mode-map (kbd "C-c C-c") #'ponelat/quick-build)

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
                (cons (cons (format "%s %s " (make-string (+ 1 title-level) ?*) title-text) title-pos) index-alist)))))
        (list (cons "Title" (nreverse index-alist)))))

    (defun ponelat/adoc-imenu-expresssions ()
      (interactive)
      "Create only the top level titles. With proper titles"
      (setq-local imenu-create-index-function #'ponelat/adoc-imenu-create-index)
      ;; (setq imenu-create-index-function #'imenu-default-create-index-function)
      (setq-local imenu-prev-index-position-function nil))


    (add-hook 'adoc-mode-hook #'ponelat/adoc-imenu-expresssions)
    (add-hook 'adoc-mode-hook #'visual-line-mode) ))

(defun ponelat/adoc-imenu-to-org-headings (&optional filename)
      "Captures the imenu into the kill ring.  Optionally use FILENAME instead of current buffer."
      (interactive)
  (let* ((imenu-data
           (if filename
             (ponelat/grab-imenu-of filename)
             (imenu--make-index-alist)))
          (titles-list (cdr (car (cdr imenu-data))))
          (titles (mapcar (lambda (title-thing) (car title-thing)) titles-list)))
    (pp (kill-new (format "%s" (string-join titles "\n"))))))

;;; IBM Box symbols
;; ┌ ┬ ┐ ├ ┼ ┤ └ ┴ ┘ ─ │

;;; org-mode pre
;; (defun ponelat/org-mode-styles ()
;;   "It styles org mode."
;;   (interactive)
;;   ;; Headline sizes
;;   (progn
;;     (let* ((variable-tuple
;;              (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                ((x-list-fonts "Noto Sans")         '(:font "Noto Sans"))
;;                ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;             (fixed-tuple
;;               (cond ((x-list-fonts "Noto Mono") '(:font "Noto Mono"))
;;                 (nil (warn "Cannot find a Sans Serif Font.  Install Noto Sans"))))
;;             (base-font-color     (face-foreground 'default nil 'default))
;;             (headline           `(:inherit default :foreground ,base-font-color)))
;;       (progn
;;         (custom-theme-set-faces
;;           'user
;;           `(variable-pitch ((t ( ,@variable-tuple :height ,(face-attribute 'default :height) :weight light))))
;;           `(fixed-pitch    ((t ( ,@fixed-tuple :slant normal :weight normal :height ,(face-attribute 'default :height) :width normal)))))

;;         (custom-theme-set-faces
;;           'user
;;           `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.0))))
;;           `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.0))))
;;           `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.1))))
;;           `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.1))))
;;           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
;;           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
;;           `(org-level-1 ((t (,@headline :height 1.3))))
;;           `(org-document-title ((t (,@headline ,@variable-tuple :height 3.0 :weight bold :underline nil))))
;;           '(org-block                 ((t (:inherit fixed-pitch))))
;;           '(org-table                 ((t (:inherit fixed-pitch))))
;;           '(org-todo                  ((t (:inherit fixed-pitch))))
;;           '(org-code                  ((t (:inherit fixed-pitch))))
;;           '(org-indent                ((t (:inherit org-hide fixed-pitch))))
;;           '(org-hide                  ((t (:inherit fixed-pitch))))
;;           '(org-document-info-keyword ((t (:inhert (shadow) :height 0.8))))
;;           '(org-document-info         ((t :height 1.0)))
;;           '(org-link                  ((t (:inherit (fixed-pitch) :weight semi-bold))))
;;           '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
;;           '(org-property-value        ((t (:inherit fixed-pitch))) t)
;;           '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
;;           '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;           '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))))
;;     (ponelat/face-extras)))

;; (use-package ob-restclient)


;;; org-mode
;; (use-package org
;;   :hook ((org-mode . org-display-inline-images))
;;   :config
;;   (progn
;;     (define-key org-mode-map (kbd "C-c ;") nil)
;;     ;; (define-key org-mode-map (kbd "C-j") nil)
;;     (define-key org-mode-map (kbd "C-c C-'") 'org-cycle-list-bullet)
;;     (global-set-key (kbd "C-c C-L") #'org-store-link)
;;     (add-hook 'org-open-link-functions #'ponelat/org-open-link-shub)

;;     (setq org-directory ponelat/org-dir
;;       org-agenda-files (list ponelat/org-dir)
;;       org-default-notes-file "notes.org"
;;       org-confirm-elisp-link-function nil
;;       org-src-fontify-natively t
;;       org-insert-heading-respect-content t
;;       org-agenda-start-day "1d"
;;       org-agenda-span 5
;;       org-agenda-start-on-weekday nil
;;       org-deadline-warning-days 1
;;       org-confirm-babel-evaluate nil
;;       org-export-with-toc nil
;;       org-export-initial-scope 'subtree
;;       org-goto-interface 'outline-path-completionp
;;       org-src-preserve-indentation t
;;       org-outline-path-complete-in-steps nil)

;; ;;; Babel
;;     (progn
;;       (setq org-babel-load-languages
;;         (append
;;           org-babel-load-languages
;;           '((js . t)
;;              (shell . t)
;;              (restclient . t))))
;;       (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;       (add-to-list 'org-babel-tangle-lang-exts '("js" . "js")))

;; ;;; Org link keymap
;;     (comment progn
;;       ;; TODO: Figure out how to make this work for all "TYPES"
;;       (org-link-set-parameters
;;         "file"
;;         :keymap (let ((map (copy-keymap org-mouse-map)))
;;                   (define-key map (kbd "TAB") 'org-toggle-link-display)
;;                   map)))

;; ;;; Styles
;;     (setq
;;       org-hide-emphasis-markers t
;;       org-startup-indented t
;;       org-hide-leading-stars t)

;;     (font-lock-add-keywords 'org-mode
;;       '(("^ *\\([-]\\) "
;;           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "—"))))))
;;     (add-hook 'org-mode-hook 'variable-pitch-mode)
;;     (add-hook 'org-mode-hook 'visual-line-mode)
;;     (add-hook 'org-mode-hook #'ponelat/org-mode-styles)
;;     (add-hook 'org-mode-hook (lambda () (setq electric-pair-local-mode nil)))

;; ;;; Org refile
;;     (setq org-refile-use-outline-path 'file)
;;     (setq org-refile-allow-creating-parent-nodes 'confirm)
;;     (setq org-outline-path-complete-in-steps nil)
;;     (setq org-refile-targets
;;       '((nil :maxlevel . 3)
;;          (org-agenda-files :maxlevel . 3)))

;; ;;; TODOs labels
;;     (setq org-todo-keywords
;;       '((sequence "NEXT(n)" "TODO(t)" "InProgress(p)" "|" "DONE(d!)")
;;          (sequence "MEET(m)" "BLOCKED(b@)" "|" "CANCELLED(c@)")
;;          (sequence "DISCUSS(i/@)" "|" "DONE(d!)")))
;; ;;; org templates
;;     (setq org-capture-templates
;;       '(("t" "Todo" entry (file (lambda () (concat org-directory "/notes.org")))
;;           "* TODO %?\n  %i\n  %a")
;;          ("b" "Blank Point" entry (file (lambda () (concat org-directory "/notes.org")))
;;            "* %?")
;;          ("d" "Today" entry (file+olp+datetree (lambda () (concat org-directory "/today.org")))
;;            "* TODO %? \nAdded: %T")
;;          ("s" "Shopping" entry (file (lambda () (concat org-directory "/shopping.org")))
;;            "* %?")
;;          ("j" "Jokes" entry (file (lambda () (concat org-directory "/jokes.org")))
;;            "* %?")
;;          ("n" "Notes" entry (file (lambda () (concat org-directory "/notes.org")))
;;            "* %?\n  %i\n  %a")
;;          ("h" "Thought" entry (file (lambda () (concat org-directory "/thoughts.org")))
;;            "* LOOSE %?\n  %i\n  %a")))))


;;; Org screenshots

;; (use-package org-attach-screenshot
;;   :config
;;   (let ((screenshot-entry '((?p ?\C-p)
;;             org-attach-screenshot
;;             "Grab a screenshot and attach it to the task, using `org-attach-screenshot-command-line'.")))
;;     (setq org-attach-commands
;;       (cons screenshot-entry org-attach-commands))))



;; (use-package org-journal
;;   :custom
;;   (org-journal-date-prefix "#+TITLE: ")
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-dir ponelat/org-roam-dir)
;;   (org-journal-date-format "%A, %d %B %Y"))

;; (use-package ox-reveal
;;   :config
;;   (setq org-reveal-root "file:///home/josh/revealjs"))

;;; Org Trello
;; (use-package org-trello)

;; (use-package org-download)

;; (progn
;;   (use-package md-roam
;;     :straight '(md-roam :host github :repo "nobiot/md-roam"))
;;   (require 'md-roam)
;;   (setq md-roam-file-extension-single "md")
;;   (setq md-roam-use-org-file-links nil)
;;   (setq md-roam-use-markdown-file-links t)  ; default is nil

  ;; (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias))))
  ;you need this as of commit `5f24103`.

;;; Org Roam

;; (use-package org-roam
;;   :hook '((after-init . org-roam-mode))
;;   :custom (org-roam-directory ponelat/org-roam-dir)
;;   :config
;;   (comment
;;     (setq org-roam-file-extensions '("org" "md"))
;;     (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))))



;;; Time world clock
(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
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

;;; Agenda, reminders
(progn
  (defun ponelat/org-agenda-to-appt ()
    "Rebuild all appt reminders"
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))
  (add-hook 'org-agenda-mode-hook 'ponelat/org-agenda-to-appt 'append)
  (appt-activate t))

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


;;; Helper functions
;;; Helper functions

(defun ponelat/link-at-point ()
  "It uses org-mode functions to get link at point. ff"
  (cond
   ((org-in-regexp org-plain-link-re)
    (buffer-substring
     (match-beginning 0)
      (match-end 0)))))

;;; PDFs / doc view (nix managed)
(with-eval-after-load 'pdf-tools  ;; Nix manages installation of pdf-tools package
    (setq doc-view-continuous nil)
    (evil-define-key
      'normal doc-view-mode-map "j"
      (lambda () (interactive) (doc-view-scroll-down-or-previous-page 1)))
    (evil-set-initial-state 'pdf-view-mode 'normal)
    ;; open pdfs scaled to fit page
    (setq-default pdf-view-display-size 'fit-page)
    ;; automatically annotate highlights
    (setq pdf-annot-activate-created-annotations t)
    ;; use normal isearch
    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

 ;;; External Org mode, Office, Gmail

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

;;; Org mode helpers

(defun ponelat/org-insert-child-headline ()
  "It inserts a child headline ( ie: Lower than the current."
  (interactive)
  (org-insert-heading-respect-content)
  (org-demote)
  (call-interactively 'evil-insert))
;;; Ox / Org Mode Exporters

(use-package ox-jira)
(use-package ox-slack)

(comment require 'ox-pointy)

;;; Open with external tools

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
;; (use-package openwith
 ;;  :config
 ;;  (progn
 ;;     (setq openwith-associations
 ;;      (list
 ;;        (list (openwith-make-extension-regexp
 ;;                '("svg"))
 ;;          "inkscape"
 ;;          '(file))))
 ;;    (openwith-mode 1))
 ;;  )

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

(defun ponelat/org-open-link-shub (link)
  "Open LINK as JIRA issue, if it matches shub-xxxx."
  (cond ((string-match "\\(\\(shub|sdes|sonp|splat|steam|scons\\)-[0-9]\\{4\\}\\)" link) ; [[shub-xxxx]]
         (let* ((shub (match-string 1 link))
                (url (concat "https://smartbear.atlassian.net/browse/" (url-encode-url (upcase shub)))))
           (browse-url url)))))

(defun ponelat/evil-org->-on-line ()
  "Uses `evil-org->' but without waiting for a vim motion. Operates on the current line."
  (interactive)
  (evil-org-> (line-beginning-position) (line-end-position) 1))

(defun ponelat/evil-org-<-on-line ()
  "Uses `evil-org-<' but without waiting for a vim motion. Operates on the current line."
  (interactive)
  (evil-org-< (line-beginning-position) (line-end-position) 1))

(use-package evil-org
  :after org
  :config
  (add-hook 'evil-org-mode-hook
    (lambda ()
      (progn
        (evil-org-set-key-theme)
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)
        (evil-define-key 'normal 'evil-org-mode
          "t" 'org-todo)
        (evil-define-key 'normal 'evil-org-mode
          (kbd "C-j") nil)
        (evil-define-key 'normal 'evil-org-mode
          (kbd "C-S-<return>") (evil-org-define-eol-command ponelat/org-insert-child-headline))
        (evil-define-key 'normal 'evil-org-mode
          (kbd "<return>") 'evil-org-org-insert-heading-respect-content-below)
        (evil-define-key 'insert 'evil-org-mode
          (kbd "C-S-<return>") (evil-org-define-eol-command ponelat/org-insert-child-headline))
        (evil-define-key '(insert normal) 'evil-org-mode
          (kbd "M-h") #'ponelat/evil-org-<-on-line)
        (evil-define-key '(insert normal) 'evil-org-mode
          (kbd "M-l") #'ponelat/evil-org->-on-line))))
  (add-hook 'org-mode-hook 'evil-org-mode))

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
      (concat org-directory "/projects.org"))
    (push (org-projectile-project-todo-entry) org-capture-templates)
    ))

;;; Package stuff

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

(defun chatgpt/file-has-shebang-p (filename)
  "Check if the file has a shebang line."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename nil 0 2)
      (goto-char (point-min))
      (let ((first-two-chars (buffer-substring-no-properties (point-min) (point-max))))
        (string-equal first-two-chars "#!")))))


;;; Run current file
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
           ("nix" . "nix eval -f")
           ("go" . "go run")
           ("js" . "bun") ; node.js
	   ("ts" . "bun") ; "ts-node") ; TypeScript
           ;; ("ts" . "tsc --alwaysStrict --lib DOM,ES2015,DOM.Iterable,ScriptHost --target ES5") ; TypeScript
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
     ((chatgpt/file-has-shebang-p filename)
      (progn
	(message "Running with Shebang")
        (async-shell-command (format "cd %s && \"%s\"" -fdir -fname) "*Run this*" )))
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
    (if (y-or-n-p (format "Sure you want to run %s" -fname))
      (xah/run-this-file-fn -fname))))

;;; Themes
;; Disable previous theme, before enabling new one. Not fool-proof.
;; Themes have a lot of power, and some of it cannot be reversed here
;;; Theme hooks

; Allow all themes to run arbitrary code without a prompt
(setq custom-safe-themes t)

;;; Modeline theme
;; (use-package xmlgen)
;; (use-package ocodo-svg-modelines)

;; (use-package minimal-theme
;;   :disabled)

(use-package mood-line
  :config (mood-line-mode))

(use-package all-the-icons)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (comment setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (comment doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

;;; Faces, font, style

(defun ponelat/face-extras ()
  "Change some faces, regardless of theme.  To my liking."
  (interactive)
  (progn
    (let ((bg-color (face-attribute 'default :background)))
      (set-face-attribute 'org-hide nil :foreground bg-color :background bg-color)
      (set-face-attribute 'org-code nil :foreground (face-attribute 'org-formula :foreground))
      (set-face-attribute 'fringe nil :foreground bg-color :background bg-color))))

(defun ponelat/add-frame-padding (&optional PREFIX)
  "Add a padding to frame.  Will give options when used with PREFIX.  "
  (interactive "P")
  (let* ((size (if PREFIX
                 (string-to-number (read-from-minibuffer "Padding (number): " "50"))
                 50)))
    (set-frame-parameter nil 'internal-border-width size)))

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

;;; for zerodark - markup/adoc
(defun ponelat/markup-fonts ()
  "It changes the faces in zerodark them for markup and adoc."
  (interactive)

    (let* ((variable-tuple
             (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
               ((x-list-fonts "Noto Sans")         '(:font "Noto Sans"))
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
          `(markup-gen-face ((t (,@headline ,@variable-tuple :height 1.0))))
          `(markup-title-0-face ((t (,@headline ,@variable-tuple :height 1.0))))
          `(markup-typewriter-face ((t (,@headline ,@variable-tuple :height 1.0))))
          `(markup-verbatim-face ((t (,@headline ,@variable-tuple :height 1.0))))
          ))))

(use-package zerodark-theme
  :config
  (setq
    zerodark-buffer-coding nil
    zerodark-use-paddings-in-mode-line nil
    )
  )

;; (use-package gruvbox-theme
;;   )

;; (use-package soothe-theme
;;   :defer t
;;   :config
;;   (progn
;;     (gh/add-theme-hook
;;       'soothe
;;       (lambda (a) (ponelat/theme-soothe-extras)))))

(use-package solarized-theme
  :defer t)

;; (with-eval-after-load 'zerodark-theme ())
;; This can only run in window mode...

;; Fails on Verdana font. Could add it, but lazy for now.
(comment use-package org-beautify-theme :defer t)

;; (use-package sublime-themes
;;   :defer t
;;   :disabled t
;;   )

(use-package org-bullets
  :config
  (setq
    org-bullets-bullet-list '("●" "○")
    org-bullets-face-name 'shadow)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Disabling for now...
(defun ponelat/setup-mode-line ()
  "Set up the modeline."
  (interactive)
  (comment progn
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

;;; Window stuff / Golden ratio
(progn
  ;; This will auto-resize windows as you move between them.
  (use-package zoom
    :disabled
    :config
    (evil-global-set-key 'normal (kbd "C-w =") #'zoom)
    (setq zoom-size '(0.618 . 0.618))
    (zoom-mode t)))

;; Load theme on first frame ( only once )
(defvar ponelat:theme-window-loaded nil "A flag used to indicate that the GUI theme got loaded.")
(defvar ponelat:theme-terminal-loaded nil "A flag used to indicate that the Terminal theme got loaded.")
;; (defvar ponelat:theme 'gruvbox-dark-medium "The initial theme.")
;; (defvar ponelat:theme 'gruvbox-dark-hard "The initial theme.")
(defvar ponelat:theme 'doom-dark+ "The initial theme.")

;; Due to starting a daemon at the same time as our client
;; The follow code exists to ensure that the theme is loaded at the right time.
(defun ponelat/setup-theme ()
  "Enable or load gui/window theme."
  (interactive)
  (unless ponelat:theme-window-loaded
    (progn
      ;; (ponelat/setup-mode-line)
      (load-theme-only ponelat:theme)
      ;; (setq org-beautify-theme-use-box-hack nil)
      ;; (load-theme 'org-beautify 1)
      (setq ponelat:theme-window-loaded t)
      (ponelat/face-extras)
      (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
      (add-to-list 'default-frame-alist '( alpha . (100 . 100))))))

(defun ponelat/setup-theme-terminal ()
  "Enable or load terminal theme."
  (if ponelat:theme-terminal-loaded
    (progn
      (enable-theme ponelat:theme))
    (progn
      (load-theme ponelat:theme t)
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


;;; Font,face
(defvar ponelat/default-font-family "Noto Mono"
  "The font family I like. Options...
 - Noto Mono
 - SF Mono
")

(defvar ponelat/fonts
  '( ("Small"  (:family ponelat/default-font-family :height 105  :weight normal))
     ("Normal" (:family ponelat/default-font-family :height 140 :weight normal))
     ("Large" (:family ponelat/default-font-family :height 180 :weight normal))))

(defun ponelat/default-font (font-name)
"Set the font.  FONT-NAME is the key found in ponelat/fonts.
Interactively you can choose the FONT-NAME"
  (interactive
    (list
      (completing-read "Choose font: " (alist-keys ponelat/fonts))))
  (let ((font-props (car (assoc-default font-name ponelat/fonts))))
    (apply 'set-face-attribute (append '(default nil) font-props))))

;;; Set default font
(ponelat/default-font "Small")

;;; Cycle through fonts
(defvar ponelat/default-font-index 1)
(defun ponelat/toggle-default-font ()
  "It cycles through the default fonts."
  (interactive)
  (let* ((index ponelat/default-font-index)
          (next-index (mod (+ 1 index) (length ponelat/fonts))))
    (ponelat/default-font (first (nth next-index ponelat/fonts)))
    (setq ponelat/default-font-index next-index)))
(bind-key "C-x f" #'ponelat/toggle-default-font)

;;; General Emacs stuff
(setq warning-minimum-level :error)

;;; Line endings, Windows, Linux
(defun xah-change-file-line-ending-style (@files @style)
  "Change current file or dired marked file's newline convention.

When called non-interactively, *style is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2016-10-16"
  (interactive
   (list
    (if (eq major-mode 'dired-mode )
        (dired-get-marked-files)
      (list (buffer-file-name)))
    (ido-completing-read "Line ending:" '("Linux/MacOSX/Unix" "MacOS9" "Windows") "PREDICATE" "REQUIRE-MATCH")))
  (let* (
         ($codingSystem
          (cond
           ((equal @style "Linux/MacOSX/Unix") 'unix)
           ((equal @style "MacOS9") 'mac)
           ((equal @style "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." )))))
    (mapc
     (lambda (x) (xah-convert-file-coding-system x $codingSystem))
     @files)))

(defun xah-convert-file-coding-system (@fpath @coding-system)
  "Convert file's encoding.
 *fpath is full path to file.
 *coding-system is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2015-07-24"
  (let ($buffer
        ($bufferOpened-p (get-file-buffer @fpath)))
    (if $bufferOpened-p
        (with-current-buffer $bufferOpened-p
          (set-buffer-file-coding-system @coding-system)
          (save-buffer))
      (progn
        (setq $buffer (find-file @fpath))
        (set-buffer-file-coding-system @coding-system)
        (save-buffer)
        (kill-buffer $buffer)))))

;;; Eval, inline, Emacs lisp
(use-package eros
  :bind (("C-c C-c" . #'eval-defun))
  :config
  (progn (eros-mode 1)))

;; Disable flycheck for emacs-lisp mode
(progn 
  (defun ponelat/disable-flycheck-mode ()
    "Disable `flycheck-mode'."
    (flycheck-mode -1))
  (add-hook 'emacs-lisp-mode-hook #'ponelat/disable-flycheck-mode))

(use-package direnv)

;;; Diff, vimdiff
(use-package vdiff)

;; Kill all other buffers
(defun ponelat/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
            (remove-if-not 'buffer-file-name (buffer-list)))))

;;; Toggle fullscreen, buffer
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
    (setq which-key-popup-type 'minibuffer)
    (setq which-key-side-window-location 'bottom)
    (setq which-key-side-window-max-height 0.4)
    (setq which-key-min-display-lines 1)
    (setq which-key-show-transient-maps t)
    (which-key-mode)))

;;; Global Bindings, keys
(bind-key "C-x C-k" 'kill-this-buffer)
(bind-key "C-x Q" 'save-buffers-kill-emacs)
(bind-key "C-x y" #'eval-buffer)

(bind-key "C-c l l" #'imenu)

(bind-key "C-c ;" 'delete-other-windows)
(bind-key "C-c C-;"
  (lambda () (interactive)
    (delete-window)
    (balance-windows)))

(bind-key "C-h l" #'find-library)
(bind-key "C-x a n" #'ponelat/spotify-next)
(bind-key "C-x a p" #'ponelat/spotify-previous)
(bind-key "C-x a SPC" #'ponelat/spotify-play-toggle)

;;; Search
(use-package noccur)

(defun thing-at-point-or-mark (&optional type)
  (let* ((bounds
           (if (use-region-p)
             (cons (region-beginning) (region-end))
             (bounds-of-thing-at-point (or type 'symbol))))
          ($from (car bounds))
          ($to (cdr bounds)))
    (buffer-substring-no-properties $from $to)))

(defun ponelat/use-region-bounds ()
  "Returns the bounds if `use-region-p' is t."
  (if (use-region-p)
      (cons (region-beginning) (region-end))))

(defun ponelat/replace-thing-at-point-or-mark (fn &optional thing)
  "Replace THING at point or Mark by running that text though FN."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point (or thing 'symbol))))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (apply fn (list text))))))


(defun ponelat/mark-bounds (bounds)
  "Visually mark the BOUNDS provided."
  (let ((start-pos (car bounds))
	(end-pos (cdr bounds)))
    (progn 
      (message (format "Marking bounds: %s" bounds))
      (goto-char start-pos)
      (set-mark (point))
      (goto-char (- end-pos 1)))))

(defvar ponelat/things-i-know-of
  '(symbol list sexp defun filename
	   existing-filename url email uuid word sentence
	   whitespace line page) "The things I know of, from `thing-at-point' documentation.")

(defun ponelat/mark-thing-at-point (&optional provided-thing)
  "Mark thing at point from list or from PROVIDED-THING."
  (interactive "P")
  (let* ((thing
	 (or provided-thing
	     (intern (completing-read "Choose an option: " ponelat/things-i-know-of nil t nil nil "line"))))
	 (bounds (bounds-of-thing-at-point thing)))
    (if bounds  (ponelat/mark-bounds bounds))))

(defun ponelat/replace-within-string-or-visual (new-str)
  "Replace contents within string at point or mark with NEW-STR."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (thing-at-point-bounds-of-string-at-point))))
    (when bounds
      (delete-region (+ 1 (car bounds)) (- (cdr bounds) 1))
      (insert new-str))))

(defun replace-bounds (str bounds)
  "Replace BOUNDS with STR."
  (let ((beg (car bounds))
         (end (cdr bounds)))
    (goto-char beg)
    (delete-region (car bounds) (cdr bounds))
    (insert str)))

;;; Visual regexp, search replace
(use-package visual-regexp)

;;; Keys, emojis

;; Can you see this face: 😬
;; (use-package emojify
;;   :config
;;   (emojify-set-emoji-styles '(unicode))
;;   (global-emojify-mode)
;;   (global-set-key (kbd "C-x 8 e") 'emojify-insert-emoji))

(progn
  ;;; Maybe useful

  (defun ponelat/read-file-into-string (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  ;; From http://xahlee.info/emacs/emacs/elisp_read_file_content.html
  (defun read-lines (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer
      (insert-file-contents filePath)
      (split-string (buffer-string) "\n" t)))

  (defun ponelat/read-file-into-cmd-lines (filePath)
    "Read FILEPATH into a seq of non-empty lines while respecting escaped newlines."
    (seq-filter (lambda (s) (not (string-empty-p s)))
      (split-string
        (replace-regexp-in-string "\\\\\n" " " (ponelat/read-file-into-string filePath))
        "\n")))


  (defun ponelat/regexp-list (regex string &optional index)
    "Return a list of all matching REGEX in STRING.  Optionally using INDEX instead of the whole match."
    ;; source: http://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
    (let ( (index (or index 0))
           (pos 0)                      ; string marker
           (matches ()))                ; return list
      (while (string-match regex string pos)
        (push (match-string index string) matches)
        (setq pos (match-end index)))
      (setq matches (reverse matches))
      matches)))

;;; Template langs, mustache
(use-package mustache-mode)


 (progn
  ;;; Emacs-Commands.xml, execute project files
  (require 'seq)

  (defun emacs-commands/string-template (hash str)
    "Replace all instances of the HASH keys with their values in STR."
    (seq-reduce
      (lambda (acc key)
        (replace-regexp-in-string key (gethash key hash) acc t t))
      (hash-table-keys hash) str))

  (defun emacs-commands/hash-get-arg-values (args-nodes)
    "Extracts arguments out of the XML node ARGS-NODES. Optionally adds PREFIX to the key names."
    (seq-reduce
      (lambda (acc arg-node)
        (let* ((key (xml-get-attribute arg-node 'name))
                (file-from (xml-get-attribute-or-nil arg-node 'file-from))
                (file-from-values (if file-from
                               (let ((default-directory (projectile-project-root)))
                                 (file-expand-wildcards file-from))))

                (file-contents (xml-get-attribute-or-nil arg-node 'file-contents))
                (file-contents-values (if file-contents
                               (let ((default-directory (projectile-project-root)))
                                 (read-lines file-contents))))

                (child-values
                  (mapcar
                    (lambda (node)
                      (string-trim (car (xml-node-children node))))
                    (xml-get-children arg-node 'value)))
                (values (append child-values file-from-values file-contents-values))
                (default-value (xml-get-attribute-or-nil arg-node 'default))
                (value
                  (completing-read (format "%s: " key) values nil nil default-value)))
          (puthash (format "$%s" key) value acc)
          )
        acc)
      (xml-get-children args-nodes 'arg)
      (make-hash-table :test 'equal)))


  (defun emacs-commands/execute-command (command base-dir)
    "It does something"
    (let* (
            (cmd-node (car (xml-get-children command 'cmd)))
            (cmd-string (string-trim (car (xml-node-children cmd-node))))
            (args (emacs-commands/hash-get-arg-values command))
            (title (xml-get-attribute command 'title))
            ($dir (xml-get-attribute command 'dir))
            (dir
              (if (string-match "^/" $dir)  ; Absolute path
                $dir
                (concat (file-name-as-directory base-dir) $dir)))
            (cmd-compiled (emacs-commands/string-template args cmd-string))
            (stdout-buffer-name (format "*Emacs Commands %s*" title dir))
            (stderr-buffer-name (format "*Emacs Command %s - Error*" title))
            (stdout-buffer (get-buffer stdout-buffer-name))
            (stderr-buffer (get-buffer stderr-buffer-name)))

      (progn
        (when stdout-buffer
          (kill-buffer stdout-buffer))
        (async-shell-command
          (format "cd %s && %s" dir cmd-compiled)
          stdout-buffer-name
          stderr-buffer-name)
        (switch-to-buffer-other-frame stdout-buffer-name))))

  (defun emacs-commands/pick-command (root-xml)
    "Pick the command node based on `completing-read' on the command[title]."
    (let* ((root-xml (car root-xml))
            (commands (xml-get-children root-xml 'command))
            (command-titles (mapcar (lambda (node) (string-trim (xml-get-attribute node 'title))) commands))
            (command-pick (completing-read (format"%s: " (projectile-project-name)) command-titles))
            (command (seq-find (lambda (command) (equal command-pick (xml-get-attribute command 'title))) commands)))
      command))


   (defconst ponelat/emacs-commands-template
"<emacs>
  <command title=\"Some title\" dir=\".\" >

    <cmd>
      nix-shell -p cowsay --run \"cowsay Hello $NAME\"
    </cmd>

    <arg name=\"NAME\" default=\"Josh\" >
      <value> Hezzie </value>
      <value> Josh </value>
    </arg>

  </command>

</emacs>")

   (defun emacs-commands/insert-xml-arg ()
     "It does something"
     (interactive)

     (insert "
    <arg name=\"NAME\" default=\"Josh\" >
      <value> Hezzie </value>
      <value> Josh </value>
    </arg>
"))


   (defun ponelat/emacs-commands ()
    "
Runs emacs-commands from local project.

In the root of your project get a file named .emacs-commands.xml with the following
<emacs>
  <command title=\"Some title\" dir=\".\" >

    <cmd>
      cowsay Hello $NAME
    </cmd>

    <arg name=\"NAME\" default=\"Josh\" >
      <value> Hezzie </value>
      <value> Josh </value>
    </arg>

  </command>

</emacs>
"
    (interactive)
    (let* ((emacs-command-name ".emacs-commands.xml")
            (base-dir (projectile-project-root))
            (xml-path (concat (file-name-as-directory base-dir) emacs-command-name))
            (xml-root (xml-parse-file xml-path))
            (command (emacs-commands/pick-command xml-root)))
      (emacs-commands/execute-command command base-dir)))

   (defun ponelat/emacs-commands-open ()
     "It opens the .emacs-commands.xml relative for this project(ile)."
     (interactive)
     (find-file
       (concat
         (file-name-as-directory
           (projectile-project-root)) ".emacs-commands.xml"))

     (if (equal "" (string-trim (buffer-string)))
       (insert ponelat/emacs-commands-template))))


;; ;;; Stupid stuff
;; (defun ponelat/sentence-macro ()
;;   "Moves to the start of the line, downcases the first word and moves to insert-mode."
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (let ((char (following-char)))
;;     (message (format "Char: %c" char))
;;     (delete-char 1 nil)
;;     (insert-char (downcase char) 1)
;;     (move-beginning-of-line 1)
;;     (insert-char ?  1)
;;     (move-beginning-of-line 1)
;;     (evil-insert-state)))


;; (defun ponelat/sentence-macro-push ()
;;   "Downcases the first word and moves to insert-mode."
;;   (interactive)
;;   (let ((char (following-char)))
;;     (delete-char 1 nil)
;;     (insert-char (downcase char) 1)
;;     (backward-char 1)
;;     (insert-char ?  1)
;;     (backward-char 1)
;;     (evil-insert-state)))

  ;; ;;; Org keys
  ;;  (ponelat/local-leader
  ;;   "ss" #'ponelat/sentence-macro
  ;;   "sh" #'ponelat/sentence-macro-push)


(defun ponelat/toggle-whitespace ()
  "Toggles whitespace mode with long(76), short(55) and off."
  (interactive)
  (defvar-local whitespace-manning-toggle nil)
  (cond
    ((eq whitespace-manning-toggle nil)
      (progn
        (setq whitespace-line-column 76)
        (whitespace-mode +1)
        (setq whitespace-manning-toggle :long)))

    ((eq whitespace-manning-toggle :long)
      (progn
        (setq whitespace-line-column 55)
        (whitespace-mode +1)
        (setq whitespace-manning-toggle :short)))

    ((eq whitespace-manning-toggle :short)
      (progn
        (setq whitespace-line-column 80)
        (whitespace-mode 0)
        (setq whitespace-manning-toggle nil)))

    (t
      (progn
        (let ((state whitespace-manning-toggle))
          (setq whitespace-manning-toggle nil)
          (message (format "Unknown state %s" state))
          )
        ))
    )
  (message (format "Whitespace: %s" whitespace-manning-toggle)))


;;; General key bindings local and global leader keys
(progn

  ;; Need to find a way to use  this in insert mode
   (general-unbind "C-SPC")

   (general-create-definer ponelat/global-leader
     :prefix ponelat/global-leader-key
     :states '(normal visual)
     :keymaps 'override)

   (general-create-definer ponelat/local-leader
     :prefix ponelat/local-leader-key
     :states '(normal visual))

;;; Global leader (space key)
   (ponelat/global-leader
     "Q" #'save-buffers-kill-terminal
     "p" #'projectile-command-map
     "w" #'evil-window-map
     "s" #'save-buffer
     "l" #'avy-goto-line
     "b" #'consult-buffer
     "a" #'consult-ripgrep
     "j" #'execute-extended-command

     ;; "s" #'avy-goto-char-2
     "fe" #'flycheck-list-errors
     "fm" #'flymake-show-buffer-diagnostics
     "fj" #'consult-flymake
     "ff" #'find-file
     "fw" #'write-file
     ;; "s" #'avy-goto-char-timer

;;; Open stuff
      "o" '(:wk "open")
      "oh" '(ponelat/edit-hosts-file :wk "/etc/hosts")
      "oe" '(ponelat/emacs-lisp-imenu-init :wk "init.el")
      "oo" '(xah-open-in-external-app :wk "<external>")
      "or" '(ponelat/jump-to-restclient :wk "rest-scratch")
      "od" `(,(lambda () (interactive) (find-file  (read-file-name "File: " "~/Downloads/"))) :wk "Downloads")
      "oi" `(,(lambda () (interactive) (find-file (format "%s/dotfiles/dots/config/sway/config" ponelat/projects-dir))) :wk "i3 config")
      "oz" `(,(lambda () (interactive) (find-file (format "%s/dotfiles/dots/zshrc" ponelat/projects-dir))) :wk ".zshrc")
      "oz" `(,(lambda () (interactive) (find-file (format "%s/dotfiles/dots/zshrc" ponelat/projects-dir))) :wk ".zshrc")
      "on" `(,(lambda () (interactive) (find-file "/etc/nixos/configuration.nix" )) :wk "NixOS")
      "oc" `(,(lambda () (interactive) (find-file (format "%s/dotfiles/nixos/common.nix" ponelat/projects-dir))) :wk "NixOS Common")
      ;; "op" `(,(lambda () (interactive) (find-file (format "%s/dotfiles/dots/profile" ponelat/projects-dir))) :wk ".profile")
      "os" `(,(lambda () (interactive) (find-file "~/.ssh/config")) :wk "ssh config")
      "ok" `(,(lambda () (interactive) (find-file "~/.kube/config")) :wk "kube config")

;;; Magit Keys
     "gg" #'magit-status
     "gs" #'magit-stage

;;; Org/Roam/Agenda/Trello
     "ra" 'org-agenda
     "rf" 'org-roam-find-file
     "rg" 'org-roam-show-graph
     "ri" 'org-roam-insert
     "rj" 'org-journal-new-entry
     "rl" 'org-roam
     "rn" 'org-capture
     "rt" 'org-journal-open-current-journal-file

;;; Move this into projectile
     "cr" #'ponelat/emacs-commands
     "co" #'ponelat/emacs-commands-open

     ";" #'delete-other-windows
     "i" #'consult-imenu
     "d" #'dired-jump
     "e" #'projectile-run-eshell

;;; Quit keys
     "q" #'quit-window)

;;; Local leader (comma key) 

  (setq org-trello-current-prefix-keybinding "SPC ro")

  (ponelat/local-leader

     ;; Narrow / widen
     "i"  '(:wk "narrow")
     "ii" #'org-narrow-to-subtree
     "io" #'widen

    "aa" #'ponelat/insert-ledger-account-into-string
    "ar" #'ponelat/insert-ledger-batch-rule

     "x"  '(:wk "extra")
     "xx" #'xah/run-this-file
     "xd"  '(:wk "decode")
     "xdb" #'base64-decode-region
     "xdu" #'xah-html-decode-percent-encoded-url
     "xdc" #'ponelat/pretty-cert-in-region
     "xe"  '(:wk "encode")
     "xeb" #'base64-encode-region
     "xeu" #'xah-html-encode-percent-encoded-url
     "xeU" (lambda () (interactive) (call-interactively 'xah-html-encode-percent-encoded-url))

     "xds" #'ponelat/split-lines-in-region
     "xes" #'ponelat/collapse-lines-in-region

    "xs" #'ponelat/toggle-whitespace

    "m" #'hydra-volume/body

    "dd" #'dired-jump
    "dg" #'dired-gitignore-mode

    "e" '(:wk "edit")
    "eu" 'ponelat/edit-url

    "v" '(:wk "diff")
    "vv" 'vdiff-hydra/body
    "vb" 'vdiff-buffers
    "vf" 'vdiff-files
    "vF" 'vdiff-files3
    "vB" 'vdiff-buffers3

     "xw" #'count-words-region

     "g"  '(:wk "git")
     "gn" 'git-gutter+-next-hunk
     "gp" 'git-gutter+-previous-hunk
     "gv" 'git-gutter+-show-hunk
     "gr" 'git-gutter+-revert-hunks
     "gt" 'git-gutter+-stage-hunks
     "gc" 'git-gutter+-commit
     "gC" 'git-gutter+-stage-and-commit
     "gY" 'git-gutter+-stage-and-commit-whole-buffer
     "gU" 'git-gutter+-unstage-whole-buffer

    "f" '(:wk "file")
    "fr" #'fdx/rename-current-buffer-file
    "fd" #'fdx/delete-current-buffer-file
    "fm" #'make-directory

    "r" '(:wk "replace")
    "rr" #'vr/replace
    "rq" #'vr/query-replace
    "rt" #'string-rectangle

    "b" '(:wk "buffer")
    "bk" 'kill-this-buffer
    "bK" 'kill-buffer


;;; Toggle things globally
    "tv" 'visual-line-mode
    "tf" 'ponelat/toggle-default-font
    "tw" 'ponelat/toggle-whitespace

;;; Lisp keys

    "l" '(:wk "Lisp")
    "l c" 'paredit-duplicate-after-point
    "l d" 'kill-sexp
    "l r" 'paredit-raise-sexp

      ;; Font size
    "=" '(hydra-zoom/body :wk "size")

    "o" '(:wk "org")
    "ot" #'org-teleport
    "ol" '(:wk "org link")
    "olt" #'org-toggle-link-display
    "oll" #'org-insert-link)


  (ponelat/local-leader
    :modes 'org-journal
    :state '(normal)
    "c" '(:wk "org-journal")
    "ch" #'org-journal-open-previous-entry
    "cl" #'org-journal-open-next-entry)

  (ponelat/local-leader
    :modes 'org-mode
    :state '(normal)
    "o" '(:wk "org-mode")
    "h" #'org-toggle-heading)

;;; Lisp/Paredit keys

  (general-define-key
    :states '(normal insert)
    :keymaps '(emacs-lisp-mode-map clojure-mode-map)
    "C-." 'paredit-forward-slurp-sexp
    "C-," 'paredit-forward-barf-sexp)

;;; Yaml/OpenAPI
  (general-define-key
    :states '(normal insert)
    :keymaps 'yaml-mode-map
    "C-w" #'yaml-delete-backward-word-electric)


;;; Avy keys
  (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix "ga"
    "" '(:wk "avy")
    "s" 'avy-goto-char-timer
    "o" 'avy-org-goto-heading-timer
    "t" 'avy-org-refile-as-child)

;;; Org keys
   (general-define-key
    :states 'insert
    :keymaps 'override
    "M-j" 'org-move-subtree-down
    "M-k" 'org-move-subtree-up
    "M-l" 'org-demote-subtree
     "M-h" 'org-promote-subtree)


   (general-define-key
    :keymaps 'org-journal
     "C-," nil)

   (general-define-key
    :keymaps 'override
    "M-u" 'evil-scroll-up)

   (general-define-key
    :keymaps 'override
    "M-d" 'evil-scroll-down)

  (general-def 'insert
    "C-x C-l" #'ponelat/expand-lines)

   (general-define-key
    :prefix "C-SPC"
     "ri" #'org-roam-insert)

;;; Key go or g
   (general-define-key
    :prefix "C-SPC"
     "ga" '(:wk "avy")
     "gas" #'avy-goto-char-timer)

   (general-define-key
    :states '(normal visual)
     "gl" #'evilmi-jump-items)


  (general-define-key
    :states 'normal
    :keymaps 'tide-mode-map
    "C-c C-c" 'flycheck-buffer)

  (general-define-key
   "C-S-l" #'evil-window-right
   "C-S-h" #'evil-window-left
   "C-S-k" #'evil-window-up
    "C-S-j" #'evil-window-down)

  (progn
    (require 'dired)
    (general-define-key
     :states 'normal
     :keymaps 'dired-mode-map
     "s" #'xah-dired-sort)))

(progn
;; Type a symbol then hit C-j to wrap it in parens.
  (defun ponelat/emmet-for-lisp ()
    "Wraps the previous symbol with parens."
    (interactive)
    (ponelat/replace-thing-at-point-or-mark (lambda (str) (format "(%s )" str)) 'symbol)
    (backward-char))

  (general-define-key
    :states '(visual normal insert)
    :keymaps 'emacs-lisp-mode-map)
    ;; "C-j" 'ponelat/emmet-for-lisp)
  (general-define-key
   :states '(visual normal)
   :keymaps 'override
   "r" #'evil-replace-with-register))

(progn
  (use-package marginalia
    :ensure t
    :config
    (marginalia-mode))

  (use-package embark
    :ensure t

    :bind
    (("C-S-a" . embark-act)       ;; pick some comfortable binding
      ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init

    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config

    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
      '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
         nil
         (window-parameters (mode-line-format . none)))))

  ;; Consult users will also want the embark-consult package.
  (use-package embark-consult
    :ensure t
    :after (embark consult)
    :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(use-package nix-mode
  :general
  (:keymaps 'nix-mode-map
   "C-c C-c" (lambda () (interactive) (async-shell-command "sudo nixos-rebuild switch" "*NixOS Rebuild*"))
   "C-c C-r" (lambda () (interactive) (async-shell-command (format "nix eval -f %s" buffer-file-name ) "*NixOS Rebuild*")))
  :mode ("\\.nix\\'"))

;; (use-package company-nixos-options
;;   :config
;;    (add-to-list 'company-backends 'company-nixos-options))

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

;; Image mode
(evil-define-key '(normal visual) image-mode-map "," nil)

(use-package journalctl-mode)

(defun reboot ()
  "Reboot the machine, after checking if any files need to be saved. "
  (interactive)
  (save-some-buffers)
  (or (not (memq t (mapcar (lambda (buf)
			     (and (buffer-file-name buf)
				  (buffer-modified-p buf)))
			   (buffer-list))))
      (yes-or-no-p "Modified buffers exist; exit anyway? "))
  (shell-command "reboot"))

;; ;; https://github.com/orzechowskid/tsi.el/
;; ;; great tree-sitter-based indentation for typescript/tsx, css, json

(use-package tsi
  :after tree-sitter
  :straight '(tsi :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))



;;; Custom.el file
(load custom-file 'noerror)
(put 'narrow-to-region 'disabled nil)
;;; init.el ends here
(provide 'init)
