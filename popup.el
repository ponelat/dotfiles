;; scraped from https://www.reddit.com/r/emacs/comments/7i7s8s/use_emacs_as_your_textexpander_in_x11/
(defvar cm/emacs-expander-alist
  "alist of things to expand to"
  '(("//" . (text "  //  comment"))))

(setq cm/emacs-expander-alist
      '(("//" . (text "  //  comment"))
	("sg." . (text "Sounds good."))))

(defun cm/emacs-expander-frame-cleanup-if-not-empty ()
  "Close the emacs-expander frame. If it's empty"
  (if (buffer-empty-p) (cm/emacs-expander-frame-cleanup)))

(defun cm/xdotool-key (key)
  (make-process
   :name "xdotool key process"
   :buffer "xdotool-process-buffer"
   :command `("xdotool" "key" "--clearmodifiers" "--delay" "5" ,key)
   :connection-type 'pipe
   :sentinel #'cm/expander-sentinel))

(defun cm/xdotool-type (text)
  (let ((proc (make-process
	       :name "xdotool type process"
	       :buffer "xdotool-process-buffer"
	       :command '("xdotool" "type" "--clearmodifiers" "--delay" "5" "--file" "-")
	       :connection-type 'pipe
	       :sentinel #'cm/expander-sentinel)))
    (process-send-string proc text)
    (process-send-eof proc)))

(defvar cm/expander-temp-storage-reply
  "So the callback can reply!" '())

(defun cm/expander-sentinel (proc event)
  (cm/run-expander-plist cm/expander-temp-storage-reply))


(defun cm/run-expander-plist (pls)
  (when (> (length pls) 1)
    (let ((sym (car pls))
	  (val (cadr pls)))
      (setq cm/expander-temp-storage-reply (cddr pls))
      (pcase sym
	('text (cm/xdotool-type val))
	('key (cm/xdotool-key val))))))


;; ;; Main entry point
;; (defun cm/emacs-expander ()
;;   (interactive)
;;   (save-window-excursion
;;     (let* ((key (completing-read "Abbrev to expand: " cm/emacs-expander-alist))
;; 	   (kpl (cdr (assoc key cm/emacs-expander-alist))))
;;       (cm/emacs-expander-frame-cleanup)
;;       (cm/run-expander-plist kpl))))
(defun buffer-empty-p (&optional buffer)
  (= (buffer-size buffer) 0))

;; Main entry point
(defun cm/emacs-expander ()
  (interactive)
  (save-window-excursion
    (counsel-M-x)
    (cm/emacs-expander-frame-cleanup-if-not-empty)))

(defun cm/emacs-expander-frame ()
  "Create a new frame and run cm/emacs-expander."
  (interactive)
  (save-window-excursion
    (make-frame '((name . "emacs-expander")
		  (width . 120)
		  (height . 20)
		  (menu-bar-lines . 0)
		  (tool-bar-lines . 0)
		  (minibuffer . only)
		  (auto-lower . nil)
		  (auto-raise . t)))
    (select-frame-by-name "emacs-expander")
    (condition-case nil
	(progn (cm/emacs-expander) t)
      ((error debug quit) nil)))
  (cm/emacs-expander-frame-cleanup))

(defun cm/emacs-expander-frame-cleanup ()
  "Close the emacs-expander frame. If it's empty"
  (dolist (elem (frame-list))
    (if (equalp "emacs-expander" (frame-parameter elem 'name))
	(save-window-excursion
	  (delete-frame elem)))))

(defun ponelat/shell-script (script)
  "It executes the shell SCRIPT string passed in."
  (interactive)
  (shell-command-to-string script))

(defun ponelat/chrome-history (&optional seperator)
  "Scrape the sqlite chrome history file and return a list of alist pairs. Use SEPERATOR to return a flat list of strings"
    (split-string (ponelat/shell-script (format "
  cp $HOME/.config/google-chrome/Default/History /tmp/h
  sqlite3 -separator '%s' /tmp/h \\
    'select substr(title, 1, 70), url
     from urls order by last_visit_time desc'" (or seperator " -::- "))) "\n"))

(defun ponelat/chrome-history-as-alist ()
  (mapcar (lambda (str)
            (let* ((str-pair (split-string str "{::}")))
              (cons (car str-pair) (car (cdr str-pair)))))
    (ponelat/chrome-history "{::}")))


(defun ponelat/open (url)
  (call-process "xdg-open" nil 0 nil url))

(defun ponelat/open-chrome-history ()
  (interactive)
  (let* ( (seperator " -::- ")
          (list-of-urls (ponelat/chrome-history seperator))
          (selected-history (ivy-read "Chrome: " list-of-urls))
          (selected-url (car (cdr (split-string selected-history seperator)))))
    (ponelat/open selected-url)))


;; (ivy-read "Chrome: " list-of-urls
;;   :action (lambda (x) (setq tmp (cdr x))))
;; tmp))


(comment

  (defun ponelat/jira-issues--parse (output)
    (mapcar (lambda (s) (split-string s "{::}"))
      (split-string output "\n")))

  (defun ponelat/jira-search-command (str)
    (format "/home/josh/bin/jira-search.sh '%s'" str))

  ;; (defun ponelat/jira-search-command (str)
  ;;   (format "ls /home/josh/%s" str))


(defun ponelat/jira-issues (str)
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
         (ponelat/jira-search-command str))
     '("" "working..."))))

;;;###autoload
(defun ponelat/counsel-jira (&optional initial-input)
  "Call the \"jira-search.sh\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Jira: " #'ponelat/jira-issues
            :initial-input initial-input
            :dynamic-collection t
            :history 'jira-history
            :action (lambda (pair)
                      (ponelat/open (car (cdr pair))))
            :unwind #'counsel-delete-process
            :caller #'ponelat/counsel-jira))
  )
