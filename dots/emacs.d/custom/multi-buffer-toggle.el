;;; multi-buffer-toggle.el --- Toggle between specialized buffers and normal buffers -*- lexical-binding: t; -*-

;; Author: Adapted from vterm-toggle by jixiuf
;; Version: 0.1.0

;;; Commentary:
;;
;; Provides a generic mechanism to toggle between specialized buffers
;; (like directories, REPLs, etc.) and normal editing buffers.
;; Supports registering multiple toggle configurations.
;;

;;; Code:

(defvar mbt-registered-toggles (make-hash-table :test 'equal)
  "Hash table of registered toggles.")

(defvar mbt-window-configurations (make-hash-table :test 'equal)
  "Stored window configurations for each toggle.")

(defcustom mbt-default-hide-method 'delete-window
  "Default hide method for buffer toggles."
  :group 'multi-buffer-toggle
  :type '(choice
          (const :tag "Toggle without closing the window (focus other window)" nil)
          (const :tag "Reset Window configuration" reset-window-configration)
          (const :tag "Bury the buffer" bury-buffer)
          (const :tag "Quit window" quit-window)
          (const :tag "Delete window" delete-window)))

(defcustom mbt-default-fullscreen nil
  "Whether to show toggled buffers in fullscreen by default."
  :group 'multi-buffer-toggle
  :type 'boolean)

(cl-defstruct (mbt-toggle (:constructor mbt-toggle-create)
                         (:copier nil))
  "Structure to define a buffer toggle."
  (name nil :read-only t)
  (buffer-pred nil :read-only t)
  (create-func nil :read-only t)
  (setup-func nil :read-only nil)
  (hide-method mbt-default-hide-method :read-only nil)
  (fullscreen mbt-default-fullscreen :read-only nil)
  (show-hook nil :read-only nil)
  (hide-hook nil :read-only nil))

(defun mbt-register (name buffer-pred create-func &optional setup-func)
  "Register a new buffer toggle.
NAME is a string identifier for this toggle.
BUFFER-PRED is a function that returns non-nil if a buffer is of the target type.
CREATE-FUNC is a function that creates a new buffer of the target type.
Optional SETUP-FUNC is a function called with the buffer after showing it."
  (puthash name 
           (mbt-toggle-create :name name
                             :buffer-pred buffer-pred
                             :create-func create-func
                             :setup-func setup-func)
           mbt-registered-toggles))

(defun mbt-get-toggle (name)
  "Get the toggle definition for NAME."
  (gethash name mbt-registered-toggles))

(defun mbt-set-option (name option value)
  "Set OPTION to VALUE for toggle NAME.
Options include :hide-method, :fullscreen, :show-hook, :hide-hook, :setup-func."
  (let ((toggle (mbt-get-toggle name)))
    (when toggle
      (cl-case option
        (:hide-method (setf (mbt-toggle-hide-method toggle) value))
        (:fullscreen (setf (mbt-toggle-fullscreen toggle) value))
        (:show-hook (setf (mbt-toggle-show-hook toggle) value))
        (:hide-hook (setf (mbt-toggle-hide-hook toggle) value))
        (:setup-func (setf (mbt-toggle-setup-func toggle) value))))))

(defun mbt--get-window (toggle)
  "Get a window displaying a buffer matching TOGGLE's predicate."
  (cl-find-if (lambda (w)
                (funcall (mbt-toggle-buffer-pred toggle) (window-buffer w)))
              (window-list)))

(defun mbt--get-buffer (toggle)
  "Get a buffer matching TOGGLE's predicate."
  (cl-find-if (mbt-toggle-buffer-pred toggle) (buffer-list)))

(defun mbt--recent-other-buffer ()
  "Get the most recently used non-matching buffer."
  (let ((current-buffer (current-buffer))
        other-buffer)
    (cl-loop for buf in (buffer-list) do
             (when (and (not (eq buf current-buffer))
                        (not (char-equal ?\  (aref (buffer-name buf) 0))))
               (setq other-buffer buf)
               (cl-return other-buffer)))
    other-buffer))

(defun mbt-hide (name)
  "Hide the specialized buffer for toggle NAME."
  (interactive "sToggle name: ")
  (let ((toggle (mbt-get-toggle name)))
    (when toggle
      (let ((buffer-pred (mbt-toggle-buffer-pred toggle))
            (hide-method (mbt-toggle-hide-method toggle))
            (hide-hook (mbt-toggle-hide-hook toggle)))
        (or (funcall buffer-pred (current-buffer))
            (select-window (mbt--get-window toggle)))
        (when hide-hook 
          (run-hooks hide-hook))
        (cond
         ((eq hide-method 'reset-window-configration)
          (let ((config (gethash name mbt-window-configurations)))
            (when config
              (set-window-configuration config))))
         ((eq hide-method 'bury-buffer)
          (bury-buffer))
         ((eq hide-method 'quit-window)
          (quit-window))
         ((eq hide-method 'delete-window)
          (if (window-deletable-p)
              (delete-window)
            (bury-buffer)))
         ((not hide-method)
          (let ((buf (mbt--recent-other-buffer)))
            (when buf
              (if (get-buffer-window buf)
                  (select-window (get-buffer-window buf))
                (switch-to-buffer buf))))))))))

(defun mbt-show (name)
  "Show the specialized buffer for toggle NAME."
  (interactive "sToggle name: ")
  (let ((toggle (mbt-get-toggle name)))
    (when toggle
      (let ((buffer-pred (mbt-toggle-buffer-pred toggle))
            (create-func (mbt-toggle-create-func toggle))
            (setup-func (mbt-toggle-setup-func toggle))
            (fullscreen (mbt-toggle-fullscreen toggle))
            (show-hook (mbt-toggle-show-hook toggle))
            (special-buffer (mbt--get-buffer toggle)))
        (if special-buffer
            (progn
              (when (and (not (funcall buffer-pred (current-buffer)))
                         (not (get-buffer-window special-buffer)))
                (puthash name (current-window-configuration) mbt-window-configurations))
              (if fullscreen
                  (progn
                    (delete-other-windows)
                    (switch-to-buffer special-buffer))
                (if (funcall buffer-pred (current-buffer))
                    (switch-to-buffer special-buffer nil t)
                  (pop-to-buffer special-buffer)))
              (when setup-func
                (funcall setup-func special-buffer))
              (when show-hook
                (run-hooks show-hook)))
          (unless (funcall buffer-pred (current-buffer))
            (puthash name (current-window-configuration) mbt-window-configurations))
          (with-current-buffer (setq special-buffer (funcall create-func))
            (when fullscreen
              (delete-other-windows))
            (when setup-func
              (funcall setup-func special-buffer))
            (when show-hook
              (run-hooks show-hook))))
        special-buffer))))

(defun mbt-toggle (name)
  "Toggle the specialized buffer for NAME."
  (interactive 
   (list (completing-read "Toggle: " 
                         (let (names)
                           (maphash (lambda (k _v) (push k names)) mbt-registered-toggles)
                           names))))
  (let* ((toggle (mbt-get-toggle name))
         (buffer-pred (mbt-toggle-buffer-pred toggle)))
    (if (or (funcall buffer-pred (current-buffer))
            (and (mbt--get-window toggle)
                 (mbt-toggle-hide-method toggle)))
        (mbt-hide name)
      (mbt-show name))))

(defmacro mbt-define-toggle (name docstring buffer-pred create-func &optional setup-func)
  "Define a function to toggle a specific buffer.
NAME is used for the function name and toggle registration.
DOCSTRING is the documentation for the command.
BUFFER-PRED, CREATE-FUNC and SETUP-FUNC are passed to `mbt-register'."
  (let ((func-name (intern (format "mbt-toggle-%s" name))))
    `(progn
       (mbt-register ,name ,buffer-pred ,create-func ,setup-func)
       (defun ,func-name ()
         ,docstring
         (interactive)
         (mbt-toggle ,name)))))

(provide 'multi-buffer-toggle)
;;; multi-buffer-toggle.el ends here
