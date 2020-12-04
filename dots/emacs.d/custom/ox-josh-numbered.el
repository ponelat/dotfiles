;;; ox-josh.el --- Josh Exporter for org-mode -*- lexical-binding: t; -*-


;; Copyright (C) 2018 Matt Price

;; Author: Matt Price
;; Keywords: org, josh, outlines
;; Package-Version: 0.1.1
;; Package-Requires: ((emacs "24") (org "9.1.4") (ox-gfm "1.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a backend for the Org
;; exporter, based on the `md' and `gfm' back-ends.

;;; Code:

(require 'ox-gfm)

;;; Bold


(defun org-josh-bold (_bold contents _info)
  "Transcode bold from Org to JOSH
  CONTENTS is the text with bold markup.  INFO is a plist holding
  contextual information."
  (format "*%s*" contents))


(defun org-pointy-underline (_underline contents _info)
  "Transcode underline from Org to POINTY
  CONTENTS is the text with bold markup.  INFO is a plist holding
  contextual information."
  (format "_%s_" contents))

(defun indent-string-by (str level)
  "Indent STR by LEVEL spaces."
  (let ((sep (make-string level ?\ )))
    (concat sep
      (string-join
        (split-string str "\n")
        (concat "\n" sep)))))

 (defun org-pointy-italic (_italic contents _info)
  "Transcode italic from Org to POINTY
  CONTENTS is the text with bold markup.  INFO is a plist holding
  contextual information."
  (format "_%s_" contents))

(defun org-pointy-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (concat "     " (org-make-tag-string tag-list))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
            (heading (concat todo priority title))
            (body (if contents (replace-regexp-in-string "^" (make-string level ?\ ) contents) "")))
      (cond
        ((< level 2) (format "= *%s* =\n%s" title body))
        ((< level 3) (format "*%s*\n%s" title body))
        (t (format "- %s\n%s" title body))))))

(defun org-pointy-plain-text (text info)
  "Transcode a TEXT string into Markdown format.
  TEXT is the string to transcode.  INFO is a plist holding
  contextual information."
   (progn
    (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
    (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
    (when (plist-get info :preserve-breaks)
      (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
    ;; Return value.
     text))

;;; End-user functions

;;;###autoload

(defun org-pointy-export-as-pointy-slack
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

  If narrowing is active in the current buffer, only export its
  narrowed part.

  If a region is active, export that region.

  A non-nil optional argument ASYNC means the process should happen
  asynchronously.  The resulting buffer should be accessible
  through the `org-export-stack' interface.

  When optional argument SUBTREEP is non-nil, export the sub-tree
  at point, extracting information from the headline properties
  first.

  When optional argument VISIBLE-ONLY is non-nil, don't export
  contents of hidden elements.

  When optional argument BODY-ONLY is non-nil, strip title and
  table of contents from output.

  EXT-PLIST, when provided, is a property list with external
  parameters overriding Org default settings, but still inferior to
  file-local settings.

  Export is done in a buffer named \"*Org Pointy Export*\", which
  will be displayed when `org-export-show-temporary-export-buffer'
  is non-nil."
  (interactive)
  (org-export-to-buffer 'pointy "*Org Pointy Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))
;;;###autoload

(defun org-pointy-export-to-clipboard-as-pointy ()
  "Export region to pointy, and copy to the kill ring for pasting into other programs."
  (interactive)
  (let* ((org-export-with-toc nil)
         (org-export-with-smart-quotes nil))
    (kill-new (org-export-as 'pointy) ))
  )

(org-export-define-derived-backend 'pointy 'gfm
  :translate-alist
  '(
    (bold . org-pointy-bold)
    (underline . org-pointy-underline)
    (headline . org-pointy-headline)
    (italic . org-pointy-italic)
     (plain-text . org-pointy-plain-text))

  :menu-entry
  '(?s "Export to Slack/Pointy syntax"
       ((?n "To temporary buffer (numbered)"
            (lambda (a s v b) (org-pointy-export-as-pointy a s v)))))
  )

(provide 'ox-pointy)

;; Local variables:
;; coding: utf-8
;; End:

;;; ox-pointy-numbered.el ends here
