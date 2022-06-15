;;; denote-link.el --- Link facility for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The linking facility is subject to review and there will likely be
;; breaking changes.  This is the only area that needs to be fixed
;; before we release the first stable version of the package.

;;; Code:

(require 'denote-retrieve)

(defgroup denote-link ()
  "Link facility for Denote."
  :group 'denote)

;;;; User options

(defcustom denote-link-fontify-backlinks t
  "When non-nil, apply faces to files in the backlinks' buffer."
  :type 'boolean
  :group 'denote-link)

(defcustom denote-link-backlinks-display-buffer-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . fit-window-to-buffer))
  "The action used to display the current file's backlinks buffer.

The value has the form (FUNCTION . ALIST), where FUNCTION is
either an \"action function\", a list thereof, or a possibly
empty list.  ALIST is a list of \"action alist\" which may be
omitted (or be empty).

Sample configuration to display the buffer in a side window on
the left of the Emacs frame:

    (setq denote-link-backlinks-display-buffer-action
          (quote ((display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (side . left)
                  (slot . 99)
                  (window-width . 0.3))))

See Info node `(elisp) Displaying Buffers' for more details
and/or the documentation string of `display-buffer'."
  :type '(cons (choice (function :tag "Display Function")
                       (repeat :tag "Display Functions" function))
               alist)
  :group 'denote-link)

;;;; Link to note

;; FIXME 2022-06-14 16:58:24 +0300: Plain text links will use the
;; identifier only.  But for Org we need to figure out a better way of
;; integrating with Org/Markdown in a standard way.  Relying on
;; org-id.el may be the right course of action for Org.  What does
;; markdown-mode require?
;;
;; Whatever we do, we need to consider the implications very carefully.
;; This is not something we can undo once the package gets its first
;; stable release.
;;
;; My principle is to avoid dependencies as much as possible.  The
;; `denote-link-find-file' exemplifies this idea.
;;
;; Discussions on the GitHub mirror:
;;
;; * https://github.com/protesilaos/denote/issues/8
;; * https://github.com/protesilaos/denote/issues/13
;;
;; And on the mailing list:
;;
;; * https://lists.sr.ht/~protesilaos/denote/%3C9ac1913b-7e8f-7d38-b547-771861a8d641%40eh-is.de%3E
;; * https://lists.sr.ht/~protesilaos/denote/%3C87edzvd5oz.fsf%40cassou.me%3E

;; Arguments are: FILE-ID FILE-TITLE
(defconst denote-link--format-org "[[denote:%s][%s]]"
  "Format of Org link to note.")

(defconst denote-link--format-markdown "[%2$s](denote:%1$s)"
  "Format of Markdown link to note.")

(defconst denote-link--format-text "[[%2$s] [%1$s]]"
  "Format of plain text link to note.")

(defconst denote-link--regexp-org
  (concat "\\[\\[" "denote:"  "\\(?1:" denote--id-regexp "\\)" "]" "\\[.*?]]"))

(defconst denote-link--regexp-markdown
  (concat "\\[.*?]" "(denote:"  "\\(?1:" denote--id-regexp "\\)" ")"))

(defconst denote-link--regexp-text
  (concat "\\[\\["  ".*?]" "\s?" "\\[" "\\(?1:" denote--id-regexp "\\)" "]]"))

(defun denote-link--file-type-format (file)
  "Return link format based on FILE format."
  (pcase (file-name-extension file)
    ("md" denote-link--format-markdown)
    ("txt" denote-link--format-text)
    (_ denote-link--format-org))) ; Includes backup files.  Maybe we can remove them?

(defun denote-link--file-type-regexp (file)
  "Return link regexp based on FILE format."
  (pcase (file-name-extension file)
    ("md" denote-link--regexp-markdown)
    ("txt" denote-link--regexp-text)
    (_ denote-link--regexp-org)))

(defun denote-link--format-link (file pattern)
  "Prepare link to FILE using PATTERN."
  (let* ((file-id (denote-retrieve--filename-identifier file))
         (file-title (denote-retrieve--value file denote-retrieve--title-front-matter-regexp)))
    (format pattern file-id file-title)))

;;;###autoload
(defun denote-link (target)
  "Create Org link to TARGET note in variable `denote-directory'.
Run `denote-link-insert-functions' afterwards."
  (interactive (list (denote-retrieve--read-file-prompt)))
  (insert
   (denote-link--format-link
    target
    (denote-link--file-type-format (buffer-file-name)))))

(defun denote-link--collect-identifiers (regexp)
  "Return collection of identifiers in buffer matching REGEXP."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string-no-properties 1) matches)))
    matches))

(defun denote-link--expand-identifiers (regexp)
  "Expend identifiers matching REGEXP into file paths."
  (delq nil (mapcar (lambda (i)
                      (file-name-completion i (denote-directory)))
                    (denote-link--collect-identifiers regexp))))

(defvar denote-link--find-file-history nil
  "History for `denote-link-find-file'.")

(defun denote-link--find-file-prompt (files)
  "Prompt for linked file among FILES."
  (completing-read "Find linked file "
                   (denote--completion-table 'file files)
                   nil t
                   nil 'denote-link--find-file-history))

;; TODO 2022-06-14: We should document the use of Embark for
;; `denote-link-find-file'.  Users are gonna love it!

;; TODO 2022-06-14: Do we need to add any sort of extension to better
;; integrate with Embark?  For the minibuffer interaction it is not
;; necessary, but maybe it can be done to immediately recognise the
;; identifiers are links to files?

;;;###autoload
(defun denote-link-find-file ()
  "Use minibuffer completion to visit linked file."
  (interactive)
  (if-let* ((regexp (denote-link--file-type-regexp (buffer-file-name)))
            (files (denote-link--expand-identifiers regexp)))
      (find-file (denote-link--find-file-prompt files))
    (user-error "No links found in the current buffer")))

;;;; Backlinks' buffer (WORK-IN-PROGRESS)

(define-button-type 'denote-link-find-file
  'follow-link t
  'action #'denote-link--find-file
  'face 'unspecified)

;; NOTE 2022-06-15: I add this as a variable for advanced users who may
;; prefer something else.  If there is demand for it, we can make it a
;; defcustom, but I think it would be premature at this stage.
(defvar denote-link-buton-action #'find-file-other-window
  "Action for `denote-link--find-file'.")

(defun denote-link--find-file (button)
  "Action for BUTTON to `find-file'."
  (funcall denote-link-buton-action (buffer-substring (button-start button) (button-end button))))

(declare-function denote-dired-mode "denote-dired")

(defun denote-link--display-buffer (buf)
  "Run `display-buffer' on BUF."
  (display-buffer
   buf
   `(,@denote-link-backlinks-display-buffer-action)))

(defun denote-link--prepare-backlinks (id files &optional title)
  "Create backlinks' buffer for ID including FILES.
Use optional TITLE for a prettier heading."
  (let ((inhibit-read-only t)
        (buf (format "*denote-backlinks to %s*" id)))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (special-mode)
      (goto-char (point-min))
      (when-let* ((title)
                  (heading (format "Backlinks to %S (%s)" title id))
                  (l (length heading)))
        (insert (format "%s\n%s\n\n" heading (make-string l ?-))))
      (mapc (lambda (f)
              (insert (file-name-nondirectory f))
              (make-button (point-at-bol) (point-at-eol) :type 'denote-link-find-file)
              (newline))
            files)
      (goto-char (point-min))
      ;; NOTE 2022-06-15: Technically this is not Dired.  Maybe we
      ;; should abstract the fontification into a general purpose
      ;; minor-mode.
      (when denote-link-fontify-backlinks
        (denote-dired-mode 1)))
    (denote-link--display-buffer buf)))

;;;###autoload
(defun denote-link-backlinks ()
  "Produce a buffer with files linking to current note.
Each file is a clickable/actionable button that visits the
referenced entry.  Files are fontified if the user option
`denote-link-fontify-backlinks' is non-nil.

The placement of the backlinks' buffer is controlled by the user
option `denote-link-backlinks-display-buffer-action'.  By
default, it will show up below the current window."
  (interactive)
  (let* ((default-directory (denote-directory))
         (file (file-name-nondirectory (buffer-file-name)))
         (id (denote-retrieve--filename-identifier file))
         (title (denote-retrieve--value file denote-retrieve--title-front-matter-regexp)))
    (if-let ((files (denote-retrieve--proces-grep id)))
        (denote-link--prepare-backlinks id files title)
      (user-error "No links to the current note"))))

(provide 'denote-link)
;;; denote-link.el ends here
