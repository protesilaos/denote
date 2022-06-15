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

(defcustom denote-link-register-ol-hyperlink t
  "When non-nil, register the `denote:' custom Org hyperlink type.
This practically means that the links Denote creates will behave
link ordinary links in Org files.  They can be followed with a
mouse click or the `org-open-at-point' command, and they can be
insterted with completion via the `org-insert-link' command after
selecting the `denote:' hyperlink type.

When this option is nil, Denote links will not work properly in
Org files.  All commands that Denote defines, such as
`denote-link-backlinks' and `denote-link-find-file' will work as
intended.

Note that if you do not want to `require' ol.el, you must set
this option to nil BEFORE loading denote-link.el."
  :type 'boolean
  :group 'denote-link)

(defcustom denote-link-backlinks-display-buffer-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . fit-window-to-buffer))
  "The action used to display the current file's backlinks buffer.

The value has the form (FUNCTION . ALIST), where FUNCTION is
either an \"action function\", a list thereof, or possibly an
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

;; Arguments are: FILE-ID FILE-TITLE
(defconst denote-link--format-org "[[denote:%s][%s]]"
  "Format of Org link to note.")

(defconst denote-link--format-markdown "[%2$s](denote:%1$s)"
  "Format of Markdown link to note.")

(defconst denote-link--regexp-org
  (concat "\\[\\[" "denote:"  "\\(?1:" denote--id-regexp "\\)" "]" "\\[.*?]]"))

(defconst denote-link--regexp-markdown
  (concat "\\[.*?]" "(denote:"  "\\(?1:" denote--id-regexp "\\)" ")"))

(defun denote-link--file-type-format (file)
  "Return link format based on FILE format."
  (pcase (file-name-extension file)
    ("md" denote-link--format-markdown)
    (_ denote-link--format-org))) ; Includes backup files.  Maybe we can remove them?

(defun denote-link--file-type-regexp (file)
  "Return link regexp based on FILE format."
  (pcase (file-name-extension file)
    ("md" denote-link--regexp-markdown)
    (_ denote-link--regexp-org)))

(defun denote-link--format-link (file pattern)
  "Prepare link to FILE using PATTERN."
  (let* ((file-id (denote-retrieve--filename-identifier file))
         (file-title (denote-retrieve--value file denote-retrieve--title-front-matter-regexp)))
    (format pattern file-id file-title)))

;;;###autoload
(defun denote-link (target)
  "Create link to TARGET note in variable `denote-directory'."
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

(defvar denote-link--links-to-files nil
  "String of `denote-link-add-links-matching-keyword'.")

(defun denote-link--prepare-links (files ext)
  "Prepare links to FILES using format of EXT."
  (setq denote-link--links-to-files
        (with-temp-buffer
          (mapc (lambda (f)
                  (insert (concat "- " (denote-link--format-link f ext)))
                  (newline))
                files)
          (let ((min (point-min))
                (max (point-max)))
            (buffer-substring-no-properties min max)))))

(defvar denote-link--add-links-history nil
  "Minibuffer history for `denote-link-add-links'.")

;;;###autoload
(defun denote-link-add-links (regexp)
  "Insert links to all notes matching REGEXP.
Use this command to reference multiple files at once.
Particularly useful for the creation of metanotes (read the
manual for more on the matter)."
  (interactive
   (list (read-regexp "Insert links matching REGEX: " nil 'denote-link--add-links-history)))
  (let* ((default-directory (denote-directory))
         (ext (denote-link--file-type-format (buffer-file-name))))
    (if-let ((files (denote--directory-files-matching-regexp regexp)))
        (insert (denote-link--prepare-links files ext))
      (user-error "No links matching `%s'" regexp))))

;;;; Register `denote:' custom Org hyperlink

(declare-function org-link-set-parameters "ol.el" (type &rest parameters))

;; REVIEW 2022-06-15: Maybe there is a better way to make this optional.
(when denote-link-register-ol-hyperlink
  (require 'ol)
  (org-link-set-parameters
   "denote"
   :follow #'denote-link-ol
   :complete #'denote-link-ol-complete))

(defun denote-link--ol-find-file (identifier)
  "Visit file with IDENTIFIER.
Uses the function `denote-directory' to establish the path to the
file."
  (find-file (file-name-completion identifier (denote-directory))))

(defun denote-link-ol (identifier _)
  "Find file of type `denote:' matching IDENTIFIER."
  (funcall #'denote-link--ol-find-file identifier))

(defun denote-link-ol-complete ()
  "Like `denote-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `denote:' hyperlink type."
  (insert
   (denote-link--format-link
    (denote-retrieve--read-file-prompt)
    (denote-link--file-type-format (buffer-file-name)))))

(provide 'denote-link)
;;; denote-link.el ends here
