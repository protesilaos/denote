;;; denote-org-extras.el --- Denote extensions for Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

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
;; WORK-IN-PROGRESS

;;; Code:

(require 'denote)
(require 'denote-sort)
(require 'org)

;;;; Link to file and heading

(defun denote-org-extras--get-outline (file)
  "Return `outline-regexp' headings and line numbers of FILE."
  (with-current-buffer (find-file-noselect file)
    (let ((outline-regexp (format "^\\(?:%s\\)" (or (bound-and-true-p outline-regexp) "[*\^L]+")))
          candidates)
      (save-excursion
        (goto-char (point-min))
        (while (if (bound-and-true-p outline-search-function)
                   (funcall outline-search-function)
                 (re-search-forward outline-regexp nil t))
          (push
           ;; NOTE 2024-01-20: The -5 (minimum width) is a
           ;; sufficiently high number to keep the alignment
           ;; consistent in most cases.  Larger files will simply
           ;; shift the heading text in minibuffer, but this is not an
           ;; issue anymore.
           (format "%-5s %s"
                   (line-number-at-pos (point))
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           candidates)
          (goto-char (1+ (line-end-position)))))
      (if candidates
          (nreverse candidates)
        (user-error "No outline")))))

(defun denote-org-extras--outline-prompt (&optional file)
  "Prompt for outline among headings retrieved by `denote-org-extras--get-outline'.
With optional FILE use the outline of it, otherwise use that of
the current file."
  (completing-read
   (format "Select heading inside `%s': "
           (propertize (file-name-nondirectory file) 'face 'denote-faces-prompt-current-name))
   (denote--completion-table-no-sort 'imenu (denote-org-extras--get-outline (or file buffer-file-name)))
   nil :require-match))

(defun denote-org-extras--get-heading-and-id-from-line (line file)
  "Return heading text and CUSTOM_ID from the given LINE in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (cons (denote-link-ol-get-heading) (denote-link-ol-get-id)))))

(defun denote-org-extras-format-link-with-heading (file heading-id description)
  "Prepare link to FILE with HEADING-ID using DESCRIPTION."
  (format "[[denote:%s::#%s][%s]]"
          (denote-retrieve-filename-identifier file)
          heading-id
          description))

;;;###autoload
(defun denote-org-extras-link-to-heading ()
  "Link to file and then specify a heading to extend the link to.

The resulting link has the following pattern:

[[denote:IDENTIFIER::#ORG-HEADING-CUSTOM-ID]][Description::Heading text]].

Because only Org files can have links to individual headings,
limit the list of possible files to those which include the .org
file extension (remember that Denote works with many file types,
per the user option `denote-file-type').

The user option `denote-org-extras-store-link-to-heading'
determined whether the `org-store-link' function can save a link
to the current heading.  Such links look the same as those of
this command, though the functionality defined herein is
independent of it.

To only link to a file, use the `denote-link' command."
  (declare (interactive-only t))
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Links to headings only work between Org files"))
  (when-let ((relative-file (denote-relative-file-prompt ".*\\.org"))
             (file (concat (denote-directory) relative-file))
             (file-text (denote--link-get-description file))
             (heading (denote-org-extras--outline-prompt file))
             (line (string-to-number (car (split-string heading "\t"))))
             (heading-data (denote-org-extras--get-heading-and-id-from-line line file))
             (heading-text (car heading-data))
             (heading-id (cdr heading-data))
             (description (denote-link-format-heading-description file-text heading-text)))
    (insert (denote-org-extras-format-link-with-heading file heading-id description))))

;;;; Extract subtree into its own note

(defun denote-org-extras--get-heading-date ()
  "Try to return a timestamp for the current Org heading.
This can be used as the value for the DATE argument of the
`denote' command."
  (when-let ((pos (point))
             (timestamp (or (org-entry-get pos "DATE")
                            (org-entry-get pos "CREATED"))))
    (date-to-time timestamp)))

;;;###autoload
(defun denote-org-extras-extract-org-subtree ()
  "Create new Denote note using the current Org subtree.
Remove the subtree from its current file and move its contents
into the new Denote file.

Take the text of the subtree's top level heading and use it as
the title of the new note.

If the heading has any tags, use them as the keywords of the new
note.  Else do not include any keywords.

If the subtree has a PROPERTIES drawer, retain it for further
review.  If the PROPERTIES drawer includes a DATE or CREATED
property with a timestamp value, use that to derive the date (or
date and time) of the new note (if there is only a date, the time
is taken as 00:00).  If both DATE and CREATED properties are
present, the former is used.

Make the new note an Org file regardless of the value of
`denote-file-type'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Headings can only be extracted from Org files"))
  (if-let ((text (org-get-entry))
           (heading (denote-link-ol-get-heading)))
      (let ((tags (org-get-tags))
            (date (denote-org-extras--get-heading-date)))
        (delete-region (org-entry-beginning-position)
                       (save-excursion (org-end-of-subtree t) (point)))
        (denote heading tags 'org nil date)
        (insert text))
    (user-error "No subtree to extract; aborting")))

;;;; Org dynamic blocks

;; NOTE 2024-01-22 12:26:13 +0200: The following is copied from the
;; now-deleted denote-org-dblock.el.  Its original author was Elias
;; Storms <elias.storms@gmail.com>, with substantial contributions and
;; further developments by me (Protesilaos).

;; This section defines Org dynamic blocks using the facility described
;; in the Org manual.  Evaluate this:
;;
;;    (info "(org) Dynamic Blocks")
;;
;; The dynamic blocks defined herein are documented at length in the
;; Denote manual.  See the following node and its subsections:
;;
;;    (info "(denote) Use Org dynamic blocks")

;;;;; Common helper functions

(defun denote-org-extras-dblock--files (files-matching-regexp &optional sort-by-component reverse)
  "Return list of FILES-MATCHING-REGEXP in variable `denote-directory'.
SORT-BY-COMPONENT and REVERSE have the same meaning as
`denote-sort-files'.  If both are nil, do not try to perform any
sorting."
  (cond
   ((and sort-by-component reverse)
    (denote-sort-get-directory-files files-matching-regexp sort-by-component reverse :omit-current))
   (sort-by-component
    (denote-sort-get-directory-files files-matching-regexp sort-by-component reverse :omit-current))
   (reverse
    (denote-sort-get-directory-files files-matching-regexp :no-component-specified reverse :omit-current))
   (t
    (denote-directory-files files-matching-regexp :omit-current))))

;;;;; Dynamic block to insert links

;;;###autoload
(defun denote-org-extras-dblock-insert-links (regexp)
  "Create Org dynamic block to insert Denote links matching REGEXP."
  (interactive
   (list
    (denote-files-matching-regexp-prompt))
   org-mode)
  (org-create-dblock (list :name "denote-links"
                           :regexp regexp
                           :sort-by-component nil
                           :reverse-sort nil
                           :id-only nil))
  (org-update-dblock))

(org-dynamic-block-define "denote-links" 'denote-org-extras-dblock-insert-links)

(defun org-dblock-write:denote-links (params)
  "Function to update `denote-links' Org Dynamic blocks.
Used by `org-dblock-update' with PARAMS provided by the dynamic block."
  (let* ((regexp (plist-get params :regexp))
         (rx (if (listp regexp) (macroexpand `(rx ,regexp)) regexp))
         (sort (plist-get params :sort-by-component))
         (reverse (plist-get params :reverse-sort))
         (block-name (plist-get params :block-name))
         (files (denote-org-extras-dblock--files rx sort reverse)))
    (when block-name (insert "#+name: " block-name "\n"))
    (denote-link--insert-links files 'org (plist-get params :id-only) :no-other-sorting)
    (join-line))) ; remove trailing empty line

;;;;; Dynamic block to insert backlinks

(defun denote-org-extras-dblock--maybe-sort-backlinks (files sort-by-component reverse)
  "Sort backlink FILES if SORT-BY-COMPONENT and/or REVERSE is non-nil."
  (cond
   ((and sort-by-component reverse)
    (denote-sort-files files sort-by-component reverse))
   (sort-by-component
    (denote-sort-files files sort-by-component))
   (reverse
    (denote-sort-files files :no-component-specified reverse))
   (t
    files)))

;;;###autoload
(defun denote-org-extras-dblock-insert-backlinks ()
  "Create Org dynamic block to insert Denote backlinks to current file."
  (interactive nil org-mode)
  (org-create-dblock (list :name "denote-backlinks"
                           :sort-by-component nil
                           :reverse-sort nil
                           :id-only nil))
  (org-update-dblock))

(org-dynamic-block-define "denote-backlinks" 'denote-org-extras-dblock-insert-backlinks)

(defun org-dblock-write:denote-backlinks (params)
  "Function to update `denote-backlinks' Org Dynamic blocks.
Used by `org-dblock-update' with PARAMS provided by the dynamic block."
  (when-let ((files (denote-link-return-backlinks)))
    (let* ((sort (plist-get params :sort-by-component))
           (reverse (plist-get params :reverse-sort))
           (files (denote-org-extras-dblock--maybe-sort-backlinks files sort reverse)))
      (denote-link--insert-links files 'org (plist-get params :id-only) :no-other-sorting)
      (join-line)))) ; remove trailing empty line

;;;;; Dynamic block to insert entire file contents

(defun denote-org-extras-dblock--get-file-contents (file &optional no-front-matter add-links)
  "Insert the contents of FILE.
With optional NO-FRONT-MATTER as non-nil, try to remove the front
matter from the top of the file.  If NO-FRONT-MATTER is a number,
remove that many lines starting from the top.  If it is any other
non-nil value, delete from the top until the first blank line.

With optional ADD-LINKS as non-nil, first insert a link to the
file and then insert its contents.  In this case, format the
contents as a typographic list.  If ADD-LINKS is `id-only', then
insert links as `denote-link' does when supplied with an ID-ONLY
argument."
  (when (denote-file-is-note-p file)
    (with-temp-buffer
      (when add-links
        (insert
         (format "- %s\n\n"
                 (denote-format-link
                  file
                  (denote--link-get-description file)
                  'org
                  (eq add-links 'id-only)))))
      (let ((beginning-of-contents (point)))
        (insert-file-contents file)
        (when no-front-matter
          (delete-region
           (if (natnump no-front-matter)
               (progn (forward-line no-front-matter) (line-beginning-position))
             (1+ (re-search-forward "^$" nil :no-error 1)))
           beginning-of-contents))
        (when add-links
          (indent-region beginning-of-contents (point-max) 2)))
      (buffer-string))))

(defvar denote-org-extras-dblock-file-contents-separator
  (concat "\n\n" (make-string 50 ?-) "\n\n\n")
  "Fallback separator used by `denote-org-extras-dblock-add-files'.")

(defun denote-org-extras-dblock--separator (separator)
  "Return appropriate value of SEPARATOR for `denote-org-extras-dblock-add-files'."
  (cond
   ((null separator) "")
   ((stringp separator) separator)
   (t denote-org-extras-dblock-file-contents-separator)))

(defun denote-org-extras-dblock-add-files (regexp &optional separator no-front-matter add-links sort-by-component reverse)
  "Insert files matching REGEXP.

Seaprate them with the optional SEPARATOR.  If SEPARATOR is nil,
use the `denote-org-extras-dblock-file-contents-separator'.

If optional NO-FRONT-MATTER is non-nil try to remove the front
matter from the top of the file.  Do it by finding the first
blank line, starting from the top of the buffer.

If optional ADD-LINKS is non-nil, first insert a link to the file
and then insert its contents.  In this case, format the contents
as a typographic list.

If optional SORT-BY-COMPONENT is a symbol among `denote-sort-components',
sort files matching REGEXP by the corresponding Denote file name
component.  If the symbol is not among `denote-sort-components',
fall back to the default identifier-based sorting.

If optional REVERSE is non-nil reverse the sort order."
  (let* ((files (denote-org-extras-dblock--files regexp sort-by-component reverse))
         (files-contents (mapcar
                          (lambda (file) (denote-org-extras-dblock--get-file-contents file no-front-matter add-links))
                          files)))
    (insert (string-join files-contents (denote-org-extras-dblock--separator separator)))))

;;;###autoload
(defun denote-org-extras-dblock-insert-files (regexp sort-by-component)
  "Create Org dynamic block to insert Denote files matching REGEXP.
Sort the files according to SORT-BY-COMPONENT, which is a symbol
among `denote-sort-components'."
  (interactive
   (list
    (denote-files-matching-regexp-prompt)
    (denote-sort-component-prompt))
   org-mode)
  (org-create-dblock (list :name "denote-files"
                           :regexp regexp
                           :sort-by-component sort-by-component
                           :reverse-sort nil
                           :no-front-matter nil
                           :file-separator nil
                           :add-links nil))
  (org-update-dblock))

(org-dynamic-block-define "denote-files" 'denote-org-extras-dblock-insert-files)

(defun org-dblock-write:denote-files (params)
  "Function to update `denote-files' Org Dynamic blocks.
Used by `org-dblock-update' with PARAMS provided by the dynamic block."
  (let* ((regexp (plist-get params :regexp))
         (rx (if (listp regexp) (macroexpand `(rx ,regexp)) regexp))
         (sort (plist-get params :sort-by-component))
         (reverse (plist-get params :reverse-sort))
         (block-name (plist-get params :block-name))
         (separator (plist-get params :file-separator))
         (no-f-m (plist-get params :no-front-matter))
         (add-links (plist-get params :add-links)))
    (when block-name (insert "#+name: " block-name "\n"))
    (when rx (denote-org-extras-dblock-add-files rx separator no-f-m add-links sort reverse)))
  (join-line)) ; remove trailing empty line


(provide 'denote-org-extras)
;;; denote-org-extras.el ends here
