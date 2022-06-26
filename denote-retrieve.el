;;; denote-retrieve.el --- Internal search functions for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing list: https://lists.sr.ht/~protesilaos/denote
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))

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
;; Used internally by commands that operate on file contents.

;;; Code:

(require 'denote)
(require 'xref)

(defconst denote-retrieve--title-front-matter-regexp
  "^\\(?:#\\+\\)?\\(?:title\\)\\s-*[:=]\\s-*[\"']?\\(?1:.*\\b\\)[\"']?"
  "Regular expression for title key and value.
The match that needs to be extracted is explicityly marked as
group 1.")

(defconst denote-retrieve--id-front-matter-regexp
  "^.?.?\\b\\(?:identifier\\|[Ii][Dd]\\)\\s-*[:=]\\s-*[\"']?\\(?1:[0-9T]+\\)[\"']?"
  "Regular expression for identifier key and value.
The match that needs to be extracted is explicityly marked as
group 1.")

(defconst denote-retrieve--date-front-matter-regexp
  "^\\(?:#\\+\\)?\\(?:date\\)\\s-*[:=]\\s-*[\"']?\\(?1:.*\\b]?\\)[\"']?"
  "Regular expression for date key and value.
The match that needs to be extracted is explicityly marked as
group 1.")

(defun denote-retrieve--filename-identifier (file)
  "Extract identifier from FILE name."
  (if (file-exists-p file)
      (progn
        (string-match denote--id-regexp file)
        (match-string 0 file))
    (error "Cannot find `%s' as a file" file)))

(defun denote-retrieve--search (regexp &optional group)
  "Search for REGEXP in the current buffer.
With optional GROUP match it, else match group 1."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward regexp nil t 1)
      (unless (eq (point) (point-min))
        (match-string-no-properties (or group 1))))))

(defun denote-retrieve--value (file regexp &optional group)
  "Return REGEXP value from FILE.
FILE is a note in the variable `denote-directory'.

Optional GROUP is a regexp construct for
`denote-retrieve--search'."
  (with-temp-buffer
    (insert-file-contents file)
    (or (denote-retrieve--search regexp group)
        nil)))

(defun denote-retrieve--value-title (file &optional group)
  "Return title from FILE, optionally matching regexp GROUP."
  (denote-retrieve--value file denote-retrieve--title-front-matter-regexp group))

(defun denote-retrieve--value-date (file &optional group)
  "Return date from FILE, optionally matching regexp GROUP."
  (denote-retrieve--value file denote-retrieve--date-front-matter-regexp group))

(defun denote-retrieve--read-file-prompt ()
  "Prompt for regular file in variable `denote-directory'."
  (read-file-name "Select note: " (denote-directory) nil nil nil #'denote--only-note-p))

(defun denote-retrieve--files-in-output (files)
  "Return list of FILES from `find' output."
  (delq nil (mapcar (lambda (f)
                      (when (denote--only-note-p f) f))
                    files)))

(defun denote-retrieve--xrefs (identifier)
  "Return xrefs of IDENTIFIER in variable `denote-directory'.
The xrefs are returned as an alist."
  (xref--analyze
   (xref-matches-in-files identifier (denote--directory-files :absolute))))

(defun denote-retrieve--files-in-xrefs (xrefs)
  "Return sorted file names sans directory from XREFS.
Parse `denote-retrieve--xrefs'."
  (sort
   (mapcar (lambda (x)
             (file-name-nondirectory (car x)))
           xrefs)
   #'string-lessp))

(defun denote-retrieve--proces-grep (identifier)
  "Process lines matching IDENTIFIER and return list of files."
  (let* ((default-directory (denote-directory))
         (file (file-name-nondirectory (buffer-file-name))))
    (denote-retrieve--files-in-output
     (delete file (denote-retrieve--files-in-xrefs
                   (denote-retrieve--xrefs identifier))))))

(provide 'denote-retrieve)
;;; denote-retrieve.el ends here
