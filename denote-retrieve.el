;;; denote-retrieve.el --- Internal search functions for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
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

(defconst denote-retrieve--title-front-matter-key-regexp
  "^\\(?:#\\+\\)?\\(?:title\\)\\s-*[:=]"
  "Regular expression for title key.")

(defconst denote-retrieve--id-front-matter-key-regexp
  "^.?.?\\b\\(?:identifier\\)\\s-*[:=]"
  "Regular expression for identifier key.")

(defconst denote-retrieve--date-front-matter-key-regexp
  "^\\(?:#\\+\\)?\\(?:date\\)\\s-*[:=]"
  "Regular expression for date key.")

(defun denote-retrieve--filename-identifier (file)
  "Extract identifier from FILE name."
  (if (file-exists-p file)
      (progn
        (string-match denote--id-regexp file)
        (match-string 0 file))
    (error "Cannot find `%s' as a file" file)))

(defun denote-retrieve--search (file key-regexp &optional key)
  "Return the value associated with the KEY-REGEXP key in the
current buffer from FILE.
If optional KEY is non-nil, return the key instead."
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward key-regexp nil t 1)
          (if key
              (match-string-no-properties 0)
            (let ((trims "[ \t\n\r\"']+"))
              (string-trim
               (buffer-substring-no-properties (point) (point-at-eol))
               trims trims))))))))

(defun denote-retrieve--value-title (file &optional key)
  "Return title value from FILE.
If optional KEY is non-nil, return the key instead."
  (denote-retrieve--search file denote-retrieve--title-front-matter-key-regexp key))

(defun denote-retrieve--value-date (file &optional key)
  "Return date value from FILE.
If optional KEY is non-nil, return the key instead."
  (denote-retrieve--search file denote-retrieve--date-front-matter-key-regexp key))

(defun denote-retrieve--read-file-prompt ()
  "Prompt for regular file in variable `denote-directory'."
  (read-file-name "Select note: " (denote-directory) nil nil nil
                  (lambda (f) (or (denote--only-note-p f) (file-directory-p f)))))

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
             (denote--file-name-relative-to-denote-directory (car x)))
           xrefs)
   #'string-lessp))

(defun denote-retrieve--proces-grep (identifier)
  "Process lines matching IDENTIFIER and return list of files."
  (let* ((default-directory (denote-directory))
         (file (denote--file-name-relative-to-denote-directory (buffer-file-name))))
    (denote-retrieve--files-in-output
     (delete file (denote-retrieve--files-in-xrefs
                   (denote-retrieve--xrefs identifier))))))

(provide 'denote-retrieve)
;;; denote-retrieve.el ends here
