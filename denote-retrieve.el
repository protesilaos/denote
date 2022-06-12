;;; denote-retrieve.el --- Link facility for Denote -*- lexical-binding: t -*-

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
;; Used internally by commands that operate on file contents.

;;; Code:

(require 'denote)

(defconst denote-retrieve--title-regexp
  "^\\(?:#\\+\\)?\\(?:title:\\)[\s\t]+\\(?1:.*\\)"
  "Regular expression for title key and value.

The match that needs to be extracted is explicityly marked as
group 1.  `denote-retrieve--find-value' uses the group 1 sting.")

(defconst denote-retrieve--identifier-regexp
  "^.?.?\\b\\(?:identifier\\|ID\\)\\s-*[:=]\\s-*\"?\\(?1:[0-9T]+\\)"
  "Regular expression for filename key and value.

The match that needs to be extracted is explicityly marked as
group 1.  `denote-retrieve--find-value' uses the group 1 sting.")

(defun denote-retrieve--search (regexp)
  "Search for REGEXP in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward regexp nil t 1)
      (match-string-no-properties 1))))

(defun denote-retrieve--find-value (regexp)
  "Return value from REGEXP by searching the file."
  (or (denote-retrieve--search regexp)
      (user-error "Cannot retrieve %s" regexp)))

(defun denote-retrieve--retrieve-value (file regexp)
  "Return REGEXP value from FILE.
FILE is a note in the variable `denote-directory'."
  (let ((default-directory (denote-directory)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (denote-retrieve--find-value regexp))))

(defun denote-retrieve--read-file-prompt ()
  "Prompt for regular file in variable `denote-directory'."
  (read-file-name "Select note: " (denote-directory) nil t nil #'file-regular-p))

(provide 'denote-retrieve)
;;; denote-retrieve.el ends here
