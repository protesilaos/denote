;;; denote-link.el --- Link facility for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022  Protesilaos Stavrou

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
;; Links for denote.

;;; Code:

(require 'denote)

(defgroup denote-link ()
  "Link facility for Denote."
  :group 'denote)

;;; User options

(defcustom denote-link-insert-functions
  (list #'denote-link-backlink)
  "Functions that run after `denote-link'.
Each function accepts a TARGET-FILE and an ORIGIN-LINK argument.
Both are supplied by `denote-link'."
  :type 'hook
  :group 'denote-link)

;;;; Link to note

(defun denote-link--find-key-value-pair (regexp)
  "Produce a cons cell from REGEXP by searching the file."
  (goto-char (point-min))
  (re-search-forward regexp)
  (cons (match-string-no-properties 1)
        (match-string-no-properties 2)))

(defconst denote-link--title-regexp "^\\(#\\+title:\\)[\s\t]+\\(.*\\)"
  "Regular expression for title key and value.")

(defconst denote-link--filename-regexp "^\\(#\\+filename:\\)[\s\t]+\\(.*\\)"
  "Regular expression for filename key and value.")

(defconst denote-link--identifier-regexp "^\\(#\\+identifier:\\)[\s\t]+\\(.*\\)"
  "Regular expression for filename key and value.")

(defconst denote-link--link-format "[[denote:%s][%s (%s)]]"
  "Format of Org link to note.")

(defconst denote-link--backlink-format "[[denote:%s][backlink: %s (%s)]]"
  "Format of Org backlink to note.")

(defconst denote-link--backlink-regexp "\\[\\[denote:\\(.*?\\)\\]\\[backlink: \\(.*?\\) (\\(.*?\\))\\]\\]"
  "Regexp of `denote-link--backlink-format'.")

(defun denote-link--retrieve-value (note regexp)
  "Return REGEXP value from NOTE."
  (let ((default-directory (denote-directory)))
    (with-temp-buffer
      (insert-file-contents-literally note)
      (denote-link--find-key-value-pair regexp))))

(defun denote-link--read-file-prompt ()
  "Prompt for regular file in variable `denote-directory'."
  (read-file-name "Select note: " (denote-directory)
                  nil t nil #'file-regular-p)) ; Includes backup files.  Maybe we can remove them?

;;;###autoload
(defun denote-link (target)
  "Create Org link to TARGET note in variable `denote-directory'.
Run `denote-link-insert-functions' afterwards."
  (interactive (list (denote-link--read-file-prompt)))
  (let* ((dir (denote-directory))
         (target-id (cdr (denote-link--retrieve-value target denote-link--identifier-regexp)))
         (target-name (string-remove-prefix
                       dir (cdr (denote-link--retrieve-value target denote-link--filename-regexp))))
         (target-title (cdr (denote-link--retrieve-value target denote-link--title-regexp)))
         (target-link (format denote-link--link-format target-name target-title target-id))
         (origin-note (buffer-file-name))
         (origin-id (cdr (denote-link--retrieve-value origin-note denote-link--identifier-regexp)))
         (origin-name (string-remove-prefix
                       dir (cdr (denote-link--retrieve-value origin-note denote-link--filename-regexp))))
         (origin-title (cdr (denote-link--retrieve-value origin-note denote-link--title-regexp)))
         (origin-link (format denote-link--backlink-format origin-name origin-title origin-id)))
    (insert target-link)
    (run-hook-with-args 'denote-link-insert-functions target origin-link)))

(defconst denote-link-backlink-heading "Denote backlinks"
  "String of the backlink's heading.
This heading is appended to a file when another links to it.")

(defun denote-link-backlink (target-file origin-link)
  "Insert ORIGIN-LINK to TARGET-FILE."
  (let ((default-directory (denote-directory))
        (heading denote-link-backlink-heading)
        heading-point)
    (with-current-buffer (find-file-noselect target-file)
      (goto-char (point-max))
      (unless (save-excursion (setq heading-point (re-search-backward heading nil t)))
        (unless (denote--line-regexp-p 'empty 0)
          (newline))
        (insert
         (format "* %s\n%s\n\n" heading "# Do not edit; this is for denote.el and related")))
      (insert (format "- %s\n" origin-link))
      ;; delete duplicate links
      (when heading-point
          (delete-duplicate-lines heading-point (point-max) nil nil t)))))

(defun denote-link-clear-stale-backlinks ()
  "Delete backlinks that no longer point to files."
  (interactive)
  (let ((default-directory (denote-directory)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward denote-link-backlink-heading nil t))
      (while (re-search-forward denote-link--backlink-regexp nil t)
        (unless (file-exists-p (match-string-no-properties 1))
          (delete-line))))))

(provide 'denote-link)
;;; denote-link.el ends here
