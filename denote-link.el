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

;;;; Link to note

(defconst denote-link--link-format-org "[[file:%s][%s (%s)]]"
  "Format of Org link to note.")

(defconst denote-link--link-format-markdown "[%2$s (%3$s)](file:%1$s)"
  "Format of Markdown link to note.")

(defconst denote-link--link-format-text "<LINK: %s> [NAME %s (%s)]"
  "Format of plain text link to note.")

(defun denote-link--file-type-format (file)
  "Return link pattern based on FILE format."
  (pcase (file-name-extension file)
    ("markdown" denote-link--link-format-markdown)
    ("text" denote-link--link-format-text)
    (_ denote-link--link-format-org))) ; Includes backup files.  Maybe we can remove them?

(defun denote-link--format-link (file pattern)
  "Prepare link to FILE using PATTERN."
  (let* ((file-id (denote-retrieve--value file denote-retrieve--identifier-regexp))
         (file-title (denote-retrieve--value file denote-retrieve--title-regexp)))
    (format pattern file-id file-title)))

;;;###autoload
(defun denote-link (target)
  "Create Org link to TARGET note in variable `denote-directory'.
Run `denote-link-insert-functions' afterwards."
  (interactive (list (denote-retrieve--read-file-prompt)))
  (let* ((origin (buffer-file-name))
         (link (denote-link--format-link target (denote-link--file-type-format origin))))
    (insert link)))

;; TODO 2022-06-14: Write `denote-link-find-file' command.  It should be
;; able to enhance this core idea:
;;
;; (file-name-completion file-id dir)

;;;; Backlinks' buffer (WORK-IN-PROGRESS)

(define-button-type 'denote-link-find-file
  'follow-link t
  'action #'denote-link--find-file
  'face 'unspecified)

(defun denote-link--find-file (button)
  "Action for BUTTON."
  (find-file (buffer-substring (button-start button) (button-end button))))

(declare-function denote-dired-mode "denote-dired")

(defun denote-link--prettify-compilation (buffer _output)
  "Narrow to grep matches in BUFFER.
PROOF-OF-CONCEPT."
  (with-current-buffer buffer
    (narrow-to-region
     (progn
       (re-search-forward "find" nil t)
       (forward-line 1)
       (point))
     (progn
       (re-search-forward "Grep" nil t)
       (forward-line -1)
       (point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "%s" denote--file-regexp) (point-max) t)
        (make-button (match-beginning 0) (match-end 0) :type 'denote-link-find-file)))
    (denote-dired-mode 1)))

;;;###autoload
(defun denote-link-backlinks ()
  "PROOF-OF-CONCEPT."
  (interactive)
  (let* ((default-directory (denote-directory))
         (file (file-name-nondirectory (buffer-file-name)))
         (id (denote-retrieve--value file denote-retrieve--identifier-regexp))
         (buf (format "*denote-backlinks to %s*" id)))
  (compilation-start
   (format "find * -type f -exec %s --color=auto -l -m 1 -e %s- %s %s"
           grep-program
           id
           (shell-quote-argument "{}")
		   (shell-quote-argument ";"))
   'grep-mode
   (lambda (_) buf)
   t)
  (with-current-buffer buf
    (add-hook 'compilation-finish-functions #'denote-link--prettify-compilation nil t))))

(provide 'denote-link)
;;; denote-link.el ends here
