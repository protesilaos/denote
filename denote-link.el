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

;;; User options

(defcustom denote-link-insert-functions nil
  "Functions that run after `denote-link'.
Each function accepts a TARGET file and a BACKLINK argument.
Both are supplied by `denote-link'.

Advanced users are encouraged to study `denote-link-backlink' for
how those arguments are used.  Add that function to this hook if
you want Denote to automatically insert backlinks in the
applicable files.  Though you might prefer to use the command
`denote-link-backlinks', which does not touch the underlying
files."
  :type 'hook
  :group 'denote-link)

;;;; Link to note

(defconst denote-link--link-format-org "[[file:%s][%s (%s)]]"
  "Format of Org link to note.")

(defconst denote-link--backlink-format-org "[[file:%s][backlink: %s (%s)]]"
  "Format of Org backlink to note.")

(defconst denote-link--link-format-markdown "[%2$s (%3$s)](file:%1$s)"
  "Format of Markdown link to note.")

(defconst denote-link--backlink-format-markdown "[backlink: %2$s (%3$s)](file:%1$s)"
  "Format of Markdown backlink to note.")

(defconst denote-link--link-format-text "<LINK: %s> [NAME %s (%s)]"
  "Format of plain text link to note.")

(defconst denote-link--backlink-format-text "BACKLINK: <%s> [NAME %s (%s)]"
  "Format of plain text backlink to note.")

(defconst denote-link--backlink-regexp "\\[\\[file:\\(.*?\\)\\]\\[backlink: \\(.*?\\) (\\(.*?\\))\\]\\]"
  "Regexp of `denote-link--backlink-format-org'.")

(defun denote-link--file-type-format (file &optional backlink)
  "Return link pattern based on FILE format.
With optional BACKLINK, return a backlink pattern"
  (pcase (file-name-extension file)
    ("markdown" (if backlink denote-link--backlink-format-markdown denote-link--link-format-markdown))
    ("text" (if backlink denote-link--backlink-format-text denote-link--link-format-text))
    (_ (if backlink denote-link--backlink-format-org denote-link--link-format-org)))) ; Includes backup files.  Maybe we can remove them?

(defun denote-link--format-link (file pattern)
  "Prepare link to FILE using PATTERN."
  (let* ((dir (denote-directory))
         (file-id (denote-retrieve--value file denote-retrieve--identifier-regexp))
         (file-path (file-name-completion file-id dir))
         (file-title (denote-retrieve--value file denote-retrieve--title-regexp)))
    (format pattern file-path file-title file-id)))

;;;###autoload
(defun denote-link (target)
  "Create Org link to TARGET note in variable `denote-directory'.
Run `denote-link-insert-functions' afterwards."
  (interactive (list (denote-retrieve--read-file-prompt)))
  (let* ((origin (buffer-file-name))
         (link (denote-link--format-link target (denote-link--file-type-format origin)))
         (backlink (denote-link--format-link origin (denote-link--file-type-format target :backlink))))
    (insert link)
    (run-hook-with-args 'denote-link-insert-functions target backlink)))

;;;; Backlinks' buffer (WORK-IN-PROGRESS)

;; (require 'button)
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

;;;; Automatic backlink insertion (SUBJECT TO REMOVAL)

(defconst denote-link-backlink-heading "Denote backlinks"
  "String of the backlink's heading.
This heading is appended to a file when another links to it.")

(defvar denote-link-backlink-warning
  "Do not edit past this line; this is for denote.el and related."
  "String that warns about not editing the backlinks' section.")

(defvar denote-link--markdown-comment "<!-- %s -->"
  "Specifier for Markdown comments passed to `format'.")

(defvar denote-link--org-comment "# %s"
  "Specifier for Markdown comments passed to `format'.")

(defun denote-link--format-comment (comment filetype)
  "Use appropriate COMMENT for FILETYPE."
  (pcase filetype
    ("md" (format denote-link--markdown-comment comment))
    (_ (format denote-link--org-comment comment))))

(defvar denote-link--markdown-heading "%s\n# %s\n\n"
  "Specifier for Markdown heading passed to `format'.")

(defvar denote-link--org-heading "%s\n* %s\n\n"
  "Specifier for Org heading passed to `format'.")

(defvar denote-link--text-heading "%s\n%s\n%s\n\n"
  "Specifier for plain text heading passed to `format'.")

(defun denote-link--format-heading (heading filetype comment)
  "Use appropriate HEADING for FILETYPE, while prepending COMMENT."
  (pcase filetype
    ("md" (format denote-link--markdown-heading comment heading))
    ("org" (format denote-link--org-heading comment heading))
    (_ (format denote-link--text-heading comment heading (make-string 16 ?=)))))

(defun denote-link--format-backlinks-heading (heading)
  "Format HEADING for backlinks."
  (let* ((ext (file-name-extension (buffer-file-name)))
         (comment (denote-link--format-comment denote-link-backlink-warning ext)))
    (denote-link--format-heading heading ext comment)))

(defun denote-link-backlink (target backlink)
  "Insert BACKLINK to TARGET file."
  (let ((default-directory (denote-directory))
        (heading denote-link-backlink-heading)
        heading-point)
    (with-current-buffer (find-file-noselect target)
      (goto-char (point-max))
      (unless (save-excursion (setq heading-point (re-search-backward heading nil t)))
        (unless (denote--line-regexp-p 'empty 0)
          (newline))
        (insert (denote-link--format-backlinks-heading heading)))
      (insert (format "- %s\n" backlink))
      ;; delete duplicate links
      (when heading-point
        (delete-duplicate-lines heading-point (point-max) nil nil t)))))

(defun denote-link-clear-stale-backlinks ()
  "Delete backlinks that no longer point to files."
  (interactive)
  (let ((default-directory (denote-directory)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward denote-link-backlink-heading nil t)
        (while (re-search-forward denote-link--backlink-regexp nil t)
          (unless (file-exists-p (match-string-no-properties 1))
            (delete-region (point-at-bol) (point-at-bol 2))))))))

(provide 'denote-link)
;;; denote-link.el ends here
