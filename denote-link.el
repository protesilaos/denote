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
         (id (denote-retrieve--filename-identifier file))
         (buf (format "*denote-backlinks to %s*" id)))
  (compilation-start
   (format "find * -type f ! -name '%s' -exec %s --color=auto -l -m 1 -e %s %s %s"
           file
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
