;;; denote-org-dblock.el --- Denote Org Dynamic blocks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Authors: Elias Storms <elias.storms@gmail.com>,
;;          Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote

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
;; This file defines Org dynamic blocks using the facility described
;; in the Org manual.  Evaluate this:
;;
;;    (info "(org) Dynamic Blocks")
;;
;; The dynamic blocks defined herein are documented at length in the
;; Denote manual.  See the following node and its subsections:
;;
;;    (info "(denote) Use Org dynamic blocks")

;;; Code:

(require 'denote)
(require 'org)

;;;; Dynamic block to insert links

;;;###autoload
(defun denote-org-dblock-insert-links (regexp)
  "Create Org dynamic block to insert Denote links matching REGEXP."
  (interactive
    (list
     (read-regexp "Search for notes matching REGEX: " nil 'denote-link--add-links-history)))
  (org-create-dblock (list :name "denote-links"
                           :regexp regexp
                           :missing-only nil
                           :reverse nil))
  (org-update-dblock))

(org-dynamic-block-define "denote-links" 'denote-org-dblock-insert-links)

;; By using the `org-dblock-write:' format, Org-mode knows how to
;; compute the dynamic block. Inner workings of this function copied
;; from `denote-add-links'.
(defun org-dblock-write:denote-links (params)
  "Function to update `denote-links' Org Dynamic blocks.
Used by `org-dblock-update' with PARAMS provided by the dynamic block."
  (let* ((regexp (plist-get params :regexp))
         (rx (if (listp regexp) (macroexpand `(rx ,regexp)) regexp))
         (missing-only (plist-get params :missing-only))
         (block-name (plist-get params :block-name))
         (denote-link-add-links-sort (plist-get params :reverse))
         (current-file (buffer-file-name)))
    (when block-name
      (insert "#+name: " block-name "\n"))
    (if missing-only
        (progn
          (denote-add-missing-links rx)
          (join-line)) ;; remove trailing empty line left by denote-link--prepare-links
      (when-let ((files (delete current-file
                                (denote-directory-files-matching-regexp rx))))
        (insert (denote-link--prepare-links files current-file nil))
        (join-line))))) ;; remove trailing empty line

;;;; Dynamic block to insert backlinks

;;;###autoload
(defun denote-org-dblock-insert-backlinks ()
  "Insert new Org dynamic block to include backlinks."
  (interactive)
  (org-create-dblock (list :name "denote-backlinks" :reverse nil))
  (org-update-dblock))

(org-dynamic-block-define "denote-backlinks" 'denote-org-dblock-insert-backlinks)

(defun org-dblock-write:denote-backlinks (params)
  "Function to update `denote-backlinks' Org Dynamic blocks.
Used by `org-dblock-update' with PARAMS provided by the dynamic block."
  (when-let ((file (buffer-file-name))
             (id (denote-retrieve-filename-identifier file))
             (files (delete file (denote--retrieve-files-in-xrefs id))))
    (let ((denote-link-add-links-sort (plist-get params :reverse)))
      (insert (denote-link--prepare-links files file nil))
      (join-line)))) ;; remove trailing empty line

;;;; Dynamic block to insert entire file contents

(defun denote-org-dblock--get-file-contents (file &optional no-front-matter add-links)
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
  (with-temp-buffer
    (when add-links
      (insert
       (format "- %s\n\n"
               (denote-format-link
                file
                (if (eq add-links 'id-only)
                    denote-id-only-link-format
                  denote-org-link-format)
                nil))))
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
    (buffer-string)))

(defvar denote-org-dblock-file-contents-separator
  (concat "\n\n" (make-string 50 ?-) "\n\n\n")
  "Fallback separator used by `denote-org-dblock-add-files'.")

(defun denote-org-dblock--separator (separator)
  "Return appropriate value of SEPARATOR for `denote-org-dblock-add-files'."
  (cond
   ((eq separator 'none) "")
   ((stringp separator) separator)
   (t denote-org-dblock-file-contents-separator)))

(defun denote-org-dblock-add-files (regexp &optional separator no-front-matter add-links)
  "Insert files matching REGEXP.
Seaprate them with the optional SEPARATOR.  If SEPARATOR is nil,
use the `denote-org-dblock-file-contents-separator'.

If optional NO-FRONT-MATTER is non-nil try to remove the front
matter from the top of the file.  Do it by finding the first
blank line, starting from the top of the buffer.

If optional ADD-LINKS is non-nil, first insert a link to the file
and then insert its contents.  In this case, format the contents
as a typographic list."
  (let ((files (denote-directory-files-matching-regexp regexp)))
    ;; FIXME 2023-11-23: Do not use a separator for the last file.
    ;; Not a big issue, but is worth checking.
    (mapc
     (lambda (file)
       ;; NOTE 2023-11-23: I tried to just do `insert-file-contents'
       ;; without the temporary buffer, but it seems that the point is
       ;; not moved, so the SEPARATOR does not follow the contents.
       (let ((contents (denote-org-dblock--get-file-contents file no-front-matter add-links)))
         (insert (concat contents (denote-org-dblock--separator separator)))))
     files)))

;;;###autoload
(defun denote-org-dblock-insert-files (regexp)
  "Create Org dynamic block to insert Denote files matching REGEXP."
  (interactive
    (list
     (read-regexp "Search for notes matching REGEX: " nil 'denote--file-history)))
  (org-create-dblock (list :name "denote-files"
                           :regexp regexp
                           :no-front-matter nil
                           :file-separator t
                           :add-links nil))
  (org-update-dblock))

(org-dynamic-block-define "denote-files" 'denote-org-dblock-insert-files)

(defun org-dblock-write:denote-files (params)
  "Function to update `denote-files' Org Dynamic blocks.
Used by `org-dblock-update' with PARAMS provided by the dynamic block."
  (let* ((regexp (plist-get params :regexp))
         (rx (if (listp regexp) (macroexpand `(rx ,regexp)) regexp))
         (block-name (plist-get params :block-name))
         (separator (plist-get params :file-separator))
         (no-front-matter (plist-get params :no-front-matter))
         (add-links (plist-get params :add-links)))
    (when block-name
      (insert "#+name: " block-name "\n"))
    (when rx (denote-org-dblock-add-files rx separator no-front-matter add-links))))

(provide 'denote-org-dblock)
;;; denote-org-dblock.el ends here
