;;; denote-org.el --- Org-functionalities in addition to denote.el -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Elias Storms <elias.storms@gmail.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; This file provides extensions to Denote that are specific to
;; Org-mode.  The file should be required after both denote.el and
;; Org-mode are loaded.

;;; Code:

(require 'denote)
(require 'org)

;;; Org-mode Subtree to new note

(defun denote-org-extract-subtree ()
  "Create new Denote note as an Org file using current Org subtree.

The Org-tags are used as note keywords, and the subtree title as note title.
This command deletes the original subtree."
  (interactive)
  (let ((text (org-get-entry))
        (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment))
        (tags (org-get-tags)))
    (delete-region (org-entry-beginning-position) (org-entry-end-position))
    (denote heading tags 'org)
    (insert text)))

;;; Org-mode Dynamic blocks

;;;; Dynamic block to search links

;; Org-mode has Dynamic blocks the content of which can be computed
;; dynamically based on their header. This functionality can be
;; leveraged to create automated lists of links to specific notes
;; (similar to 'denote-link-add-links', but with the added benefit
;; that the list can be updated easily).
;;
;; A dynamic block of the 'denote-links' type looks like this:
;;
;;     #+BEGIN: denote-links :regexp "denote"
;;
;;     #+END:
;;
;; With point at the #+BEGIN: line, pressing 'C-c C-c' will replace the
;; contents of the block with links to notes matching the search
;; ':regexp'. See also the denote manual on 'denote-link-add-links'.
;;
;; To only include "missing links" (i.e., links to notes that the
;; current buffer doesn't already link to), add ':missing-only t' to
;; the block's header.
;;
;; With ':block-name "string"', include a name in the Dynamic block,
;; formated as '#+NAME: string'. This enables users to use the Dynamic
;; block as inputs for further computation, e.g. in Org source blocks.
;;
;; In summary, Org Dynamic blocks of the denote-links type can have
;; three arguments:
;;  1. :regexp "string" -- the search input (required)
;;  2. :missing-only t  -- to only include missing links
;;  3. :block-name "n"  -- to include a name for later processing
;;
;; Inserting a block can be done via the Org-mode entry point
;; 'org-dynamic-block-insert-dblock' and selecting 'denote-links' from
;; the list, or directly by calling 'org-dblock-insert-denote-links'.
;;
;;;###autoload
(defun org-dblock-insert-denote-links (regexp)
  "Insert new Org dynamic block to insert denote links."
  (interactive
    (list (read-string "Search for (include _ for keyword): ")))
  (org-create-dblock (list :name "denote-links"
                           :regexp regexp
                           :missing-only 't))
  (org-update-dblock))

(org-dynamic-block-define "denote-links" 'org-dblock-insert-denote-links)

;; By using the `org-dblock-write:' format, Org-mode knows how to
;; compute the dynamic block. Inner workings of this function copied
;; from `denote-link-add-links'.
;;
;;;###autoload
(defun org-dblock-write:denote-links (params)
  (let ((regexp (plist-get params :regexp))
        (missing-only (plist-get params :missing-only))
        (block-name (plist-get params :block-name))
        (current-file (buffer-file-name)))
    (when block-name
      (insert "#+NAME: " block-name "\n"))
    (if missing-only
        (progn
          (denote-link-add-missing-links regexp)
          (join-line)) ;; remove trailing empty line left by denote-link--prepare-links
      (when-let ((files (delete current-file
                                (denote-directory-files-matching-regexp regexp))))
        (insert (denote-link--prepare-links files current-file nil))
        (join-line))))) ;; remove trailing empty line

;;;; Dynamic block for backlinks

;; Similarly, we can create a 'denote-backlinks' block that inserts
;; links to notes that link to the current note.

;; Note that this block type doesn't take any additional parameters
;; (such as ':missing-only').

;;;###autoload
(defun org-dblock-insert-denote-backlinks ()
  "Insert new Org dynamic block to include backlinks."
  (interactive)
  (org-create-dblock (list :name "denote-backlinks"))
  (org-update-dblock))

(org-dynamic-block-define "denote-backlinks" 'org-dblock-insert-denote-backlinks)

;;;###autoload
(defun org-dblock-write:denote-backlinks (params)
  (when-let* ((file (buffer-file-name))
              (id (denote-retrieve-filename-identifier file))
              (files (denote--retrieve-files-in-xrefs
                      (denote--retrieve-process-grep id))))
    (insert (denote-link--prepare-links files file nil))
    (join-line))) ;; remove trailing empty line

(provide 'denote-org)
;;; denote-org.el ends here
