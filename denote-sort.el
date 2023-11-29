;;; denote-sort.el ---  Sort Denote files based on a file name component -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
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
;; Sort Denote files based on their file name components, namely, the
;; signature, title, or keywords.
;;
;; NOTE 2023-11-29: This file is in development.  My plan is to
;; integrate it with denote-org-dblock.el and maybe with denote.el
;; based on user feedback, but I first want to have a stable
;; interface.

;;; Code:

(require 'denote)

(defgroup denote-sort nil
  "Sort Denote files based on a file name component."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defvar denote-sort-comparison-function #'string-collate-lessp
  "String comparison function used by `denote-sort-files' subroutines.")

(defmacro denote-sort--define (component)
  "Define Denote sort function for file name COMPONENT."
  `(defun ,(intern (format "denote-sort-%s-lessp" component)) (file1 file2)
     ,(format "Return smallest between FILE1 and FILE2 based on their %s.
The comparison is done with `denote-sort-comparison-function' between the
two signature values." component)
     (when-let ((one (,(intern (format "denote-retrieve-filename-%s" component)) file1))
                (two (,(intern (format "denote-retrieve-filename-%s" component)) file2))
                (sort (funcall denote-sort-comparison-function one two)))
       file1)))

(denote-sort--define title)
(denote-sort--define keywords)
(denote-sort--define signature)

;;;###autoload
(defun denote-sort-files (files &optional component reverse)
  "Returned sorted list of Denote FILES.

With optional COMPONENT as a keyword of `:signature', `:title',
`:keywords', sort files based on the corresponding file name
component.

Without COMPONENT, do not sort: keep the original date-based
sorting which relies on the identifier of each file name.

With optional REVERSE as a non-nil value, reverse the sort order."
  (let* ((files-to-sort (copy-sequence files))
         (sorted-files (if component
                           (sort files
                                 (pcase component
                                   (:title #'denote-sort-title-lessp)
                                   (:keywords #'denote-sort-keywords-lessp)
                                   (:signature #'denote-sort-signature-lessp)
                                   (_ #'ignore)))
                         files-to-sort)))
    (if reverse
        (reverse sorted-files)
      sorted-files)))

(defun denote-sort-get-directory-files (files-matching-regexp sort-by-component &optional reverse)
  "Return sorted list of files in variable `denote-directory'.

With FILES-MATCHING-REGEXP as a string limit files to those
matching the given regular expression.

With SORT-BY-COMPONENT as a Lisp keyword, pass it to
`denote-sort-files' to sort by the corresponding file name
component.

With optional REVERSE as a non-nil value, reverse the sort order."
  (denote-sort-files
   (denote-directory-files-matching-regexp files-matching-regexp)
   sort-by-component
   reverse))

(defvar denote-sort--files-matching-regexp-hist nil
  "Minibuffer history of `denote-sort--files-matching-regexp-prompt'.")

(defun denote-sort--files-matching-regexp-prompt ()
  "Prompt for REGEXP to filter Denote files by."
  (read-regexp "Match files with the given REGEXP: " nil 'denote-sort--files-matching-regexp-hist))

(defvar denote-sort--component-key-hist nil
  "Minibuffer history of `denote-sort--component-key-prompt'.")

(defvar denote-sort-files-keys '(:title :keywords :signature :identifier)
  "List of sorting keys applicable for `denote-sort-files'.")

(defun denote-sort--component-key-prompt ()
  "Prompt `denote-sort-files' for sorting key among `denote-sort-files-keys'."
  (let ((default (car denote-sort--component-key-hist)))
    (intern
     (completing-read
      (format-prompt "Sort by file name component " default)
      denote-sort-files-keys nil :require-match
      nil 'denote-sort--component-key-hist default))))

;;;###autoload
(defun denote-sort-dired (files-matching-regexp sort-by-component reverse)
  "Produce Dired buffer with sorted files from variable `denote-directory'.
When called interactively, prompt for both FILES-MATCHING-REGEXP
and SORT-BY-COMPONENT.  The former limits the list of Denote
files to those matching the regular expression, while the latter
sorts them by their file name component (title, signature, or
keywords).


REVERSE."
  (interactive
   (list
    (denote-sort--files-matching-regexp-prompt)
    (denote-sort--component-key-prompt)
    (y-or-n-p "Reverse sort? ")))
  ;; TODO 2023-11-29: Can we not show the full file path?  Maybe by
  ;; binding `default-directory' to `denote-directory'?  Should we do
  ;; that?  What are the downsides?
  (dired
   (cons
    (format "Denote files matching `%s' sorted by %s" files-matching-regexp sort-by-component)
    (denote-sort-get-directory-files files-matching-regexp sort-by-component reverse))))

(provide 'denote-sort)
;;; denote-sort.el ends here
