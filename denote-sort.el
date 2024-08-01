;;; denote-sort.el ---  Sort Denote files based on a file name component -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

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

;;; Code:

(require 'denote)

(defgroup denote-sort nil
  "Sort Denote files based on a file name component."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defconst denote-sort-comparison-fallback-function #'string-collate-lessp
  "String comparison function used by `denote-sort-files' subroutines.")

(defconst denote-sort-components '(title keywords signature identifier)
  "List of sorting keys applicable for `denote-sort-files' and related.")

(defcustom denote-sort-title-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the TITLE component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-keywords-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the KEYWORDS component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-signature-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the SIGNATURE component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-dired-extra-prompts '(sort-by-component reverse-sort)
  "Determine what `denote-sort-dired' prompts for beside a search query.
This concerns the additional prompts issued by `denote-sort-dired' about
whether to sort by a given file name component and to then reverse the
sort.

The value is a list of symbols, which can include the symbols
`sort-by-component' and `reverse-sort'.  The order is significant, with
the leftmost symbol coming first.

If the value is nil, skip all prompts.  In this scenario, the sorting is
done according to `denote-sort-dired-default-sort-component' and
`denote-sort-dired-default-reverse-sort'."
  :type '(radio (const :tag "Do not prompt for anything" nil)
                (set :tag "Available prompts" :greedy t
                     (const :tag "Sort by file name component" sort-by-component)
                     (const :tag "Reverse the sort" reverse-sort)))
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-dired-default-sort-component 'identifier
  "Set the default file name component to sort by.
This is used only if `denote-sort-dired-extra-prompts' omits the
minibuffer prompt for which file name component to sort by."
  :type '(radio
          (const :tag "Sort by identifier (default)" identifier)
          (const :tag "Sort by title" title)
          (const :tag "Sort by keywords" keywords)
          (const :tag "Sort by signature" signature))
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-dired-default-reverse-sort nil
  "If non-nil, reverse the sorting order by default.
This is used only if `denote-sort-dired-extra-prompts' omits the
minibuffer prompt that asks for a reverse sort or not."
  :type 'boolean
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)


;; NOTE 2023-12-04: We can have compound sorting algorithms such as
;; title+signature, but I want to keep this simple for the time being.
;; Let us first hear from users to understand if there is a real need
;; for such a feature.
(defmacro denote-sort--define-lessp (component)
  "Define function to sort by COMPONENT."
  (let ((retrieve-fn (intern (format "denote-retrieve-filename-%s" component)))
        (comparison-fn (intern (format "denote-sort-%s-comparison-function" component))))
    `(defun ,(intern (format "denote-sort-%s-lessp" component)) (file1 file2)
       ,(format
         "Return smallest among FILE1, FILE2 based on their %s.
The `%s' performs the comparison."
         component comparison-fn)
       (let* ((one (,retrieve-fn file1))
              (two (,retrieve-fn file2))
              (one-empty-p (or (null one) (string-empty-p one)))
              (two-empty-p (or (null two) (string-empty-p two))))
         (cond
          (one-empty-p nil)
          ((and (not one-empty-p) two-empty-p) one)
          (t (funcall (or ,comparison-fn denote-sort-comparison-fallback-function) one two)))))))

;; TODO 2023-12-04: Subject to the above NOTE, we can also sort by
;; directory and by file length.
(denote-sort--define-lessp title)
(denote-sort--define-lessp keywords)
(denote-sort--define-lessp signature)

;;;###autoload
(defun denote-sort-files (files component &optional reverse)
  "Returned sorted list of Denote FILES.

With COMPONENT as a symbol among `denote-sort-components',
sort files based on the corresponding file name component.

With COMPONENT as a nil value keep the original date-based
sorting which relies on the identifier of each file name.

With optional REVERSE as a non-nil value, reverse the sort order."
  (let* ((files-to-sort (copy-sequence files))
         (sort-fn (when component
                    (pcase component
                      ('title #'denote-sort-title-lessp)
                      ('keywords #'denote-sort-keywords-lessp)
                      ('signature #'denote-sort-signature-lessp))))
         (sorted-files (if sort-fn (sort files sort-fn) files-to-sort)))
    (if reverse
        (reverse sorted-files)
      sorted-files)))

(defun denote-sort-get-directory-files (files-matching-regexp sort-by-component &optional reverse omit-current)
  "Return sorted list of files in variable `denote-directory'.

With FILES-MATCHING-REGEXP as a string limit files to those
matching the given regular expression.

With SORT-BY-COMPONENT as a symbol among `denote-sort-components',
pass it to `denote-sort-files' to sort by the corresponding file
name component.

With optional REVERSE as a non-nil value, reverse the sort order.

With optional OMIT-CURRENT, do not include the current file in
the list."
  (denote-sort-files
   (denote-directory-files files-matching-regexp omit-current)
   sort-by-component
   reverse))

(defun denote-sort-get-links (files-matching-regexp sort-by-component current-file-type id-only &optional reverse)
  "Return sorted typographic list of links for FILES-MATCHING-REGEXP.

With FILES-MATCHING-REGEXP as a string, match files stored in the
variable `denote-directory'.

With SORT-BY-COMPONENT as a symbol among `denote-sort-components',
sort FILES-MATCHING-REGEXP by the given Denote file name
component.  If SORT-BY-COMPONENT is nil or an unknown non-nil
value, default to the identifier-based sorting.

With CURRENT-FILE-TYPE as a symbol among those specified in
`denote-file-type' (or the `car' of each element in
`denote-file-types'), format the link accordingly.  With a nil or
unknown non-nil value, default to the Org notation.

With ID-ONLY as a non-nil value, produce links that consist only
of the identifier, thus deviating from CURRENT-FILE-TYPE.

With optional REVERSE as a non-nil value, reverse the sort order."
  (denote-link--prepare-links
   (denote-sort-get-directory-files files-matching-regexp sort-by-component reverse)
   current-file-type
   id-only))

(defvar denote-sort-component-history nil
  "Minibuffer history of `denote-sort-component-prompt'.")

(defalias 'denote-sort--component-hist 'denote-sort-component-history
  "Compatibility alias for `denote-sort-component-history'.")

(defun denote-sort-component-prompt ()
  "Prompt for sorting key among `denote-sort-components'."
  (let ((default (car denote-sort-component-history)))
    (intern
     (completing-read
      (format-prompt "Sort by file name component" default)
      denote-sort-components nil :require-match
      nil 'denote-sort-component-history default))))

(defvar-local denote-sort--dired-buffer nil
  "Buffer object of current `denote-sort-dired'.")

(defun denote-sort-dired--prompts ()
  "Return list of prompts per `denote-sort-dired-extra-prompts'."
  (let (sort-by-component reverse-sort)
    (dolist (prompt denote-sort-dired-extra-prompts)
      (pcase prompt
        ('sort-by-component (setq sort-by-component (denote-sort-component-prompt)))
        ('reverse-sort (setq reverse-sort (y-or-n-p "Reverse sort? ")))))
    (list sort-by-component reverse-sort)))

;;;###autoload
(defun denote-sort-dired (files-matching-regexp sort-by-component reverse)
  "Produce Dired buffer with sorted files from variable `denote-directory'.
When called interactively, prompt for FILES-MATCHING-REGEXP and,
depending on the value of the user option `denote-sort-dired-extra-prompts',
also prompt for SORT-BY-COMPONENT and REVERSE.

1. FILES-MATCHING-REGEXP limits the list of Denote files to
   those matching the provided regular expression.

2. SORT-BY-COMPONENT sorts the files by their file name component (one
   among `denote-sort-components').  If it is nil, sorting is performed
   according to the user option `denote-sort-dired-default-sort-component',
   falling back to the identifier.

3. REVERSE is a boolean to reverse the order when it is a non-nil value.
   If `denote-sort-dired-extra-prompts' is configured to skip this
   prompt, then the sorting is done according to the user option
   `denote-sort-dired-default-reverse-sort', falling back to
   nil (i.e. no reverse sort).

When called from Lisp, the arguments are a string, a symbol among
`denote-sort-components', and a non-nil value, respectively."
  (interactive
   (append (list (denote-files-matching-regexp-prompt)) (denote-sort-dired--prompts)))
  (let ((component (or sort-by-component
                       denote-sort-dired-default-sort-component
                       'identifier))
        (reverse-sort (or reverse
                          denote-sort-dired-default-reverse-sort
                          nil)))
    (if-let ((default-directory (denote-directory))
             (files (denote-sort-get-directory-files files-matching-regexp component reverse-sort))
             ;; NOTE 2023-12-04: Passing the FILES-MATCHING-REGEXP as
             ;; buffer-name produces an error if the regexp contains a
             ;; wildcard for a directory. I can reproduce this in emacs
             ;; -Q and am not sure if it is a bug. Anyway, I will report
             ;; it upstream, but even if it is fixed we cannot use it
             ;; for now (whatever fix will be available for Emacs 30+).
             (denote-sort-dired-buffer-name (format "Denote sort `%s' by `%s'" files-matching-regexp component))
             (buffer-name (format "Denote sort by `%s' at %s" component (format-time-string "%T"))))
        (let ((dired-buffer (dired (cons buffer-name (mapcar #'file-relative-name files)))))
          (setq denote-sort--dired-buffer dired-buffer)
          (with-current-buffer dired-buffer
            (setq-local revert-buffer-function
                        (lambda (&rest _)
                          (kill-buffer dired-buffer)
                          (denote-sort-dired files-matching-regexp component reverse-sort))))
          ;; Because of the above NOTE, I am printing a message.  Not
          ;; what I want, but it is better than nothing...
          (message denote-sort-dired-buffer-name))
      (message "No matching files for: %s" files-matching-regexp))))

(provide 'denote-sort)
;;; denote-sort.el ends here
