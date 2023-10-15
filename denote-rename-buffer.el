;;; denote-rename-buffer.el --- Rename Denote buffers to be shorter and easier to read -*- lexical-binding: t -*-

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
;; Rename Denote buffers to be shorter and easier to read.  Enable
;; `denote-rename-buffer-mode' to automatically rename the buffer of a
;; Denote file.  The renaming function is specified in the user option
;; `denote-rename-buffer-function'.

;;; Code:

(require 'denote)

(defgroup denote-rename-buffer nil
  "Rename Denote buffers to be shorter and easier to read."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defcustom denote-rename-buffer-function #'denote-rename-buffer-with-title
  "Symbol of function that is called to rename the Denote file buffer.

The function is called without arguments from the
`find-file-hook' and `denote-after-new-note-hook' when
`denote-rename-buffer-mode' is enabled (or when the user manually
sets up the hooks).

See the function `denote-rename-buffer-with-title' (the default
value) for a reference implementation."
  :type '(choice
          (const :tag "Rename using only the title" denote-rename-buffer-with-title)
          (const :tag "Rename using only the identifier" denote-rename-buffer-with-identifier)
          (function :tag "Use a custom renaming function"))
  :group 'denote-rename-buffer)

(defun denote-rename-buffer--common-check (buffer)
  "Determine if BUFFER shall be renamed.
Return the file path and the type of it as a cons cell."
  (when-let ((file (buffer-file-name buffer))
             ((denote-file-has-identifier-p file))
             (type (denote-filetype-heuristics file)))
    (cons file type)))

(defun denote-rename-buffer--with-unique-name (name)
  "Call `rename-buffer' with NAME and uniquify it."
  (rename-buffer name :unique))

(defun denote-rename-buffer-with-title (&optional buffer)
  "Retrieve Denote file of BUFFER and rename BUFFER based on the file title.
BUFFER is an object that satisfies `bufferp'.  If nil, then use
the return value of `current-buffer'.

This is a generic reference implementation for use in the user
option `denote-rename-buffer-function'.  If you need something
else, check the Denote manual for functions/variables that
extract the data you are looking for."
  (when-let ((file-and-type (denote-rename-buffer--common-check (or buffer (current-buffer))))
             (title (denote-retrieve-title-value (car file-and-type) (cdr file-and-type))))
    (denote-rename-buffer--with-unique-name title)))

(defun denote-rename-buffer-with-identifier (&optional buffer)
  "Retrieve Denote file of BUFFER and rename BUFFER based on the file identifier.
BUFFER is an object that satisfies `bufferp'.  If nil, then use
the return value of `current-buffer'.

This is a generic reference implementation for use in the user
option `denote-rename-buffer-function'.  If you need something
else, check the Denote manual for functions/variables that
extract the data you are looking for."
  (when-let* ((file-and-type (denote-rename-buffer--common-check (or buffer (current-buffer))))
              (identifier (denote-retrieve-filename-identifier (car file-and-type))))
    (denote-rename-buffer--with-unique-name identifier)))

(defun denote-rename-buffer-rename-function-or-fallback ()
  "Call `denote-rename-buffer-function' or its fallback to rename with title.
Add this to `find-file-hook' and `denote-after-new-note-hook'."
  (funcall (or denote-rename-buffer-function #'denote-rename-buffer-with-title)))

;;;###autoload
(define-minor-mode denote-rename-buffer-mode
  "Automatically rename Denote buffers to be easier to read.
A buffer is renamed upon visiting the underlying file.  This
means that existing buffers are not renamed until they are
visited again in a new buffer (files are visited with the command
`find-file' or related)."
  :global t
  (if denote-rename-buffer-mode
      (progn
        (add-hook 'denote-after-new-note-hook #'denote-rename-buffer-rename-function-or-fallback)
        (add-hook 'find-file-hook #'denote-rename-buffer-rename-function-or-fallback))
    (remove-hook 'denote-after-new-note-hook #'denote-rename-buffer-rename-function-or-fallback)
    (remove-hook 'find-file-hook #'denote-rename-buffer-rename-function-or-fallback)))

(provide 'denote-rename-buffer-with-title)
;;; denote-rename-buffer.el ends here
