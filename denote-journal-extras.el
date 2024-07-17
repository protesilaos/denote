;;; denote-journal-extras.el --- Convenience functions for daily journaling  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a set of optional convenience functions that used to be
;; provided in the Denote manual.  They facilitate the use of Denote
;; for daily journaling.

;;; Code:

(require 'denote)

(defgroup denote-journal-extras nil
  "Denote for daily journaling."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defcustom denote-journal-extras-directory
  (expand-file-name "journal" denote-directory)
  "Directory for storing daily journal entries.
This can either be the same as the variable `denote-directory' or
a subdirectory of it.

A value of nil means to use the variable `denote-directory'.
Journal entries will thus be in a flat listing together with all
other notes.  They can still be retrieved easily by searching for
the `denote-journal-extras-keyword'."
  :group 'denote-journal-extras
  :type '(choice (directory :tag "Provide directory path (is created if missing)")
                 (const :tag "Use the `denote-directory'" nil)))

(defcustom denote-journal-extras-keyword "journal"
  "Single word keyword to tag journal entries.
It is used by `denote-journal-extras-new-entry' to add a keyword
to the newly created file."
  :group 'denote-journal-extras
  :type 'string)

(defcustom denote-journal-extras-title-format 'day-date-month-year-24h
  "Date format to construct the title with `denote-journal-extras-new-entry'.
The value is either a symbol or an arbitrary string that is
passed to `format-time-string' (consult its documentation for the
technicalities).

Acceptable symbols and their corresponding styles are:

| Symbol                  | Style                             |
|-------------------------+-----------------------------------|
| day                     | Monday                            |
| day-date-month-year     | Monday 19 September 2023          |
| day-date-month-year-24h | Monday 19 September 2023 20:49    |
| day-date-month-year-12h | Monday 19 September 2023 08:49 PM |

With a nil value, make `denote-journal-extras-new-entry' prompt
for a title."
  :group 'denote-journal-extras
  :type '(choice
          (const :tag "Prompt for title with `denote-journal-extras-new-entry'" nil)
          (const :tag "Monday"
                 :doc "The `format-time-string' is: %A"
                 day)
          (const :tag "Monday 19 September 2023"
                 :doc "The `format-time-string' is: %A %e %B %Y"
                 day-date-month-year)
          (const :tag "Monday 19 September 2023 20:49"
                 :doc "The `format-time-string' is: %A %e %B %Y %H:%M"
                 day-date-month-year-24h)
          (const :tag "Monday 19 September 2023 08:49 PM"
                 :doc "The `format-time-string' is: %A %e %B %Y %I:%M %^p"
                 day-date-month-year-12h)
          (string :tag "Custom string with `format-time-string' specifiers")))

(defcustom denote-journal-extras-hook nil
  "Normal hook called after `denote-journal-extras-new-entry'.
Use this to, for example, set a timer after starting a new
journal entry (refer to the `tmr' package on GNU ELPA)."
  :group 'denote-journal-extras
  :type 'hook)

(defun denote-journal-extras-directory ()
  "Make the variable `denote-journal-extras-directory' and its parents."
  (if-let (((stringp denote-journal-extras-directory))
           (directory (file-name-as-directory (expand-file-name denote-journal-extras-directory))))
      (progn
        (when (not (file-directory-p denote-journal-extras-directory))
          (make-directory directory :parents))
        directory)
    (denote-directory)))

(defun denote-journal-extras-daily--title-format (&optional date)
  "Return present date in `denote-journal-extras-title-format' or prompt for title.
With optional DATE, use it instead of the present date.  DATE has
the same format as that returned by `current-time'."
  (format-time-string
   (if (and denote-journal-extras-title-format
            (stringp denote-journal-extras-title-format))
       denote-journal-extras-title-format
     (pcase denote-journal-extras-title-format
       ('day "%A")
       ('day-date-month-year "%A %e %B %Y")
       ('day-date-month-year-24h "%A %e %B %Y %H:%M")
       ('day-date-month-year-12h "%A %e %B %Y %I:%M %^p")
       (_ (denote-title-prompt (format-time-string "%F" date)))))
   date))

(defun denote-journal-extras--get-template ()
  "Return template that has `journal' key in `denote-templates'.
If no template with `journal' key exists but `denote-templates'
is non-nil, prompt the user for a template among
`denote-templates'.  Else return nil.

Also see `denote-journal-extras-new-entry'."
  (if-let ((template (alist-get 'journal denote-templates)))
      template
    (when denote-templates
      (denote-template-prompt))))

;;;###autoload
(defun denote-journal-extras-new-entry (&optional date)
  "Create a new journal entry in variable `denote-journal-extras-directory'.
Use `denote-journal-extras-keyword' as a keyword for the newly
created file.  Set the title of the new entry according to the
value of the user option `denote-journal-extras-title-format'.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp DATE is a string and has the same format as
that covered in the documentation of the `denote' function.  It
is internally processed by `denote-parse-date'."
  (interactive (list (when current-prefix-arg (denote-date-prompt))))
  (let ((internal-date (denote-parse-date date))
        (denote-directory (denote-journal-extras-directory)))
    (denote
     (denote-journal-extras-daily--title-format internal-date)
     `(,denote-journal-extras-keyword)
     nil nil date
     (denote-journal-extras--get-template))
    (run-hooks 'denote-journal-extras-hook)))

(defun denote-journal-extras--entry-today (&optional date)
  "Return list of files matching a journal for today or optional DATE.
DATE has the same format as that returned by `denote-parse-date'."
  (let* ((identifier (format "%sT[0-9]\\{6\\}" (format-time-string "%Y%m%d" date)))
         (files (denote-directory-files identifier))
         (keyword (concat "_" (regexp-quote denote-journal-extras-keyword))))
    (seq-filter
     (lambda (file)
       (string-match-p keyword file))
     files)))

;;;###autoload
(defun denote-journal-extras-new-or-existing-entry (&optional date)
  "Locate an existing journal entry or create a new one.
A journal entry is one that has `denote-journal-extras-keyword' as
part of its file name.

If there are multiple journal entries for the current date,
prompt for one using minibuffer completion.  If there is only
one, visit it outright.  If there is no journal entry, create one
by calling `denote-journal-extra-new-entry'.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp, DATE is a string and has the same format
as that covered in the documentation of the `denote' function.
It is internally processed by `denote-parse-date'."
  (interactive
   (list
    (when current-prefix-arg
      (denote-date-prompt))))
  (let* ((internal-date (denote-parse-date date))
         (files (denote-journal-extras--entry-today internal-date)))
    (cond
     ((length> files 1)
      (find-file (completing-read "Select journal entry: " files nil :require-match)))
     (files
      (find-file (car files)))
     (t
      (denote-journal-extras-new-entry date)))))

;;;###autoload
(defun denote-journal-extras-link-or-create-entry (&optional date id-only)
  "Use `denote-link' on journal entry, creating it if necessary.
A journal entry is one that has `denote-journal-extras-keyword' as
part of its file name.

If there are multiple journal entries for the current date,
prompt for one using minibuffer completion.  If there is only
one, link to it outright.  If there is no journal entry, create one
by calling `denote-journal-extra-new-entry' and link to it.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp, DATE is a string and has the same format
as that covered in the documentation of the `denote' function.
It is internally processed by `denote-parse-date'.

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'."
  (interactive
   (pcase current-prefix-arg
     ('(16) (list (denote-date-prompt) :id-only))
     ('(4) (list (denote-date-prompt)))))
  (let* ((internal-date (denote-parse-date date))
         (files (denote-journal-extras--entry-today internal-date))
         (path))
    (cond
     ((length> files 1)
      (setq path (completing-read "Select journal entry: " files nil :require-match)))
     (files
      (setq path (car files)))
     (t
      (save-window-excursion
        (denote-journal-extras-new-entry date)
        (save-buffer)
        (setq path (buffer-file-name)))))
    (denote-link path
                 (denote-filetype-heuristics (buffer-file-name))
                 (denote--link-get-description path)
                 id-only)))

(provide 'denote-journal-extras)
;;; denote-journal-extras.el ends here
