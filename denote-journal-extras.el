;;; denote-journal-extras.el --- Convenience functions for daily journaling  -*- lexical-binding: t; -*-

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
a subdirectory of it."
  :group 'denote-journal-extras
  :type 'directory)

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
| day-date-month-year     | Monday 19 September 2023          |
| day-date-month-year-24h | Monday 19 September 2023 20:49    |
| day-date-month-year-12h | Monday 19 September 2023 08:49 PM |

With a nil value, make `denote-journal-extras-new-entry' prompt
for a title."
  :group 'denote-journal-extras
  :type '(choice
          (const :tag "Prompt for title with `denote-journal-extras-new-entry'" nil)
          (const :tag "Monday 19 September 2023" :value "%A %e %B %Y" day-date-month-year)
          (const :tag "Monday 19 September 2023 20:49" :value "%A %e %B %Y %H:%M" day-date-month-year-24h)
          (const :tag "Monday 19 September 2023 08:49 PM" :value "%A %e %B %Y %I:%M %^p" day-date-month-year-12h)
          (string :tag "Custom string with `format-time-string' specifiers")))

(defcustom denote-journal-extras-hook nil
  "Normal hook called after `denote-journal-extras-new-entry'.
Use this to, for example, set a timer after starting a new
journal entry (refer to the `tmr' package on GNU ELPA)."
  :group 'denote-journal-extras
  :type 'hook)

(defun denote-journal-extras-directory ()
  "Make the variable `denote-journal-extras-directory' and its parents."
  (when-let (((stringp denote-journal-extras-directory))
             (directory (file-name-as-directory (expand-file-name denote-journal-extras-directory))))
    (when (not (file-directory-p denote-journal-extras-directory))
      (make-directory directory :parents))
    directory))

(defun denote-journal-extras-daily--title-format ()
  "Return `denote-journal-extras-title-format' or prompt for title."
  (cond
   ((stringp denote-journal-extras-title-format)
    (format-time-string denote-journal-extras-title-format))
   ((symbolp denote-journal-extras-title-format)
    (format-time-string
     (pcase denote-journal-extras-title-format
       ('day-date-month-year "%A %e %B %Y")
       ('day-date-month-year-24h "%A %e %B %Y %H:%M")
       ('day-date-month-year-12h "%A %e %B %Y %I:%M %^p"))))
   (t (denote-title-prompt (format-time-string "%F")))))

;;;###autoload
(defun denote-journal-extras-new-entry ()
  "Create a new journal entry in variable `denote-journal-extras-directory'.
Use `denote-journal-extras-keyword' as a keyword for the newly
created file."
  (interactive)
  (let ((denote-user-enforced-denote-directory (denote-journal-extras-directory)))
    ;; TODO 2023-09-18: Let's see how best to incorporate templates.
    ;; I think it is better to use the `denote-templates' variable,
    ;; since this is what we have it for.  Perhaps we can make the
    ;; behaviour do-what-I-mean, such that the user is prompted for a
    ;; template only if one exists.  Otherwise, no template is used.
    (denote
     (denote-journal-extras-daily--title-format)
     `(,denote-journal-extras-keyword))
    (run-hooks 'denote-journal-extras-hook)))

(defun denote-journal-extras--entry-today ()
  "Return list of files matching a journal for today."
  (denote-directory-files-matching-regexp
   (format "%sT[0-9]\\{6\\}.*_%s"
           (format-time-string "%Y%m%d")
           denote-journal-extras-keyword)))

;;;###autoload
(defun denote-journal-extras-new-or-existing-entry ()
  "Locate an existing journal entry or create a new one.
A journal entry is one that has `denote-journal-extras-keyword' as
part of its file name.

If there are multiple journal entries for the current date,
prompt for one using minibuffer completion.  If there is only
one, visit it outright.  If there is no journal entry, create one
by calling `denote-journal-extra-new-entry'."
  (interactive)
  (let ((files (denote-journal-extras--entry-today)))
    (cond
     ((length> files 1)
      (find-file (completing-read "Select journal entry: " files nil :require-match)))
     (files
      (find-file (car files)))
     (t
      (call-interactively 'denote-journal-extras-new-entry)))))

(provide 'denote-journal-extras)
;;; denote-journal-extras.el ends here
