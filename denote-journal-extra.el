;;; denote-journal-extra.el --- Convenience functions for daily journaling  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'denote)

(defgroup denote-journal-extra nil
  "Denote for daily journaling."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defcustom denote-journal-extra-directory
  (expand-file-name "journal" denote-directory)
  "Directory for storing daily journal entries."
  :group 'denote-journal-extra
  :type 'directory)

(defun denote-journal-extra--make-journal-directory ()
  "Make the variable `denote-journal-extra-directory' and its parents."
  (when (and (stringp denote-journal-extra-directory)
             (not (file-directory-p denote-journal-extra-directory)))
    (make-directory denote-journal-extra-directory :parents)))

(defcustom denote-journal-extra-templates
  '((journal-morningpage . "* The Morning Journaling Routine

Stream of consciousness writing to empty your mind.")
    (journal-emotion . "* I am feeling <emotion>

- Capture your emotion here.")
    (journal-insight . "* I had an insight!   :insight:

- Capture your insight here")
    (journal-checkin . "* The Daily Ongoing Check-in

- Capture your generic check-in here"))
  "Templates for your daily journal entries."
  :type '(alist :key-type symbol :value-type string)
  :link '(info-link "(denote) The denote-templates option")
  :group 'denote-journal-extra)

(dolist (tem denote-journal-extra-templates)
  (add-to-list 'denote-templates tem))

(defun denote-journal-extra-new-stand-alone-journal-entry ()
  "Create a new stand-alone journal entry in `denote-journal-extra-directory`."
  (interactive)
  (denote-journal-extra--make-journal-directory)
  (let ((denote-user-enforced-denote-directory denote-journal-extra-directory))
    (denote
     ;; format like Tuesday 14 June 2022 05:49:37 PM
     (format-time-string "%A %e %B %Y %I:%M:%S %p")
     ;; No need to specify `keywords`, `file-type`, `subdirectory` or `date`
     nil nil nil nil
     ;; Pick the right template for your journal.
     (denote-template-prompt))))

(provide 'denote-journal-extra)
;;; denote-journal-extra.el ends here
