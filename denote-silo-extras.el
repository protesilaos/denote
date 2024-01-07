;;; denote-silo-extras.el --- Convenience functions for using Denote in multiple silos  -*- lexical-binding: t; -*-

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

;; This is a set of convenience functions that used to be provided in
;; the Denote manual.  A "silo" is a `denote-directory' that is
;; self-contained.  Users can maintain multiple silos.  Consult the
;; manual for the details.  With the Denote package installed,
;; evaluate the following to read the relevant node:
;;
;;     (info "(denote) Maintain separate directory silos for notes")

;;; Code:

(require 'denote)

(defgroup denote-silo-extras nil
  "Make it easier to use Denote across Silos."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defcustom denote-silo-extras-directories
  `(,denote-directory)
  "List of file paths pointing to Denote silos.
Each file path points to a directory, which takes the same value
as the variable `denote-directory'."
  :group 'denote-silo-extras
  :link '(info-link "(denote) Maintain separate directories for notes")
  :type '(repeat directory))

(defvar denote-silo-extras--directory-history nil
  "Minibuffer history for `denote-silo-extras--directory-prompt'.")

(defun denote-silo-extras--directory-prompt ()
  "Prompt for directory among `denote-silo-extras-directories'."
  (let ((default (car denote-silo-extras--directory-history)))
    (completing-read
     (format-prompt "Select a silo" default)
     denote-silo-extras-directories nil :require-match
     nil 'denote-silo-extras--directory-history)))

;;;###autoload
(defun denote-silo-extras-create-note (&optional silo)
  "Select SILO and run `denote' in it.
SILO is a file path from `denote-silo-extras-directories'."
  (interactive
   (list
    (when current-prefix-arg
      (denote-silo-extras--directory-prompt))))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively #'denote)))

;;;###autoload
(defun denote-silo-extras-open-or-create (&optional silo)
  "Select SILO and run `denote-open-or-create' in it.
SILO is a file path from `denote-silo-extras-directories'."
  (interactive
   (list
    (when current-prefix-arg
      (denote-silo-extras--directory-prompt))))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively #'denote-open-or-create)))

;;;###autoload
(defun denote-silo-extras-select-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND in it.
SILO is a file path from `denote-silo-extras-directories', while
COMMAND is one among `denote-silo-extras-commands'."
  (interactive
   (list
    (denote-silo-extras--directory-prompt)
    (denote-command-prompt)))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively command)))

(provide 'denote-silo-extras)
;;; denote-silo-extras.el ends here
