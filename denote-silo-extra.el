;;; denote-silo-extra.el --- Convenience functions for using Denote in multiple silos  -*- lexical-binding: t; -*-

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

(defgroup denote-silo-extra nil
  "Make it easier to use Denote across Silos."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defcustom denote-silo-extra-directories
  `(,denote-directory)
  "List of file paths pointing to my Denote silos."
  :group 'denote-silo-extra
  :link '(info-link "(denote) Maintain separate directories for notes")
  :type '(repeat directory))

(defvar denote-silo-extra-commands-for-silos
  '(denote
    denote-date
    denote-subdirectory
    denote-template
    denote-type
    denote-signature)
  "List of commands to call after selecting a silo.")

(defun denote-silo-extra-pick-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND in it.
SILO is a file path from `denote-silo-extra-directories', while
COMMAND is one among `denote-silo-extra-commands-for-silos'."
  (interactive
   (list (completing-read "Select a silo: "
                          denote-silo-extra-directories nil t)
         (intern
          (completing-read "Run command in silo: "
                           denote-silo-extra-commands-for-silos nil t))))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively command)))

(defun denote-silo-extra-create (&optional silo)
  "Select SILO and run `denote' in it.
SILO is a file path from `denote-silo-extra-directories'."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Select a silo: "
                            denote-silo-extra-directories nil t))))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively #'denote)))

(defun denote-silo-extra-open-or-create (&optional silo)
  "Select SILO and run `denote-open-or-create' in it.
SILO is a file path from `denote-silo-extra-directories'."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Select a silo: "
                            denote-silo-extra-directories nil t))))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively #'denote-open-or-create)))

(provide 'denote-silo-extra)
;;; denote-silo-extra.el ends here
