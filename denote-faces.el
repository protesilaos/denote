;;; denote-faces.el --- Faces and fontification rules for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.2"))

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
;; Used internally by Denote to fontify file names in Dired, the
;; backlinks' buffer, and related.

;;; Code:

(require 'denote)

(defgroup denote-faces ()
  "Faces for Denote."
  :group 'denote)

(defface denote-faces-subdirectory
  '((t :inherit bold))
  "Face for subdirectory of file name.
This should only ever needed in the backlinks' buffer (or
equivalent), not in Dired."
  :group 'denote-faces)

(defface denote-faces-date
  '((t :inherit font-lock-variable-name-face))
  "Face for file name date in Dired buffers.
This is the part of the identifier that covers the year, month,
and day."
  :group 'denote-faces)

(defface denote-faces-time
  '((t :inherit denote-faces-date))
  "Face for file name time in Dired buffers.
This is the part of the identifier that covers the hours, minutes,
and seconds."
  :group 'denote-faces)

(defface denote-faces-title
  '((t ))
  "Face for file name title in Dired buffers."
  :group 'denote-faces)

(defface denote-faces-extension
  '((t :inherit shadow))
  "Face for file extension type in Dired buffers."
  :group 'denote-faces)

(defface denote-faces-keywords
  '((t :inherit font-lock-builtin-face))
  "Face for file name keywords in Dired buffers."
  :group 'denote-faces)

(defface denote-faces-delimiter
  '((((class color) (min-colors 88) (background light))
     :foreground "gray70")
    (((class color) (min-colors 88) (background dark))
     :foreground "gray30")
    (t :inherit shadow))
  "Face for file name delimiters in Dired buffers."
  :group 'denote-faces)

;; For character classes, evaluate: (info "(elisp) Char Classes")
(defvar denote-faces--file-name-regexp
  (concat "\\(?1:[0-9]\\{8\\}\\)\\(?2:T[0-9]\\{6\\}\\)"
          "\\(?:\\(?3:--\\)\\(?4:[[:alnum:][:nonascii:]-]*\\)\\)?"
          "\\(?:\\(?5:__\\)\\(?6:[[:alnum:][:nonascii:]_-]*\\)\\)?"
          "\\(?7:\\..*\\)?$")
  "Regexp of file names for fontification.")

(defconst denote-faces-file-name-keywords
  `((,(concat " " denote-faces--file-name-regexp)
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-title nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-keywords nil t)
     (7 'denote-faces-extension nil t )))
  "Keywords for fontification of file names.")

(defconst denote-faces-file-name-keywords-for-backlinks
  `((,(concat "^\\(?8:.*/\\)?" denote-faces--file-name-regexp)
     (8 'denote-faces-subdirectory nil t)
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-title nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-keywords nil t)
     (7 'denote-faces-extension nil t )))
  "Keywords for fontification of file names in the backlinks buffer.")

(provide 'denote-faces)
;;; denote-faces.el ends here
