;;; denote-dired.el --- Integration of denote with Dired -*- lexical-binding: t -*-

;; Copyright (C) 2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

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
;; Integration of denote with Dired.  NOTE: does not work if
;; diredfl-mode is enabled.
;;
;; Setup (only affects `denote-directory'):
;;
;;     (add-hook 'dired-mode-hook #'denote-dired-mode)

;;; Code:

(require 'denote)

(defgroup denote-dired ()
  "Simple tool for plain text notes."
  :group 'files)

(defvar dired-font-lock-keywords)

(defvar denote-dired-original-keywords dired-font-lock-keywords
  "Original Dired fontification keywords.")

(defface denote-dired-dired-field-date
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00d3d0")
    (t :inherit font-lock-variable-name-face))
  "Face for file name date in `dired-mode' buffers."
  :group 'denote-dired)

(defface denote-dired-dired-field-keywords
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7")
    (t :inherit font-lock-builtin-face))
  "Face for file name keywords in `dired-mode' buffers."
  :group 'denote-dired)

(defface denote-dired-dired-field-delimiter
  '((((class color) (min-colors 88) (background light))
     :foreground "gray65")
    (((class color) (min-colors 88) (background dark))
     :foreground "gray35")
    (t :inherit shadow))
  "Face for file name delimiters in `dired-mode' buffers."
  :group 'denote-dired)

(defun denote-dired--fontify ()
  "Append fontification rules to `dired-font-lock-keywords'."
  (setq dired-font-lock-keywords
        (append (list `(,denote--file-regexp
                        (1 'denote-dired-dired-field-date)
                        (2 'denote-dired-dired-field-delimiter)
                        (3 'denote-dired-dired-field-keywords)
                        (4 'denote-dired-dired-field-delimiter)))
                dired-font-lock-keywords)))

(defvar diredfl-mode)
(declare-function diredfl-mode "diredfl")

(defun denote-dired--setup (&optional reverse)
  "Setup `denote-dired--fontify' local hook.
If optional REVERSE is non-nil, remove the hook."
  (cond
   (reverse
    (setq dired-font-lock-keywords denote-dired-original-keywords))
   ((or (string-match-p (denote--directory) default-directory)
        (string-match-p (abbreviate-file-name denote-directory) default-directory)
        (string-match-p denote-directory default-directory))
    (denote-dired--fontify)))
  (font-lock-refresh-defaults))

;;;###autoload
(define-minor-mode denote-dired-mode
  "Integrate denote with Dired."
  :global nil
  :group 'denote-dired
  (if denote-dired-mode
      (denote-dired--setup)
    (denote-dired--setup :reverse)))

(provide 'denote-dired)
;;; denote-dired.el ends here
