;;; denote-md-extras.el --- Denote extensions for Markdown mode -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

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
;; Optional extensions to Denote that work specifically with Markdown files.

;;; Code:

(require 'denote)

;;;; Convert links from `:denote' to absolute/relative file paths and vice versa

(defun denote-md-extras--get-regexp (type)
  "Return regular expression to match link TYPE.
TYPE is either the symbol `denote' or `file' for Denote-style links and
file paths, respectively."
  (pcase type
    ('denote "(denote:\\(?1:.*?\\))")
    ('file (format "(.*?\\(?1:%s\\).*?)" denote-id-regexp))
    (_ (error "`%s' is an unknown type of link" type))))

;;;###autoload
(defun denote-md-extras-convert-links-to-file-paths (&optional absolute)
  "Convert denote: links to file paths.
Ignore all other link types.  Also ignore links that do not
resolve to a file in the variable `denote-directory'.

With optional ABSOLUTE, format paths as absolute, otherwise do them
relative to the variable `denote-directory'."
  (interactive "P" markdown-mode)
  (if (derived-mode-p 'markdown-mode)
      (save-excursion
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward (denote-md-extras--get-regexp 'denote) nil :no-error)
            (when-let* ((id (match-string-no-properties 1))
                        (file (save-match-data
                                (if absolute
                                    (denote-get-path-by-id id)
                                  (denote-get-relative-path-by-id id)))))
              (replace-match (format "(%s)" file) :fixed-case :literal)
              (setq count (1+ count))))
          (message "Converted %d `denote:' links to %s paths" count (if absolute "ABSOLUTE" "RELATIVE"))))
    (user-error "The current file is not using Markdown mode")))

;;;###autoload
(defun denote-md-extras-convert-links-to-denote-type ()
  "Convert file: links to denote: links in the current Markdown buffer.
Ignore all other link types.  Also ignore file: links that do not
point to a file with a Denote file name."
  (interactive nil markdown-mode)
  (if (derived-mode-p 'markdown-mode)
      (save-excursion
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward (denote-md-extras--get-regexp 'file) nil :no-error)
            (let* ((file (match-string-no-properties 1))
                   (id (save-match-data (denote-retrieve-filename-identifier file))))
              (when id
                (replace-match (format "(denote:%s)" id) :fixed-case :literal)
                (setq count (1+ count)))))
          (message "Converted %d file links to `denote:' links" count)))
    (user-error "The current file is not using Markdown mode")))

(provide 'denote-md-extras)
;;; denote-md-extras.el ends here
