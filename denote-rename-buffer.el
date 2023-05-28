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
;; Rename Denote buffers to be shorter and easier to read.

;;; Code:

(require 'denote)

(defgroup denote-rename-buffer nil
  "Rename Denote buffers to be shorter and easier to read."
  :group 'denote)

;; TODO 2023-05-28: Provide a `denote-rename-buffer-pattern' user option.

(defun denote-rename-buffer (&optional buffer)
  "Retrieve Denote file of BUFFER and rename BUFFER based on the file title.
BUFFER is an object that satisfies `bufferp'.  If nil, then use
the return value of `current-buffer'."
  (when-let* ((file (buffer-file-name (or buffer (current-buffer))))
              ((denote-file-has-identifier-p file))
              (type (denote-filetype-heuristics file))
              (title (denote--retrieve-title-or-filename file type)))
    (rename-buffer title :unique)))

;;;###autoload
(define-minor-mode denote-rename-buffer-mode
  "Automatically rename Denote buffers to be easier to read."
  :global t
  (if denote-rename-buffer-mode
      (add-hook 'find-file-hook #'denote-rename-buffer)
    (remove-hook 'find-file-hook #'denote-rename-buffer)))

(provide 'denote-rename-buffer)
;;; denote-rename-buffer.el ends here
