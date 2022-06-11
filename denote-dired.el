;;; denote-dired.el --- Integration between Denote and Dired -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

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
;; One of the upsides of Denote's file-naming scheme is the predictable
;; pattern it establishes, which appears as a near-tabular presentation in
;; a listing of notes (i.e. in Dired).  The `denote-dired-mode' can help
;; enhance this impression, by fontifying the components of the file name
;; to make the date (identifier) and keywords stand out.
;;
;; There are two ways to set the mode.  Either use it for all directories,
;; which probably is not needed:
;;
;;     (require 'denote-dired)
;;     (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; Or configure the user option `denote-dired-directories' and then set up
;; the function `denote-dired-mode-in-directories':
;;
;;     (require 'denote-dired)
;;
;;     ;; We use different ways to specify a path for demo purposes.
;;     (setq denote-dired-directories
;;           (list denote-directory
;;                 (thread-last denote-directory (expand-file-name "attachments"))
;;                 (expand-file-name "~/Documents/vlog")))
;;
;;     (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
;;
;; The `denote-dired-mode' does not only fontify note files that were
;; created by Denote: it covers every file name that follows our naming
;; conventions (read about "The file-naming scheme" in the manual).
;; This is particularly useful for scenaria where, say, one wants to
;; organise their collection of PDFs and multimedia in a systematic way
;; (and, perhaps, use them as attachments for the notes Denote
;; produces).
;;
;; For the time being, the `diredfl' package is not compatible with this
;; facility.

;;; Code:

(require 'denote)
(require 'dired)

(defgroup denote-dired ()
  "Integration between Denote and Dired."
  :group 'denote)

(defcustom denote-dired-directories
  ;; We use different ways to specify a path for demo purposes.
  (list denote-directory
        (thread-last denote-directory (expand-file-name "attachments"))
        (expand-file-name "~/Documents/vlog"))
  "List of directories where `denote-dired-mode' should apply to."
  :type '(repeat directory)
  :group 'denote-dired)

;;;; Commands

;;;###autoload
(defun denote-dired-rename-file (title keywords)
  "Rename file at point to new file with TITLE and KEYWORDS.
This command is intended to complement note-taking, such as by
renaming attachments that the user adds to their notes."
  (interactive
   (list
    (denote--title-prompt)
    (denote--keywords-prompt)))
  (let* ((file (dired-get-filename))
         (dir (file-name-directory file))
         (old-name (file-name-nondirectory file))
         (extension (file-name-extension file t))
         (new-name (denote--format-file
                    dir
                    (format-time-string denote--id)
                    keywords
                    (denote--sluggify title)
                    extension)))
    (when (y-or-n-p
           (format "Rename %s to %s?"
                   (propertize old-name 'face 'error)
                   (propertize (file-name-nondirectory new-name) 'face 'success)))
      (rename-file old-name new-name nil)
      (revert-buffer))))

;;;; Extra fontification

(defface denote-dired-dired-field-date
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00d3d0")
    (t :inherit font-lock-variable-name-face))
  "Face for file name date in `dired-mode' buffers."
  :group 'denote-dired)

(defface denote-dired-dired-field-time
  '((t :inherit denote-dired-dired-field-date))
  "Face for file name time in `dired-mode' buffers."
  :group 'denote-dired)

(defface denote-dired-dired-field-title
  '((t ))
  "Face for file name title in `dired-mode' buffers."
  :group 'denote-dired)

(defface denote-dired-dired-field-extension
  '((t :inherit shadow))
  "Face for file extension type in `dired-mode' buffers."
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

(defconst denote-dired-font-lock-keywords
  `((,denote--file-regexp
     (1 'denote-dired-dired-field-date)
     (2 'denote-dired-dired-field-time)
     (3 'denote-dired-dired-field-delimiter)
     (4 'denote-dired-dired-field-title)
     (5 'denote-dired-dired-field-delimiter)
     (6 'denote-dired-dired-field-keywords)
     (7 'denote-dired-dired-field-extension)))
  "Keywords for fontification.")

;;;###autoload
(define-minor-mode denote-dired-mode
  "Fontify all Denote-style file names in Dired."
  :global nil
  :group 'denote-dired
  (if denote-dired-mode
      (font-lock-add-keywords nil denote-dired-font-lock-keywords t)
    (font-lock-remove-keywords nil denote-dired-font-lock-keywords))
  (font-lock-flush (point-min) (point-max)))

(defun denote-dired--modes-dirs-as-dirs ()
  "Return `denote-dired-directories' as directories.
The intent is to basically make sure that however a path is
written, it is always returned as a directory."
  (mapcar
   (lambda (dir)
     (file-name-as-directory (file-truename dir)))
   denote-dired-directories))

;;;###autoload
(defun denote-dired-mode-in-directories ()
  "Enable `denote-dired-mode' in `denote-dired-directories'.
Add this function to `dired-mode-hook'."
  (when (member (file-truename default-directory) (denote-dired--modes-dirs-as-dirs))
    (denote-dired-mode 1)))

(provide 'denote-dired)
;;; denote-dired.el ends here
