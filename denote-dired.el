;;; denote-dired.el --- Integration between Denote and Dired -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Version: 0.1.0
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
;; Denote's file-naming scheme is not specific to notes or text files:
;; it is useful for all sorts of files, such as multimedia and PDFs that
;; form part of the user's longer-term storage (read manual's "The
;; file-naming scheme").  While Denote does not manage such files, it
;; already has all the mechanisms to facilitate the task of renaming
;; them.
;;
;; To this end, we provide the `denote-dired-rename-file' command.  It
;; has a two-fold purpose: (i) to change the name of an existing file
;; while retaining its identifier and (ii) to write a Denote-compliant
;; file name for an item that was not created by `denote' or related
;; commands (such as an image or PDF).
;;
;; The `denote-dired-rename-file' command will target the file at point
;; if it finds one in the current Dired buffer.  Otherwise it prompts
;; with minibuffer completion for a file name.  It then uses the
;; familiar prompts for a `TITLE' and `KEYWORDS' the same way the
;; `denote' command does (read manual's "Points of entry).  As a final
;; step, it asks for confirmation before renaming the file at point,
;; showing a message like:
;;
;;     Rename sample.pdf to 20220612T052900--my-sample-title__testing.pdf? (y or n)
;;
;; However, if the user option `denote-dired-rename-expert' is non-nil,
;; conduct the renaming operation outright---no questions asked.
;;
;; When operating on a file that has no identifier, such as
;; `sample.pdf', Denote reads the file properties to retrieve its last
;; modification time.  If the file was from a past date like 2000-11-31
;; it will get an identifier starting with `20001131' followed by the
;; time component (per our file-naming scheme).
;;
;; The file type extension (e.g. `.pdf') is read from the underlying
;; file and is preserved through the renaming process.  Files that have
;; no extension are simply left without one.
;;
;; Renaming only occurs relative to the current directory.  Files are not
;; moved between directories.
;;
;; The final step of the `denote-dired-rename-file' command is to call
;; the special hook `denote-dired-post-rename-functions'.  Functions
;; added to that hook must accept three arguments, as explained in its
;; doc string.  For the time being, the only function we define is the
;; one which updates the underlying note's front matter to match the new
;; file name: `denote-dired-rewrite-front-matter'.  The function takes
;; care to only operate on an actual note, instead of arbitrary files.
;;
;; DEVELOPMENT NOTE: the `denote-dired-rewrite-front-matter' needs to be
;; tested thoroughly.  It rewrites file contents so we have to be sure
;; it does the right thing.  To avoid any trouble, it always asks for
;; confirmation before performing the replacement.  This confirmation
;; ignores `denote-dired-rename-expert' for the time being, though we
;; might want to lift that restriction once everything works as
;; intended.
;;
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

(require 'denote-retrieve)
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

(defcustom denote-dired-rename-expert nil
  "If t, `denote-dired-rename-file' doesn't ask for confirmation.
The confiration is asked via a `y-or-n-p' prompt which shows the
old name followed by the new one."
  :type 'boolean
  :group 'denote-dired)

(defcustom denote-dired-post-rename-functions
  (list #'denote-dired-update-dired-buffers
        #'denote-dired-rewrite-front-matter)
  "List of functions called after `denote-dired-rename-file'.
Each function must accept three arguments: FILE, TITLE, and
KEYWORDS.  The first is the full path to the file provided as a
string, the second is the human-readable file name (not what
Denote sluggifies) also as a string, and the third are the
keywords.  If there is only one keyword, it is a string, else a
list of strings.

DEVELOPMENT NOTE: the `denote-dired-rewrite-front-matter' needs
to be tested thoroughly.  It rewrites file contents so we have to
be sure it does the right thing.  To avoid any trouble, it always
asks for confirmation before performing the replacement.  This
confirmation ignores `denote-dired-rename-expert' for the time
being, though we might want to lift that restriction once
everything works as intended."
  :type 'hook
  :group 'denote-dired)

;;;; Commands

(defun denote-dired--file-attributes-time (file)
  "Return `file-attribute-modification-time' of FILE as identifier."
  (format-time-string
   denote--id-format
   (file-attribute-modification-time (file-attributes file))))

(defun denote-dired--file-name-id (file)
  "Return FILE identifier, else generate one."
  (cond
   ((string-match denote--id-regexp file)
    (substring file (match-beginning 0) (match-end 0)))
   ((denote-dired--file-attributes-time file))
   (t (format-time-string denote--id-format))))

(defun denote-dired--rename-buffer (old-name new-name)
  "Rename OLD-NAME buffer to NEW-NAME, when appropriate."
  (when-let* ((buffer (find-buffer-visiting old-name)))
    (with-current-buffer buffer
      (set-visited-file-name new-name nil t))))

(defun denote-dired--rename-dired-file-or-prompt ()
  "Return Dired file at point, else prompt for one."
  (or (dired-get-filename nil t)
      (let* ((file (buffer-file-name))
             (format (if file
                         (format "Rename file Denote-style [%s]: " file)
                       "Rename file Denote-style: ")))
        (read-file-name format nil file t nil))))

(defun denote-dired--rename-file-is-regular (file)
  "Throw error is FILE is not regular, else return FILE."
  (if (or (file-directory-p file)
          (not (file-regular-p file)))
      (user-error "Only rename regular files")
    file))

;;;###autoload
(defun denote-dired-rename-file (file title keywords)
  "Rename FILE to include TITLE and KEYWORDS.

If in Dired, consider FILE to be the one at point, else prompt
with completion.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the file's attribute of last
modification time.  If such attribute cannot be found, the
identifier falls back to the current time.

As a final step, prompt for confirmation, showing the difference
between old and new file names.  If `denote-dired-rename-expert'
is non-nil, conduct the renaming operation outright---no
questions asked!

The file type extension (e.g. .pdf) is read from the underlying
file and is preserved through the renaming process.  Files that
have no extension are simply left without one.

Renaming only occurs relative to the current directory.  Files
are not moved between directories.  As a final step, call the
`denote-dired-post-rename-functions'.

This command is intended to (i) rename existing Denote
notes, (ii) complement note-taking, such as by renaming
attachments that the user adds to their notes."
  (interactive
   (list
    (denote-dired--rename-file-is-regular (denote-dired--rename-dired-file-or-prompt))
    (denote--title-prompt)
    (denote--keywords-prompt)))
  (let* ((dir (file-name-directory file))
         (old-name (file-name-nondirectory file))
         (extension (file-name-extension file t))
         (new-name (denote--format-file
                    dir
                    (denote-dired--file-name-id file)
                    keywords
                    (denote--sluggify title)
                    extension))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (unless (string= old-name (file-name-nondirectory new-name))
      (when (y-or-n-p
             (format "Rename %s to %s?"
                     (propertize old-name 'face 'error)
                     (propertize (file-name-nondirectory new-name) 'face 'success)))
        (rename-file old-name new-name nil)
        (denote-dired--rename-buffer old-name new-name)
        (run-hook-with-args 'denote-dired-post-rename-functions new-name title keywords)))))

(defun denote-dired-update-dired-buffers (&rest _)
  "Update Dired buffers of variable `denote-directory'.
Can run after `denote-dired-post-rename-functions', though it
ignores all its arguments."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (and (eq major-mode 'dired-mode)
                  (string-match-p (expand-file-name default-directory)
                                  (expand-file-name (denote-directory))))
         (revert-buffer))))
   (buffer-list)))

(defun denote-dired--file-meta-header (title date keywords id filetype)
  "Front matter for renamed notes.

TITLE, DATE, KEYWORDS, FILENAME, ID, and FILETYPE are all strings
 which are provided by `denote-dired-rewrite-front-matter'."
  (let ((kw-space (denote--file-meta-keywords keywords))
        (kw-toml (denote--file-meta-keywords keywords 'toml)))
    (pcase filetype
      ('markdown-toml (format denote-toml-front-matter title date kw-toml id))
      ('markdown-yaml (format denote-yaml-front-matter title date kw-space id))
      ('text (format denote-text-front-matter title date kw-space id denote-text-front-matter-delimiter))
      (_ (format denote-org-front-matter title date kw-space id)))))

(defun denote-dired--filetype-heuristics (file)
  "Return likely file type of FILE.
The return value is for `denote--file-meta-header'."
  (pcase (file-name-extension file)
    ("md" (if (string-match-p "title\\s-*=" (denote-retrieve--value-title file 0))
              'markdown-toml
            'markdown-yaml))
    ("txt" 'text)
    (_ 'org)))

(defun denote-dired--front-matter-search-delimiter (filetype)
  "Return likely front matter delimiter search for FILETYPE."
  (pcase filetype
    ('markdown-toml (re-search-forward "^\\+\\+\\+$" nil t 2))
    ('markdown-yaml (re-search-forward "^---$" nil t 2))
    ;; 2 at most, as the user might prepend it to the block as well.
    ;; Though this might give us false positives, it ultimately is the
    ;; user's fault.
    ('text (or (re-search-forward denote-text-front-matter-delimiter nil t 2)
               (re-search-forward denote-text-front-matter-delimiter nil t 1)
               (re-search-forward "^[\s\t]*$" nil t 1)))
    ;; Org does not have a real delimiter.  This is the trickiest one.
    (_ (re-search-forward "^[\s\t]*$" nil t 1))))

(defun denote-dired--edit-front-matter-p (file)
  "Test if FILE should be subject to front matter rewrite.
This is relevant for `denote-dired-rewrite-front-matter': if FILE
has no front matter, then we abort early instead of trying to
replace what isn't there."
  (when-let ((ext (file-name-extension file)))
    (and (file-regular-p file)
         (file-writable-p file)
         (not (denote--file-empty-p file))
         (string-match-p "\\(md\\|org\\|txt\\)\\'" ext)
         ;; Heuristic to check if this is one of our notes
         (string= (expand-file-name default-directory) (denote-directory)))))

(defun denote-dired-rewrite-front-matter (file title keywords)
  "Rewrite front matter of note after `denote-dired-rename-file'.
The FILE, TITLE, and KEYWORDS are passed from the renaming
command and are used to construct a new front matter block if
appropriate."
  (when-let* ((denote-dired--edit-front-matter-p file)
              (id (denote-retrieve--filename-identifier file))
              (date (denote-retrieve--value-date file))
              (filetype (denote-dired--filetype-heuristics file))
              (new-front-matter (denote--file-meta-header title date keywords id filetype)))
    (let (old-front-matter front-matter-delimiter)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (setq front-matter-delimiter (denote-dired--front-matter-search-delimiter filetype))
            (when front-matter-delimiter
              (setq old-front-matter
                    (buffer-substring-no-properties
                     (point-min)
                     (progn front-matter-delimiter (point)))))))
        (when (and old-front-matter
                   (y-or-n-p
                    (format "%s\n%s\nReplace front matter?"
                            (propertize old-front-matter 'face 'error)
                            (propertize new-front-matter 'face 'success))))
          (delete-region (point-min) front-matter-delimiter)
          (goto-char (point-min))
          (insert new-front-matter)
          ;; FIXME 2022-06-16: Instead of `delete-blank-lines', we
          ;; should check if we added any new lines and delete only
          ;; those.
          (delete-blank-lines))))))

;;;; Extra fontification

(require 'denote-faces)

(defconst denote-dired-font-lock-keywords
  `((,denote--file-regexp
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter)
     (4 'denote-faces-title)
     (5 'denote-faces-delimiter)
     (6 'denote-faces-keywords)
     (7 'denote-faces-extension))
    ("_"
     (0 'denote-faces-delimiter t)))
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
