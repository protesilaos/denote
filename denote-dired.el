;;; denote-dired.el --- Integration between Denote and Dired -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 0.3.1
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
;; form part of the user's longer-term storage (see manual's "The
;; file-naming scheme").  While Denote does not manage such files
;; (e.g. doesn't create links to them), it already has all the
;; mechanisms to facilitate the task of renaming them.
;;
;; The `denote-dired-rename-file' command renames a file and updates
;; existing front matter if appropriate.
;;
;; If in Dired, the `FILE' to be renamed is the one at point, else the
;; command prompts with minibuffer completion for a target file.
;;
;; If `FILE' has a Denote-compliant identifier, the command retains it
;; while updating the `TITLE' and `KEYWORDS' fields of the file name.
;; Otherwise it creates an identifier based on the file's attribute of last
;; modification time.  If such attribute cannot be found, the identifier
;; falls back to the current date and time.
;;
;; The default `TITLE' is retrieved from a line starting with a title
;; field in the file's contents, depending on the given file type (see
;; manual's "Front matter").  Else, the file name is used as a default
;; value at the minibuffer prompt.
;;
;; As a final step after the `FILE', `TITLE', and `KEYWORDS' prompts, ask
;; for confirmation, showing the difference between old and new file names.
;; For example:
;;
;;     Rename sample.pdf to 20220612T052900--my-sample-title__testing.pdf? (y or n)
;;
;; However, if the user option `denote-dired-rename-expert' is non-nil, the
;; command conducts the renaming operation outright---no questions asked!
;;
;; The file type extension (e.g. `.pdf') is read from the underlying file
;; and is preserved through the renaming process.  Files that have no
;; extension are simply left without one.
;;
;; Renaming only occurs relative to the current directory.  Files are not
;; moved between directories.
;;
;; If the FILE has Denote-style front matter for the TITLE and KEYWORDS,
;; ask to rewrite their values in order to reflect the new input (this
;; step always requires confirmation and the underlying buffer is not
;; saved, so consider invoking `diff-buffer-with-file' to double-check
;; the effect).  The rewrite of the FILE and KEYWORDS in the front
;; matter should not affect the rest of the block.
;;
;; If the file doesn't have front matter, skip this step (see the
;; command `denote-dired-rename-file-and-add-front-matter').
;;
;; The `denote-dired-rename-file' command is intended to (i) rename
;; existing Denote notes while updating their title and keywords in the
;; front matter, (ii) rename files that can benefit from Denote's
;; file-naming scheme.  The latter is a convenience we provide, since we
;; already have all the requisite mechanisms in place (though Denote
;; does not---and will not---manage such files).
;;
;;
;; The command `denote-dired-rename-file-and-add-front-matter' has the
;; same modalities of interaction as the `denote-dired-rename-file'
;; command (see manual's "Rename a single file").  The difference is
;; that it unconditionally inserts front matter at the start of a file.
;;
;; This command is thus suitable for a workflow where an existing writable
;; file needs to be converted into a Denote-style note.  Whereas the other
;; command does not insert front matter if one doesn't already exist.
;;
;; Front matter is added when the file type extension is among the
;; supported ones (per `denote-file-type').
;;
;;
;; The `denote-dired-rename-marked-files' command renames marked files in
;; Dired to conform with our file-naming scheme.  The operation does the
;; following:
;;
;; - the file's existing file name is retained and becomes the `TITLE'
;;   field, per our file-naming scheme;
;;
;; - the `TITLE' is sluggified and downcased, per our conventions;
;;
;; - an identifier is prepended to the `TITLE';
;;
;; - the file's contents are not touched (no insertion of front
;;   matter, no other changes);
;;
;; - the file's extension is retained;
;;
;; - a prompt is asked once for the `KEYWORDS' field and the input is
;;   applied to all files.
;;
;; This command ignores files that comply with Denote's file-naming
;; scheme.
;;
;; The command `denote-dired-rename-marked-files-and-add-front-matters' is
;; like `denote-dired-rename-marked-files' but also adds front matter.  The
;; additon of front matter takes place only if the file has the appropriate
;; file type extension (per the user option `denote-file-type').
;;
;; Buffers are not saved.  The user can thus check them to confirm that the
;; new front matter does not cause any problems (e.g. by invoking the
;; command `diff-buffer-with-file').
;;
;; Multiple buffers can be saved with `save-some-buffers' (read its doc
;; string).
;;
;;
;; One of the upsides of Denote's file-naming scheme is the predictable
;; pattern it establishes, which appears as a near-tabular presentation
;; in a listing of notes (i.e. in Dired).  The `denote-dired-mode' can
;; help enhance this impression, by fontifying the components of the
;; file name to make the date (identifier) and keywords stand out.
;;
;; There are two ways to set the mode.  Either use it for all
;; directories, which probably is not needed:
;;
;;     (require 'denote-dired)
;;     (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; Or configure the user option `denote-dired-directories' and then set
;; up the function `denote-dired-mode-in-directories':
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
        ;; (thread-last denote-directory (expand-file-name "attachments"))
        (expand-file-name "~/Documents/vlog"))
  "List of directories where `denote-dired-mode' should apply to."
  :type '(repeat directory)
  :group 'denote-dired)

(defcustom denote-dired-rename-expert nil
  "If t, renaming a file doesn't ask for confirmation.
The confiration is asked via a `y-or-n-p' prompt which shows the
old name followed by the new one.  This applies to the command
`denote-dired-rename-file'."
  :type 'boolean
  :group 'denote-dired)

(make-obsolete 'denote-dired-post-rename-functions nil "0.4.0")

;;;; File helper functions

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
  "Return Dired file at point, else prompt for one.

Throw error is FILE is not regular, else return FILE."
  (or (dired-get-filename nil t)
      (let* ((file (buffer-file-name))
             (format (if file
                         (format "Rename file Denote-style [%s]: " file)
                       "Rename file Denote-style: "))
             (selected-file (read-file-name format nil file t nil)))
        (if (or (file-directory-p selected-file)
                (not (file-regular-p selected-file)))
            (user-error "Only rename regular files")
          selected-file))))

(defun denote-dired--rename-file (old-name new-name)
  "Rename file named OLD-NAME to NEW-NAME.
Update Dired buffers if the file is renamed.
Return t if the file is renamed, nil otherwise."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (let ((response
           (y-or-n-p
            (format "Rename %s to %s?"
                    (propertize (file-name-nondirectory old-name) 'face 'error)
                    (propertize (file-name-nondirectory new-name) 'face 'success)))))
      (when response
        (rename-file old-name new-name nil)
        (denote-dired--rename-buffer old-name new-name))
      response)))

(defun denote-dired-update-dired-buffers ()
  "Update Dired buffers of variable `denote-directory'."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (and (eq major-mode 'dired-mode)
                  (string-prefix-p (denote-directory)
                                   (expand-file-name default-directory)))
         (revert-buffer))))
   (buffer-list)))

(defun denote-dired--filetype-heuristics (file)
  "Return likely file type of FILE.
The return value is for `denote--file-meta-header'."
  (pcase (file-name-extension file)
    ("md" (if (string-match-p "title\\s-*=" (denote-retrieve--value-title file t))
              'markdown-toml
            'markdown-yaml))
    ("txt" 'text)
    (_ 'org)))

(defun denote-dired--edit-front-matter-p (file)
  "Test if FILE should be subject to front matter rewrite.
This is relevant for `denote-dired--rewrite-front-matter': if FILE
has no front matter, then we abort early instead of trying to
replace what isn't there."
  (when-let ((ext (file-name-extension file)))
    (and (file-regular-p file)
         (file-writable-p file)
         (not (denote--file-empty-p file))
         (string-match-p "\\(md\\|org\\|txt\\)\\'" ext)
         ;; Heuristic to check if this is one of our notes
         (string= (expand-file-name default-directory) (denote-directory)))))

(defun denote-dired--rewrite-front-matter (file title keywords)
  "Rewrite front matter of note after `denote-dired-rename-file'.
The FILE, TITLE, and KEYWORDS are passed from the renaming
command and are used to construct a new front matter block if
appropriate."
  (when-let* ((denote-dired--edit-front-matter-p file)
              (id (denote-retrieve--filename-identifier file))
              (date (denote-retrieve--value-date file)))
    (let ((old-title (denote-retrieve--value-title file))
          (old-keywords (denote-retrieve--value-keywords file))
          (new-title title)
          (new-keywords (denote--file-meta-keywords
                         keywords (denote-dired--filetype-heuristics file))))
      (with-current-buffer (find-file-noselect file)
        (when (y-or-n-p (format
                         "Replace front matter?\n-%s\n+%s\n\n-%s\n+%s"
                         (propertize old-title 'face 'error)
                         (propertize new-title 'face 'success)
                         (propertize old-keywords 'face 'error)
                         (propertize new-keywords 'face 'success)))
          (save-excursion
            (goto-char (point-min))
            (search-forward old-title nil t 1)
            (replace-match (concat "\\1" new-title) t)
            (goto-char (point-min))
            (search-forward old-keywords nil t 1)
            (replace-match (concat "\\1" new-keywords) t)))))))

(defun denote-dired--add-front-matter (file title keywords id)
  "Add front matter to the beginning of FILE.
The TITLE, KEYWORDS and ID are passed from the renaming
command and are used to construct a new front matter block if
appropriate."
  (when-let* ((denote--only-note-p file)
              (filetype (denote-dired--filetype-heuristics file))
              (date (denote--date (date-to-time id)))
              (new-front-matter (denote--file-meta-header title date keywords id filetype)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (insert new-front-matter))))

;;;; Renaming commands

;;;###autoload
(defun denote-dired-rename-file (file title keywords)
  "Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the file's attribute of last
modification time.  If such attribute cannot be found, the
identifier falls back to the `current-time'.

The default TITLE is retrieved from a line starting with a title
field in the file's contents, depending on the given file type.
Else, the file name is used as a default value at the minibuffer
prompt.

As a final step after the FILE, TITLE, and KEYWORDS prompts, ask
for confirmation, showing the difference between old and new file
names.  If `denote-dired-rename-expert' is non-nil, conduct the
renaming operation outright---no question asked!

The file type extension (e.g. .pdf) is read from the underlying
file and is preserved through the renaming process.  Files that
have no extension are simply left without one.

Renaming only occurs relative to the current directory.  Files
are not moved between directories.

If the FILE has Denote-style front matter for the TITLE and
KEYWORDS, ask to rewrite their values in order to reflect the new
input (this step always requires confirmation and the underlying
buffer is not saved, so consider invoking `diff-buffer-with-file'
to double-check the effect).  The rewrite of the FILE and
KEYWORDS in the front matter should not affect the rest of the
block.

If the file doesn't have front matter, skip this step (see the
command `denote-dired-rename-file-and-add-front-matter').

This command is intended to (i) rename existing Denote notes
while updating their title and keywords in the front matter, (ii)
rename files that can benefit from Denote's file-naming scheme.
The latter is a convenience we provide, since we already have all
the requisite mechanisms in place (though Denote does not---and
will not---manage such files)."
  (interactive
   (let ((file (denote-dired--rename-dired-file-or-prompt)))
     (list
      file
      (denote--title-prompt
       (or (denote-retrieve--value-title file)
           (file-name-sans-extension (file-name-nondirectory file))))
      (denote--keywords-prompt))))
  (let* ((dir (file-name-directory file))
         (id (denote-dired--file-name-id file))
         (extension (file-name-extension file t))
         (new-name (denote--format-file
                    dir id keywords (denote--sluggify title) extension))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (when (denote-dired--rename-file file new-name)
      (denote-dired-update-dired-buffers)
      (denote-dired--rewrite-front-matter new-name title keywords))))

;;;###autoload
(defun denote-dired-rename-file-and-add-front-matter (file title keywords)
  "Rename FILE and unconditionally add front matter.

This command has the same modalities of interaction as
`denote-dired-rename-file' in terms of the FILE, TITLE, and
KEYWORDS prompts, except it always inserts front matter at the
start of the file.  It does not check if any front matter is
already present.

Front matter is added only when the file is one of the supported
file types (per `denote-file-type').  For per-file-type front
matter, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'"
  (interactive
   (let ((file (denote-dired--rename-dired-file-or-prompt)))
     (list
      file
      (denote--title-prompt
       (or (denote-retrieve--value-title file)
           (file-name-sans-extension (file-name-nondirectory file))))
      (denote--keywords-prompt))))
  (let* ((dir (file-name-directory file))
         (id (denote-dired--file-name-id file))
         (extension (file-name-extension file t))
         (new-name (denote--format-file
                    dir id keywords (denote--sluggify title) extension))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (when (denote-dired--rename-file file new-name)
      (denote-dired-update-dired-buffers)
      (denote-dired--add-front-matter new-name title keywords id))))

(define-obsolete-function-alias
  'denote-dired-convert-file-to-denote
  'denote-dired-rename-file-and-add-front-matter
  "0.4.0")

;;;###autoload
(defun denote-dired-rename-marked-files ()
  "Rename marked files in Dired to Denote file name.

The operation does the following:

- the file's existing file name is retained and becomes the TITLE
  field, per Denote's file-naming scheme;

- the TITLE is sluggified and downcased, per our conventions;

- an identifier is prepended to the TITLE;

- the file's contents are not touched (no insertion of front
  matter, no other changes);

- the file's extension is retained;

- a prompt is asked once for the KEYWORDS field and the input is
  applied to all files.

This command ignores files that comply with Denote's file-naming
scheme."
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files))
           (keywords (denote--keywords-prompt)))
      (progn
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (denote-dired--file-name-id file))
                 (title (or (denote-retrieve--value-title file)
                            (file-name-sans-extension
                             (file-name-nondirectory file))))
                 (extension (file-name-extension file t))
                 (new-name (denote--format-file
                            dir id keywords (denote--sluggify title) extension)))
            (rename-file file new-name)
            (denote-dired--rename-buffer file new-name)))
        (revert-buffer))
    (user-error "No marked files; aborting")))

;;;###autoload
(defun denote-dired-rename-marked-files-and-add-front-matters ()
  "Like `denote-dired-rename-marked-files' but add front matter.

The additon of front matter takes place only if the given file
has the appropriate file type extension (per the user option
`denote-file-type').

Buffers are not saved.  The user can thus check them to confirm
that the new front matter does not cause any problems (e.g. by
invoking the command `diff-buffer-with-file').

Multiple buffers can be saved with `save-some-buffers' (read its
doc string)."
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files))
           (keywords (denote--keywords-prompt))
           ((y-or-n-p "Add front matter to all FILES (buffers are not saved)?")))
      (progn
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (denote-dired--file-name-id file))
                 (title (or (denote-retrieve--value-title file)
                            (file-name-sans-extension
                             (file-name-nondirectory file))))
                 (extension (file-name-extension file t))
                 (new-name (denote--format-file
                            dir id keywords (denote--sluggify title) extension)))
            (rename-file file new-name)
            (denote-dired--rename-buffer file new-name)
            (denote-dired--add-front-matter new-name title keywords id)))
        (revert-buffer))
    (user-error "No marked files; aborting")))

;;;; Extra fontification

(require 'denote-faces)

;;;###autoload
(define-minor-mode denote-dired-mode
  "Fontify all Denote-style file names in Dired."
  :global nil
  :group 'denote-dired
  (if denote-dired-mode
      (font-lock-add-keywords nil denote-faces-file-name-keywords t)
    (font-lock-remove-keywords nil denote-faces-file-name-keywords))
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
