;;; denote-dired.el --- Integration between Denote and Dired -*- lexical-binding: t -*-

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
;; - the file's existing file name is retained and becomes the TITLE
;;   field, per Denote's file-naming scheme;
;;
;; - the TITLE is sluggified and downcased, per our conventions;
;;
;; - an identifier is prepended to the TITLE;
;;
;; - the file's extension is retained;
;;
;; - a prompt is asked once for the KEYWORDS field and the input is
;;   applied to all file names;
;;
;; - if the file is recognized as a Denote note, rewrite its front
;;   matter to include the new keywords.  A confirmation to carry out
;;   this step is performed once at the outset.  Note that the affected
;;   buffers are not saved.  The user can thus check them to confirm
;;   that the new front matter does not cause any problems (e.g. with
;;   the command `diff-buffer-with-file').  Multiple buffers can be
;;   saved with `save-some-buffers' (read its doc string).
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

(require 'denote)
(require 'denote-faces)
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

;;;; Extra fontification

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
