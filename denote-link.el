;;; denote-link.el --- Link facility for Denote -*- lexical-binding: t -*-

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
;; The `denote-link' command inserts a link at point to an entry specified
;; at the minibuffer prompt.  Links are formatted depending on the file
;; type of current note.  In Org and plain text buffers, links are
;; formatted thus: `[[denote:IDENTIFIER][TITLE]]'.  While in Markdown they
;; are expressed as `[TITLE](denote:IDENTIFIER)'.
;;
;; When `denote-link' is called with a prefix argument (`C-u' by default),
;; it formats links like `[[denote:IDENTIFIER]]'.  The user might prefer
;; its simplicity.
;;
;; Inserted links are automatically buttonized and remain active for as
;; long as the buffer is available.  In Org this is handled by the major
;; mode: the `denote:' hyperlink type works exactly like the standard
;; `file:'.  In Markdown and plain text, Denote performs the buttonization
;; of those links.  To buttonize links in existing files while visiting
;; them, the user must add this snippet to their setup (it already excludes
;; Org):
;;
;;     (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
;;
;; Denote has a major-mode-agnostic mechanism to collect all linked file
;; references in the current buffer and return them as an appropriately
;; formatted list.  This list can then be used in interactive commands.
;; The `denote-link-find-file' is such a command.  It uses minibuffer
;; completion to visit a file that is linked to from the current note.
;; The candidates have the correct metadata, which is ideal for
;; integration with other standards-compliant tools (see manual's
;; "Extending Denote").  For instance, a package such as `marginalia'
;; will display accurate annotations, while the `embark' package will be
;; able to work its magic such as in exporting the list into a filtered
;; Dired buffer (i.e. a familiar Dired listing with only the files of
;; the current minibuffer session).
;;
;; The command `denote-link-backlinks' produces a bespoke buffer which
;; displays the file name of all notes linking to the current one.  Each
;; file name appears on its own line and is buttonized so that it performs
;; the action of visiting the referenced file.  The backlinks' buffer looks
;; like this:
;;
;;     Backlinks to "On being honest" (20220614T130812)
;;     ------------------------------------------------
;;
;;     20220614T145606--let-this-glance-become-a-stare__journal.txt
;;     20220616T182958--not-feeling-butterflies-in-your-stomach__journal.txt
;;
;; The backlinks' buffer is fontified by default, though the user has
;; access to the `denote-link-fontify-backlinks' option to disable this
;; effect by setting its value to nil.
;;
;; The placement of the backlinks' buffer is subject to the user option
;; `denote-link-backlinks-display-buffer-action'.  Due to the nature of the
;; underlying `display-buffer' mechanism, this inevitably is a relatively
;; advanced feature.  By default, the backlinks' buffer is displayed below
;; the current window.  The doc string of our user option includes a sample
;; configuration that places the buffer in a left side window instead.
;; Reproducing it here for the sake of convenience:
;;
;;     (setq denote-link-backlinks-display-buffer-action
;;           '((display-buffer-reuse-window
;;              display-buffer-in-side-window)
;;             (side . left)
;;             (slot . 99)
;;             (window-width . 0.3)))
;;
;; The command `denote-link-add-links' adds links at point matching a
;; regular expression or plain string.  The links are inserted as a
;; typographic list, such as:
;;
;;     - link1
;;     - link2
;;     - link3
;;
;; Each link is formatted according to the file type of the current note,
;; as explained further above about the `denote-link' command.  The current
;; note is excluded from the matching entries (adding a link to itself is
;; pointless).
;;
;; When called with a prefix argument (`C-u') `denote-link-add-links' will
;; format all links as `[[denote:IDENTIFIER]]', hence a typographic list:
;;
;;     - [[denote:IDENTIFIER-1]]
;;     - [[denote:IDENTIFIER-2]]
;;     - [[denote:IDENTIFIER-3]]
;;
;; Same examples of a regular expression that can be used with this
;; command:
;;
;; - `journal' match all files which include `journal' anywhere in their
;;   name.
;;
;; - `_journal' match all files which include `journal' as a keyword.
;;
;; - `^2022.*_journal' match all file names starting with `2022' and
;;   including the keyword `journal'.
;;
;; - `\.txt' match all files including `.txt'.  In practical terms, this
;;   only applies to the file extension, as Denote automatically removes
;;   dots (and other characters) from the base file name.
;;
;; If files are created with `denote-sort-keywords' as non-nil (the
;; default), then it is easy to write a regexp that includes multiple
;; keywords in alphabetic order:
;;
;; - `_denote.*_package' match all files that include both the `denote' and
;;   `package' keywords, in this order.
;;
;; - `\(.*denote.*package.*\)\|\(.*package.*denote.*\)' is the same as
;;   above, but out-of-order.
;;
;; Remember that regexp constructs only need to be escaped once (like `\|')
;; when done interactively but twice when called from Lisp.  What we show
;; above is for interactive usage.
;;
;; For convenience, the `denote-link' command has an alias called
;; `denote-link-insert-link'.  The `denote-link-backlinks' can also be used
;; as `denote-link-show-backlinks-buffer'.  While `denote-link-add-links'
;; is aliased `denote-link-insert-links-matching-regexp'.  The purpose of
;; these aliases is to offer alternative, more descriptive names of select
;; commands.

;;; Code:

(require 'denote-retrieve)

(defgroup denote-link ()
  "Link facility for Denote."
  :group 'denote)

;;;; User options

(defcustom denote-link-fontify-backlinks t
  "When non-nil, apply faces to files in the backlinks' buffer."
  :type 'boolean
  :group 'denote-link)

(defcustom denote-link-backlinks-display-buffer-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . fit-window-to-buffer))
  "The action used to display the current file's backlinks buffer.

The value has the form (FUNCTION . ALIST), where FUNCTION is
either an \"action function\", a list thereof, or possibly an
empty list.  ALIST is a list of \"action alist\" which may be
omitted (or be empty).

Sample configuration to display the buffer in a side window on
the left of the Emacs frame:

    (setq denote-link-backlinks-display-buffer-action
          (quote ((display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (side . left)
                  (slot . 99)
                  (window-width . 0.3))))

See Info node `(elisp) Displaying Buffers' for more details
and/or the documentation string of `display-buffer'."
  :type '(cons (choice (function :tag "Display Function")
                       (repeat :tag "Display Functions" function))
               alist)
  :group 'denote-link)

;;;; Link to note

;; Arguments are: FILE-ID FILE-TITLE
(defconst denote-link--format-org "[[denote:%s][%s]]"
  "Format of Org link to note.")

(defconst denote-link--format-markdown "[%2$s](denote:%1$s)"
  "Format of Markdown link to note.")

(defconst denote-link--format-id-only "[[denote:%s]]"
  "Format of identifier-only link to note.")

(defconst denote-link--regexp-org
  (concat "\\[\\[" "denote:"  "\\(?1:" denote--id-regexp "\\)" "]" "\\[.*?]]"))

(defconst denote-link--regexp-markdown
  (concat "\\[.*?]" "(denote:"  "\\(?1:" denote--id-regexp "\\)" ")"))

(defconst denote-link--regexp-plain
  (concat "\\[\\[" "denote:"  "\\(?1:" denote--id-regexp "\\)" "]]"))

(defun denote-link--file-type-format (current-file id-only)
  "Return link format based on CURRENT-FILE format.
With non-nil ID-ONLY, use the generic link format without a
title."
  ;; Includes backup files.  Maybe we can remove them?
  (let ((current-file-ext (file-name-extension current-file)))
    (cond
     (id-only denote-link--format-id-only)
     ((string= current-file-ext "md")
      denote-link--format-markdown)
     ;; Plain text also uses [[denote:ID][TITLE]]
     (t denote-link--format-org))))

(defun denote-link--file-type-regexp (file)
  "Return link regexp based on FILE format."
  (pcase (file-name-extension file)
    ("md" denote-link--regexp-markdown)
    (_ denote-link--regexp-org)))

(defun denote-link--format-link (file pattern)
  "Prepare link to FILE using PATTERN."
  (let ((file-id (denote-retrieve--filename-identifier file))
        (file-title (unless (string= pattern denote-link--format-id-only)
                      (denote-retrieve--value-title file))))
    (format pattern file-id file-title)))

;;;###autoload
(defun denote-link (target &optional id-only)
  "Create link to TARGET note in variable `denote-directory'.
With optional ID-ONLY, such as a universal prefix
argument (\\[universal-argument]), insert links with just the
identifier and no further description.  In this case, the link
format is always [[denote:IDENTIFIER]]."
  (interactive (list (denote-retrieve--read-file-prompt) current-prefix-arg))
  (let ((beg (point)))
    (insert
     (denote-link--format-link
      target
      (denote-link--file-type-format (buffer-file-name) id-only)))
    (unless (derived-mode-p 'org-mode)
      (make-button beg (point) 'type 'denote-link-button))))

(defalias 'denote-link-insert-link (symbol-function 'denote-link))

(defun denote-link--collect-identifiers (regexp)
  "Return collection of identifiers in buffer matching REGEXP."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string-no-properties 1) matches)))
    matches))

(defun denote-link--expand-identifiers (regexp)
  "Expend identifiers matching REGEXP into file paths."
  (let ((files (denote--directory-files))
        (found-files))
    (dolist (file files)
      (dolist (i (denote-link--collect-identifiers regexp))
        (when (string-prefix-p i (file-name-nondirectory file))
          (push file found-files))))
    found-files))

(defvar denote-link--find-file-history nil
  "History for `denote-link-find-file'.")

(defun denote-link--find-file-prompt (files)
  "Prompt for linked file among FILES."
  (completing-read "Find linked file "
                   (denote--completion-table 'file files)
                   nil t
                   nil 'denote-link--find-file-history))

;; TODO 2022-06-14: Do we need to add any sort of extension to better
;; integrate with Embark?  For the minibuffer interaction it is not
;; necessary, but maybe it can be done to immediately recognise the
;; identifiers are links to files?

;;;###autoload
(defun denote-link-find-file ()
  "Use minibuffer completion to visit linked file."
  (interactive)
  (if-let* ((regexp (denote-link--file-type-regexp (buffer-file-name)))
            (files (denote-link--expand-identifiers regexp)))
      (find-file (denote-link--find-file-prompt files))
    (user-error "No links found in the current buffer")))

;;;; Link buttons

;; Evaluate: (info "(elisp) Button Properties")
;;
;; Button can provide a help-echo function as well, but I think we might
;; not need it.
(define-button-type 'denote-link-button
  'follow-link t
  'action #'denote-link--find-file-at-button)

(autoload 'thing-at-point-looking-at "thingatpt")

(defun denote-link--link-at-point-string ()
  "Return identifier at point."
  (when (or (thing-at-point-looking-at denote-link--regexp-plain)
            (thing-at-point-looking-at denote-link--regexp-markdown)
            (thing-at-point-looking-at denote-link--regexp-org)
            ;; Meant to handle the case where a link is broken by
            ;; `fill-paragraph' into two lines, in which case it
            ;; buttonizes only the "denote:ID" part.  Example:
            ;;
            ;; [[denote:20220619T175212][This is a
            ;; test]]
            ;;
            ;; Maybe there is a better way?
            (thing-at-point-looking-at "\\[\\(denote:.*\\)]"))
    (match-string-no-properties 0)))

(defun denote-link--id-from-string (string)
  "Extract identifier from STRING."
  (replace-regexp-in-string
   (concat ".*denote:" "\\(" denote--id-regexp "\\)" ".*")
   "\\1" string))

;; NOTE 2022-06-15: I add this as a variable for advanced users who may
;; prefer something else.  If there is demand for it, we can make it a
;; defcustom, but I think it would be premature at this stage.
(defvar denote-link-buton-action #'find-file-other-window
  "Action for Denote buttons.")

(defun denote-link--find-file-at-button (button)
  "Visit file referenced by BUTTON."
  (let* ((id (denote-link--id-from-string
              (buffer-substring-no-properties
               (button-start button)
               (button-end button))))
         (file (denote--get-note-path-by-id id)))
    (funcall denote-link-buton-action file)))

;;;###autoload
(defun denote-link-buttonize-buffer (&optional beg end)
  "Make denote: links actionable buttons in the current buffer.

Add this to `find-file-hook'.  It will only work with Denote
notes and will not do anything in `org-mode' buffers, as buttons
already work there.  If you do not use Markdown or plain text,
then you do not need this.

When called from Lisp, with optional BEG and END as buffer
positions, limit the process to the region in-between."
  (when (and (not (derived-mode-p 'org-mode)) (denote--current-file-is-note-p))
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward denote--id-regexp end t)
        (when-let ((string (denote-link--link-at-point-string))
                   (beg (match-beginning 0))
                   (end (match-end 0)))
          (make-button beg end 'type 'denote-link-button))))))

;;;; Backlinks' buffer

(define-button-type 'denote-link-backlink-button
  'follow-link t
  'action #'denote-link--backlink-find-file
  'face 'unspecified)     ; we use this face attribute to style it later

(defun denote-link--backlink-find-file (button)
  "Action for BUTTON to `find-file'."
  (funcall denote-link-buton-action (buffer-substring (button-start button) (button-end button))))

(defun denote-link--display-buffer (buf)
  "Run `display-buffer' on BUF.
Expand `denote-link-backlinks-display-buffer-action'."
  (display-buffer
   buf
   `(,@denote-link-backlinks-display-buffer-action)))

(require 'denote-faces)

(defun denote-link--prepare-backlinks (id files &optional title)
  "Create backlinks' buffer for ID including FILES.
Use optional TITLE for a prettier heading."
  (let ((inhibit-read-only t)
        (buf (format "*denote-backlinks to %s*" id)))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (special-mode)
      (goto-char (point-min))
      (when-let* ((title)
                  (heading (format "Backlinks to %S (%s)" title id))
                  (l (length heading)))
        (insert (format "%s\n%s\n\n" heading (make-string l ?-))))
      (mapc (lambda (f)
              (insert f)
              (make-button (point-at-bol) (point-at-eol) :type 'denote-link-backlink-button)
              (newline))
            files)
      (goto-char (point-min))
      (when denote-link-fontify-backlinks
        (font-lock-add-keywords nil denote-faces-file-name-with-subdir-keywords t)))
    (denote-link--display-buffer buf)))

;;;###autoload
(defun denote-link-backlinks ()
  "Produce a buffer with files linking to current note.
Each file is a clickable/actionable button that visits the
referenced entry.  Files are fontified if the user option
`denote-link-fontify-backlinks' is non-nil.

The placement of the backlinks' buffer is controlled by the user
option `denote-link-backlinks-display-buffer-action'.  By
default, it will show up below the current window."
  (interactive)
  (let* ((default-directory (denote-directory))
         (file (buffer-file-name))
         (id (denote-retrieve--filename-identifier file))
         (title (denote-retrieve--value-title file)))
    (if-let ((files (denote-retrieve--proces-grep id)))
        (denote-link--prepare-backlinks id files title)
      (user-error "No links to the current note"))))

(defalias 'denote-link-show-backlinks-buffer (symbol-function 'denote-link-backlinks))

;;;; Add links matching regexp

(defvar denote-link--links-to-files nil
  "String of `denote-link-add-links-matching-keyword'.")

(defvar denote-link--prepare-links-format "- %s\n"
  "Format specifiers for `denote-link-add-links'.")

;; NOTE 2022-06-16: There is no need to overwhelm the user with options,
;; though I expect someone to want to change the sort order.
(defvar denote-link-add-links-sort nil
  "Add REVERSE to `sort-lines' of `denote-link-add-links' when t.")

(defun denote-link--prepare-links (files current-file id-only)
  "Prepare links to FILES from CURRENT-FILE.
When ID-ONLY is non-nil, use a generic link format.  See
`denote-link--file-type-format'."
  (setq denote-link--links-to-files
        (with-temp-buffer
          (mapc (lambda (file)
                  (insert
                   (format
                    denote-link--prepare-links-format
                    (denote-link--format-link
                     file
                     (denote-link--file-type-format current-file id-only)))))
                files)
          (sort-lines denote-link-add-links-sort (point-min) (point-max))
          (buffer-string))))

(defvar denote-link--add-links-history nil
  "Minibuffer history for `denote-link-add-links'.")

;;;###autoload
(defun denote-link-add-links (regexp &optional id-only)
  "Insert links to all notes matching REGEXP.
Use this command to reference multiple files at once.
Particularly useful for the creation of metanotes (read the
manual for more on the matter).

Optional ID-ONLY has the same meaning as in `denote-link': it
inserts links with just the identifier."
  (interactive
   (list
    (read-regexp "Insert links matching REGEX: " nil 'denote-link--add-links-history)
    current-prefix-arg))
  (let* ((default-directory (denote-directory))
         (current-file (buffer-file-name)))
    (if-let ((files (denote--directory-files-matching-regexp regexp)))
        (let ((beg (point)))
          (insert (denote-link--prepare-links files current-file id-only))
          (unless (derived-mode-p 'org-mode)
            (denote-link-buttonize-buffer beg (point))))
      (user-error "No links matching `%s'" regexp))))

(defalias 'denote-link-insert-links-matching-regexp (symbol-function 'denote-link-add-links))

;;;; Register `denote:' custom Org hyperlink

(declare-function org-link-set-parameters "ol.el" (type &rest parameters))

(org-link-set-parameters
 "denote"
 :follow #'denote-link-ol-follow
 :complete #'denote-link-ol-complete
 :export #'denote-link-ol-export)

(declare-function org-link-open-as-file "ol" (path arg))

(defun denote-link--ol-resolve-link-to-target (link &optional path-id)
  "Resolve LINK into the appropriate target.
With optional PATH-ID return a cons cell consisting of the path
and the identifier."
  (let* ((search (and (string-match "::\\(.*\\)\\'" link)
                      (match-string 1 link)))
         (id (if (and (stringp search) (not (string-empty-p search)))
                 (substring link 0 (match-beginning 0))
               link))
         (path (denote--get-note-path-by-id id)))
    (cond
     (path-id
      (cons (format "%s" path) (format "%s" id)))
     ((and (stringp search) (not (string-empty-p search)))
      (concat path "::" search))
     (path))))

(defun denote-link-ol-follow (link)
  "Find file of type `denote:' matching LINK.
LINK is the identifier of the note, optionally followed by a
search option akin to that of standard Org `file:' link types.
Read Info node `(org) Search Options'.

Uses the function `denote-directory' to establish the path to the
file."
  (org-link-open-as-file
   (denote-link--ol-resolve-link-to-target link)
   nil))

(defun denote-link-ol-complete ()
  "Like `denote-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `denote:' hyperlink type."
  (concat
   "denote:"
   (denote-retrieve--filename-identifier (denote-retrieve--read-file-prompt))))

(defun denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
         (path (file-name-nondirectory (car path-id)))
         (p (file-name-sans-extension path))
         (id (cdr path-id))
         (desc (or description (concat "denote:" id))))
    (cond
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <denote:%s>" desc path)) ; NOTE 2022-06-16: May be tweaked further
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

(provide 'denote-link)
;;; denote-link.el ends here
