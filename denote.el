;;; denote.el --- Simple notes with a strict file-naming scheme -*- lexical-binding: t -*-

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
;; NOTE THAT WE ARE ACTIVELY WORKING TOWARDS VERSION `0.1.0' AND MIGHT
;; STILL INTRODUCE BREAKING, BACKWARD-INCOMPATIBLE CHANGES.  This is
;; particularly true for the linking facility.  Everything else is in a
;; stable state.
;;
;; Denote aims to be a simple-to-use, focused-in-scope, and effective
;; note-taking tool for Emacs.  It is based on the following core design
;; principles:
;;
;; * Predictability :: File names must follow a consistent and
;;   descriptive naming convention (read the manual's "The file-naming
;;   scheme").  The file name alone should offer a clear indication of
;;   what the contents are, without reference to any other metadatum.
;;   This convention is not specific to note-taking, as it is pertinent
;;   to any form of file that is part of the user's long-term storage
;;   (read the manual's "Renaming files").
;;
;; * Composability :: Be a good Emacs citizen, by integrating with other
;;   packages or built-in functionality instead of re-inventing
;;   functions such as for filtering or greping.  Do not introduce
;;   dependencies on specific libraries.  While Org is a killer app for
;;   Emacs and the default file type for new notes, Denote does not
;;   depend on org.el nor its extensions and does allow notes to be
;;   created in a variety of formats ((read the manual's "Notes in
;;   multiple file types").  The author of Denote (Protesilaos, aka
;;   "Prot") writes ordinary notes in plain text (`.txt'), switching to
;;   an Org file only when its expanded set of functionality is required
;;   for the task at hand (read the manual's "Points of entry").
;;
;; * Portability :: Notes are plain text and should remain portable.
;;   The way Denote writes file names, the front matter it include in
;;   the note's header, and the links it establishes must all be
;;   adequately usable with standard Unix tools.  No need for a databse
;;   or some specialised software.  As Denote develops and this manual
;;   is fully fleshed out, there will be concrete examples on how to do
;;   the Denote-equivalent on the command-line.
;;
;; * Flexibility :: Do not assume the user's preference for a
;;   note-taking methodology.  Denote is conceptually similar to the
;;   Zettelkasten Method, which you can learn more about in this
;;   detailed introduction: <https://zettelkasten.de/introduction/>.
;;   Notes are atomic (one file per note) and have a unique identifier.
;;   However, Denote does not enforce a particular methodology for
;;   knowledge management, such as a restricted vocabulary or mutually
;;   exclusive sets of keywords.  It is up to the user to apply the
;;   requisite rigor in pursuit of their preferred workflow.
;;
;; Now the important part...  "Denote" is the familiar word, though it
;; also is a play on the "note" concept.  Plus, we can come up with
;; acronyms, recursive or otherwise, of increasingly dubious utility
;; like:
;;
;; * Don't Ever Note Only The Epiphenomenal
;; * Denote Everything Neatly; Omit The Excesses
;;
;; But we'll let you get back to work.  Don't Eschew or Neglect your
;; Obligations, Tasks, and Engagements.

;;; Code:

(defgroup denote ()
  "Simple notes with a strict file-naming scheme."
  :group 'files)

;;;; User options

(defcustom denote-directory (expand-file-name "~/Documents/notes/")
  "Directory for storing personal notes.
If you intend to reference this variable in Lisp, consider using
the function `denote-directory' instead: it returns the path as a
directory."
  :group 'denote
  :type 'directory)

(defcustom denote-known-keywords
  '("emacs" "philosophy" "politics" "economics")
  "List of strings with predefined keywords for `denote'.

The implicit assumption is that a keyword is a single word.  If
you need a keyword to be multiple words long, use underscores to
separate them.  Do not use hyphens or other characters, as those
are assumed to demarcate distinct keywords."
  :group 'denote
  :type '(repeat string))

(defcustom denote-infer-keywords t
  "Whether to infer keywords.

When non-nil, search the file names of existing notes in the
variable `denote-directory' for their keyword field and extract
the entries as \"inferred keywords\".  These are combined with
`denote-known-keywords' and are presented as completion
candidated while using `denote' interactively.

If nil, refrain from inferring keywords.  The aforementioned
completion prompt only shows the `denote-known-keywords'."
  :group 'denote
  :type 'boolean)

(defcustom denote-sort-keywords t
  "Whether to sort keywords in new files.

When non-nil, the keywords of `denote' are sorted with
`string-lessp' regardless of the order they were inserted at the
minibuffer prompt.

If nil, show the keywords in their given order."
  :group 'denote
  :type 'boolean)

(defcustom denote-allow-multi-word-keywords t
  "If non-nil keywords can consist of multiple words.
Words are automatically separated by a hyphen when using the
`denote' command or related.  The hyphen is the only legal
character---no spaces, no other characters.  If, for example, the
user types <word1+word2> or <word1 word2>, it is converted to
<word1-word2>.

When nil, do not allow keywords to consist of multiple words.
Reduce them to a single word, such as by turning <word1+word2> or
<word1 word2> into <word1word2>."
  :group 'denote
  :type 'boolean)

(defcustom denote-file-type nil
  "The file type extension for new notes.

By default (a nil value), the file type is that of Org mode.

When the value is the symbol `markdown-yaml', the file type is
that of Markdown mode and the front matter uses YAML.  Similarly,
`markdown-toml' will use Markdown but apply TOML to the front
matter.

When the value is `text', the file type is that of Text mode.

Any other non-nil value is the same as the default."
  :type '(choice
          (const :tag "Org mode (default)" nil)
          (const :tag "Markdown (YAML front matter)" markdown-yaml)
          (const :tag "Markdown (TOML front matter)" markdown-toml)
          (const :tag "Plain text" text))
  :group 'denote)

(defcustom denote-front-matter-date-format nil
  "Date format in the front matter (file header) of new notes.

If the value is nil, use a plain date in YEAR-MONTH-DAY notation,
like 2022-06-08 (the ISO 8601 standard).

If the value is the `org-timestamp' symbol, format the date as an
inactive Org timestamp such as: [2022-06-08 Wed 06:19].

If a string, use it as the argument of `format-time-string'.
Read the documentation of that function for valid format
specifiers.

When `denote-file-type' specifies one of the Markdown flavors, we
ignore this user option in order to enforce the RFC3339
specification (Markdown is typically employed in static site
generators as source code for Web pages).  However, when
`denote-front-matter-date-format' has a string value, this rule
is suspended: we use whatever the user wants."
  :type '(choice
          (const :tag "Just the date like 2022-06-08" nil)
          (const :tag "An inactive Org timestamp like [2022-06-08 Wed 06:19]" org-timestamp)
          (string :tag "Custom format for `format-time-string'"))
  :group 'denote)

;;;; Main variables

(defconst denote--id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.")

(defconst denote--id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote--id-format'.")

(defconst denote--file-title-regexp
  (concat denote--id-regexp "\\(--\\)\\(.*\\)\\(__\\)")
  "Regular expression to match file names from `denote'.")

(defconst denote--file-regexp
  (concat denote--file-title-regexp "\\([0-9A-Za-z_-]*\\)\\(\\.?.*\\)")
  "Regular expression to match the entire file name'.")

(defconst denote--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.
We consider those characters illigal for our purposes.")

(defvar denote-last-path nil "Store last path.")
(defvar denote-last-title nil "Store last title.")
(defvar denote-last-keywords nil "Store last keywords.")
(defvar denote-last-buffer nil "Store last buffer.")
(defvar denote-last-front-matter nil "Store last front-matter.")

;;;; File helper functions

(defun denote--completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defun denote-directory ()
  "Return path of variable `denote-directory' as a proper directory."
  (let ((path denote-directory))
    (unless (file-directory-p path)
      (make-directory path t))
    (file-name-as-directory path)))

(defun denote--extract (regexp str &optional group)
  "Extract REGEXP from STR, with optional regexp GROUP."
  (when group
    (unless (and (integerp group) (> group 0))
      (error "`%s' is not a positive integer" group)))
  (with-temp-buffer
    (insert str)
    (when (re-search-forward regexp nil t -1)
      (match-string (or group 1)))))

(defun denote--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string denote--punctuation-regexp "" str))

(defun denote--slug-hyphenate (str)
  "Replace spaces with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
   "-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun denote--sluggify (str)
  "Make STR an appropriate slug for file names and related."
  (downcase
   (if denote-allow-multi-word-keywords
       (denote--slug-hyphenate (denote--slug-no-punct str))
     (replace-regexp-in-string
      "-" ""
      (denote--slug-hyphenate (denote--slug-no-punct str))))))

(defun denote--sluggify-keywords (keywords)
  "Sluggify KEYWORDS."
  (if (listp keywords)
      (mapcar #'denote--sluggify keywords)
    (denote--sluggify keywords)))

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defvar denote--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^\\*+ +"))              ; assumes Org markup
  "Alist of regexp types used by `denote-line-regexp-p'.")

(defun denote--line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`denote--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type denote--line-regexp-alist))))))

;;;; Keywords

(defun denote--directory-files (&optional absolute)
  "List note files, assuming flat directory.
If optional ABSOLUTE, show full paths, else only show base file
names that are relative to the variable `denote-directory'."
  (let* ((dir (denote-directory))
         (default-directory dir))
    (seq-remove
     (lambda (file)
       (file-directory-p file))
     (directory-files dir absolute directory-files-no-dot-files-regexp t))))

(defun denote--directory-files-matching-regexp (regexp)
  "Return list of files matching REGEXP."
  (delq
   nil
   (mapcar (lambda (f)
             (when (and (string-match-p regexp f)
                        (not (string= (file-name-nondirectory (buffer-file-name)) f)))
               f))
           (denote--directory-files))))

(defun denote--keywords-in-files ()
  "Produce list of keywords in `denote--directory-files'."
  (delq nil (mapcar
             (lambda (x)
               (denote--extract denote--file-regexp x 6))
             (denote--directory-files))))

(defun denote--inferred-keywords ()
  "Extract keywords from `denote--directory-files'."
  (let ((sequence (denote--keywords-in-files)))
    (mapcan (lambda (s)
              (split-string s "_" t))
            sequence)))

(defun denote-keywords ()
  "Combine `denote--inferred-keywords' with `denote-known-keywords'."
  (delete-dups (append (denote--inferred-keywords) denote-known-keywords)))

(defvar denote--keyword-history nil
  "Minibuffer history of inputted keywords.")

(defun denote--keywords-crm (keywords)
  "Use `completing-read-multiple' for KEYWORDS."
  (completing-read-multiple
   "File keyword: " keywords
   nil nil nil 'denote--keyword-history))

(defun denote--keywords-prompt ()
  "Prompt for one or more keywords.
In the case of multiple entries, those are separated by the
`crm-sepator', which typically is a comma.  In such a case, the
output is sorted with `string-lessp'."
  (let ((choice (denote--keywords-crm (denote-keywords))))
    (setq denote-last-keywords
          (cond
           ((null choice)
            "")
           ((= (length choice) 1)
            (car choice))
           ((if denote-sort-keywords
                (sort choice #'string-lessp)
              choice))))))

(defun denote--keywords-combine (keywords)
  "Format KEYWORDS output of `denote--keywords-prompt'."
  (if (and (> (length keywords) 1)
           (not (stringp keywords)))
      (mapconcat #'downcase keywords "_")
    keywords))

(defun denote--keywords-add-to-history (keywords)
  "Append KEYWORDS to `denote--keyword-history'."
  (if-let ((listed (listp keywords))
           (length (length keywords)))
      (cond
       ((and listed (= length 1))
        (car keywords))
       ((and listed (> length 1))
        (mapc (lambda (kw)
                (add-to-history 'denote--keyword-history kw))
              (delete-dups keywords))))
    (add-to-history 'denote--keyword-history keywords)))

;;;; New note

(defun denote--file-extension ()
  "Return file type extension based on `denote-file-type'."
  (pcase denote-file-type
    ('markdown-toml ".md")
    ('markdown-yaml ".md")
    ('text ".txt")
    (_ ".org")))

(defun denote--format-file (path id keywords slug extension)
  "Format file name.
PATH, ID, KEYWORDS, SLUG are expected to be supplied by `denote'
or equivalent: they will all be converted into a single string.
EXTENSION is the file type extension, either a string which
include the starting dot or the return value of
`denote--file-extension'."
  (let ((kws (if denote-infer-keywords
                 (denote--keywords-combine keywords)
               keywords))
        (ext (or extension (denote--file-extension))))
    (format "%s%s--%s__%s%s" path id slug kws ext)))

(defun denote--map-quote-downcase (seq)
  "Quote and downcase elements in SEQ."
  (mapconcat (lambda (k)
               (format "%S" (downcase k)))
             seq ", "))

(defun denote--file-meta-keywords (keywords &optional type)
  "Prepare KEYWORDS for inclusion in the file's front matter.
Parse the output of `denote--keywords-prompt', using `downcase'
on the keywords and separating them by two spaces.  A single
keyword is just downcased.

With optional TYPE, format the keywords accordingly (this might
be `toml' or, in the future, some other spec that needss special
treatment)."
  (let ((kw (denote--sluggify-keywords keywords)))
    (cond
     ((and (> (length kw) 1) (not (stringp kw)))
      (pcase type
        ('toml (format "[%s]" (denote--map-quote-downcase kw)))
        (_ (mapconcat #'downcase kw "  "))))
     (t
      (pcase type
        ('toml (format "[%S]" (downcase kw)))
        (_ (downcase kw)))))))

(defvar denote-toml-front-matter
  "+++
title      = %S
date       = %s
tags       = %s
identifier = %S
+++\n\n"
  "TOML front matter value for `format'.
Read `denote-org-front-matter' for the technicalities.")

(defvar denote-yaml-front-matter
  "---
title:      %S
date:       %s
tags:       %s
identifier: %S
---\n\n"
  "YAML front matter value for `format'.
Read `denote-org-front-matter' for the technicalities.")

(defvar denote-text-front-matter
  "title:      %s
date:       %s
tags:       %s
identifier: %s
%s\n\n"
  "Plain text front matter value for `format'.
Read `denote-org-front-matter' for the technicalities of the
first four specifiers this variable accepts.  The fifth specifier
this specific to this variable: it expect a delimiter:
`denote-text-front-matter-delimiter'.")

(defvar denote-text-front-matter-delimiter (make-string 27 ?-)
  "Final delimiter for plain text front matter.")

(defvar denote-org-front-matter
  "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
\n"
  "Org front matter value for `format'.
The order of the arguments is TITLE, DATE, KEYWORDS, ID.  If you
are an avdanced user who wants to edit this variable to affect
how front matter is produced, consider using something like %2$s
to control where Nth argument is placed.")

(defun denote--file-meta-header (title date keywords id)
  "Front matter for new notes.

TITLE, DATE, KEYWORDS, FILENAME, ID are all strings which are
 provided by `denote'."
  (let ((kw-space (denote--file-meta-keywords keywords))
        (kw-toml (denote--file-meta-keywords keywords 'toml)))
    (pcase denote-file-type
      ('markdown-toml (format denote-toml-front-matter title date kw-toml id))
      ('markdown-yaml (format denote-yaml-front-matter title date kw-space id))
      ('text (format denote-text-front-matter title date kw-space id denote-text-front-matter-delimiter))
      (_ (format denote-org-front-matter title date kw-space id)))))

(defun denote--path (title keywords &optional dir id)
  "Return path to new file with TITLE and KEYWORDS.
With optional DIR, use it instead of variable `denote-directory'.
With optional ID, use it else format the current time."
  (setq denote-last-path
        (denote--format-file
         (or dir (file-name-as-directory denote-directory))
         (or id (format-time-string denote--id-format))
         (denote--sluggify-keywords keywords)
         (denote--sluggify title)
         (denote--file-extension))))

;; Adapted from `org-hugo--org-date-time-to-rfc3339' in the `ox-hugo'
;; package: <https://github.com/kaushalmodi/ox-hugo>.
(defun denote--date-rfc3339 ()
  "Format date using the RFC3339 specification."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z")))

(defun denote--date-org-timestamp ()
  "Format date using the Org inactive timestamp notation."
  (format-time-string "[%F %a %R]"))

(defun denote--date-iso-8601 ()
  "Format date according to ISO 8601 standard."
  (format-time-string "%F"))

(defun denote--date ()
  "Expand the date for a new note's front matter."
  (let ((format denote-front-matter-date-format))
    (cond
     ((stringp format)
      (format-time-string format))
     ((or (eq denote-file-type 'markdown-toml)
          (eq denote-file-type 'markdown-yaml))
      (denote--date-rfc3339))
     ((eq format 'org-timestamp)
      (denote--date-org-timestamp))
     (t (denote--date-iso-8601)))))

(defun denote--prepare-note (title keywords &optional path)
  "Use TITLE and KEYWORDS to prepare new note file.
Use optional PATH, else create it with `denote--path'."
  (let* ((p (or path (denote--path title keywords)))
         (default-directory denote-directory)
         (buffer (unless path (find-file p)))
         (header (denote--file-meta-header
                  title (denote--date) keywords
                  (format-time-string denote--id-format))))
    (unless path
      (with-current-buffer buffer (insert header))
      (setq denote-last-buffer buffer))
    (setq denote-last-front-matter header)))

(defvar denote--title-history nil
  "Minibuffer history of `denote--title-prompt'.")

(defun denote--title-prompt ()
  "Read file title for `denote'."
  (setq denote-last-title
        (read-string "File title: " nil 'denote--title-history)))

;;;###autoload
(defun denote (title keywords)
  "Create new note with the appropriate metadata and file name.

This command first prompts for a file TITLE and then for one or
more KEYWORDS (separated by the `crm-separator', typically a
comma).  The latter supports completion though any arbitrary
string can be inserted.

Completion candidates are those of `denote-known-keywords'.  If
`denote-infer-keywords' is non-nil, then keywords in existing
file names are also provided as candidates.

When `denote-sort-keywords' is non-nil, keywords are sorted
alphabetically in both the file name and file contents."
  (interactive
   (list
    (denote--title-prompt)
    (denote--keywords-prompt)))
  (denote--prepare-note title keywords)
  (denote--keywords-add-to-history keywords))

(defvar denote--file-type-history nil
  "Minibuffer history of `denote--file-type-prompt'.")

(defun denote--file-type-prompt ()
  "Prompt for `denote-file-type'.
Note that a non-nil value other than `text', `markdown-yaml', and
`markdown-toml' falls back to an Org file type.  We use `org'
here for clarity."
  (completing-read
   "Select file type: " '(org markdown-yaml markdown-toml text) nil t
   nil 'denote--file-type-history))

(defun denote--file-type-symbol (filetype)
  "Return FILETYPE as a symbol."
  (cond
   ((stringp filetype)
    (intern filetype))
   ((symbolp filetype)
    filetype)
   (t (user-error "`%s' is not a symbol or string" filetype))))

;;;###autoload
(defun denote-type (filetype)
  "Like `denote' but with FILETYPE for `denote-file-type'.
In practice, this command lets you create, say, a Markdown file
even when your default is Org.

When called from Lisp the FILETYPE must be a symbol."
  (interactive (list (denote--file-type-prompt)))
  (let ((denote-file-type (denote--file-type-symbol filetype)))
    (call-interactively #'denote)))

(provide 'denote)
;;; denote.el ends here
