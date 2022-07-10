;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 0.2.1
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
;; Denote aims to be a simple-to-use, focused-in-scope, and effective
;; note-taking tool for Emacs.  The manual describes all the
;; technicalities about the file-naming scheme, points of entry to
;; creating new notes, commands to check links between notes, and more:
;; <https://protesilaos.com/emacs/denote>.  If you have the info manual
;; available, evaluate:
;;
;;    (info "(denote) Top")
;;
;; What follows is a general overview of its core core design
;; principles:
;;
;; * Predictability :: File names must follow a consistent and
;;   descriptive naming convention (see the manual's "The file-naming
;;   scheme").  The file name alone should offer a clear indication of
;;   what the contents are, without reference to any other metadatum.
;;   This convention is not specific to note-taking, as it is pertinent
;;   to any form of file that is part of the user's long-term storage
;;   (see the manual's "Renaming files").
;;
;; * Composability :: Be a good Emacs citizen, by integrating with other
;;   packages or built-in functionality instead of re-inventing
;;   functions such as for filtering or greping.  The author of Denote
;;   (Protesilaos, aka "Prot") writes ordinary notes in plain text
;;   (`.txt'), switching on demand to an Org file only when its expanded
;;   set of functionality is required for the task at hand (see the
;;   manual's "Points of entry").
;;
;; * Portability :: Notes are plain text and should remain portable.
;;   The way Denote writes file names, the front matter it includes in
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
;;   exclusive sets of keywords.  Denote also does not check if the user
;;   writes thematically atomic notes.  It is up to the user to apply
;;   the requisite rigor and/or creativity in pursuit of their preferred
;;   workflow (see the manual's "Writing metanotes").
;;
;; * Hackability :: Denote's code base consists of small and reusable
;;   functions.  They all have documentation strings.  The idea is to
;;   make it easier for users of varying levels of expertise to
;;   understand what is going on and make surgical interventions where
;;   necessary (e.g. to tweak some formatting).  In this manual, we
;;   provide concrete examples on such user-level configurations (see
;;   the manual's "Keep a journal or diary").
;;
;; Now the important part...  "Denote" is the familiar word, though it
;; also is a play on the "note" concept.  Plus, we can come up with
;; acronyms, recursive or otherwise, of increasingly dubious utility
;; like:
;;
;; + Don't Ever Note Only The Epiphenomenal
;; + Denote Everything Neatly; Omit The Excesses
;;
;; But we'll let you get back to work.  Don't Eschew or Neglect your
;; Obligations, Tasks, and Engagements.

;;; Code:

(require 'seq)
(eval-when-compile (require 'cl-lib))

(defgroup denote ()
  "Simple notes with an efficient file-naming scheme."
  :group 'files)

;;;; User options

;; About the autoload: (info "(elisp) File Local Variables")

;;;###autoload (put 'denote-directory 'safe-local-variable (lambda (val) (or (eq val 'local) (eq val 'default-directory))))
(defcustom denote-directory (expand-file-name "~/Documents/notes/")
  "Directory for storing personal notes.

A safe local value of either `default-directory' or `local' can
be added as a value in a .dir-local.el file.  Do this if you
intend to use multiple directory silos for your notes while still
relying on a global value (which is the value of this variable).
The Denote manual has a sample (search for '.dir-locals.el').
Those silos do not communicate with each other: they remain
separate.

The local value influences where commands such as `denote' will
place the newly created note.  If the command is called from a
directory or file where the local value exists, then that value
take precedence, otherwise the global value is used.

If you intend to reference this variable in Lisp, consider using
the function `denote-directory' instead: it returns the path as a
directory and also checks if a safe local value should be used."
  :group 'denote
  :safe (lambda (val) (or (eq val 'local) (eq val 'default-directory)))
  :type 'directory)

(defcustom denote-known-keywords
  '("emacs" "philosophy" "politics" "economics")
  "List of strings with predefined keywords for `denote'.
Also see user options: `denote-allow-multi-word-keywords',
`denote-infer-keywords', `denote-sort-keywords'."
  :group 'denote
  :type '(repeat string))

(defcustom denote-infer-keywords t
  "Whether to infer keywords from existing notes' file names.

When non-nil, search the file names of existing notes in the
variable `denote-directory' for their keyword field and extract
the entries as \"inferred keywords\".  These are combined with
`denote-known-keywords' and are presented as completion
candidates while using `denote' and related commands
interactively.

If nil, refrain from inferring keywords.  The aforementioned
completion prompt only shows the `denote-known-keywords'.  Use
this if you want to enforce a restricted vocabulary.

Inferred keywords are specific to the value of the variable
`denote-directory'.  If a silo with a local value is used, as
explained in that variable's doc string, the inferred keywords
are specific to the given silo.

For advanced Lisp usage, the function `denote-keywords' returns
the appropriate list of strings."
  :group 'denote
  :type 'boolean)

(defconst denote--prompt-symbols
  '(title keywords date file-type subdirectory)
  "List of symbols representing `denote' prompts.")

(defcustom denote-prompts '(title keywords)
  "Specify the prompts of the `denote' command.

The value is a list of symbols, which includes any of the following:

- `title': Prompt for the title of the new note.  If unspecified, the
  title is left empty.

- `keywords': Prompts with completion for the keywords of the new
  note.  Available candidates are those specified in the user
  option `denote-known-keywords'.  If the user option
  `denote-infer-keywords' is non-nil, keywords in existing note
  file names are included in the list of candidates.  The
  `keywords' prompt uses `completing-read-multiple', meaning that
  it can accept multiple keywords, separated by a comma (or
  whatever the value of `crm-sepator' is).

- `date': Prompts for the date of the new note.  It will expect a
  date like 2022-06-16 or a date plus time: 2022-06-16 14:30.
  Without the `date' prompt, the `denote' command uses the
  `current-time'.

- `file-type': Prompts with completion for the filetype of the
  new note.  Available candidates are those specified in the user
  option `denote-file-type'.  Without this prompt, `denote' uses
  the value of `denote-file-type'.

- `subdirectory': Prompts with completion for a subdirectory in
  which to create the note.  Available candidates are the value
  of the user option `denote-directory' and all of its
  subdirectories.  Any subdirectory must already exist: Denote
  will not create it.

The prompts will happen in the given order.

If the value of this user option is nil, no prompts are used.
The resulting file name will consist of an identifier (i.e. the
date and time) and a supported file type extension (per
`denote-file-type').

Recall that Denote's standard file-naming scheme is as
follows (read the manual for the technicalities):

    DATE--TITLE__KEYWORDS.EXT

If either or both of the `title' and `keywords' prompts are not
included in the value of this variable, file names will be any of
those permutations:

    DATE.EXT
    DATE--TITLE.EXT
    DATE__KEYWORDS.EXT

When in doubt, always include the `title' and `keywords' prompts."
  :group 'denote
  :type '(radio (const :tag "Use no prompts" nil)
                (set :tag "Available prompts" :greedy t
                     (const :tag "Title" title)
                     (const :tag "Keywords" keywords)
                     (const :tag "Date" date)
                     (const :tag "File type extension" file-type)
                     (const :tag "Subdirectory" subdirectory))))

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

(defcustom denote-date-format nil
  "Date format in the front matter (file header) of new notes.

When nil (the default value), use a file-type-specific
format (also check `denote-file-type'):

- For Org, an inactive timestamp is used, such as [2022-06-30 Wed
  15:31].

- For Markdown, the RFC3339 standard is applied:
  2022-06-30T15:48:00+03:00.

- For plain text, the format is that of ISO 8601: 2022-06-30.

If the value is a string, ignore the above and use it instead.
The string must include format specifiers for the date.  These
are described in the doc string of `format-time-string'."
  :type '(choice
          (const :tag "Use appropiate format for each file type" nil)
          (string :tag "Custom format for `format-time-string'"))
  :group 'denote)

(make-obsolete 'denote-front-matter-date-format 'denote-date-format "0.2.0")

;;;; Main variables

(defconst denote--id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.")

(defconst denote--id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote--id-format'.")

(defconst denote--title-regexp "--\\([0-9A-Za-z-]*\\)"
  "Regular expression to match keywords.")

(defconst denote--keywords-regexp "__\\([0-9A-Za-z_-]*\\)"
  "Regular expression to match keywords.")

(defconst denote--extension-regexp "\\.\\(org\\|md\\|txt\\)"
  "Regular expression to match supported Denote extensions.")

(defconst denote--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”/]*"
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
  (let* ((val (or (buffer-local-value 'denote-directory (current-buffer))
                  denote-directory))
         (path (if (or (eq val 'default-directory) (eq val 'local)) default-directory val)))
    (unless (file-directory-p path)
      (make-directory path t))
    (file-name-as-directory path)))

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
  (downcase (denote--slug-hyphenate (denote--slug-no-punct str))))

(defun denote--sluggify-and-join (str)
  "Sluggify STR while joining separate words."
  (downcase
   (replace-regexp-in-string
    "-" ""
    (denote--slug-hyphenate (denote--slug-no-punct str)))))

(defun denote--sluggify-keywords (keywords)
  "Sluggify KEYWORDS."
  (let ((kws (if (listp keywords) keywords (list keywords))))
    (mapcar (if denote-allow-multi-word-keywords
                #'denote--sluggify
              #'denote--sluggify-and-join)
            kws)))

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun denote--only-note-p (file)
  "Make sure FILE is an actual Denote note.
FILE is relative to the variable `denote-directory'."
  (let ((file-name (file-name-nondirectory file)))
    (and (not (file-directory-p file))
         (file-regular-p file)
         (string-match-p (concat "\\`" denote--id-regexp
                                 ".*" denote--extension-regexp "\\'")
                         file-name)
         (not (string-match-p "[#~]\\'" file)))))

(defun denote--file-name-relative-to-denote-directory (file)
  "Return file name of FILE relative to the variable `denote-directory'.
FILE must be an absolute path."
  (if-let* ((dir (denote-directory))
            ((file-name-absolute-p file))
            ((string-prefix-p dir file)))
      (substring-no-properties file (length dir))
    file))

(defun denote--current-file-is-note-p ()
  "Return non-nil if current file likely is a Denote note."
  (and (or (string-match-p denote--id-regexp (buffer-file-name))
           (string-match-p denote--id-regexp (buffer-name)))
       (string-prefix-p (denote-directory) (expand-file-name default-directory))))

;;;; Keywords

(defun denote--directory-files-recursively (directory)
  "Return expanded files in DIRECTORY recursively."
  (mapcar
   (lambda (s) (expand-file-name s))
   (seq-remove
    (lambda (f)
      (not (denote--only-note-p f)))
    (directory-files-recursively directory directory-files-no-dot-files-regexp t))))

(defun denote--directory-files (&optional absolute)
  "List note files.
If optional ABSOLUTE, show full paths, else only show base file
names that are relative to the variable `denote-directory'."
  (let* ((default-directory (denote-directory))
         (files (denote--directory-files-recursively default-directory)))
    (if absolute
        files
      (mapcar
       (lambda (s) (denote--file-name-relative-to-denote-directory s))
       files))))

(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))

(defun denote--get-note-path-by-id (id)
  "Return the absolute path of ID note in variable `denote-directory'."
  (cl-find-if
   (lambda (f)
     (string-prefix-p id (file-name-nondirectory f)))
   (denote--directory-files :absolute)))

(defun denote--directory-files-matching-regexp (regexp &optional no-check-current)
  "Return list of files matching REGEXP.
With optional NO-CHECK-CURRENT do not test if the current file is
part of the list."
  (delq
   nil
   (mapcar
    (lambda (f)
      (when (and (denote--only-note-p f)
                 (string-match-p regexp f)
                 (or no-check-current
                     (not (string= (file-name-nondirectory (buffer-file-name)) f))))
        f))
    (denote--directory-files))))

(defun denote--extract-keywords-from-path (path)
  "Extract keywords from PATH."
  (let* ((file-name (file-name-nondirectory path))
         (kws (when (string-match denote--keywords-regexp file-name)
                (match-string-no-properties 1 file-name))))
    (when kws
      (split-string kws "_"))))

(defun denote--inferred-keywords ()
  "Extract keywords from `denote--directory-files'."
  (delete-dups
   (mapcan (lambda (p)
             (denote--extract-keywords-from-path p))
           (denote--directory-files))))

(defun denote-keywords ()
  "Return appropriate list of keyword candidates.
If `denote-infer-keywords' is non-nil, infer keywords from
existing notes and combine them into a list with
`denote-known-keywords'.  Else use only the latter."
  (delete-dups
   (if denote-infer-keywords
       (append (denote--inferred-keywords) denote-known-keywords)
     denote-known-keywords)))

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
          (if denote-sort-keywords
              (sort choice #'string-lessp)
            choice))))

(defun denote--keywords-combine (keywords)
  "Format KEYWORDS output of `denote--keywords-prompt'."
  (mapconcat #'downcase keywords "_"))

(defun denote--keywords-add-to-history (keywords)
  "Append KEYWORDS to `denote--keyword-history'."
  (mapc (lambda (kw)
          (add-to-history 'denote--keyword-history kw))
        (delete-dups keywords)))

;;;; New note

;;;;; Common helpers for new notes

(defun denote--file-extension ()
  "Return file type extension based on `denote-file-type'."
  (pcase denote-file-type
    ('markdown-toml ".md")
    ('markdown-yaml ".md")
    ('text ".txt")
    (_ ".org")))

(defun denote--format-file (path id keywords title-slug extension)
  "Format file name.
PATH, ID, KEYWORDS, TITLE-SLUG are expected to be supplied by
`denote' or equivalent: they will all be converted into a single
string.  EXTENSION is the file type extension, either a string
which include the starting dot or the return value of
`denote--file-extension'."
  (let ((kws (denote--keywords-combine keywords))
        (ext (or extension (denote--file-extension)))
        (empty-title (string-empty-p title-slug)))
    (cond
     ((and keywords title-slug (not empty-title))
      (format "%s%s--%s__%s%s" path id title-slug kws ext))
     ((and keywords empty-title)
      (format "%s%s__%s%s" path id kws ext))
     ((and title-slug (not empty-title))
      (format "%s%s--%s%s" path id title-slug ext))
     (t
      (format "%s%s%s" path id ext)))))

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
    (pcase type
      ('toml (format "[%s]" (denote--map-quote-downcase kw)))
      (_ (mapconcat #'downcase kw "  ")))))

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
is specific to this variable: it expect a delimiter such as
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
to control where Nth argument is placed.

Make sure to

1. Not use empty lines inside the front matter block.

2. Insert at least one empty line after the front matter block
and do not use any empty line before it.

These help ensure consistency and might prove useful if we need
to operate on the front matter as a whole.")

(defun denote--file-meta-header (title date keywords id &optional filetype)
  "Front matter for new notes.

TITLE, DATE, KEYWORDS, FILENAME, ID are all strings which are
 provided by `denote'.

Optional FILETYPE is one of the values of `denote-file-type',
else that variable is used."
  (let ((kw-space (denote--file-meta-keywords keywords))
        (kw-toml (denote--file-meta-keywords keywords 'toml)))
    (pcase (or filetype denote-file-type)
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
         (or dir (file-name-as-directory (denote-directory)))
         (or id (format-time-string denote--id-format))
         (denote--sluggify-keywords keywords)
         (denote--sluggify title)
         (denote--file-extension))))

;; Adapted from `org-hugo--org-date-time-to-rfc3339' in the `ox-hugo'
;; package: <https://github.com/kaushalmodi/ox-hugo>.
(defun denote--date-rfc3339 (&optional date)
  "Format date using the RFC3339 specification.
With optional DATE, use it else use the current one."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" date)))

(defun denote--date-org-timestamp (&optional date)
  "Format date using the Org inactive timestamp notation.
With optional DATE, use it else use the current one."
  (format-time-string "[%F %a %R]" date))

(defun denote--date-iso-8601 (&optional date)
  "Format date according to ISO 8601 standard.
With optional DATE, use it else use the current one."
  (format-time-string "%F" date))

(defun denote--date (&optional date)
  "Expand the date for a new note's front matter.
With optional DATE, use it else use the current one."
  (let ((format denote-date-format))
    (cond
     ((stringp format)
      (format-time-string format date))
     ((or (eq denote-file-type 'markdown-toml)
          (eq denote-file-type 'markdown-yaml))
      (denote--date-rfc3339 date))
     ((eq denote-file-type 'text)
      (denote--date-iso-8601 date))
     (t
      (denote--date-org-timestamp date)))))

(defun denote--prepare-note (title keywords &optional path date id)
  "Use TITLE and KEYWORDS to prepare new note file.
Use optional PATH, else create it with `denote--path'.  When PATH
is provided, refrain from writing to a buffer (useful for org
capture).

Optional DATE is passed to `denote--date', while optional ID is
used to construct the path's identifier."
  (let* ((default-directory (denote-directory))
         (p (or path (denote--path title keywords default-directory id)))
         (buffer (unless path (find-file p)))
         (header (denote--file-meta-header
                  title (denote--date date) keywords
                  (format-time-string denote--id-format date))))
    (unless path
      (with-current-buffer buffer (insert header))
      (setq denote-last-buffer buffer))
    (setq denote-last-front-matter header)))

(defvar denote--title-history nil
  "Minibuffer history of `denote--title-prompt'.")

(defun denote--title-prompt (&optional default-title)
  "Read file title for `denote'.

Optional DEFAULT-TITLE is used as the default value."
  (let ((format (if default-title
                    (format "File title [%s]: " default-title)
                  "File title: ")))
    (setq denote-last-title
          (read-string format nil 'denote--title-history default-title))))

;;;;; The `denote' command

;;;###autoload
(defun denote (&optional title keywords type date subdir)
  "Create a new note with the appropriate metadata and file name.

When called interactively, the metadata and file name are prompted
according to the value of `denote-prompts'.

When called from Lisp, all arguments are optional.

- TITLE is a string or a function returning a string.  If nil, an
  empty string is used.

- KEYWORDS is a list of strings.  The list can be empty or the
  value can be set to nil.

- TYPE is a symbol among those described in `denote-file-type'.

- DATE is a string representing a date like 2022-06-30 or a date
  and time like 2022-06-16 14:30.  A nil value or an empty string
  is interpreted as the `current-time'.

- SUBDIR is a string representing the path to either the value of
  the variable `denote-directory' or a subdirectory thereof.  The
  subdirectory must exist: Denote will not create it."
  (interactive)
  (when (called-interactively-p 'any)
    (dolist (prompt denote-prompts)
      (pcase prompt
        ('title (setq title (denote--title-prompt)))
        ('date (setq date (denote--date-prompt)))
        ('file-type (setq type (denote--file-type-prompt)))
        ('subdirectory (setq subdir (denote--subdirs-prompt)))
        ('keywords (setq keywords (denote--keywords-prompt))))))
  (let* ((denote-file-type (denote--file-type-symbol (or type denote-file-type)))
         (date (if (or (null date) (string-empty-p date))
                   (current-time)
                 (denote--valid-date date)))
         (id (format-time-string denote--id-format date))
         (denote-directory (or subdir (denote-directory))))
    (denote--barf-duplicate-id id)
    (denote--prepare-note (or title "") keywords nil date id)
    (denote--keywords-add-to-history keywords)))

(defalias 'denote-create-note (symbol-function 'denote))

;;;;; The `denote-type' command

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
(defun denote-type ()
  "Create note while prompting for a file type.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\'(file-type title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(file-type title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-type (symbol-function 'denote-type))

;;;;; The `denote-date' command

(defvar denote--date-history nil
  "Minibuffer history of `denote--date-prompt'.")

(defun denote--date-prompt ()
  "Prompt for date."
  (read-string
   "DATE and TIME for note (e.g. 2022-06-16 14:30): "
   nil 'denote--date-history))

(defun denote--valid-date (date)
  "Return DATE if parsed by `date-to-time', else signal error."
  (date-to-time date))

;; This should only be relevant for `denote-date', otherwise the
;; identifier is always unique (we trust that no-one writes multiple
;; notes within fractions of a second).
(defun denote--id-exists-p (identifier no-check-current)
  "Return non-nil if IDENTIFIER already exists.
NO-CHECK-CURRENT passes the appropriate flag to
`denote--directory-files-matching-regexp'."
  (denote--directory-files-matching-regexp identifier no-check-current))

(defun denote--barf-duplicate-id (identifier)
  "Throw a user-error if IDENTIFIER already exists else return t."
  (if (denote--id-exists-p identifier :no-check-current)
      (user-error "`%s' already exists; aborting new note creation" identifier)
    t))

;;;###autoload
(defun denote-date ()
  "Create note while prompting for a date.

The date can be in YEAR-MONTH-DAY notation like 2022-06-30 or
that plus the time: 2022-06-16 14:30

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\'(date title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(date title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-date (symbol-function 'denote-date))

;;;;; The `denote-subdirectory' command

(defvar denote--subdir-history nil
  "Minibuffer history of `denote-subdirectory'.")

(defun denote--subdirs ()
  "Return list of subdirectories in variable `denote-directory'."
  (seq-remove
   (lambda (filename)
     ;; TODO 2022-07-03: Generalise for all VC backends.  Which ones?
     ;;
     ;; TODO 2022-07-03: Maybe it makes sense to also allow the user to
     ;; specify a blocklist of directories that should always be
     ;; excluded?
     (or (string-match-p "\\.git" filename)
         (not (file-directory-p filename))))
   (directory-files-recursively (denote-directory) ".*" t t)))

(defun denote--subdirs-completion-table (dirs)
  "Match DIRS as a completion table."
  (let* ((def (car denote--subdir-history))
         (table (denote--completion-table 'file dirs))
         (prompt (if def
                     (format "Select subdirectory [%s]: " def)
                   "Select subdirectory: ")))
    (completing-read prompt table nil t nil 'denote--subdir-history def)))

(defun denote--subdirs-prompt ()
  "Handle user input on choice of subdirectory."
  (let* ((root (directory-file-name (denote-directory)))
         (subdirs (denote--subdirs))
         (dirs (push root subdirs)))
    (denote--subdirs-completion-table dirs)))

;;;###autoload
(defun denote-subdirectory ()
  "Create note while prompting for a subdirectory.

Available candidates include the value of the variable
`denote-directory' and any subdirectory thereof.

This is equivalent to calling `denote' when `denote-prompts' is set to
\\'(subdirectory title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(subdirectory title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-in-subdirectory (symbol-function 'denote-subdirectory))

(provide 'denote)
;;; denote.el ends here
