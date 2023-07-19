;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 1.2.0
;; Package-Requires: ((emacs "28.1"))

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

;; Denote aims to be a simple-to-use, focused-in-scope, and effective
;; note-taking and file-naming tool for Emacs.
;;
;; Denote is based on the idea that files should follow a predictable
;; and descriptive file-naming scheme.  The file name must offer a
;; clear indication of what the contents are about, without reference
;; to any other metadata.  Denote basically streamlines the creation
;; of such files or file names while providing facilities to link
;; between them (where those files are editable).
;;
;; Denote's file-naming scheme is not limited to "notes".  It can be used
;; for all types of file, including those that are not editable in Emacs,
;; such as videos.  Naming files in a constistent way makes their
;; filtering and retrieval considerably easier.  Denote provides relevant
;; facilities to rename files, regardless of file type.
;;
;; The manual describes all the technicalities about the file-naming
;; scheme, points of entry to creating new notes, commands to check
;; links between notes, and more: ;; <https://protesilaos.com/emacs/denote>.
;; If you have the info manual available, evaluate:
;;
;;    (info "(denote) Top")
;;
;; What follows is a general overview of its core core design
;; principles (again: please read the manual for the technicalities):
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
(require 'xref)
(require 'dired)
(eval-when-compile (require 'subr-x))

(defgroup denote ()
  "Simple notes with an efficient file-naming scheme."
  :group 'files
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

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
  :package-version '(denote . "2.0.0")
  :link '(info-link "(denote) Maintain separate directories for notes")
  :type 'directory)

(defcustom denote-known-keywords
  '("emacs" "philosophy" "politics" "economics")
  "List of strings with predefined keywords for `denote'.
Also see user options: `denote-allow-multi-word-keywords',
`denote-infer-keywords', `denote-sort-keywords'."
  :group 'denote
  :package-version '(denote . "0.1.0")
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

The user option `denote-excluded-keywords-regexp' can be used to
exclude keywords that match a regular expression.

Inferred keywords are specific to the value of the variable
`denote-directory'.  If a silo with a local value is used, as
explained in that variable's doc string, the inferred keywords
are specific to the given silo.

For advanced Lisp usage, the function `denote-keywords' returns
the appropriate list of strings."
  :group 'denote
  :package-version '(denote . "0.1.0")
  :type 'boolean)

(defcustom denote-prompts '(title keywords)
  "Specify the prompts of the `denote' command for interactive use.

The value is a list of symbols, which includes any of the following:

- `title': Prompt for the title of the new note.

- `keywords': Prompts with completion for the keywords of the new
  note.  Available candidates are those specified in the user
  option `denote-known-keywords'.  If the user option
  `denote-infer-keywords' is non-nil, keywords in existing note
  file names are included in the list of candidates.  The
  `keywords' prompt uses `completing-read-multiple', meaning that
  it can accept multiple keywords separated by a comma (or
  whatever the value of `crm-separator' is).

- `file-type': Prompts with completion for the file type of the
  new note.  Available candidates are those specified in the user
  option `denote-file-type'.  Without this prompt, `denote' uses
  the value of `denote-file-type'.

- `subdirectory': Prompts with completion for a subdirectory in
  which to create the note.  Available candidates are the value
  of the user option `denote-directory' and all of its
  subdirectories.  Any subdirectory must already exist: Denote
  will not create it.

- `date': Prompts for the date of the new note.  It will expect
  an input like 2022-06-16 or a date plus time: 2022-06-16 14:30.
  Without the `date' prompt, the `denote' command uses the
  `current-time'.  (To leverage the more sophisticated Org
  method, see the `denote-date-prompt-use-org-read-date'.)

- `template': Prompts for a KEY among `denote-templates'.  The
  value of that KEY is used to populate the new note with
  content, which is added after the front matter.

- `signature': Prompts for an arbitrary string that can be used
  to establish a sequential relationship between files (e.g. 1,
  1a, 1b, 1b1, 1b2, ...).  Signatures have no strictly defined
  function and are up to the user to apply as they see fit.  One
  use-case is to implement Niklas Luhmann's Zettelkasten system
  for a sequence of notes (Folgezettel).  Signatures are not
  included in a file's front matter and are not shown in the
  description of a link.  They are reserved solely for creating a
  sequence in a file listing, at least for the time being.

The prompts occur in the given order.

If the value of this user option is nil, no prompts are used.
The resulting file name will consist of an identifier (i.e. the
date and time) and a supported file type extension (per
`denote-file-type').

Recall that Denote's standard file-naming scheme is defined as
follows (read the manual for the technicalities):

    DATE--TITLE__KEYWORDS.EXT

Depending on the inclusion of the `title', `keywords', and
`signature' prompts, file names will be any of those
permutations:

    DATE.EXT
    DATE--TITLE.EXT
    DATE__KEYWORDS.EXT
    DATE==SIGNATURE.EXT
    DATE==SIGNATURE--TITLE.EXT
    DATE==SIGNATURE--TITLE__KEYWORDS.EXT
    DATE==SIGNATURE__KEYWORDS.EXT

When in doubt, always include the `title' and `keywords'
prompts (the default style).

Finally, this user option only affects the interactive use of the
`denote' command (advanced users can call it from Lisp).  For
ad-hoc interactive actions that do not change the default
behaviour of the `denote' command, users can invoke these
convenience commands: `denote-type', `denote-subdirectory',
`denote-date', `denote-template', `denote-signature'."
  :group 'denote
  :package-version '(denote . "2.0.0")
  :link '(info-link "(denote) The denote-prompts option")
  :type '(radio (const :tag "Use no prompts" nil)
                (set :tag "Available prompts" :greedy t
                     (const :tag "Title" title)
                     (const :tag "Keywords" keywords)
                     (const :tag "Date" date)
                     (const :tag "File type extension" file-type)
                     (const :tag "Subdirectory" subdirectory)
                     (const :tag "Template" template)
                     (const :tag "Signature" signature))))

(defcustom denote-sort-keywords t
  "Whether to sort keywords in new files.

When non-nil, the keywords of `denote' are sorted with
`string-lessp' regardless of the order they were inserted at the
minibuffer prompt.

If nil, show the keywords in their given order."
  :group 'denote
  :package-version '(denote . "0.1.0")
  :type 'boolean)

(defcustom denote-allow-multi-word-keywords nil
  "If non-nil keywords can consist of multiple words.
Words are automatically separated by a hyphen when using the
`denote' command or related.  The hyphen is the only legal
character---no spaces, no other characters.  If, for example, the
user types <word1_word2> or <word1 word2>, it is converted to
<word1-word2>.

When nil (the default), do not allow keywords to consist of
multiple words.  Reduce them to a single word, such as by turning
<word1_word2> or <word1 word2> into <word1word2>.

[ The author of Denote encourages you to use single words for
  keywords and, if needed, rely on multiple separate keywords to
  derive meaning.]"
  :group 'denote
  :package-version '(denote . "2.0.0")
  :type 'boolean)

(defcustom denote-file-type nil
  "The file type extension for new notes.

By default (a nil value), the file type is that of Org mode.
Though the `org' symbol can be specified for the same effect.

When the value is the symbol `markdown-yaml', the file type is
that of Markdown mode and the front matter uses YAML notation.
Similarly, `markdown-toml' is Markdown but has TOML syntax in the
front matter.

When the value is `text', the file type is that of Text mode.

Any other non-nil value is the same as the default.

NOTE: expert users can change the supported file types by leaving
the value of this user option to nil and directly editing the
value of `denote-file-types'.  That variable, which is not a user
option, controls the behaviour of all file-type-aware
functions (creating notes, renaming them, inserting front matter,
formatting a link, etc.).  Consult its documentation for the
technicalities."
  :type '(choice
          (const :tag "Unspecified (defaults to Org)" nil)
          (const :tag "Org mode (default)" org)
          (const :tag "Markdown (YAML front matter)" markdown-yaml)
          (const :tag "Markdown (TOML front matter)" markdown-toml)
          (const :tag "Plain text" text))
  :package-version '(denote . "0.6.0")
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
  :package-version '(denote . "0.2.0")
  :group 'denote)

(defcustom denote-date-prompt-use-org-read-date nil
  "Whether to use `org-read-date' in date prompts.

If non-nil, use `org-read-date'.  If nil, input the date as a
string, as described in `denote'.

This option is relevant when `denote-prompts' includes a `date'
and/or when the user invokes the command `denote-date'."
  :group 'denote
  :package-version '(denote . "0.6.0")
  :type 'boolean)

(defcustom denote-templates nil
  "Alist of content templates for new notes.
A template is arbitrary text that Denote will add to a newly
created note right below the front matter.

Templates are expressed as a (KEY . STRING) association.

- The KEY is the name which identifies the template.  It is an
  arbitrary symbol, such as `report', `memo', `statement'.

- The STRING is ordinary text that Denote will insert as-is.  It
  can contain newline characters to add spacing.  The manual of
  Denote contains examples on how to use the `concat' function,
  beside writing a generic string.

The user can choose a template either by invoking the command
`denote-template' or by changing the user option `denote-prompts'
to always prompt for a template when calling the `denote'
command."
  :type '(alist :key-type symbol :value-type string)
  :package-version '(denote . "0.5.0")
  :link '(info-link "(denote) The denote-templates option")
  :group 'denote)

(defcustom denote-backlinks-show-context nil
  "When non-nil, show link context in the backlinks buffer.

The context is the line a link to the current note is found in.
The context includes multiple links to the same note, if those
are present.

When nil, only show a simple list of file names that link to the
current note."
  :group 'denote
  :package-version '(denote . "1.2.0")
  :type 'boolean)

(make-obsolete-variable 'denote-link-fontify-backlinks 'denote-backlinks-show-context "1.2.0")

(defcustom denote-excluded-directories-regexp nil
  "Regular expression of directories to exclude from all operations.
Omit matching directories from file prompts and also exclude them
from all functions that check the contents of the variable
`denote-directory'.  The regexp needs to match only the name of
the directory, not its full path.

File prompts are used by several commands, such as `denote-link'
and `denote-subdirectory'.

Functions that check for files include `denote-directory-files'
and `denote-directory-subdirectories'.

The match is performed with `string-match-p'."
  :group 'denote
  :package-version '(denote . "1.2.0")
  :type 'string)

(defcustom denote-excluded-keywords-regexp nil
  "Regular expression of keywords to not infer.
Keywords are inferred from file names and provided at relevant
prompts as completion candidates when the user option
`denote-infer-keywords' is non-nil.

The match is performed with `string-match-p'."
  :group 'denote
  :package-version '(denote . "1.2.0")
  :type 'string)

;;;; Main variables

;; For character classes, evaluate: (info "(elisp) Char Classes")
(define-obsolete-variable-alias
  'denote--id-format
  'denote-id-format
  "1.0.0")

(defconst denote-id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.
The note's ID is derived from the date and time of its creation.")

(define-obsolete-variable-alias
  'denote--id-regexp
  'denote-id-regexp
  "1.0.0")

(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote-id-format'.")

(defconst denote-signature-regexp "==\\([[:alnum:][:nonascii:]=]*\\)"
  "Regular expression to match the SIGNATURE field in a file name.")

(define-obsolete-variable-alias
  'denote--title-regexp
  'denote-title-regexp
  "1.0.0")

(defconst denote-title-regexp "--\\([[:alnum:][:nonascii:]-]*\\)"
  "Regular expression to match the TITLE field in a file name.")

(define-obsolete-variable-alias
  'denote--keywords-regexp
  'denote-keywords-regexp
  "1.0.0")

(defconst denote-keywords-regexp "__\\([[:alnum:][:nonascii:]_-]*\\)"
  "Regular expression to match the KEYWORDS field in a file name.")

(define-obsolete-variable-alias
  'denote--punctuation-regexp
  'denote-excluded-punctuation-regexp
  "1.0.0")

(defconst denote-excluded-punctuation-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*"
  "Punctionation that is removed from file names.
We consider those characters illegal for our purposes.")

(define-obsolete-variable-alias
  'denote-punctuation-excluded-extra-regexp
  'denote-excluded-punctuation-extra-regexp
  "1.0.0")

(defvar denote-excluded-punctuation-extra-regexp nil
  "Additional punctuation that is removed from file names.
This variable is for advanced users who need to extend the
`denote-excluded-punctuation-regexp'.  Once we have a better
understanding of what we should be omitting, we will update
things accordingly.")

;;;; File helper functions

(defun denote--completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defun denote--default-directory-is-silo-p ()
  "Return path to silo if `default-directory' is a silo."
  (when-let ((dir-locals (dir-locals-find-file default-directory))
             ((alist-get 'denote-directory dir-local-variables-alist)))
    (cond
     ((listp dir-locals)
      (car dir-locals))
     ((stringp dir-locals)
      dir-locals)
     (t nil))))

(defun denote--make-denote-directory ()
  "Make the variable `denote-directory' and its parents, if needed."
  (when (and (stringp denote-directory)
             (not (file-directory-p denote-directory)))
    (make-directory denote-directory :parents)))

(defvar denote-user-enforced-denote-directory nil
  "Value of the variable `denote-directory'.
Use this to `let' bind a directory path, thus overriding what the
function `denote-directory' ordinarily returns.")

(defun denote-directory ()
  "Return path of variable `denote-directory' as a proper directory.
Custom Lisp code can `let' bind the value of the variable
`denote-user-enforced-denote-directory' to override what this
function returns.

Otherwise, the order of precedence is to first check for a silo
before falling back to the value of the variable
`denote-directory'."
  (let ((path (or denote-user-enforced-denote-directory
                  (denote--default-directory-is-silo-p)
                  (denote--make-denote-directory)
                  (default-value 'denote-directory))))
    (file-name-as-directory (expand-file-name path))))

(defun denote--slug-no-punct (str)
  "Remove punctuation from STR.
Concretely, replace with spaces anything that matches the
`denote-excluded-punctuation-regexp' and
`denote-excluded-punctuation-extra-regexp'."
  (replace-regexp-in-string
   (concat denote-excluded-punctuation-regexp
           denote-excluded-punctuation-extra-regexp)
   "" str))

(defun denote--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun denote-sluggify (str)
  "Make STR an appropriate slug for file names and related."
  (downcase (denote--slug-hyphenate (denote--slug-no-punct str))))

(define-obsolete-function-alias
  'denote--sluggify
  'denote-sluggify
  "1.0.0")

(defun denote--slug-put-equals (str)
  "Replace spaces and underscores with equals signs in STR.
Also replace multiple equals signs with a single one and remove
any leading and trailing signs."
  (replace-regexp-in-string
   "^=\\|=$" ""
   (replace-regexp-in-string
    "=\\{2,\\}" "="
    (replace-regexp-in-string "_\\|\s+" "=" str))))

(defun denote-sluggify-signature (str)
  "Make STR an appropriate slug for signatures."
  (downcase (denote--slug-put-equals (denote--slug-no-punct str))))

(defun denote-sluggify-and-join (str)
  "Sluggify STR while joining separate words."
  (downcase
   (replace-regexp-in-string
    "-" ""
    (denote--slug-hyphenate (denote--slug-no-punct str)))))

(define-obsolete-function-alias
  'denote--sluggify-and-join
  'denote-sluggify-and-join
  "1.0.0")

(defun denote-sluggify-keywords (keywords)
  "Sluggify KEYWORDS, which is a list of strings."
  (if (listp keywords)
    (mapcar
     (if denote-allow-multi-word-keywords
         #'denote-sluggify
       #'denote-sluggify-and-join)
     keywords)
    (error "`%s' is not a list" keywords)))

(define-obsolete-function-alias
  'denote--sluggify-keywords
  'denote-sluggify-keywords
  "1.0.0")

;; TODO 2023-05-22: Review name of `denote-desluggify' to signify what
;; the doc string warns about.
(defun denote-desluggify (str)
  "Upcase first char in STR and dehyphenate STR, inverting `denote-sluggify'.
The intent of this function is to be used on individual strings,
such as the TITLE component of a Denote file name, but not on the
entire file name.  Put differently, it does not work with
signatures and keywords."
  (let ((str (replace-regexp-in-string "-" " " str)))
    (aset str 0 (upcase (aref str 0)))
    str))

(define-obsolete-function-alias
  'denote--desluggify
  'denote-desluggify
  "1.0.0")

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun denote-file-is-note-p (file)
  "Return non-nil if FILE is an actual Denote note.
For our purposes, a note must not be a directory, must satisfy
`file-regular-p', its path must be part of the variable
`denote-directory', it must have a Denote identifier in its name,
and use one of the extensions implied by `denote-file-type'."
  (let ((file-name (file-name-nondirectory file)))
    (and (not (file-directory-p file))
         (file-regular-p file)
         (string-prefix-p (denote-directory) (expand-file-name file))
         (string-match-p (concat "\\`" denote-id-regexp) file-name)
         (denote-file-has-supported-extension-p file))))

(define-obsolete-function-alias
  'denote--only-note-p
  'denote-file-is-note-p
  "1.0.0")

(defun denote-file-has-identifier-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (when file
    (string-match-p (concat "\\`" denote-id-regexp)
                    (file-name-nondirectory file))))

(define-obsolete-function-alias
  'denote--file-has-identifier-p
  'denote-file-has-identifier-p
  "1.0.0")

(defun denote-file-has-signature-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (when file
    (string-match-p denote-signature-regexp
                    (file-name-nondirectory file))))

(make-obsolete 'denote-file-directory-p nil "2.0.0")

(defun denote-file-has-supported-extension-p (file)
  "Return non-nil if FILE has supported extension.
Also account for the possibility of an added .gpg suffix.
Supported extensions are those implied by `denote-file-type'."
  (seq-some (lambda (e)
              (string-suffix-p e file))
            (denote-file-type-extensions-with-encryption)))

(define-obsolete-function-alias
  'denote--file-supported-extension-p
  'denote-file-has-supported-extension-p
  "1.0.0")

(defun denote--file-regular-writable-p (file)
  "Return non-nil if FILE is regular and writable."
  (and (file-regular-p file)
       (file-writable-p file)))

(defun denote-file-is-writable-and-supported-p (file)
  "Return non-nil if FILE is writable and has supported extension."
  (and (denote--file-regular-writable-p file)
       (denote-file-has-supported-extension-p file)))

(define-obsolete-function-alias
  'denote--writable-and-supported-p
  'denote-file-is-writable-and-supported-p
  "1.0.0")

(defun denote-get-file-name-relative-to-denote-directory (file)
  "Return name of FILE relative to the variable `denote-directory'.
FILE must be an absolute path."
  (when-let* ((dir (denote-directory))
              ((file-name-absolute-p file))
              (file-name (expand-file-name file))
              ((string-prefix-p dir file-name)))
    (substring-no-properties file-name (length dir))))

(define-obsolete-function-alias
  'denote--file-name-relative-to-denote-directory
  'denote-get-file-name-relative-to-denote-directory
  "1.0.0")

(defun denote-extract-id-from-string (string)
  "Return existing Denote identifier in STRING, else nil."
  (when (string-match denote-id-regexp string)
    (match-string 0 string)))

(define-obsolete-function-alias
  'denote-link--id-from-string
  'denote-extract-id-from-string
  "1.0.0")

;; TODO 2022-09-26: Maybe we can consolidate this with
;; `denote--dir-in-denote-directory-p'?  Another check for the
;; directory prefix is done in `denote-file-is-note-p'.
(defun denote--default-dir-has-denote-prefix ()
  "Test `default-directory' for variable `denote-directory' prefix."
  (string-prefix-p (denote-directory)
                   (expand-file-name default-directory)))

(defun denote--exclude-directory-regexp-p (file)
  "Return non-nil if FILE matches `denote-excluded-directories-regexp'."
  (and denote-excluded-directories-regexp
       (string-match-p denote-excluded-directories-regexp file)))

(defun denote--directory-all-files-recursively ()
  "Return list of all files in variable `denote-directory'.
Avoids traversing dotfiles (unconditionally) and whatever matches
`denote-excluded-directories-regexp'."
  (directory-files-recursively
   (denote-directory)
   directory-files-no-dot-files-regexp
   :include-directories
   (lambda (f)
     (cond
      ((string-match-p "\\`\\." f) nil)
      ((string-match-p "/\\." f) nil)
      ((denote--exclude-directory-regexp-p f) nil)
      ((file-readable-p f))
      (t)))
   :follow-symlinks))

(defun denote-directory-files ()
  "Return list of absolute file paths in variable `denote-directory'.

Files only need to have an identifier.  The return value may thus
include file types that are not implied by `denote-file-type'.
To limit the return value to text files, use the function
`denote-directory-text-only-files'.

Remember that the variable `denote-directory' accepts a dir-local
value, as explained in its doc string."
  (mapcar
   #'expand-file-name
   (seq-remove
    (lambda (f)
      (not (denote-file-has-identifier-p f)))
    (denote--directory-all-files-recursively))))

(defun denote-directory-text-only-files ()
  "Return list of text files in variable `denote-directory'.
Filter `denote-directory-files' using `denote-file-is-note-p'."
  (seq-filter #'denote-file-is-note-p (denote-directory-files)))

(define-obsolete-function-alias
  'denote--directory-files
  'denote-directory-files
  "1.0.0")

(defun denote-directory-subdirectories ()
  "Return list of subdirectories in variable `denote-directory'.
Omit dotfiles (such as .git) unconditionally.  Also exclude
whatever matches `denote-excluded-directories-regexp'."
  (seq-remove
   (lambda (filename)
     (let ((rel (denote-get-file-name-relative-to-denote-directory filename)))
       (or (not (file-directory-p filename))
           (string-match-p "\\`\\." rel)
           (string-match-p "/\\." rel)
           (denote--exclude-directory-regexp-p rel))))
   (denote--directory-all-files-recursively)))

(define-obsolete-function-alias
  'denote--subdirs
  'denote-directory-subdirectories
  "1.0.0")

(defun denote-get-path-by-id (id)
  "Return absolute path of ID string in `denote-directory-files'."
  (let ((files
         (seq-filter
          (lambda (file)
            (and (denote-file-has-identifier-p file)
                 (string-prefix-p id (file-name-nondirectory file))))
          (denote-directory-files))))
    (if (length< files 2)
        (car files)
      (seq-find
       (lambda (file)
         (let ((file-extension (file-name-extension file :period)))
           (and (denote-file-is-note-p file)
                (or (string= (denote--file-extension denote-file-type)
                             file-extension)
                    (string= ".org" file-extension)
                    (member file-extension (denote-file-type-extensions))))))
       files))))

(define-obsolete-function-alias
  'denote--get-note-path-by-id
  'denote-get-path-by-id
  "1.0.0")

(defun denote-get-relative-path-by-id (id &optional directory)
  "Return relative path of ID string in `denote-directory-files'.
The path is relative to DIRECTORY (default: ‘default-directory’)."
  (file-relative-name (denote-get-path-by-id id) directory))

(defun denote-directory-files-matching-regexp (regexp)
  "Return list of files matching REGEXP in `denote-directory-files'."
  (seq-filter
   (lambda (f)
     (string-match-p regexp (denote-get-file-name-relative-to-denote-directory f)))
   (denote-directory-files)))

(define-obsolete-function-alias
  'denote--directory-files-matching-regexp
  'denote-directory-files-matching-regexp
  "1.0.0")

(defun denote-all-files ()
  "Return the list of Denote files in variable `denote-directory'."
  (let* ((project-find-functions #'denote-project-find)
         (project (project-current nil (denote-directory)))
         (dirs (list (project-root project))))
    (project-files project dirs)))

(defvar denote--file-history nil
  "Minibuffer history of `denote-file-prompt'.")

(defun denote-file-prompt (&optional initial-text)
  "Prompt for file with identifier in variable `denote-directory'.
With optional INITIAL-TEXT, use it to prepopulate the minibuffer."
  (let* ((all-files (denote-all-files))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (when all-files
      (funcall project-read-file-name-function
               "Select note: " all-files nil 'denote--file-history initial-text))))

(define-obsolete-function-alias
  'denote--retrieve-read-file-prompt
  'denote-file-prompt
  "1.0.0")

;;;; Keywords

(defun denote-extract-keywords-from-path (path)
  "Extract keywords from PATH and return them as a list of strings.
PATH must be a Denote-style file name where keywords are prefixed
with an underscore.

If PATH has no such keywords, return nil."
  (let* ((file-name (file-name-nondirectory path))
         (kws (when (string-match denote-keywords-regexp file-name)
                (match-string-no-properties 1 file-name))))
    (when kws
      (split-string kws "_"))))

(define-obsolete-function-alias
  'denote--extract-keywords-from-path
  'denote-extract-keywords-from-path
  "1.0.0")

(defun denote--inferred-keywords ()
  "Extract keywords from `denote-directory-files'.
This function returns duplicates.  The `denote-keywords' is the
one that doesn't."
  (let ((kw (mapcan #'denote-extract-keywords-from-path (denote-directory-files))))
    (if-let ((regexp denote-excluded-keywords-regexp))
        (seq-filter (lambda (k) (not (string-match-p regexp k))) kw)
      kw)))

(defun denote-keywords ()
  "Return appropriate list of keyword candidates.
If `denote-infer-keywords' is non-nil, infer keywords from
existing notes and combine them into a list with
`denote-known-keywords'.  Else use only the latter.

Inferred keywords are filtered by the user option
`denote-excluded-keywords-regexp'."
  (delete-dups
   (if denote-infer-keywords
       (append (denote--inferred-keywords) denote-known-keywords)
     denote-known-keywords)))

(defvar denote--keyword-history nil
  "Minibuffer history of inputted keywords.")

(defun denote--keywords-crm (keywords &optional prompt)
  "Use `completing-read-multiple' for KEYWORDS.
With optional PROMPT, use it instead of a generic text for file
keywords."
  (delete-dups
   (completing-read-multiple
    (or prompt "File keyword: ") keywords
    nil nil nil 'denote--keyword-history)))

(defun denote-keywords-prompt ()
  "Prompt for one or more keywords.
In the case of multiple entries, those are separated by the
`crm-sepator', which typically is a comma.  In such a case, the
output is sorted with `string-lessp'.

Process the return value with `denote-keywords-sort'."
  (denote-keywords-sort (denote--keywords-crm (denote-keywords))))

(defun denote-keywords-sort (keywords)
  "Sort KEYWORDS if `denote-sort-keywords' is non-nil.
KEYWORDS is a list of strings, per `denote-keywords-prompt'."
  (if denote-sort-keywords
      (sort keywords #'string-lessp)
    keywords))

(define-obsolete-function-alias
  'denote--keywords-prompt
  'denote-keywords-prompt
  "1.0.0")

(defun denote--keywords-combine (keywords)
  "Format KEYWORDS output of `denote-keywords-prompt'."
  (mapconcat #'downcase keywords "_"))

(defun denote--keywords-add-to-history (keywords)
  "Append KEYWORDS to `denote--keyword-history'."
  (mapc (lambda (kw)
          (add-to-history 'denote--keyword-history kw))
        (delete-dups keywords)))

;;;; File types

(defvar denote-org-front-matter
  "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
\n"
  "Org front matter.
It is passed to `format' with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node `(denote)
Change the front matter format'.")

(defvar denote-yaml-front-matter
  "---
title:      %s
date:       %s
tags:       %s
identifier: %S
---\n\n"
  "YAML (Markdown) front matter.
It is passed to `format' with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node `(denote)
Change the front matter format'.")

(defvar denote-toml-front-matter
  "+++
title      = %s
date       = %s
tags       = %s
identifier = %S
+++\n\n"
  "TOML (Markdown) front matter.
It is passed to `format' with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node `(denote)
Change the front matter format'.")

(defvar denote-text-front-matter
  "title:      %s
date:       %s
tags:       %s
identifier: %s
---------------------------\n\n"
  "Plain text front matter.
It is passed to `format' with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node `(denote)
Change the front matter format'.")

(defun denote-surround-with-quotes (s)
  "Surround string S with quotes.
This can be used in `denote-file-types' to format front mattter."
  (format "%S" s))

(defun denote-trim-whitespace (s)
  "Trim whitespace around string S.
This can be used in `denote-file-types' to format front mattter."
  (if (string-blank-p s)
      ""
    (let ((trims "[ \t\n\r]+"))
      (string-trim s trims trims))))

(defun denote--trim-quotes (s)
  "Trim quotes around string S."
  (let ((trims "[\"']+"))
    (string-trim s trims trims)))

(defun denote-trim-whitespace-then-quotes (s)
  "Trim whitespace then quotes around string S.
This can be used in `denote-file-types' to format front mattter."
  (if (string-blank-p s)
      ""
    (denote--trim-quotes (denote-trim-whitespace s))))

(defun denote-format-keywords-for-md-front-matter (keywords)
  "Format front matter KEYWORDS for markdown file type.
KEYWORDS is a list of strings.  Consult the `denote-file-types'
for how this is used."
  (format "[%s]" (mapconcat (lambda (k) (format "%S" k)) keywords ", ")))

(defun denote-format-keywords-for-text-front-matter (keywords)
  "Format front matter KEYWORDS for text file type.
KEYWORDS is a list of strings.  Consult the `denote-file-types'
for how this is used."
  (string-join keywords "  "))

(defun denote-format-keywords-for-org-front-matter (keywords)
  "Format front matter KEYWORDS for org file type.
KEYWORDS is a list of strings.  Consult the `denote-file-types'
for how this is used."
  (if keywords
      (format ":%s:" (string-join keywords ":"))
    ""))

(defun denote-extract-keywords-from-front-matter (keywords-string)
  "Extract keywords list from front matter KEYWORDS-STRING.
Split KEYWORDS-STRING into a list of strings.  If KEYWORDS-STRING
satisfies `string-blank-p', return an empty string.

Consult the `denote-file-types' for how this is used."
  (if (string-blank-p keywords-string)
      ""
    (split-string keywords-string "[:,\s]+" t "[][ \"']+")))

(defvar denote-file-types
  '((org
     :extension ".org"
     :date-function denote-date-org-timestamp
     :front-matter denote-org-front-matter
     :title-key-regexp "^#\\+title\\s-*:"
     :title-value-function identity
     :title-value-reverse-function denote-trim-whitespace
     :keywords-key-regexp "^#\\+filetags\\s-*:"
     :keywords-value-function denote-format-keywords-for-org-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :link denote-org-link-format
     :link-in-context-regexp denote-org-link-in-context-regexp)
    (markdown-yaml
     :extension ".md"
     :date-function denote-date-rfc3339
     :front-matter denote-yaml-front-matter
     :title-key-regexp "^title\\s-*:"
     :title-value-function denote-surround-with-quotes
     :title-value-reverse-function denote-trim-whitespace-then-quotes
     :keywords-key-regexp "^tags\\s-*:"
     :keywords-value-function denote-format-keywords-for-md-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :link denote-md-link-format
     :link-in-context-regexp denote-md-link-in-context-regexp)
    (markdown-toml
     :extension ".md"
     :date-function denote-date-rfc3339
     :front-matter denote-toml-front-matter
     :title-key-regexp "^title\\s-*="
     :title-value-function denote-surround-with-quotes
     :title-value-reverse-function denote-trim-whitespace-then-quotes
     :keywords-key-regexp "^tags\\s-*="
     :keywords-value-function denote-format-keywords-for-md-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :link denote-md-link-format
     :link-in-context-regexp denote-md-link-in-context-regexp)
    (text
     :extension ".txt"
     :date-function denote-date-iso-8601
     :front-matter denote-text-front-matter
     :title-key-regexp "^title\\s-*:"
     :title-value-function identity
     :title-value-reverse-function denote-trim-whitespace
     :keywords-key-regexp "^tags\\s-*:"
     :keywords-value-function denote-format-keywords-for-text-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :link denote-org-link-format
     :link-in-context-regexp denote-org-link-in-context-regexp))
  "Alist of `denote-file-type' and their format properties.

Each element is of the form (SYMBOL PROPERTY-LIST).  SYMBOL is
one of those specified in `denote-file-type' or an arbitrary
symbol that defines a new file type.

PROPERTY-LIST is a plist that consists of the following elements:

- `:extension' is a string with the file extension including the
  period.

- `:date-function' is a function that can format a date.  See the
  functions `denote-date-iso-8601', `denote-date-rfc3339', and
  `denote-date-org-timestamp'.

- `:front-matter' is either a string passed to `format' or a
  variable holding such a string.  The `format' function accepts
  four arguments, which come from `denote' in this order: TITLE,
  DATE, KEYWORDS, IDENTIFIER.  Read the doc string of `format' on
  how to reorder arguments.

- `:title-key-regexp' is a regular expression that is used to
  retrieve the title line in a file.  The first line matching
  this regexp is considered the title line.

- `:title-value-function' is the function used to format the raw
  title string for inclusion in the front matter (e.g. to
  surround it with quotes).  Use the `identity' function if no
  further processing is required.

- `:title-value-reverse-function' is the function used to
  retrieve the raw title string from the front matter.  It
  performs the reverse of `:title-value-function'.

- `:keywords-key-regexp' is a regular expression used to retrieve
  the keywords' line in the file.  The first line matching this
  regexp is considered the keywords' line.

- `:keywords-value-function' is the function used to format the
  keywords' list of strings as a single string, with appropriate
  delimiters, for inclusion in the front matter.

- `:keywords-value-reverse-function' is the function used to
  retrieve the keywords' value from the front matter.  It
  performs the reverse of the `:keywords-value-function'.

- `:link' is a string, or variable holding a string, that
  specifies the format of a link.  See the variables
  `denote-org-link-format', `denote-md-link-format'.

- `:link-in-context-regexp' is a regular expression that is used
  to match the aforementioned link format.  See the variables
  `denote-org-link-in-context-regexp',`denote-md-link-in-context-regexp'.

If `denote-file-type' is nil, use the first element of this list
for new note creation.  The default is `org'.")

(defun denote--date-format-function (file-type)
  "Return date format function of FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :date-function))

(defun denote--file-extension (file-type)
  "Return file type extension based on FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :extension))

(defun denote--front-matter (file-type)
  "Return front matter based on FILE-TYPE."
  (let ((prop (plist-get
               (alist-get file-type denote-file-types)
               :front-matter)))
    (if (symbolp prop)
        (symbol-value prop)
      prop)))

(defun denote--title-key-regexp (file-type)
  "Return the title key regexp associated to FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :title-key-regexp))

(defun denote--title-value-function (file-type)
  "Convert title string to a front matter title, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :title-value-function))

(defun denote--title-value-reverse-function (file-type)
  "Convert front matter title to the title string, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :title-value-reverse-function))

(defun denote--keywords-key-regexp (file-type)
  "Return the keywords key regexp associated to FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :keywords-key-regexp))

(defun denote--keywords-value-function (file-type)
  "Convert keywords' string to front matter keywords, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :keywords-value-function))

(defun denote--keywords-value-reverse-function (file-type)
  "Convert front matter keywords to keywords' list, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :keywords-value-reverse-function))

(defun denote--link-format (file-type)
  "Return link format extension based on FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :link))

(defun denote--link-in-context-regexp (file-type)
  "Return link regexp in context based on FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :link-in-context-regexp))

(define-obsolete-function-alias
  'denote--extensions
  'denote-file-type-extensions
  "2.0.0")

(defun denote-file-type-extensions ()
  "Return all file type extensions in `denote-file-types'."
  (delete-dups
   (mapcar (lambda (type)
             (plist-get (cdr type) :extension))
           denote-file-types)))

(define-obsolete-variable-alias
  'denote--encryption-file-extensions
  'denote-encryption-file-extensions
  "2.0.0")

;; TODO 2023-01-24: Perhaps there is a good reason to make this a user
;; option, but I am keeping it as a generic variable for now.
(defvar denote-encryption-file-extensions '(".gpg" ".age")
  "List of strings specifying file extensions for encryption.")

(define-obsolete-function-alias
  'denote--extensions-with-encryption
  'denote-file-type-extensions-with-encryption
  "2.0.0")

(defun denote-file-type-extensions-with-encryption ()
  "Derive `denote-file-type-extensions' plus `denote-encryption-file-extensions'."
  (let ((file-extensions (denote-file-type-extensions))
        all)
    (dolist (ext file-extensions)
      (dolist (enc denote-encryption-file-extensions)
        (push (concat ext enc) all)))
    (append file-extensions all)))

(defun denote--file-type-keys ()
  "Return all `denote-file-types' keys."
  (delete-dups (mapcar #'car denote-file-types)))

(defun denote--get-title-line-from-front-matter (title file-type)
  "Retrieve title line from front matter based on FILE-TYPE.
Format TITLE in the title line.  The returned line does not
contain the newline."
  (let ((front-matter (denote--format-front-matter title "" nil "" file-type))
        (key-regexp (denote--title-key-regexp file-type)))
    (with-temp-buffer
      (insert front-matter)
      (goto-char (point-min))
      (when (re-search-forward key-regexp nil t 1)
        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun denote--get-keywords-line-from-front-matter (keywords file-type)
  "Retrieve keywords line from front matter based on FILE-TYPE.
Format KEYWORDS in the keywords line.  The returned line does not
contain the newline."
  (let ((front-matter (denote--format-front-matter "" "" keywords "" file-type))
        (key-regexp (denote--keywords-key-regexp file-type)))
    (with-temp-buffer
      (insert front-matter)
      (goto-char (point-min))
      (when (re-search-forward key-regexp nil t 1)
        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

;;;; Front matter or content retrieval functions

(defun denote-retrieve-filename-identifier (file)
  "Extract identifier from FILE name.
To return an existing identifier or create a new one, refer to
the function `denote-retrieve-or-create-file-identifier'."
  (if (denote-file-has-identifier-p file)
      (progn
        (string-match denote-id-regexp file)
        (match-string 0 file))
    (error "Cannot find `%s' as a file with a Denote identifier" file)))

(define-obsolete-function-alias
  'denote--retrieve-filename-identifier
  'denote-retrieve-filename-identifier
  "1.0.0")

(defun denote-retrieve-or-create-file-identifier (file &optional date files)
  "Return FILE identifier, generating one if appropriate.

The conditions are as follows:

- If FILE has an identifier, return it.

- If FILE does not have an identifier and optional DATE is
  non-nil, invoke `denote-prompt-for-date-return-id'.

- If FILE does not have an identifier and DATE is nil, use the
  file attributes to determine the last modified date and format
  it as an identifier.

- As a fallback, derive an identifier from the current time.

With optional FILES as a list of file names, test that the
identifier is unique among them.

With optional FILES as non-nil, test that the identifier is
unique among all files and buffers in variable
`denote-directory'.

To only return an existing identifier, refer to the function
`denote-retrieve-filename-identifier'."
  (let ((id
         (cond
          ((string-match denote-id-regexp file)
           (substring file (match-beginning 0) (match-end 0)))
          (date (denote-prompt-for-date-return-id))
          ((denote--file-attributes-time file))
          (t (format-time-string denote-id-format)))))
    (cond
     ((and files (listp files))
      (denote--return-new-identifier-if-duplicate id files))
     (files
      (denote--return-new-identifier-if-duplicate id))
     (t
      id))))

(define-obsolete-function-alias
  'denote--file-name-id
  'denote-retrieve-or-create-file-identifier
  "1.0.0")

(defun denote-retrieve-filename-signature (file)
  "Extract signature from FILE name, if present, else return nil."
  (when (denote-file-has-signature-p file)
    (string-match denote-signature-regexp file)
    (match-string 1 file)))

(defun denote-retrieve-filename-title (file)
  "Extract title from FILE name, else return `file-name-base'.
Run `denote-desluggify' on title if the extraction is sucessful."
  (if-let* (((file-exists-p file))
            ((denote-file-has-identifier-p file))
            ((string-match denote-title-regexp file))
            (title (match-string 1 file)))
      (denote-desluggify title)
    (file-name-base file)))

(define-obsolete-function-alias
  'denote--retrieve-filename-title
  'denote-retrieve-filename-title
  "1.0.0")

(defun denote-retrieve-title-value (file file-type)
  "Return title value from FILE front matter per FILE-TYPE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
      (funcall (denote--title-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(define-obsolete-function-alias
  'denote--retrieve-title-value
  'denote-retrieve-title-value
  "1.0.0")

(defun denote-retrieve-title-line (file file-type)
  "Return title line from FILE front matter per FILE-TYPE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(define-obsolete-function-alias
  'denote--retrieve-title-line
  'denote-retrieve-title-line
  "1.0.0")

(defun denote-retrieve-keywords-value (file file-type)
  "Return keywords value from FILE front matter per FILE-TYPE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
      (funcall (denote--keywords-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(define-obsolete-function-alias
  'denote--retrieve-keywords-value
  'denote-retrieve-keywords-value
  "1.0.0")

(defun denote-retrieve-keywords-line (file file-type)
  "Return keywords line from FILE front matter per FILE-TYPE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(define-obsolete-function-alias
  'denote--retrieve-keywords-line
  'denote-retrieve-keywords-line
  "1.0.0")

(defun denote--retrieve-title-or-filename (file type)
  "Return appropriate title for FILE given its TYPE."
  (if-let (((denote-file-is-note-p file))
           (title (denote-retrieve-title-value file type))
           ((not (string-blank-p title))))
      title
    (denote-retrieve-filename-title file)))

(defun denote--retrieve-location-in-xrefs (identifier)
  "Return list of xrefs for IDENTIFIER with their respective location.
Limit the search to text files, per `denote-directory-text-only-files'."
  (mapcar #'xref-match-item-location
          (xref-matches-in-files identifier
                                 (denote-directory-text-only-files))))

(defun denote--retrieve-group-in-xrefs (identifier)
  "Access location of xrefs for IDENTIFIER and group them per file.
See `denote--retrieve-locations-in-xrefs'."
  (mapcar #'xref-location-group
          (denote--retrieve-location-in-xrefs identifier)))

(defun denote--retrieve-files-in-xrefs (identifier)
  "Return sorted, deduplicated file names with IDENTIFIER in their contents."
  (sort
   (delete-dups
    (denote--retrieve-group-in-xrefs identifier))
   #'string-lessp))

;;;; New note

;;;;; Common helpers for new notes

(defun denote-format-file-name (path id keywords title-slug extension &optional signature)
  "Format file name.
PATH, ID, KEYWORDS, TITLE-SLUG, EXTENSION and optional SIGNATURE
are expected to be supplied by `denote' or equivalent command."
  (let ((kws (denote--keywords-combine keywords))
        (file-name (concat path id)))
    (when (and signature (not (string-empty-p signature)))
      (setq file-name (concat file-name "==" signature)))
    (when (and title-slug (not (string-empty-p title-slug)))
      (setq file-name (concat file-name "--" title-slug)))
    (when (and keywords (not (string-blank-p kws)))
      (setq file-name (concat file-name "__" kws)))
    (concat file-name extension)))

(define-obsolete-function-alias
  'denote--format-file
  'denote-format-file-name
  "1.0.0")

(defun denote--format-front-matter-title (title file-type)
  "Format TITLE according to FILE-TYPE for the file's front matter."
  (funcall (denote--title-value-function file-type) title))

(defun denote--format-front-matter-keywords (keywords file-type)
  "Format KEYWORDS according to FILE-TYPE for the file's front matter.
Apply `downcase' to KEYWORDS."
  (let ((kw (mapcar #'downcase (denote-sluggify-keywords keywords))))
    (funcall (denote--keywords-value-function file-type) kw)))

(make-obsolete-variable 'denote-text-front-matter-delimiter nil "0.6.0")

(defun denote--format-front-matter (title date keywords id filetype)
  "Front matter for new notes.

TITLE, DATE, and ID are all strings or functions that return a
string.  KEYWORDS is a list of strings.  FILETYPE is one of the
values of `denote-file-type'."
  (let* ((fm (denote--front-matter filetype))
         (title (denote--format-front-matter-title title filetype))
         (kws (denote--format-front-matter-keywords keywords filetype)))
    (if fm (format fm title date kws id) "")))

(defun denote--path (title keywords dir id file-type &optional signature)
  "Return path to new file.
Use ID, TITLE, KEYWORDS, FILE-TYPE and optional SIGNATURE to
construct path to DIR."
  (denote-format-file-name
   dir id
   (denote-sluggify-keywords keywords)
   (denote-sluggify title)
   (denote--file-extension file-type)
   (when signature
     (denote-sluggify-signature signature))))

;; Adapted from `org-hugo--org-date-time-to-rfc3339' in the `ox-hugo'
;; package: <https://github.com/kaushalmodi/ox-hugo>.
(defun denote-date-rfc3339 (date)
  "Format DATE using the RFC3339 specification."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" date)))

(define-obsolete-function-alias
  'denote--date-rfc3339
  'denote-date-rfc3339
  "1.2.0")

(defun denote-date-org-timestamp (date)
  "Format DATE using the Org inactive timestamp notation."
  (format-time-string "[%F %a %R]" date))

(define-obsolete-function-alias
  'denote--date-org-timestamp
  'denote-date-org-timestamp
  "1.2.0")

(defun denote-date-iso-8601 (date)
  "Format DATE according to ISO 8601 standard."
  (format-time-string "%F" date))

(define-obsolete-function-alias
  'denote--date-iso-8601
  'denote-date-iso-8601
  "1.2.0")

(defun denote--date (date file-type)
  "Expand DATE in an appropriate format for FILE-TYPE."
  (let ((format denote-date-format))
    (cond
     ((stringp format)
      (format-time-string format date))
     ((when-let ((fn (denote--date-format-function file-type)))
        (funcall fn date)))
     (t
      (denote-date-org-timestamp date)))))

(defun denote--prepare-note (title keywords date id directory file-type template &optional signature)
  "Prepare a new note file.

Arguments TITLE, KEYWORDS, DATE, ID, DIRECTORY, FILE-TYPE,
TEMPLATE, and optional SIGNATURE should be valid for note
creation."
  (let* ((path (denote--path title keywords directory id file-type signature))
         (buffer (find-file path))
         (header (denote--format-front-matter
                  title (denote--date date file-type) keywords
                  (format-time-string denote-id-format date)
                  file-type)))
    (with-current-buffer buffer
      (insert header)
      (insert template))))

(defun denote--dir-in-denote-directory-p (directory)
  "Return DIRECTORY if in variable `denote-directory', else nil."
  (when (and directory
             (string-prefix-p (denote-directory)
                              (expand-file-name directory)))
    directory))

(defun denote--valid-file-type (filetype)
  "Return a valid filetype given the argument FILETYPE.
If none is found, the first element of `denote-file-types' is
returned."
  (unless (or (symbolp filetype) (stringp filetype))
    (user-error "`%s' is not a symbol or string" filetype))
  (when (stringp filetype)
    (setq filetype (intern filetype)))
  (if (memq filetype (mapcar 'car denote-file-types))
      filetype
    (caar denote-file-types)))

(defun denote--date-add-current-time (date)
  "Add current time to DATE, if necessary.
The idea is to turn 2020-01-15 into 2020-01-15 16:19 so that the
hour and minute component is not left to 00:00.

This reduces the burden on the user who would otherwise need to
input that value in order to avoid the error of duplicate
identifiers.

It also addresses a difference between Emacs 28 and Emacs 29
where the former does not read dates without a time component."
  (if (<= (length date) 10)
      (format "%s %s" date (format-time-string "%H:%M:%S" (current-time)))
    date))

(defun denote--valid-date (date)
  "Return DATE if parsed by `date-to-time', else signal error."
  (let ((datetime (denote--date-add-current-time date)))
    (date-to-time datetime)))

(defun denote--buffer-file-names ()
  "Return file names of Denote buffers."
  (delq nil
        (mapcar
         (lambda (buffer)
           (when-let (((buffer-live-p buffer))
                      (file (buffer-file-name buffer))
                      ((denote-file-is-note-p file)))
             file))
         (buffer-list))))

;; In normal usage, this should only be relevant for `denote-date',
;; otherwise the identifier is always unique (we trust that no-one
;; writes multiple notes within fractions of a second).  Though the
;; `denote' command does call `denote-barf-duplicate-id'.
(defun denote--id-exists-p (identifier &optional files)
  "Return non-nil if IDENTIFIER already exists.
With optional FILES, check for IDENTIFIER among them.  Else refer
to files or buffers in the variable `denote-directory'."
  (seq-some
   (lambda (file)
     (string-prefix-p identifier (file-name-nondirectory file)))
   (or files
       (append (denote-directory-files) (denote--buffer-file-names)))))

(defun denote--increment-identifier (identifier)
  "Increment IDENTIFIER.
Preserve the date component and append to it the current time."
  (let* ((datetime (split-string identifier "T"))
         (date (car datetime)))
    (concat date "T" (format-time-string "%H%M%S"))))

(defun denote--return-new-identifier-if-duplicate (identifier &optional files)
  "Return new unique identifier if IDENTIFIER already exists.
The meaning of FILES is the same as in `denote--id-exists-p'."
  (while (denote--id-exists-p identifier files)
    (setq identifier (denote--increment-identifier identifier)))
  identifier)

(defun denote-barf-duplicate-id (identifier)
  "Throw a `user-error' if IDENTIFIER already exists."
  (when (denote--id-exists-p identifier)
    (user-error "`%s' already exists; aborting new note creation" identifier)))

(define-obsolete-function-alias
  'denote--barf-duplicate-id
  'denote-barf-duplicate-id
  "1.0.0")

;;;;; The `denote' command and its prompts

;;;###autoload
(defun denote (&optional title keywords file-type subdirectory date template signature)
  "Create a new note with the appropriate metadata and file name.

When called interactively, the metadata and file name are prompted
according to the value of `denote-prompts'.

When called from Lisp, all arguments are optional.

- TITLE is a string or a function returning a string.

- KEYWORDS is a list of strings.  The list can be empty or the
  value can be set to nil.

- FILE-TYPE is a symbol among those described in `denote-file-type'.

- SUBDIRECTORY is a string representing the path to either the
  value of the variable `denote-directory' or a subdirectory
  thereof.  The subdirectory must exist: Denote will not create
  it.  If SUBDIRECTORY does not resolve to a valid path, the
  variable `denote-directory' is used instead.

- DATE is a string representing a date like 2022-06-30 or a date
  and time like 2022-06-16 14:30.  A nil value or an empty string
  is interpreted as the `current-time'.

- TEMPLATE is a symbol which represents the key of a cons cell in
  the user option `denote-templates'.  The value of that key is
  inserted to the newly created buffer after the front matter.

- SIGNATURE is a string or a function returning a string."
  (interactive
   (let ((args (make-vector 7 nil)))
     (dolist (prompt denote-prompts)
       (pcase prompt
         ('title (aset args 0 (denote-title-prompt
                               (when (use-region-p)
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end))))))
         ('keywords (aset args 1 (denote-keywords-prompt)))
         ('file-type (aset args 2 (denote-file-type-prompt)))
         ('subdirectory (aset args 3 (denote-subdirectory-prompt)))
         ('date (aset args 4 (denote-date-prompt)))
         ('template (aset args 5 (denote-template-prompt)))
         ('signature (aset args 6 (denote-signature-prompt)))))
     (append args nil)))
  (let* ((title (or title ""))
         (file-type (denote--valid-file-type (or file-type denote-file-type)))
         (kws (if (called-interactively-p 'interactive)
                  keywords
                (denote-keywords-sort keywords)))
         (date (if (or (null date) (string-empty-p date))
                   (current-time)
                 (denote--valid-date date)))
         (id (format-time-string denote-id-format date))
         (directory (if (denote--dir-in-denote-directory-p subdirectory)
                        (file-name-as-directory subdirectory)
                      (denote-directory)))
         (template (if (stringp template)
                       template
                     (or (alist-get template denote-templates) "")))
         (signature (or signature "")))
    (denote-barf-duplicate-id id)
    (denote--prepare-note title kws date id directory file-type template signature)
    (denote--keywords-add-to-history keywords)))

(defvar denote--title-history nil
  "Minibuffer history of `denote-title-prompt'.")

(defun denote-title-prompt (&optional default-title)
  "Read file title for `denote'.
With optional DEFAULT-TITLE use it as the default value."
  (let* ((def default-title)
         (format (if (and def (not (string-empty-p def)))
                     (format "File title [%s]: " def)
                   "File title: ")))
    (read-string format nil 'denote--title-history def)))

(define-obsolete-function-alias
  'denote--title-prompt
  'denote-title-prompt
  "1.0.0")

(defvar denote--file-type-history nil
  "Minibuffer history of `denote-file-type-prompt'.")

(defun denote-file-type-prompt ()
  "Prompt for `denote-file-type'.
Note that a non-nil value other than `text', `markdown-yaml', and
`markdown-toml' falls back to an Org file type.  We use `org'
here for clarity."
  (completing-read
   "Select file type: " (denote--file-type-keys) nil t
   nil 'denote--file-type-history))

(define-obsolete-function-alias
  'denote--file-type-prompt
  'denote-file-type-prompt
  "1.0.0")

(defvar denote--date-history nil
  "Minibuffer history of `denote-date-prompt'.")

(declare-function org-read-date "org" (&optional with-time to-time from-string prompt default-time default-input inactive))

(defun denote-date-prompt ()
  "Prompt for date, expecting YYYY-MM-DD or that plus HH:MM.
Use Org's more advanced date selection utility if the user option
`denote-date-prompt-use-org-read-date' is non-nil."
  (if (and denote-date-prompt-use-org-read-date
           (require 'org nil :no-error))
      (let* ((time (org-read-date nil t))
             (org-time-seconds (format-time-string "%S" time))
             (cur-time-seconds (format-time-string "%S" (current-time))))
        ;; When the user does not input a time, org-read-date defaults to 00 for seconds.
        ;; When the seconds are 00, we add the current seconds to avoid identifier collisions.
        (when (string-equal "00" org-time-seconds)
          (setq time (time-add time (string-to-number cur-time-seconds))))
        (format-time-string "%Y-%m-%d %H:%M:%S" time))
    (read-string
     "DATE and TIME for note (e.g. 2022-06-16 14:30): "
     nil 'denote--date-history)))

(define-obsolete-function-alias
  'denote--date-prompt
  'denote-date-prompt
  "1.0.0")

(defun denote-prompt-for-date-return-id ()
  "Use `denote-date-prompt' and return it as `denote-id-format'."
  (format-time-string
   denote-id-format
   (denote--valid-date (denote-date-prompt))))

(defvar denote--subdir-history nil
  "Minibuffer history of `denote-subdirectory-prompt'.")

;; Making it a completion table is useful for packages that read the
;; metadata, such as `marginalia' and `embark'.
(defun denote--subdirs-completion-table (dirs)
  "Match DIRS as a completion table."
  (let* ((def (car denote--subdir-history))
         (table (denote--completion-table 'file dirs))
         (prompt (if def
                     (format "Select subdirectory [%s]: " def)
                   "Select subdirectory: ")))
    (completing-read prompt table nil t nil 'denote--subdir-history def)))

(defun denote-subdirectory-prompt ()
  "Prompt for subdirectory of the variable `denote-directory'.
The table uses the `file' completion category (so it works with
packages such as `marginalia' and `embark')."
  (let* ((root (directory-file-name (denote-directory)))
         (subdirs (denote-directory-subdirectories))
         (dirs (push root subdirs)))
    (denote--subdirs-completion-table dirs)))

(define-obsolete-function-alias
  'denote--subdirs-prompt
  'denote-subdirectory-prompt
  "1.0.0")

(defvar denote--template-history nil
  "Minibuffer history of `denote-template-prompt'.")

(defun denote-template-prompt ()
  "Prompt for template key in `denote-templates' and return its value."
  (let ((templates denote-templates))
    (alist-get
     (intern
      (completing-read
       "Select template KEY: " (mapcar #'car templates)
       nil t nil 'denote--template-history))
     templates)))

(define-obsolete-function-alias
  'denote--template-prompt
  'denote-template-prompt
  "1.0.0")

(defvar denote--signature-history nil
  "Minibuffer history of `denote-signature-prompt'.")

(defun denote-signature-prompt ()
  "Prompt for signature string."
  (read-string "Provide signature: " nil 'denote--signature-history))

;;;;; Convenience commands as `denote' variants

(defalias 'denote-create-note 'denote
  "Alias for `denote' command.")

;;;###autoload
(defun denote-type ()
  "Create note while prompting for a file type.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(file-type title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(file-type title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-type 'denote-type
  "Alias for `denote-type' command.")

;;;###autoload
(defun denote-date ()
  "Create note while prompting for a date.

The date can be in YEAR-MONTH-DAY notation like 2022-06-30 or
that plus the time: 2022-06-16 14:30.  When the user option
`denote-date-prompt-use-org-read-date' is non-nil, the date
prompt uses the more powerful Org+calendar system.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(date title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(date title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-date 'denote-date
  "Alias for `denote-date' command.")

;;;###autoload
(defun denote-subdirectory ()
  "Create note while prompting for a subdirectory.

Available candidates include the value of the variable
`denote-directory' and any subdirectory thereof.

This is equivalent to calling `denote' when `denote-prompts' is
set to \\='(subdirectory title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(subdirectory title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-in-subdirectory 'denote-subdirectory
  "Alias for `denote-subdirectory' command.")

;;;###autoload
(defun denote-template ()
  "Create note while prompting for a template.

Available candidates include the keys in the `denote-templates'
alist.  The value of the selected key is inserted in the newly
created note after the front matter.

This is equivalent to calling `denote' when `denote-prompts' is
set to \\='(template title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(template title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-with-template 'denote-template
  "Alias for `denote-template' command.")

;;;###autoload
(defun denote-signature ()
  "Create note while prompting for a file signature.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(signature title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(signature title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-signature 'denote-signature
  "Alias for `denote-signature' command.")

;;;;; Other convenience commands

(defun denote--extract-title-from-file-history ()
  "Extract last file title input from `file-name-history'."
  ;; We do not need to check if `file-name-history' is initialised
  ;; because it is defined in files.el.  My understanding is that it
  ;; is always loaded.
  (when-let ((title (expand-file-name (car denote--file-history))))
    (string-match (denote-directory) title)
    (substring title (match-end 0))))

(defun denote--append-extracted-string-to-history (history)
  "Append `denote--extract-title-from-file-history' to HISTORY."
  (append
   (list (denote--extract-title-from-file-history))
   history))

(defun denote--command-with-title-history (command)
  "Call COMMAND with modified title history.
Allow COMMAND to gain access to the return value of
`denote--append-extracted-string-to-history' for the
`denote--title-history'.  This is what makes
`denote-open-or-create' and `denote-link-or-create' return the
last input on demand when prompting for a title."
  (let ((denote--title-history
         (denote--append-extracted-string-to-history denote--title-history)))
    (call-interactively command)))

;;;###autoload
(defun denote-open-or-create (target)
  "Visit TARGET file in variable `denote-directory'.
If file does not exist, invoke `denote' to create a file.

If TARGET file does not exist, add the user input that was used
to search for it to the minibuffer history of the
`denote-file-prompt'.  The user can then retrieve and possibly
further edit their last input, using it as the newly created
note's actual title.  At the `denote-file-prompt' type
\\<minibuffer-local-map>\\[previous-history-element]."
  (interactive (list (denote-file-prompt)))
  (if (and target (file-exists-p target))
      (find-file target)
    (denote--command-with-title-history #'denote)))

;;;###autoload
(defun denote-keywords-add (keywords)
  "Prompt for KEYWORDS to add to the current note's front matter.
When called from Lisp, KEYWORDS is a list of strings.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'."
  (interactive (list (denote-keywords-prompt)))
  ;; A combination of if-let and let, as we need to take into account
  ;; the scenario in which there are no keywords yet.
  (if-let* ((file (buffer-file-name))
            ((denote-file-is-note-p file))
            (file-type (denote-filetype-heuristics file)))
      (let* ((cur-keywords (denote-retrieve-keywords-value file file-type))
             (new-keywords (if (and (stringp cur-keywords)
                                    (string-blank-p cur-keywords))
                               keywords
                             (denote-keywords-sort
                              (seq-uniq (append keywords cur-keywords))))))
        (denote-rewrite-keywords file new-keywords file-type)
        (denote-rename-file-using-front-matter file t))
    (user-error "Buffer not visiting a Denote file")))

(defun denote--keywords-delete-prompt (keywords)
  "Prompt for one or more KEYWORDS.
In the case of multiple entries, those are separated by the
`crm-sepator', which typically is a comma.  In such a case, the
output is sorted with `string-lessp'."
  (let ((choice (denote--keywords-crm keywords "Keyword to remove: ")))
    (if denote-sort-keywords
        (sort choice #'string-lessp)
      choice)))

;;;###autoload
(defun denote-keywords-remove ()
  "Prompt for keywords in current note and remove them.
Keywords are retrieved from the file's front matter.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'."
  (declare (interactive-only t))
  (interactive)
  (if-let* ((file (buffer-file-name))
            ((denote-file-is-note-p file))
            (file-type (denote-filetype-heuristics file)))
      (when-let* ((cur-keywords (denote-retrieve-keywords-value file file-type))
                  ((or (listp cur-keywords) (not (string-blank-p cur-keywords))))
                  (del-keyword (denote--keywords-delete-prompt cur-keywords)))
        (denote-rewrite-keywords
         file
         (seq-difference cur-keywords del-keyword)
         file-type)
        (denote-rename-file-using-front-matter file t))
    (user-error "Buffer not visiting a Denote file")))

;;;; Note modification

;;;;; Common helpers for note modifications

(defun denote--file-types-with-extension (extension)
  "Return only the entries of `denote-file-types' with EXTENSION.
See the format of `denote-file-types'."
  (seq-filter (lambda (type)
                (string-equal (plist-get (cdr type) :extension) extension))
              denote-file-types))

(defun denote-filetype-heuristics (file)
  "Return likely file type of FILE.
Use the file extension to detect the file type of the file.

If more than one file type correspond to this file extension, use
the first file type for which the key-title-kegexp matches in the
file or, if none matches, use the first type with this file
extension in `denote-file-type'.

If no file types in `denote-file-types' has the file extension,
the file type is assumed to be the first of `denote-file-types'."
  (let* ((file-type)
         (extension (file-name-extension file t))
         (types (denote--file-types-with-extension extension)))
    (cond ((not types)
           (setq file-type (caar denote-file-types)))
          ((= (length types) 1)
           (setq file-type (caar types)))
          (t
           (if-let ((found-type
                     (seq-find
                      (lambda (type)
                        (denote--regexp-in-file-p (plist-get (cdr type) :title-key-regexp) file))
                      types)))
               (setq file-type (car found-type))
             (setq file-type (caar types)))))
    file-type))

(define-obsolete-function-alias
  'denote--filetype-heuristics
  'denote-filetype-heuristics
  "1.0.0")

(defun denote--file-attributes-time (file)
  "Return `file-attribute-modification-time' of FILE as identifier."
  (format-time-string
   denote-id-format
   (file-attribute-modification-time (file-attributes file))))

(defun denote-update-dired-buffers ()
  "Update Dired buffers of variable `denote-directory'."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (and (eq major-mode 'dired-mode)
                  (denote--default-dir-has-denote-prefix))
         (revert-buffer))))
   (buffer-list)))

(defun denote--rename-buffer (old-name new-name)
  "Rename OLD-NAME buffer to NEW-NAME, when appropriate."
  (when-let ((buffer (find-buffer-visiting old-name)))
    (with-current-buffer buffer
      (set-visited-file-name new-name nil t))))

(defun denote-rename-file-and-buffer (old-name new-name)
  "Rename file named OLD-NAME to NEW-NAME, updating buffer name."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (cond
     ((derived-mode-p 'dired-mode)
      (dired-rename-file old-name new-name nil))
     ((vc-backend old-name)
      (vc-rename-file old-name new-name))
     (t
      (rename-file old-name new-name nil)))
    (denote--rename-buffer old-name new-name)))

(define-obsolete-function-alias
  'denote--rename-file
  'denote-rename-file-and-buffer
  "1.0.0")

(defun denote--add-front-matter (file title keywords id file-type)
  "Prepend front matter to FILE if `denote-file-is-note-p'.
The TITLE, KEYWORDS ID, and FILE-TYPE are passed from the
renaming command and are used to construct a new front matter
block if appropriate."
  (when-let* ((date (denote--date (date-to-time id) file-type))
              (new-front-matter (denote--format-front-matter title date keywords id file-type)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (insert new-front-matter))))

(defun denote--regexp-in-file-p (regexp file)
  "Return t if REGEXP matches in the FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward regexp nil t 1)))

(defun denote--edit-front-matter-p (file file-type)
  "Test if FILE should be subject to front matter rewrite.
Use FILE-TYPE to look for the front matter lines.  This is
relevant for operations that insert or rewrite the front matter
in a Denote note.

For the purposes of this test, FILE is a Denote note when it
contains a title line, a keywords line or both."
  (and (denote--regexp-in-file-p (denote--title-key-regexp file-type) file)
       (denote--regexp-in-file-p (denote--keywords-key-regexp file-type) file)))

(defun denote-rewrite-keywords (file keywords file-type)
  "Rewrite KEYWORDS in FILE outright according to FILE-TYPE.

Do the same as `denote-rewrite-front-matter' for keywords,
but do not ask for confirmation.

This is for use in `denote-keywords-add',`denote-keywords-remove',
`denote-dired-rename-marked-files', or related."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
          (goto-char (line-beginning-position))
          (insert (denote--get-keywords-line-from-front-matter keywords file-type))
          (delete-region (point) (line-end-position)))))))

(define-obsolete-function-alias
  'denote--rewrite-keywords
  'denote-rewrite-keywords
  "2.0.0")

(defun denote-rewrite-front-matter (file title keywords file-type)
  "Rewrite front matter of note after `denote-rename-file'.
The FILE, TITLE, KEYWORDS, and FILE-TYPE are given by the
renaming command and are used to construct new front matter
values if appropriate."
  (when-let* ((old-title-line (denote-retrieve-title-line file file-type))
              (old-keywords-line (denote-retrieve-keywords-line file file-type))
              (new-title-line (denote--get-title-line-from-front-matter title file-type))
              (new-keywords-line (denote--get-keywords-line-from-front-matter keywords file-type)))
    (with-current-buffer (find-file-noselect file)
      (when (y-or-n-p (format
                       "Replace front matter?\n-%s\n+%s\n\n-%s\n+%s?"
                       (propertize old-title-line 'face 'error)
                       (propertize new-title-line 'face 'success)
                       (propertize old-keywords-line 'face 'error)
                       (propertize new-keywords-line 'face 'success)))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (re-search-forward (denote--title-key-regexp file-type) nil t 1)
            (goto-char (line-beginning-position))
            (insert new-title-line)
            (delete-region (point) (line-end-position))
            (goto-char (point-min))
            (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
            (goto-char (line-beginning-position))
            (insert new-keywords-line)
            (delete-region (point) (line-end-position))))))))

(define-obsolete-function-alias
  'denote--rewrite-front-matter
  'denote-rewrite-front-matter
  "2.0.0")

;;;;; The renaming commands and their prompts

(defun denote--rename-dired-file-or-prompt ()
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

(defun denote-rename-file-prompt (old-name new-name)
  "Prompt to rename file named OLD-NAME to NEW-NAME."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (y-or-n-p
     (format "Rename %s to %s?"
             (propertize (file-name-nondirectory old-name) 'face 'error)
             (propertize (file-name-nondirectory new-name) 'face 'success)))))

(define-obsolete-function-alias
  'denote--rename-file-prompt
  'denote-rename-file-prompt
  "1.0.0")

;;;###autoload
(defun denote-rename-file (file title keywords &optional date)
  "Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the following conditions:

- If FILE does not have an identifier and optional DATE is
  non-nil (such as with a prefix argument), invoke the function
  `denote-prompt-for-date-return-id'.  It prompts for a date and
  uses it to derive the identifier.

- If FILE does not have an identifier and optional DATE is
  nil (this is the case without a prefix argument), use the file
  attributes to determine the last modified date and format it as
  an identifier.

- As a fallback, derive an identifier from the current time.

- If the resulting identifier is not unique among the files in
  the variable `denote-directory', increment it such that it
  becomes unique.

The default TITLE is retrieved from a line starting with a title
field in the file's contents, depending on the given file
type (e.g. #+title for Org).  Else, the file name is used as a
default value at the minibuffer prompt.

As a final step after the FILE, TITLE, and KEYWORDS prompts, ask
for confirmation, showing the difference between old and new file
names.

The file type extension (like .txt) is read from the underlying
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

If the file doesn't have front matter but is among the supported
file types (per `denote-file-type'), add front matter at the top
of it and leave the buffer unsaved for further inspection.

For per-file-type front matter, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'

This command is intended to (i) rename existing Denote notes
while updating their title and keywords in the front matter, (ii)
convert existing supported file types to Denote notes, and (ii)
rename non-note files (e.g. PDF) that can benefit from Denote's
file-naming scheme.  The latter is a convenience we provide,
since we already have all the requisite mechanisms in
place (though Denote does not---and will not---manage such
files)."
  (interactive
   (let* ((file (denote--rename-dired-file-or-prompt))
          (file-type (denote-filetype-heuristics file)))
     (list
      file
      (denote-title-prompt
       (denote--retrieve-title-or-filename file file-type))
      (denote-keywords-prompt)
      current-prefix-arg)))
  (let* ((dir (file-name-directory file))
         (id (denote-retrieve-or-create-file-identifier file date :unique))
         (signature (denote-retrieve-filename-signature file))
         (extension (file-name-extension file t))
         (file-type (denote-filetype-heuristics file))
         (new-name (denote-format-file-name
                    dir id keywords (denote-sluggify title) extension signature))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (when (denote-rename-file-prompt file new-name)
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (when (denote-file-is-writable-and-supported-p new-name)
        (if (denote--edit-front-matter-p new-name file-type)
            (denote-rewrite-front-matter new-name title keywords file-type)
          (denote--add-front-matter new-name title keywords id file-type))))))

;;;###autoload
(defun denote-change-file-type (file new-file-type)
  "Change file type of FILE and add an appropriate front matter.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

Add a front matter in the format of the NEW-FILE-TYPE at the
beginning of the file.

Retrieve the title of FILE from a line starting with a title
field in its front matter, depending on the previous file
type (e.g.  #+title for Org).  The same process applies for
keywords.

As a final step, ask for confirmation, showing the difference
between old and new file names.

Important note: No attempt is made to modify any other elements
of the file.  This needs to be done manually."
  (interactive
   (list
    (denote--rename-dired-file-or-prompt)
    (denote--valid-file-type (or (denote-file-type-prompt) denote-file-type))))
  (let* ((dir (file-name-directory file))
         (old-file-type (denote-filetype-heuristics file))
         (id (denote-retrieve-or-create-file-identifier file))
         (title (denote-retrieve-title-value file old-file-type))
         (keywords (denote-retrieve-keywords-value file old-file-type))
         (old-extension (file-name-extension file t))
         (new-extension (denote--file-extension new-file-type))
         (new-name (denote-format-file-name
                    dir id keywords (denote-sluggify title) new-extension))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (when (and (not (eq old-extension new-extension))
               (denote-rename-file-prompt file new-name))
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (when (denote-file-is-writable-and-supported-p new-name)
        (denote--add-front-matter new-name title keywords id new-file-type)))))

;;;###autoload
(defun denote-dired-rename-marked-files (&optional skip-front-matter-prompt ensure-unique-ids)
  "Rename marked files in Dired to Denote file name.

The operation does the following:

- the file's existing file name is retained and becomes the TITLE
  field, per Denote's file-naming scheme;

- the TITLE is sluggified and downcased, per our conventions;

- an identifier is prepended to the TITLE;

- the file's extension is retained;

- a prompt is asked once for the KEYWORDS field and the input is
  applied to all file names;

- if the file is recognized as a Denote note, add a front matter
  or rewrite it to include the new keywords.  A confirmation to
  carry out this step is performed once at the outset, unless
  optional SKIP-FRONT-MATTER-PROMPT is non-nil (such as with a
  universal prefix argument).  Note that the affected buffers are
  not saved.  The user can thus check them to confirm that the
  new front matter does not cause any problems (e.g. with the
  command `diff-buffer-with-file').  Multiple buffers can be
  saved with `save-some-buffers' (read its doc string).  The
  addition of front matter takes place only if the given file has
  the appropriate file type extension (per the user option
  `denote-file-type').

With optional ENSURE-UNIQUE-IDS as a double prefix argument,
process the file identifiers of the marked files to ensure there
is no duplicate among them.  When renaming files in Dired, it is
possible to produce duplicate identifiers.  This can happen when
multiple files share the same modification time, which can be
casually done with the `touch' command, `git', and others."
  (interactive "P" dired-mode)
  (when current-prefix-arg
    (setq skip-front-matter-prompt t
          ensure-unique-ids (when (>= (car current-prefix-arg) 16) t)))
  (if-let ((marks (dired-get-marked-files)))
      (let ((keywords (denote-keywords-prompt)))
        (when (or skip-front-matter-prompt
                  (yes-or-no-p "Add front matter if necessary (buffers are not saved)?"))
          (dolist (file marks)
            (let* ((dir (file-name-directory file))
                   (id (denote-retrieve-or-create-file-identifier file nil (when ensure-unique-ids marks)))
                   (signature (denote-retrieve-filename-signature file))
                   (file-type (denote-filetype-heuristics file))
                   (title (denote--retrieve-title-or-filename file file-type))
                   (extension (file-name-extension file t))
                   (new-name (denote-format-file-name
                              dir id keywords (denote-sluggify title) extension signature)))
              (denote-rename-file-and-buffer file new-name)
              (when (denote-file-is-writable-and-supported-p new-name)
                (if (denote--edit-front-matter-p new-name file-type)
                    (denote-rewrite-keywords new-name keywords file-type)
                  (denote--add-front-matter new-name title keywords id file-type)))
              (when ensure-unique-ids
                (setq marks (delete file marks))
                (push new-name marks))))
          (revert-buffer)))
    (user-error "No marked files; aborting")))

;;;###autoload
(defun denote-rename-file-using-front-matter (file &optional auto-confirm)
  "Rename FILE using its front matter as input.
When called interactively, FILE is the return value of the
function `buffer-file-name' which is subsequently inspected for
the requisite front matter.  It is thus implied that the FILE has
a file type that is supported by Denote, per `denote-file-type'.

Unless AUTO-CONFIRM is non-nil (such as with a prefix argument),
ask for confirmation, showing the difference between the old and
the new file names.

Never modify the identifier of the FILE, if any, even if it is
edited in the front matter.  Denote considers the file name to be
the source of truth in this case to avoid potential breakage with
typos and the like.

Refrain from performing the operation if the buffer has unsaved
changes.  Inform the user about the need to save their changes
first.  If AUTO-CONFIRM is non-nil, then save the buffer and
proceed with the renaming."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (when (buffer-modified-p)
    (if (or auto-confirm
            (y-or-n-p "Would you like to save the buffer?"))
        (save-buffer)
      (user-error "Save buffer before proceeding")))
  (unless (denote-file-is-writable-and-supported-p file)
    (user-error "The file is not writable or does not have a supported file extension"))
  (if-let* ((file-type (denote-filetype-heuristics file))
            (title (denote-retrieve-title-value file file-type))
            (extension (file-name-extension file t))
            (id (denote-retrieve-or-create-file-identifier file))
            (dir (file-name-directory file))
            (new-name (denote-format-file-name
                       ;; The `denote-retrieve-keywords-value' and
                       ;; `denote-retrieve-filename-signature' are
                       ;; not inside the `if-let*' because we do not
                       ;; want to throw an exception if any is nil.
                       dir id
                       (or (denote-retrieve-keywords-value file file-type) nil)
                       (denote-sluggify title) extension
                       (or (denote-retrieve-filename-signature file) nil))))
      (when (or auto-confirm
                (denote-rename-file-prompt file new-name))
        (denote-rename-file-and-buffer file new-name)
        (denote-update-dired-buffers))
    (user-error "No front matter for title and/or keywords")))

;;;###autoload
(defun denote-dired-rename-marked-files-using-front-matter ()
  "Rename marked files in Dired using their front matter as input.
Marked files must count as notes for the purposes of Denote,
which means that they at least have an identifier in their file
name and use a supported file type, per `denote-file-type'.
Files that do not meet this criterion are ignored.

The operation does the following:

- the title in the front matter becomes the TITLE component of
  the file name, with hyphenation per Denote's file-naming
  scheme;

- the keywords in the front matter are used for the KEYWORDS
  component of the file name and are processed accordingly, if
  needed;

- the identifier remains unchanged in the file name even if it is
  modified in the front matter (this is done to avoid breakage
  caused by typos and the like).

NOTE that files must be saved, because Denote reads from the
underlying file, not a modified buffer (this is done to avoid
potential mistakes).  The return value of a modified buffer is
the one prior to the modification, i.e. the one already written
on disk.

This command is useful for synchronizing multiple file names with
their respective front matter."
  (interactive nil dired-mode)
  (if-let ((marks (seq-filter
                   #'denote-file-is-writable-and-supported-p
                   (dired-get-marked-files))))
      (progn
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (denote-retrieve-or-create-file-identifier file))
                 (signature (denote-retrieve-filename-signature file))
                 (file-type (denote-filetype-heuristics file))
                 (title (denote-retrieve-title-value file file-type))
                 (keywords (denote-retrieve-keywords-value file file-type))
                 (extension (file-name-extension file t))
                 (new-name (denote-format-file-name
                            dir id keywords (denote-sluggify title) extension signature)))
            (denote-rename-file-and-buffer file new-name)))
        (revert-buffer))
    (user-error "No marked files; aborting")))

;;;;; Creation of front matter

;;;###autoload
(defun denote-add-front-matter (file title keywords)
  "Insert front matter at the top of FILE.

When called interactively, FILE is the return value of the
function `buffer-file-name'.  FILE is checked to determine
whether it is a note for Denote's purposes.

TITLE is a string.  Interactively, it is the user input at the
minibuffer prompt.

KEYWORDS is a list of strings.  Interactively, it is the user
input at the minibuffer prompt.  This one supports completion for
multiple entries, each separated by the `crm-separator' (normally
a comma).

The purpose of this command is to help the user generate new
front matter for an existing note (perhaps because the user
deleted the previous one and could not undo the change).

This command does not rename the file (e.g. to update the
keywords).  To rename a file by reading its front matter as
input, use `denote-rename-file-using-front-matter'.

Note that this command is useful only for existing Denote notes.
If the user needs to convert a generic text file to a Denote
note, they can use one of the command which first rename the file
to make it comply with our file-naming scheme and then add the
relevant front matter."
  (interactive
   (list
    (buffer-file-name)
    (denote-title-prompt)
    (denote-keywords-prompt)))
  (when (denote-file-is-writable-and-supported-p file)
    (denote--add-front-matter
     file title keywords
     (denote-retrieve-or-create-file-identifier file nil)
     (denote-filetype-heuristics file))))

;;;; The Denote faces

(defgroup denote-faces ()
  "Faces for Denote."
  :group 'denote)

(defface denote-faces-link '((t :inherit link))
  "Face used to style Denote links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(make-obsolete-variable 'denote-faces-broken-link nil "1.0.0")

(defface denote-faces-subdirectory '((t :inherit bold))
  "Face for subdirectory of file name.
This should only ever needed in the backlinks' buffer (or
equivalent), not in Dired."
  :group 'denote-faces
  :package-version '(denote . "0.2.0"))

(defface denote-faces-date '((t :inherit font-lock-variable-name-face))
  "Face for file name date in Dired buffers.
This is the part of the identifier that covers the year, month,
and day."
  :group 'denote-faces
  :package-version '(denote . "0.1.0"))

(defface denote-faces-time '((t :inherit denote-faces-date))
  "Face for file name time in Dired buffers.
This is the part of the identifier that covers the hours, minutes,
and seconds."
  :group 'denote-faces
  :package-version '(denote . "0.1.0"))

(defface denote-faces-title nil
  "Face for file name title in Dired buffers."
  :group 'denote-faces
  :package-version '(denote . "0.1.0"))

(defface denote-faces-extension '((t :inherit shadow))
  "Face for file extension type in Dired buffers."
  :group 'denote-faces
  :package-version '(denote . "0.1.0"))

(defface denote-faces-keywords '((t :inherit font-lock-builtin-face))
  "Face for file name keywords in Dired buffers."
  :group 'denote-faces
  :package-version '(denote . "0.1.0"))

(defface denote-faces-signature '((t :inherit font-lock-warning-face))
  "Face for file name signature in Dired buffers."
  :group 'denote-faces
  :package-version '(denote . "2.0.0"))

(defface denote-faces-delimiter
  '((((class color) (min-colors 88) (background light))
     :foreground "gray70")
    (((class color) (min-colors 88) (background dark))
     :foreground "gray30")
    (t :inherit shadow))
  "Face for file name delimiters in Dired buffers."
  :group 'denote-faces
  :package-version '(denote . "0.1.0"))

;; For character classes, evaluate: (info "(elisp) Char Classes")
(defvar denote-faces--file-name-regexp
  (concat "\\(?1:[0-9]\\{8\\}\\)\\(?2:T[0-9]\\{6\\}\\)"
          "\\(?:\\(?3:==\\)\\(?4:[[:alnum:][:nonascii:]=]*?\\)\\)?"
          "\\(?:\\(?5:--\\)\\(?6:[[:alnum:][:nonascii:]-]*?\\)\\)?"
          "\\(?:\\(?7:__\\)\\(?8:[[:alnum:][:nonascii:]_-]*?\\)\\)?"
          "\\(?9:\\..*\\)?$")
  "Regexp of file names for fontification.")

(defconst denote-faces-file-name-keywords
  `((,(concat "[\t\s]+" denote-faces--file-name-regexp)
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-signature nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-title nil t)
     (7 'denote-faces-delimiter nil t)
     (8 'denote-faces-keywords nil t)
     (9 'denote-faces-extension nil t )))
  "Keywords for fontification of file names.")

(defconst denote-faces-file-name-keywords-for-backlinks
  `((,(concat "^\\(?10:.*/\\)?" denote-faces--file-name-regexp)
     (10 'denote-faces-subdirectory nil t)
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-signature nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-title nil t)
     (7 'denote-faces-delimiter nil t)
     (8 'denote-faces-keywords nil t)
     (9 'denote-faces-extension nil t )))
  "Keywords for fontification of file names in the backlinks buffer.")

;;;; Fontification in Dired

(defgroup denote-dired ()
  "Integration between Denote and Dired."
  :group 'denote)

(defcustom denote-dired-directories (list denote-directory)
  "List of directories where `denote-dired-mode' should apply to.
For this to take effect, add `denote-dired-mode-in-directories',
to the `dired-mode-hook'."
  :type '(repeat directory)
  :package-version '(denote . "0.1.0")
  :link '(info-link "(denote) Fontification in Dired")
  :group 'denote-dired)

;; NOTE 2022-09-12: I tried to use the `dired-font-lock-keywords', but
;; then it overrides the standard Dired faces.  The `diredfl' package
;; uses that method, though it redefines all Dired faces.  We don't want
;; to do that.

;; FIXME 2022-08-12: Make `denote-dired-mode' actually apply to Dired.
;; FIXME 2022-08-12: Make `denote-dired-mode' persist after WDired.
;; FIXME 2022-08-12: Make `denote-dired-mode' work with diredfl.  This
;; may prove challenging.

;;;###autoload
(define-minor-mode denote-dired-mode
  "Fontify all Denote-style file names.
Add this or `denote-dired-mode-in-directories' to
`dired-mode-hook'."
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

;;;; The linking facility

(defgroup denote-link ()
  "Link facility for Denote."
  :group 'denote)

;;;;; User options

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
  :package-version '(denote . "0.1.0")
  :group 'denote-link)

;;;;; Link to note

(define-obsolete-variable-alias
  'denote-link--format-org
  'denote-org-link-format
  "1.2.0")

(defvar denote-org-link-format "[[denote:%s][%s]]"
  "Format of Org link to note.
The value is passed to `format' with IDENTIFIER and TITLE
arguments, in this order.

Also see `denote-org-link-in-context-regexp'.")

(define-obsolete-variable-alias
  'denote-link--format-markdown
  'denote-md-link-format
  "1.2.0")

(defvar denote-md-link-format "[%2$s](denote:%1$s)"
  "Format of Markdown link to note.
The %N$s notation used in the default value is for `format' as
the supplied arguments are IDENTIFIER and TITLE, in this order.

Also see `denote-md-link-in-context-regexp'.")

(define-obsolete-variable-alias
  'denote-link--format-id-only
  'denote-id-only-link-format
  "1.2.0")

(defvar denote-id-only-link-format "[[denote:%s]]"
  "Format of identifier-only link to note.
The value is passed to `format' with IDENTIFIER as its sole
argument.

Also see `denote-id-only-link-in-context-regexp'.")

(define-obsolete-variable-alias
  'denote-link--regexp-org
  'denote-org-link-in-context-regexp
  "1.2.0")

(defvar denote-org-link-in-context-regexp
  (concat "\\[\\[" "denote:"  "\\(?1:" denote-id-regexp "\\)" "]" "\\[.*?]]")
  "Regexp to match an Org link in its context.
The format of such links is `denote-org-link-format'.")

(define-obsolete-variable-alias
  'denote-link--regexp-markdown
  'denote-md-link-in-context-regexp
  "1.2.0")

(defvar denote-md-link-in-context-regexp
  (concat "\\[.*?]" "(denote:"  "\\(?1:" denote-id-regexp "\\)" ")")
  "Regexp to match a Markdown link in its context.
The format of such links is `denote-md-link-format'.")

(define-obsolete-variable-alias
  'denote-link--regexp-plain
  'denote-id-only-link-in-context-regexp
  "1.2.0")

(defvar denote-id-only-link-in-context-regexp
  (concat "\\[\\[" "denote:"  "\\(?1:" denote-id-regexp "\\)" "]]")
  "Regexp to match an identifier-only link in its context.
The format of such links is `denote-id-only-link-format'."  )

(defun denote-link--file-type-format (file-type id-only)
  "Return link format based on FILE-TYPE.
With non-nil ID-ONLY, use the generic link format without a
title.

Fall back to `denote-org-link-format'."
  ;; Includes backup files.  Maybe we can remove them?
  (cond
   (id-only denote-id-only-link-format)
   ((when-let ((link (denote--link-format file-type)))
      link))
   ;; Plain text also uses [[denote:ID][TITLE]]
   (t denote-org-link-format)))

(defun denote-link--format-link (file format &optional description)
  "Prepare link to FILE using FORMAT.
If DESCRIPTION is non-nil, use it as link description instead of
FILE's title.

FORMAT is the symbol of a variable that specifies a string.  See
the `:link' property of `denote-file-types'."
  (let* ((file-id (denote-retrieve-filename-identifier file))
         (fm (if (symbolp format) (symbol-value format) format))
         (file-type (denote-filetype-heuristics file))
         (file-title (unless (string= fm denote-id-only-link-format)
                       (or description (denote--retrieve-title-or-filename file file-type)))))
    (format fm file-id file-title)))

;;;###autoload
(defun denote-link (target &optional id-only)
  "Create link to TARGET note in variable `denote-directory'.
With optional ID-ONLY, such as a universal prefix
argument (\\[universal-argument]), insert links with just the
identifier and no further description.  In this case, the link
format is always [[denote:IDENTIFIER]].

Use TARGET's title for the link's description.  The title comes
either from the front matter or the file name.

If region is active, use its text as the link's description
instead of TARGET's title.  If active region is empty (i.e
whitespace-only), insert an ID-ONLY link."
  (interactive (list (denote-file-prompt) current-prefix-arg))
  (let* ((beg (point))
         (description (when-let* (((region-active-p))
                                  (beg (region-beginning))
                                  (end (region-end))
                                  (selected-text
                                   (string-trim
                                    (buffer-substring-no-properties beg end))))
                        (delete-region beg end)
                        selected-text))
         (identifier-only (or id-only (string-empty-p description)))
         (file-type (denote-filetype-heuristics (buffer-file-name))))
    (when target
      (insert
       (denote-link--format-link
        target
        (denote-link--file-type-format file-type identifier-only)
        description))
      (unless (derived-mode-p 'org-mode)
        (make-button beg (point) 'type 'denote-link-button)))))

(define-obsolete-function-alias
  'denote-link-insert-link
  'denote-insert-link
  "2.0.0")

(defalias 'denote-insert-link 'denote-link
  "Alias for `denote-link' command.")

(defun denote-link--collect-identifiers (regexp)
  "Return collection of identifiers in buffer matching REGEXP."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (while (or (re-search-forward regexp nil t)
                 (re-search-forward denote-id-only-link-in-context-regexp nil t))
        (push (match-string-no-properties 1) matches)))
    matches))

(defun denote-link--expand-identifiers (regexp)
  "Expend identifiers matching REGEXP into file paths."
  (let ((files (denote-directory-files))
        (rx (if (symbolp regexp) (symbol-value regexp) regexp))
        found-files)
    (dolist (file files)
      (dolist (i (denote-link--collect-identifiers rx))
        (when (string-prefix-p i (file-name-nondirectory file))
          (push file found-files))))
    found-files))

(defvar denote-link--find-file-history nil
  "History for `denote-find-link'.")

(defun denote-link--find-file-prompt (files)
  "Prompt for linked file among FILES."
  (let ((file-names (mapcar #'denote-get-file-name-relative-to-denote-directory
                            files)))
    (completing-read
     "Find linked file: "
     (denote--completion-table 'file file-names)
     nil t nil 'denote-link--find-file-history)))

(defun denote-link-return-links (&optional file)
  "Return list of links in current or optional FILE.
Also see `denote-link-return-backlinks'."
  (when-let* ((current-file (or file (buffer-file-name)))
              ((denote-file-has-supported-extension-p current-file))
              (file-type (denote-filetype-heuristics current-file))
              (regexp (denote--link-in-context-regexp file-type))
              (links (with-current-buffer (find-file-noselect current-file)
                       (denote-link--expand-identifiers regexp))))
    links))

(defalias 'denote-link-return-forelinks 'denote-link-return-links
  "Alias for `denote-link-return-links'.")

(define-obsolete-function-alias
  'denote-link-find-file
  'denote-find-link
  "2.0.0")

;;;###autoload
(defun denote-find-link ()
  "Use minibuffer completion to visit linked file."
  (interactive)
  (find-file
   (denote-link--find-file-prompt
    (or (denote-link-return-links)
        (user-error "No links found")))))

(defun denote-link-return-backlinks (&optional file)
  "Return list of backlinks in current or optional FILE.
Also see `denote-link-return-links'."
  (when-let* ((current-file (or file (buffer-file-name)))
              (id (denote-retrieve-filename-identifier current-file))
              (backlinks (delete current-file (denote--retrieve-files-in-xrefs id))))
    backlinks))

(define-obsolete-function-alias
  'denote-link-find-backlink
  'denote-find-backlink
  "2.0.0")

;;;###autoload
(defun denote-find-backlink ()
  "Use minibuffer completion to visit backlink to current file.

Like `denote-find-link', but select backlink to follow."
  (interactive)
  (find-file
   (denote-get-path-by-id
    (denote-extract-id-from-string
     (denote-link--find-file-prompt
      (or (denote-link-return-backlinks)
          (user-error "No backlinks found")))))))

;;;###autoload
(defun denote-link-after-creating (&optional id-only)
  "Create new note in the background and link to it directly.

Use `denote' interactively to produce the new note.  Its doc
string explains which prompts will be used and under what
conditions.

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'.

IMPORTANT NOTE: Normally, `denote' does not save the buffer it
produces for the new note.  This is a safety precaution to not
write to disk unless the user wants it (e.g. the user may choose
to kill the buffer, thus cancelling the creation of the note).
However, for this command the creation of the note happens in the
background and the user may miss the step of saving their buffer.
We thus have to save the buffer in order to (i) establish valid
links, and (ii) retrieve whatever front matter from the target
file."
  (interactive "P")
  (let (path)
    (save-window-excursion
      (call-interactively #'denote)
      (save-buffer)
      (setq path (buffer-file-name)))
    (denote-link path id-only)))

;;;###autoload
(defun denote-link-or-create (target &optional id-only)
  "Use `denote-link' on TARGET file, creating it if necessary.

If TARGET file does not exist, call `denote-link-after-creating'
which runs the `denote' command interactively to create the file.
The established link will then be targeting that new file.

If TARGET file does not exist, add the user input that was used
to search for it to the minibuffer history of the
`denote-file-prompt'.  The user can then retrieve and possibly
further edit their last input, using it as the newly created
note's actual title.  At the `denote-file-prompt' type
\\<minibuffer-local-map>\\[previous-history-element].

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'."
  (interactive (list (denote-file-prompt) current-prefix-arg))
  (if (and target (file-exists-p target))
      (denote-link target id-only)
    (denote--command-with-title-history #'denote-link-after-creating)))

(defalias 'denote-link-to-existing-or-new-note 'denote-link-or-create
  "Alias for `denote-link-or-create' command.")

;;;;; Link buttons

;; Evaluate: (info "(elisp) Button Properties")
;;
;; Button can provide a help-echo function as well, but I think we might
;; not need it.
(define-button-type 'denote-link-button
  'follow-link t
  'face 'denote-faces-link
  'action #'denote-link--find-file-at-button)

(autoload 'thing-at-point-looking-at "thingatpt")

(defun denote-link--link-at-point-string ()
  "Return identifier at point."
  (when (or (thing-at-point-looking-at denote-id-only-link-in-context-regexp)
            (thing-at-point-looking-at denote-md-link-in-context-regexp)
            (thing-at-point-looking-at denote-org-link-in-context-regexp)
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

;; NOTE 2022-06-15: I add this as a variable for advanced users who may
;; prefer something else.  If there is demand for it, we can make it a
;; defcustom, but I think it would be premature at this stage.
(defvar denote-link-button-action #'find-file-other-window
  "Display buffer action for Denote buttons.")

(defun denote-link--find-file-at-button (button)
  "Visit file referenced by BUTTON."
  (let* ((id (denote-extract-id-from-string
              (buffer-substring-no-properties
               (button-start button)
               (button-end button))))
         (file (denote-get-path-by-id id)))
    (funcall denote-link-button-action file)))

;;;###autoload
(defun denote-link-buttonize-buffer (&optional beg end)
  "Make denote: links actionable buttons in the current buffer.

Buttonization applies to the plain text and Markdown file types,
per the user option `denote-file-types'.  It will not do anything
in `org-mode' buffers, as buttons already work there.  If you do
not use Markdown or plain text, then you do not need this.

Links work when they point to a file inside the variable
`denote-directory'.

To buttonize links automatically add this function to the
`find-file-hook'.  Or call it interactively for on-demand
buttonization.

When called from Lisp, with optional BEG and END as buffer
positions, limit the process to the region in-between."
  (interactive)
  (when (and (not (derived-mode-p 'org-mode))
             (denote-file-has-identifier-p (buffer-file-name)))
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward denote-id-regexp end t)
        (when-let ((string (denote-link--link-at-point-string))
                   (beg (match-beginning 0))
                   (end (match-end 0)))
          (make-button beg end 'type 'denote-link-button))))))

;;;;; Backlinks' buffer

(define-button-type 'denote-link-backlink-button
  'follow-link t
  'action #'denote-link--backlink-find-file
  'face nil)            ; we use this face though we style it later

(defun denote-link--backlink-find-file (button)
  "Action for BUTTON to `find-file'."
  (funcall denote-link-button-action (buffer-substring (button-start button) (button-end button))))

(defun denote-link--display-buffer (buf)
  "Run `display-buffer' on BUF.
Expand `denote-link-backlinks-display-buffer-action'."
  (display-buffer
   buf
   `(,@denote-link-backlinks-display-buffer-action)))

(defun denote-backlinks-next (&optional n)
  "Use appropriate command for forward motion in backlinks buffer.
With optional N as a numeric argument, move to the Nth button
from point (relevant when `denote-backlinks-show-context' is
nil)."
  (interactive "p" denote-backlinks-mode)
  (if denote-backlinks-show-context
      (xref-next-line)
    (forward-button n)))

(defun denote-backlinks-prev (&optional n)
  "Use appropriate command for backward motion in backlinks buffer.
With optional N as a numeric argument, move to the Nth button
from point (relevant when `denote-backlinks-show-context' is
nil)."
  (interactive "p" denote-backlinks-mode)
  (if denote-backlinks-show-context
      (xref-prev-line)
    (backward-button n)))

(defvar denote-backlinks-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "n" #'denote-backlinks-next)
    (define-key m "p" #'denote-backlinks-prev)
    (define-key m "g" #'revert-buffer)
    m)
  "Keymap for `denote-backlinks-mode'.")

(make-obsolete-variable 'denote-backlink-mode-map 'denote-backlinks-mode-map "0.6.0")

(define-derived-mode denote-backlinks-mode xref--xref-buffer-mode "Backlinks"
  "Major mode for backlinks buffers."
  (unless denote-backlinks-show-context
    (font-lock-add-keywords nil denote-faces-file-name-keywords-for-backlinks t))
  (add-hook 'project-find-functions #'denote-project-find nil t))

(make-obsolete-variable 'denote-backlink-mode 'denote-backlinks-mode "0.6.0")

(defun denote-link--prepare-backlinks (fetcher _alist)
  "Create backlinks' buffer for the current note.
FETCHER is a function that fetches a list of xrefs.  It is called
with `funcall' with no argument like `xref--fetcher'.

In the case of `denote', `apply-partially' is used to create a
function that has already applied another function to multiple
arguments.

ALIST is not used in favour of using
`denote-link-backlinks-display-buffer-action'."
  (let* ((inhibit-read-only t)
         (file (buffer-file-name))
         (file-type (denote-filetype-heuristics file))
         (id (denote-retrieve-filename-identifier file))
         (buf (format "*denote-backlinks to %s*" id))
         (xref-alist (xref--analyze (funcall fetcher)))
         (dir (denote-directory)))
    (with-current-buffer (get-buffer-create buf)
      (setq-local default-directory dir)
      (erase-buffer)
      (setq overlay-arrow-position nil)
      (denote-backlinks-mode)
      (goto-char (point-min))
      (when-let*  ((title (denote-retrieve-title-value file file-type))
                   (heading (format "Backlinks to %S (%s)" title id))
                   (l (length heading)))
        (insert (format "%s\n%s\n\n" heading (make-string l ?-))))
      (if denote-backlinks-show-context
          (xref--insert-xrefs xref-alist)
        (mapc (lambda (x)
                (insert (car x))
                (make-button (line-beginning-position) (line-end-position) :type 'denote-link-backlink-button)
                (newline))
              xref-alist))
      (goto-char (point-min))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (when-let ((buffer-file-name file))
                      (denote-link--prepare-backlinks
                       (apply-partially #'xref-matches-in-files id
                                        (delete file (denote-directory-text-only-files)))
                       nil)))))
    (denote-link--display-buffer buf)))

(define-obsolete-function-alias
  'denote-link-backlinks
  'denote-backlinks
  "2.0.0")

;;;###autoload
(defun denote-backlinks ()
  "Produce a buffer with backlinks to the current note.

The backlinks' buffer shows the file name of the note linking to
the current note, as well as the context of each link.

File names are fontified by Denote if the user option
`denote-link-fontify-backlinks' is non-nil.  If this user option
is nil, the buffer is fontified by Xref.

The placement of the backlinks' buffer is controlled by the user
option `denote-link-backlinks-display-buffer-action'.  By
default, it will show up below the current window."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (denote-file-is-writable-and-supported-p file)
      (let* ((id (denote-retrieve-filename-identifier file))
             (xref-show-xrefs-function #'denote-link--prepare-backlinks)
             (project-find-functions #'denote-project-find))
        (xref--show-xrefs
         (apply-partially #'xref-matches-in-files id
                          ;; remove the current buffer file from the
                          ;; backlinks
                          (delete file (denote-directory-text-only-files)))
         nil)))))

(define-obsolete-function-alias
  'denote-link-show-backlinks-buffer
  'denote-show-backlinks-buffer
  "2.0.0")

(defalias 'denote-show-backlinks-buffer 'denote-backlinks
  "Alias for `denote-backlinks' command.")

;;;;; Add links matching regexp

(defvar denote-link--prepare-links-format "- %s\n"
  "Format specifiers for `denote-link-add-links'.")

;; NOTE 2022-06-16: There is no need to overwhelm the user with options,
;; though I expect someone to want to change the sort order.
(defvar denote-link-add-links-sort nil
  "When t, add REVERSE to `sort-lines' of `denote-link-add-links'.")

(defun denote-link--prepare-links (files current-file id-only)
  "Prepare links to FILES from CURRENT-FILE.
When ID-ONLY is non-nil, use a generic link format.  See
`denote-link--file-type-format'."
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
    (buffer-string)))

(defvar denote-link--add-links-history nil
  "Minibuffer history for `denote-add-links'.")

(define-obsolete-function-alias
  'denote-link-add-links
  'denote-add-links
  "2.0.0")

;;;###autoload
(defun denote-add-links (regexp &optional id-only)
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
  (let* ((current-file (buffer-file-name))
         (file-type (denote-filetype-heuristics current-file)))
    (if-let ((files (delete current-file
                            (denote-directory-files-matching-regexp regexp)))
             (beg (point)))
        (progn
          (insert (denote-link--prepare-links files file-type id-only))
          (denote-link-buttonize-buffer beg (point)))
      (message "No links matching `%s'" regexp))))

(defalias 'denote-link-insert-links-matching-regexp 'denote-add-links
  "Alias for `denote-add-links' command.")

(define-obsolete-function-alias
  'denote-link-add-missing-links
  'denote-add-missing-links
  "2.0.0")

;;;###autoload
(defun denote-add-missing-links (regexp &optional id-only)
  "Insert missing links to all notes matching REGEXP.
Similar to `denote-add-links' but insert only links not yet
present in the current buffer.

Optional ID-ONLY has the same meaning as in `denote-link': it
inserts links with just the identifier."
  (interactive
   (list
    (read-regexp "Insert links matching REGEX: " nil 'denote-link--add-links-history)
    current-prefix-arg))
  (let* ((current-file (buffer-file-name))
         (file-type (denote-filetype-heuristics current-file))
         (current-id (denote--link-in-context-regexp file-type))
         (linked-files (denote-link--expand-identifiers current-id)))
    (if-let* ((found-files (delete current-file
                                   (denote-directory-files-matching-regexp regexp)))
              (final-files (seq-difference found-files linked-files))
              (beg (point)))
        (progn
          (insert (denote-link--prepare-links final-files file-type id-only))
          (denote-link-buttonize-buffer beg (point)))
      (message "No links matching `%s' that aren't yet present in the current buffer" regexp))))

;;;;; Links from Dired marks

;; NOTE 2022-07-21: I don't think we need a history for this one.
(defun denote-link--buffer-prompt (buffers)
  "Select buffer from BUFFERS visiting Denote notes."
  (let ((buffer-file-names (mapcar #'file-name-nondirectory
                                   buffers)))
    (completing-read
     "Select note buffer: "
     (denote--completion-table 'buffer buffer-file-names)
     nil t)))

(defun denote-link--map-over-notes ()
  "Return list of `denote-file-is-note-p' from Dired marked items."
  (seq-filter
   (lambda (f)
     (and (denote-file-is-note-p f)
          (denote--dir-in-denote-directory-p default-directory)))
   (dired-get-marked-files)))

;;;###autoload
(defun denote-link-dired-marked-notes (files buffer &optional id-only)
  "Insert Dired marked FILES as links in BUFFER.

FILES are Denote notes, meaning that they have our file-naming
scheme, are writable/regular files, and use the appropriate file
type extension (per `denote-file-type').  Furthermore, the marked
files need to be inside the variable `denote-directory' or one of
its subdirectories.  No other file is recognised (the list of
marked files ignores whatever does not count as a note for our
purposes).

The BUFFER is one which visits a Denote note file.  If there are
multiple buffers, prompt with completion for one among them.  If
there isn't one, throw an error.

With optional ID-ONLY as a prefix argument, insert links with
just the identifier (same principle as with `denote-link').

This command is meant to be used from a Dired buffer."
  (interactive
   (list
    (denote-link--map-over-notes)
    (let ((file-names (denote--buffer-file-names)))
      (find-file
       (cond
        ((null file-names)
         (user-error "No buffers visiting Denote notes"))
        ((eq (length file-names) 1)
         (car file-names))
        (t
         (denote-link--buffer-prompt file-names)))))
    current-prefix-arg)
   dired-mode)
  (if (null files)
      (user-error "No note files to link to")
    (when (y-or-n-p (format "Create links at point in %s?" buffer))
      (with-current-buffer buffer
        (insert (denote-link--prepare-links
                 files
                 (denote-filetype-heuristics (buffer-file-name))
                 id-only))
        (denote-link-buttonize-buffer)))))

;;;;; Define menu

(defvar denote--menu-contents
  '("Denote"
    ["Create a note" denote
     :help "Create a new note in the `denote-directory'"]
    ["Create a note with given file type" denote-type
     :help "Create a new note with a given file type in the `denote-directory'"]
    ["Create a note in subdirectory" denote-subdirectory
     :help "Create a new note in a subdirectory of the `denote-directory'"]
    ["Create a note with date" denote-date
     :help "Create a new note with a given date in the `denote-directory'"]
    ["Create a note with signature" denote-signature
     :help "Create a new note with a given signature in the `denote-directory'"]
    "---"
    ["Rename a file" denote-rename-file
     :help "Rename file interactively"
     :enable (derived-mode-p 'dired-mode 'text-mode)]
    ["Rename this file using its front matter" denote-rename-file-using-front-matter
     :help "Rename the current file using its front matter as input"
     :enable (derived-mode-p 'text-mode)]
    ["Rename Dired marked files" denote-dired-rename-marked-files
     :help "Rename marked files in Dired"
     :enable (derived-mode-p 'dired-mode)]
    ["Rename Dired marked files using their front matter" denote-dired-rename-marked-files-using-front-matter
     :help "Rename marked files in Dired using their front matter as input"
     :enable (derived-mode-p 'dired-mode)]
    "---"
    ["Insert a link" denote-link
     :help "Insert link to a file in the `denote-directory'"
     :enable (derived-mode-p 'text-mode)]
    ["Insert links with regexp" denote-add-links
     :help "Insert links to files matching regexp in the `denote-directory'"
     :enable (derived-mode-p 'text-mode)]
    ["Insert Dired marked files as links" denote-link-dired-marked-notes
     :help "Rename marked files in Dired as links in a Denote buffer"
     :enable (derived-mode-p 'dired-mode)]
    ["Show file backlinks" denote-backlinks
     :help "Insert link to a file in the `denote-directory'"
     :enable (derived-mode-p 'text-mode)]
    "---"
    ["Highlight Dired file names" denote-dired-mode
     :help "Apply colors to Denote file name components in Dired"
     :enable (derived-mode-p 'dired-mode)
     :style toggle
     :selected (bound-and-true-p denote-dired-mode)])
  "Contents of the Denote menu.")

(easy-menu-define denote-global-menu global-map
  "Menu with all Denote commands, each available in the right context."
  denote--menu-contents)

(defun denote-context-menu (menu _click)
  "Populate MENU with Denote commands at CLICK."
  (define-key menu [denote-separator] menu-bar-separator)
  (let ((easy-menu (make-sparse-keymap "Denote")))
    (easy-menu-define nil easy-menu nil
      denote--menu-contents)
    (dolist (item (reverse (lookup-key easy-menu [menu-bar])))
      (when (consp item)
        (define-key menu (vector (car item)) (cdr item)))))
  menu)

;;;;; Register `denote:' custom Org hyperlink

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
         (path (denote-get-path-by-id id)))
    (cond
     (path-id
      (cons (format "%s" path) (format "%s" id)))
     ((and (stringp search) (not (string-empty-p search)))
      (concat path "::" search))
     (path))))

;;;###autoload
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

;;;###autoload
(defun denote-link-ol-complete ()
  "Like `denote-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `denote:' hyperlink type."
  (if-let ((file (denote-file-prompt)))
      (concat "denote:" (denote-retrieve-filename-identifier file))
    (user-error "No files in `denote-directory'")))

(declare-function org-link-store-props "ol.el" (&rest plist))
(defvar org-store-link-plist)

;;;###autoload
(defun denote-link-ol-store ()
  "Handler for `org-store-link' adding support for denote: links."
  (when-let* ((file (buffer-file-name))
              ((denote-file-is-note-p file))
              (file-type (denote-filetype-heuristics file))
              (file-id (denote-retrieve-filename-identifier file))
              (file-title (denote--retrieve-title-or-filename file file-type)))
    (org-link-store-props
     :type "denote"
     :description file-title
     :link (concat "denote:" file-id))
    org-store-link-plist))

;;;###autoload
(defun denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
         (path (file-relative-name (car path-id)))
         (p (file-name-sans-extension path))
         (id (cdr path-id))
         (desc (or description (concat "denote:" id))))
    (cond
     ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" p desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <denote:%s>" desc path)) ; NOTE 2022-06-16: May be tweaked further
     ((eq format 'md) (format "[%s](%s.md)" desc p))
     (t path))))

;; The `eval-after-load' part with the quoted lambda is adapted from
;; Elfeed: <https://github.com/skeeto/elfeed/>.

;;;###autoload
(eval-after-load 'org
  `(funcall
    ;; The extra quote below is necessary because uncompiled closures
    ;; do not evaluate to themselves. The quote is harmless for
    ;; byte-compiled function objects.
    ',(lambda ()
        (with-no-warnings
          (org-link-set-parameters
           "denote"
           :follow #'denote-link-ol-follow
           :face 'denote-faces-link
           :complete #'denote-link-ol-complete
           :store #'denote-link-ol-store
           :export #'denote-link-ol-export)))))

;;;; Glue code for org-capture

(defgroup denote-org-capture ()
  "Integration between Denote and Org Capture."
  :group 'denote)

(defcustom denote-org-capture-specifiers "%l\n%i\n%?"
  "String with format specifiers for `org-capture-templates'.
Check that variable's documentation for the details.

The string can include arbitrary text.  It is appended to new
notes via the `denote-org-capture' function.  Every new note has
the standard front matter we define."
  :type 'string
  :package-version '(denote . "0.1.0")
  :group 'denote-org-capture)

(defvar denote-last-path nil "Store last path.")

;;;###autoload
(defun denote-org-capture ()
  "Create new note through `org-capture-templates'.
Use this as a function that returns the path to the new file.
The file is populated with Denote's front matter.  It can then be
expanded with the usual specifiers or strings that
`org-capture-templates' supports.

Note that this function ignores the `denote-file-type': it always
sets the Org file extension for the created note to ensure that
the capture process works as intended, especially for the desired
output of the `denote-org-capture-specifiers' (which can include
arbitrary text).

Consult the manual for template samples."
  (let* ((title (denote-title-prompt))
         (keywords (denote-keywords-prompt))
         (front-matter (denote--format-front-matter
                        title (denote--date nil 'org) keywords
                        (format-time-string denote-id-format nil) 'org)))
    (setq denote-last-path
          (denote--path title keywords
                        (file-name-as-directory (denote-directory))
                        (format-time-string denote-id-format) 'org))
    (denote--keywords-add-to-history keywords)
    (concat front-matter denote-org-capture-specifiers)))

;;;###autoload
(defun denote-org-capture-with-prompts (&optional title keywords subdirectory date template)
  "Like `denote-org-capture' but with optional prompt parameters.

When called without arguments, do not prompt for anything.  Just
return the front matter with title and keyword fields empty and
the date and identifier fields specified.  Also make the file
name consist of only the identifier plus the Org file name
extension.

Otherwise produce a minibuffer prompt for every non-nil value
that corresponds to the TITLE, KEYWORDS, SUBDIRECTORY, DATE, and
TEMPLATE arguments.  The prompts are those used by the standard
`denote' command and all of its utility commands.

When returning the contents that fill in the Org capture
template, the sequence is as follows: front matter, TEMPLATE, and
then the value of the user option `denote-org-capture-specifiers'.

Important note: in the case of SUBDIRECTORY actual subdirectories
must exist---Denote does not create them.  Same principle for
TEMPLATE as templates must exist and are specified in the user
option `denote-templates'."
  (let* ((title (if title (denote-title-prompt) ""))
         (kws (if keywords (denote-keywords-prompt) nil))
         (directory (file-name-as-directory (if subdirectory (denote-subdirectory-prompt) (denote-directory))))
         (date (if date (denote--valid-date (denote-date-prompt)) (current-time)))
         (id (format-time-string denote-id-format date))
         (template (if template (denote-template-prompt) ""))
         (front-matter (denote--format-front-matter
                        title (denote--date date 'org) kws
                        (format-time-string denote-id-format date) 'org)))
    (denote-barf-duplicate-id id)
    (setq denote-last-path
          (denote--path title kws directory id 'org))
    (denote--keywords-add-to-history kws)
    (concat front-matter template denote-org-capture-specifiers)))

(defun denote-org-capture-delete-empty-file ()
  "Delete file if capture with `denote-org-capture' is aborted."
  (when-let* ((file denote-last-path)
              ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

(make-obsolete 'denote-migrate-old-org-filetags nil "1.1.0")
(make-obsolete 'denote-migrate-old-markdown-yaml-tags nil "1.1.0")

;;;; Denote extension "modules"

(defvar denote-modules-available
  '(project (project-find-functions . denote-project-find)
            xref    (xref-backend-functions . denote--xref-backend)
            ffap    (denote-module-ffap-enable . denote-module-ffap-disable))
  "Denote modules currently built-in with Denote.
This variable is a plist.  Each module is represented as a pair
of a property name and its value being a cons cell; thus a module
is written in either the following forms:

    NAME (HOOK . FUNCTION\)
    NAME (FUNCTION . FUNCTION\)

NAME, HOOK, FUNCTION are symbols.

When a HOOK-FUNCTION pair is used, `denote-modules-enable'
function will add FUNCTION to HOOK and `denote-modules-disable'
function will remove FUNCTION from HOOK.  Generally, it should be
possible to set HOOK-FUNCTION modules locally.

When a FUNCTION-FUNCTION pair is used, the first FUNCTION must be
an enable function and the second, its corresponding disable
function to undo the former.  They are both called with no
arguments.  For FUNCTION-FUNCTION modules, in some cases, it may
not be possible to enable a module locally.  In these cases, some
parts of a module may be enabled globally even when local minor
mode function `denote-modules-mode' is called.

NOTES for future development to add new modules:

It is important that FUNCTION must be defined and loaded before
`denote-modules-enable' and `denote-moduel-disable' (the new
functions probably should be written in the source code lines
before these enable/disable functions)")

(defvar denote-module-ffap-last-enabled nil
  "Value of `ffap-next-regexp' beofe ffap module was last enabled.
It is used by `denote-module-ffap-disable' to undo the value
the module previoulsy set.")

(defvar denote-modules-last-enabled nil
  "Denote modules set last time.
It is used by `denote-modules-enable' and
`denote-moduules-disable' to undo the modules enabled last time.")

;; defvars to placate the compilers
(defvar denote-modules)
(defvar ffap-next-regexp)
(defvar ffap-alist)

(defun denote-module-ffap-disable (&optional local)
  "Disable Denote integration with `ffap'.
This function is meant to be set as a pair function with
`denote-module-ffap-enable' in `denote-modules-available'.

When LOCAL is non-nil, enable only for the local buffer as
much as possible.  Currently, `ffap-alist' is only disabled
globally."
  (require 'ffap)
  (setq ffap-alist (rassq-delete-all  #'denote-get-relative-path-by-id ffap-alist))
  (if local
      (when denote-module-ffap-last-enabled
        (setq-local ffap-next-regexp denote-module-ffap-last-enabled))
    ;; Reset `ffap-next-regexp' only when there is last-active.  Nil
    ;; means it is in the loading process of denote
    (when denote-module-ffap-last-enabled
      (setq ffap-next-regexp denote-module-ffap-last-enabled))))

(defun denote-module-ffap-enable (&optional local)
  "Enable Denote integration with `ffap'.
This function is meant to be set as a pair function with
`denote-module-ffap-disable' in `denote-modules-available'.

When LOCAL is non-nil, enable only for the local buffer as much
as possible.  Currently, `'ffap-alist' is only enabled globally."
  (require 'ffap)
  (if local (setq-local denote-module-ffap-last-active ffap-next-regexp)
    (setq denote-module-ffap-last-enabled ffap-next-regexp)
    (add-to-list 'ffap-alist (cons denote-id-regexp #'denote-get-relative-path-by-id)))
  (if local
      (setq-local ffap-next-regexp (concat ffap-next-regexp "\\|" denote-id-regexp))
    (setq ffap-next-regexp (concat ffap-next-regexp "\\|" denote-id-regexp))))

(defun denote-modules-disable (modules &optional local)
  "Disable Denote integration MODULES.
This function is meant to be used by `denote-modules-enable',
which calls this function, passgin `denote-modules-last-enable'
as MODULES to undo the modules currently active.

When LOCAL is non-nil, disable MODULES locally, where possible.

Refer to document string of `denote-modules-available'."
  (dolist (module modules)
    (let* ((module-def (plist-get denote-modules-available module))
           (hook (car module-def))
           (func (cdr module-def)))
      ;; If HOOK is a function, it's a setup function and FUNC is its
      ;; teardown counterpart.
      (if (functionp hook) (funcall func local)
        (remove-hook hook func local)))))

(defun denote-modules-enable (modules &optional local)
  "Enable MODULES set in `denote-modules'.
When LOCAL is non-nil, it tries to enable them only locally.
Whether this is possible or not depends on the module in
question.

Refer to document string of `denote-modules-available'."
  (denote-modules-disable denote-modules-last-enabled)
  (dolist (module modules)
    (let* ((module-def (plist-get denote-modules-available module))
           (hook (car module-def))
           (func (cdr module-def)))
      ;; If HOOK is a function, it's a setup function and FUNC is its
      ;; teardown counterpart.
      (if (functionp hook) (funcall hook local)
        (add-hook hook func nil local))))
  (if local (setq denote-modules-last-enabled modules)
    (setq denote-modules-last-enabled modules)))

;;;###autoload
(define-minor-mode denote-modules-mode
  "Enable Denote integration modules locally.
Set modules to be enabled in `denote-modules' and activate the
minor mode, either globally or locally.  The selected modules are
enabled only when the minor mode is active."
  :global nil
  :init-value nil
  (if denote-modules-mode
      (denote-modules-enable denote-modules :local)
    (denote-modules-disable denote-modules-last-enabled :local)))

;;;###autoload
(define-minor-mode denote-modules-global-mode
  "Enable Denote integration modules globally.
Set modules to be enabled in `denote-modules' and activate the
minor mode, either globally or locally.  The selected modules are
enabled only when the minor mode is active."
  :global t
  :init-value nil
  (if denote-modules-global-mode
      (denote-modules-enable denote-modules)
    (denote-modules-disable denote-modules-last-enabled)))

(defun denote-modules-set (symbol value)
  "Set SYMBOL and VALUE for `denote-modules' upon customizing.
Enable the modules set when `denote-modules-mode' or
`denote-modules-global-mode' is active.  If not, this function
does not enable them automatically.  Manually call the minor mode
globally or locally or set it in your configuration.

It is meant to be used `defcustom' of `denote-modules', thus when
the minor mode is active, changing the modules in the `customize'
UI will be effective immediately."
  (set symbol value)
  (when (or denote-modules-global-mode denote-modules-mode)
    (denote-modules-enable value)))

(defcustom denote-modules nil
  "User-selected Denote modules.
The selected modules are a list of NAME (symbols), and each
module enables integration with another Emacs built-in feature.
See `denote-modules-available' for the modules currently
available.  Set this user option as a list of NAME; for example:

    (project xref ffap)

When customized in Customize UI, it presents a set of checkboxes,
each box checked adds NAME of the module to the list.

Modules are automatically enabled only when either
`denote-modules-mode' or `denote-modules-global-mode' is active.
If not, setting the modules does not enable or disable them
automatically.  Manually call the minor mode globally or locally
or set it in your configuration."
  :group 'denote
  :set #'denote-modules-set
  :package-version '(denote . "1.2.0")
  :type
  '(set (const :tag "Project integration" project)
        (const :tag "Xref integration " xref)
        (const :tag "Integration with find-file-at-point `ffap'" ffap)))

;;;; project.el integration
;;   This is also used by xref integration

(cl-defmethod project-root ((project (head denote)))
  "Denote's implementation of `project-root' method from `project'.
Return current variable `denote-directory' as the root of the
current denote PROJECT."
  (cdr project))

(cl-defmethod project-files ((_project (head denote)) &optional _dirs)
  "Denote's implementation of `project-files' method from `project'.
Return all files that have an identifier for the current denote
PROJECT.  The return value may thus include file types that are
not implied by `denote-file-type'.  To limit the return value to
text files, use the function `denote-directory-text-only-files'."
  (denote-directory-files))

(defun denote-project-find (dir)
  "Return project instance if DIR is part of variable `denote-directory'.
The format of project instance is aligned with `project-try-vc'
defined in `project'."
  (let ((dir (expand-file-name dir)) ; canonicalize current directory name
        (root (denote-directory)))
    (when (or (file-equal-p dir root) ; currently at `denote-directory'
              (string-prefix-p root dir)) ; or its subdirectory
      (cons 'denote root))))

;;;; Xref integration
;;   Set `xref-backend-functions' like this.
;;     (add-hook 'xref-backend-functions #'denote--xref-backend)
;;
;;   You can tell xref-references not to prompt by adding the following:
;;     (add-to-list 'xref-prompt-for-identifier #'xref-find-references
;;     :append)

(defun denote--xref-backend ()
  "Return denote if `default-directory' is in denote directory."
  (when (denote--dir-in-denote-directory-p default-directory)
    'denote))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'denote)))
  "Return the \"thing\" at point.
The same logic as `elisp-mode'.  The \"thing\" is assumed to be a
Denote identifier, but can be any word.  The method checks this
and errors and if the word at point is not a Denote identifier."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (and bounds
         (let ((id (buffer-substring-no-properties
                    (car bounds) (cdr bounds))))
           (if (string-match-p denote-id-regexp id)
               ;; Use a property to transport the location of the identifier.
               (propertize id 'pos (car bounds))
             (user-error "%s is not a Denote identifier" id))))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'denote)) identifier)
  "Return xref for the note IDENTIFIER points to."
  (let ((file (denote-get-path-by-id identifier)))
    (when file
      (if (file-equal-p file (buffer-file-name (current-buffer)))
          (user-error "Identifier points to the current buffer")
        ;; Without the message, Xref will report that the ID does not
        ;; exist, which is incorrect in this case.
        (list (xref-make nil (xref-make-file-location file 0 0)))))))

(cl-defmethod xref-backend-references ((_backend (eql 'denote)) identifier)
  "Return list of xrefs where IDENTIFIER is referenced.
This include the definition itself."
  (xref-matches-in-files identifier (denote-directory-text-only-files)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend
                                                         (eql 'denote)))
  "Return list of Denote identifers as completion table."

  (mapcar #'denote-retrieve-filename-identifier (denote-all-files)))

(provide 'denote)
;;; denote.el ends here
