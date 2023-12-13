;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 2.2.4
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
;; such as videos.  Naming files in a consistent way makes their
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
Also see user options: `denote-infer-keywords',
`denote-sort-keywords', `denote-file-name-letter-casing'."
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
  included in a file's front matter.  They are reserved solely
  for creating a sequence in a file listing, at least for the
  time being.  To insert a link that includes the signature, use
  the command `denote-link-with-signature'.

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
`string-collate-lessp' regardless of the order they were inserted at the
minibuffer prompt.

If nil, show the keywords in their given order."
  :group 'denote
  :package-version '(denote . "0.1.0")
  :type 'boolean)

(make-obsolete
 'denote-allow-multi-word-keywords
 'denote-file-name-letter-casing
 "2.1.0")

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

NOTE: Expert users can change the supported file-types by editing
the value of `denote-file-types'.  That variable, which is not a
user option, controls the behaviour of all file-type-aware
functions (creating notes, renaming them, inserting front matter,
formatting a link, etc.). Consult its documentation for the
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

(defcustom denote-rename-no-confirm nil
  "When non-nil, `denote-rename-file' does not prompt for confirmation.
The default behaviour of the `denote-rename-file' command is to
ask for an affirmative answer as a final step before changing the
file name and, where relevant, inserting or updating the
corresponding front matter.

Remember that `denote-rename-file' does not save the underlying
buffer it modifies.  It leaves it unsaved so that the user can
review what happened, such as by invoking the command
`diff-buffer-with-file'.

Specialised commands that build on top of `denote-rename-file'
may internally bind this user option to a non-nil value in order
to perform their operation (e.g. `denote-dired-rename-files' goes
through each marked Dired file, prompting for the information to
use, but carries out the renaming without asking for
confirmation (buffers remain unsaved))."
  :group 'denote
  :package-version '(denote . "2.1.0")
  :type 'boolean)

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

(defcustom denote-after-new-note-hook nil
  "Normal hook that runs after the `denote' command.
This also covers all convenience functions that call `denote'
internally, such as `denote-signature' and `denote-type' (check
the default value of the user option `denote-commands-for-new-notes')."
  :group 'denote
  :package-version '(denote . "2.1.0")
  :type 'hook)

(defcustom denote-region-after-new-note-functions nil
  "Abnormal hook called after `denote-region'.
Functions in this hook are called with two arguments,
representing the beginning and end buffer positions of the region
that was inserted in the new note.  These are called only if
`denote-region' is invoked while a region is active.

A common use-case is to call `org-insert-structure-template'
after a region is inserted.  This case does not actually require
the aforementioned arguments, in which case the function can
simply declare them as ignored by prefixing the argument names
with an underscore.  For example, the following will prompt for a
structure template as soon as `denote-region' is done:

    (defun my-denote-region-org-structure-template (_beg _end)
      (when (derived-mode-p \\='org-mode)
        (activate-mark)
        (call-interactively \\='org-insert-structure-template)))

    (add-hook \\='denote-region-after-new-note-functions
              #\\='my-denote-region-org-structure-template)"
  :group 'denote
  :package-version '(denote . "2.1.0")
  :link '(info-link "(denote) Create a note with the region's contents")
  :type 'hook)

(defcustom denote-commands-for-new-notes
  '(denote
    denote-date
    denote-subdirectory
    denote-template
    denote-type
    denote-signature)
  "List of commands for `denote-command-prompt' that create a new note.
These are used by commands such as `denote-open-or-create-with-command'
and `denote-link-after-creating-with-command'."
  :group 'denote
  :package-version '(denote . "2.1.0")
  :link '(info-link "(denote) Choose which commands to prompt for")
  :type '(repeat symbol))

(defcustom denote-file-name-letter-casing
  '((title . downcase)
    (signature . downcase)
    (keywords . downcase)
    (t . downcase))
  "Specify the method Denote uses to affect the letter casing of file names.

The value is an alist where each element is a cons cell of the
form (COMPONENT . METHOD).

- The COMPONENT is an unquoted symbol among `title', `signature',
  `keywords', which refers to the corresponding component of the
  file name.  The special t COMPONENT is a fallback value in case
  the others are not specified.

- The METHOD is the letter casing scheme, which is an unquoted
  symbol of either `downcase' or `verbatim'.  A nil value has the
  same meaning as `downcase'.  Other non-nil METHOD types are
  reserved for possible future use.

  The `downcase' METHOD converts user input for the given
  COMPONENT into lower case.  The benefit of this approach (which
  is the default behaviour) is that file names remain consistent
  over the long-term.  The user never needs to account for
  varying letter casing while working with them.

  The `verbatim' METHOD means that Denote will not affect the
  letter casing of user input when generating the given file name
  COMPONENT.  As such, conventions like CamelCase or camelCase
  are respected.  The user thus assumes responsibility to keep
  file names in a good state over the long term."
  :group 'denote
  :type '(alist
          :key (choice :tag "File name component"
                       (const :tag "The --TITLE component of the file name" title)
                       (const :tag "The ==SIGNATURE component of the file name" signature)
                       (const :tag "The __KEYWORDS component of the file name" keywords)
                       (const :tag "Fallback for any unspecified file name component" t))
          :value (choice :tag "Letter casing method"
                         (const :tag "Downcase file names (default)" downcase)
                         (const :tag "Accept file name inputs verbatim" verbatim)))
  :link '(info-link "(denote) Contol the letter casing of file names")
  :package-version '(denote . "2.1.0"))

;;;; Main variables

;; For character classes, evaluate: (info "(elisp) Char Classes")

(defconst denote-id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.
The note's ID is derived from the date and time of its creation.")

(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote-id-format'.")

(defconst denote-signature-regexp "==\\([[:alnum:][:nonascii:]=]*\\)"
  "Regular expression to match the SIGNATURE field in a file name.")

(defconst denote-title-regexp "--\\([[:alnum:][:nonascii:]-]*\\)"
  "Regular expression to match the TITLE field in a file name.")

(defconst denote-keywords-regexp "__\\([[:alnum:][:nonascii:]_-]*\\)"
  "Regular expression to match the KEYWORDS field in a file name.")

(defconst denote-excluded-punctuation-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*"
  "Punctionation that is removed from file names.
We consider those characters illegal for our purposes.")

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
      dir-locals))))

(defun denote--make-denote-directory ()
  "Make the variable `denote-directory' and its parents, if needed."
  (when (not (file-directory-p denote-directory))
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

(defun denote--slug-no-punct (str &optional extra-characters)
  "Remove punctuation from STR.
Concretely, replace with an empty string anything that matches
the `denote-excluded-punctuation-regexp' and
`denote-excluded-punctuation-extra-regexp'.

EXTRA-CHARACTERS is an optional string that has the same meaning
as the aforementioned variables."
  (dolist (regexp (list denote-excluded-punctuation-regexp
                        denote-excluded-punctuation-extra-regexp
                        extra-characters))
    (when (stringp regexp)
      (setq str (replace-regexp-in-string regexp "" str))))
  str)

(defun denote--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun denote-letter-case (component args)
  "Apply letter casing specified by COMPONENT to ARGS.
COMPONENT is a symbol representing a file name component, as
described in the user option `denote-file-name-letter-casing'."
  (if (or (eq (alist-get component denote-file-name-letter-casing) 'verbatim)
          (eq (alist-get t denote-file-name-letter-casing) 'verbatim))
      args
    (funcall #'downcase args)))

(defun denote-sluggify (str &optional component)
  "Make STR an appropriate slug for file name COMPONENT.

COMPONENT is a symbol used to retrieve the letter casing method
corresponding to the file name field is references.  COMPONENT is
described in the user option `denote-file-name-letter-casing'.

A nil value of COMPONENT has the same meaning as applying
`downcase' to STR."
  (denote-letter-case component (denote--slug-hyphenate (denote--slug-no-punct str))))

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
  "Make STR an appropriate slug for signatures.
Perform letter casing according to `denote-file-name-letter-casing'."
  (denote-letter-case 'signature (denote--slug-put-equals (denote--slug-no-punct str "-+"))))

(defun denote-sluggify-and-join (str)
  "Sluggify STR while joining separate words."
  (denote-letter-case
   'keywords
   (replace-regexp-in-string
    "-" ""
    (denote--slug-hyphenate (denote--slug-no-punct str)))))

(defun denote-sluggify-keywords (keywords)
  "Sluggify KEYWORDS, which is a list of strings."
  (mapcar #'denote-sluggify-and-join keywords))

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

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun denote-file-has-supported-extension-p (file)
  "Return non-nil if FILE has supported extension.
Also account for the possibility of an added .gpg suffix.
Supported extensions are those implied by `denote-file-type'."
  (seq-some (lambda (e)
              (string-suffix-p e file))
            (denote-file-type-extensions-with-encryption)))

(defun denote-filename-is-note-p (filename)
  "Return non-nil if FILENAME is a valid name for a Denote note.
For our purposes, its path must be part of the variable
`denote-directory', it must have a Denote identifier in its name,
and use one of the extensions implied by `denote-file-type'."
  (and (string-prefix-p (denote-directory) (expand-file-name filename))
       (string-match-p (concat "\\`" denote-id-regexp)
                       (file-name-nondirectory filename))
       (denote-file-has-supported-extension-p filename)))

(defun denote-file-is-note-p (file)
  "Return non-nil if FILE is an actual Denote note.
For our purposes, a note must not be a directory, must satisfy
`file-regular-p' and `denote-filename-is-note-p'."
  (and (not (file-directory-p file))
       (file-regular-p file)
       (denote-filename-is-note-p file)))

(defun denote-file-has-identifier-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (string-match-p (concat "\\`" denote-id-regexp)
                  (file-name-nondirectory file)))

(defun denote-file-has-signature-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (string-match-p denote-signature-regexp
                  (file-name-nondirectory file)))

(make-obsolete 'denote-file-directory-p nil "2.0.0")

(defun denote--file-regular-writable-p (file)
  "Return non-nil if FILE is regular and writable."
  (and (file-regular-p file)
       (file-writable-p file)))

(defun denote-file-is-writable-and-supported-p (file)
  "Return non-nil if FILE is writable and has supported extension."
  (and (denote--file-regular-writable-p file)
       (denote-file-has-supported-extension-p file)))

(defun denote-get-file-name-relative-to-denote-directory (file)
  "Return name of FILE relative to the variable `denote-directory'.
FILE must be an absolute path."
  (when-let ((dir (denote-directory))
             ((file-name-absolute-p file))
             (file-name (expand-file-name file))
             ((string-prefix-p dir file-name)))
    (substring-no-properties file-name (length dir))))

(defun denote-extract-id-from-string (string)
  "Return existing Denote identifier in STRING, else nil."
  (when (string-match denote-id-regexp string)
    (match-string 0 string)))

(define-obsolete-function-alias
  'denote--default-dir-has-denote-prefix
  'denote--dir-in-denote-directory-p
  "2.1.0")

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

(defun denote--directory-get-files ()
  "Return list with full path of valid files in variable `denote-directory'.
Consider files that satisfy `denote-file-has-identifier-p' and
are not backups."
  (mapcar
   #'expand-file-name
   (seq-filter
    (lambda (file)
      (and (denote-file-has-identifier-p file)
           (not (backup-file-name-p file))))
      (denote--directory-all-files-recursively))))

(defun denote-directory-files (&optional files-matching-regexp omit-current text-only)
  "Return list of absolute file paths in variable `denote-directory'.

Files only need to have an identifier.  The return value may thus
include file types that are not implied by `denote-file-type'.

Remember that the variable `denote-directory' accepts a dir-local
value, as explained in its doc string.

With optional FILES-MATCHING-REGEXP, restrict files to those
matching the given regular expression.

With optional OMIT-CURRENT as a non-nil value, do not include the
current Denote file in the returned list.

With optional TEXT-ONLY as a non-nil value, limit the results to
text files that satisfy `denote-file-is-note-p'."
  (let ((files (denote--directory-get-files)))
    (when (and omit-current buffer-file-name (denote-file-has-identifier-p buffer-file-name))
      (setq files (delete buffer-file-name files)))
    (when files-matching-regexp
      (setq files (seq-filter
                   (lambda (f)
                     (string-match-p files-matching-regexp (denote-get-file-name-relative-to-denote-directory f)))
                   files)))
    (when text-only
      (setq files (seq-filter #'denote-file-is-note-p (denote-directory-files))))
    files))

;; NOTE 2023-11-30: We are declaring `denote-directory-text-only-files'
;; obsolete, though we keep it around for the foreseeable future.  It
;; WILL BE REMOVED ahead of version 3.0.0 of Denote, whenever that
;; happens.
(make-obsolete 'denote-directory-text-only-files 'denote-directory-files "2.2.0")

(defun denote-directory-text-only-files ()
  "Return list of text files in variable `denote-directory'.
Filter `denote-directory-files' using `denote-file-is-note-p'."
  (denote-directory-files nil nil :text-only))

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

(defun denote-get-file-extension (file)
  "Return extension of FILE with dot included.
Account for `denote-encryption-file-extensions'.  In other words,
return something like .org.gpg if it is part of the file, else
return .org."
  (let ((outer-extension (file-name-extension file :period)))
    (if-let (((member outer-extension denote-encryption-file-extensions))
             (file (file-name-sans-extension file))
             (inner-extension (file-name-extension file :period)))
        (concat inner-extension outer-extension)
      outer-extension)))

(defun denote-get-file-extension-sans-encryption (file)
  "Return extension of FILE with dot included and without the encryption part.
Build on top of `denote-get-file-extension' though always return
something like .org even if the actual file extension is
.org.gpg."
  (let ((extension (denote-get-file-extension file)))
    (if (string-match (regexp-opt denote-encryption-file-extensions) extension)
        (substring extension 0 (match-beginning 0))
      extension)))

(defun denote-get-path-by-id (id)
  "Return absolute path of ID string in `denote-directory-files'."
  (let ((files
         (seq-filter
          (lambda (file)
            (string-prefix-p id (file-name-nondirectory file)))
          (denote-directory-files))))
    (if (length< files 2)
        (car files)
      (seq-find
       (lambda (file)
         (let ((file-extension (denote-get-file-extension file)))
           (and (denote-file-is-note-p file)
                (or (string= (denote--file-extension denote-file-type)
                             file-extension)
                    (string= ".org" file-extension)
                    (member file-extension (denote-file-type-extensions))))))
       files))))

(defun denote-get-relative-path-by-id (id &optional directory)
  "Return relative path of ID string in `denote-directory-files'.
The path is relative to DIRECTORY (default: ‘default-directory’)."
  (file-relative-name (denote-get-path-by-id id) directory))

;; NOTE 2023-11-30: We are declaring `denote-directory-files-matching-regexp'
;; obsolete, though we keep it around for the foreseeable future.  It
;; WILL BE REMOVED ahead of version 3.0.0 of Denote, whenever that
;; happens.
(make-obsolete 'denote-directory-files-matching-regexp 'denote-directory-files "2.2.0")

(defun denote-directory-files-matching-regexp (regexp)
  "Return list of files matching REGEXP in `denote-directory-files'."
  (denote-directory-files regexp))

;; NOTE 2023-11-30: We are declaring `denote-all-files' obsolete,
;; though we keep it around for the foreseeable future.  It WILL BE
;; REMOVED ahead of version 3.0.0 of Denote, whenever that happens.
(make-obsolete 'denote-all-files 'denote-directory-files "2.2.0")

(defun denote-all-files (&optional omit-current)
  "Return the list of Denote files in variable `denote-directory'.
With optional OMIT-CURRENT, do not include the current Denote
file in the returned list."
  (denote-directory-files nil omit-current nil))

(defvar denote--file-history nil
  "Minibuffer history of `denote-file-prompt'.")

(defun denote-file-prompt (&optional files-matching-regexp)
  "Prompt for file with identifier in variable `denote-directory'.
With optional FILES-MATCHING-REGEXP, filter the candidates per
the given regular expression."
  (let ((files (denote-directory-files files-matching-regexp :omit-current)))
    (completing-read "Select note: " files nil nil nil 'denote--file-history)))

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

(defun denote--inferred-keywords ()
  "Extract keywords from `denote-directory-files'.
This function returns duplicates.  The `denote-keywords' is the
one that doesn't."
  (let ((kw (mapcan #'denote-extract-keywords-from-path (denote-directory-files))))
    (if-let ((regexp denote-excluded-keywords-regexp))
        (seq-remove (apply-partially #'string-match-p regexp) kw)
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
    (format-prompt (or prompt "File keywords") nil)
    keywords nil nil nil 'denote--keyword-history)))

(defun denote-keywords-prompt (&optional prompt-text)
  "Prompt for one or more keywords.
Read entries as separate when they are demarcated by the
`crm-separator', which typically is a comma.

With optional PROMPT-TEXT, use it to prompt the user for
keywords.  Else use a generic prompt.

Process the return value with `denote-keywords-sort' and sort
with `string-collate-lessp' if the user option
`denote-sort-keywords' is non-nil.

Return an empty string if the minibuffer input is empty."
  (if-let ((kw (denote--keywords-crm (denote-keywords) prompt-text)))
      (denote-keywords-sort kw)
    ""))

(defun denote-keywords-sort (keywords)
  "Sort KEYWORDS if `denote-sort-keywords' is non-nil.
KEYWORDS is a list of strings, per `denote-keywords-prompt'."
  (if denote-sort-keywords
      (sort keywords #'string-collate-lessp)
    keywords))

(define-obsolete-function-alias
  'denote--keywords-combine
  'denote-keywords-combine
  "2.1.0")

(defun denote-keywords-combine (keywords)
  "Combine KEYWORDS list of strings into a single string.
Keywords are separated by the underscore character, per the
Denote file-naming scheme."
  (mapconcat
   (lambda (k)
     (denote-letter-case 'keywords k))
   keywords "_"))

(defun denote--keywords-add-to-history (keywords)
  "Append KEYWORDS to `denote--keyword-history'."
  (when (listp keywords)
    (mapc
     (lambda (kw)
       (add-to-history 'denote--keyword-history kw))
     (delete-dups keywords))))

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
  (string-trim s))

(defun denote--trim-quotes (s)
  "Trim quotes around string S."
  (let ((trims "[\"']+"))
    (string-trim s trims trims)))

(defun denote-trim-whitespace-then-quotes (s)
  "Trim whitespace then quotes around string S.
This can be used in `denote-file-types' to format front mattter."
  (denote--trim-quotes (denote-trim-whitespace s)))

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
Split KEYWORDS-STRING into a list of strings.

Consult the `denote-file-types' for how this is used."
  (split-string keywords-string "[:,\s]+" t "[][ \"']+"))

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
  (let ((prop (plist-get
               (alist-get file-type denote-file-types)
               :link)))
    (if (symbolp prop)
        (symbol-value prop)
      prop)))

(defun denote--link-in-context-regexp (file-type)
  "Return link regexp in context based on FILE-TYPE."
  (let ((prop (plist-get
               (alist-get file-type denote-file-types)
               :link-in-context-regexp)))
    (if (symbolp prop)
        (symbol-value prop)
      prop)))

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

(defun denote-retrieve-filename-identifier (file &optional no-error)
  "Extract identifier from FILE name.
If NO-ERROR is nil and an identifier is not found, return an
error, else return nil.

To create a new one, refer to the function
`denote-create-unique-file-identifier'."
  (let ((file-name (file-name-nondirectory file)))
    (if (string-match (concat "\\`" denote-id-regexp) file-name)
        (match-string-no-properties 0 file-name)
      (when (not no-error)
        (error "Cannot find `%s' as a file with a Denote identifier" file)))))

(defun denote-create-unique-file-identifier (file used-ids &optional date)
  "Generate a unique identifier for FILE not in USED-IDS hash-table.

The conditions are as follows:

- If optional DATE is non-nil, invoke
  `denote-prompt-for-date-return-id'.

- If DATE is nil, use the file attributes to determine the last
  modified date and format it as an identifier.

- As a fallback, derive an identifier from the current time.

To only return an existing identifier, refer to the function
`denote-retrieve-filename-identifier'."
  (let ((id (cond
             (date (denote-prompt-for-date-return-id))
             ((denote--file-attributes-time file))
             (t (format-time-string denote-id-format)))))
    (denote--find-first-unused-id id used-ids)))

(define-obsolete-function-alias
  'denote-retrieve-or-create-file-identifier
  'denote-retrieve-filename-identifier
  "2.1.0")

(defun denote-retrieve-filename-keywords (file)
  "Extract keywords from FILE name, if present, else return an empty string.
Return matched keywords as a single string."
  (let ((filename (file-name-nondirectory file)))
    (if (string-match denote-keywords-regexp filename)
        (match-string 1 filename)
      "")))

(defun denote-retrieve-filename-signature (file)
  "Extract signature from FILE name, if present, else return an empty string."
  (let ((filename (file-name-nondirectory file)))
    (if (string-match denote-signature-regexp filename)
        (match-string 1 filename)
      "")))

(defun denote-retrieve-filename-title (file &optional file-name-base-fallback)
  "Extract Denote title component from FILE name, else return an empty string.

With optional FILE-NAME-BASE-FALLBACK return `file-name-base' if
no Denote title component exists.

If the extraction is succcessful (when no `file-name-base' is
involved) run `denote-desluggify' on the title"
  (unless (file-exists-p file)
    (error "%s does not exist as a file" file))
  (cond
   ((and (denote-file-has-identifier-p file)
         (string-match denote-title-regexp file))
    (denote-desluggify (match-string 1 file)))
   (file-name-base-fallback
    (file-name-base file))
   (t "")))

(defun denote--file-with-temp-buffer-subr (file)
  "Return path to FILE or its buffer together with the appropriate function.
Subroutine of `denote--file-with-temp-buffer'."
  (let* ((buffer (get-file-buffer file))
         (file-exists (file-exists-p file))
         (buffer-modified (buffer-modified-p buffer)))
    (cond
     ((or (and file-exists
               buffer
               (not buffer-modified)
               (not (eq buffer-modified 'autosaved)))
          (and file-exists (not buffer)))
      (cons #'insert-file-contents file))
     (buffer
      (cons #'insert-buffer buffer))
     ;; (t
     ;;  (error "Cannot find anything about file `%s'" file))
     )))

(defmacro denote--file-with-temp-buffer (file &rest body)
  "If FILE exists, insert its contents in a temp buffer and call BODY."
  (declare (indent 1))
  `(when-let ((file-and-function (denote--file-with-temp-buffer-subr ,file)))
     (with-temp-buffer
       (funcall (car file-and-function) (cdr file-and-function))
       (goto-char (point-min))
       ,@body)))

(defun denote-retrieve-title-value (file file-type)
  "Return title value from FILE front matter per FILE-TYPE."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
      (funcall (denote--title-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(defun denote-retrieve-title-line (file file-type)
  "Return title line from FILE front matter per FILE-TYPE."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun denote-retrieve-keywords-value (file file-type)
  "Return keywords value from FILE front matter per FILE-TYPE.
The return value is a list of strings.  To get a combined string
the way it would appear in a Denote file name, use
`denote-retrieve-keywords-value-as-string'."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
      (funcall (denote--keywords-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(defun denote-retrieve-keywords-value-as-string (file file-type)
  "Return keywords value from FILE front matter per FILE-TYPE.
The return value is a string, with the underscrore as a separator
between individual keywords.  To get a list of strings instead,
use `denote-retrieve-keywords-value' (the current function uses
that internally)."
  (denote-keywords-combine (denote-retrieve-keywords-value file file-type)))

(defun denote-retrieve-keywords-line (file file-type)
  "Return keywords line from FILE front matter per FILE-TYPE."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun denote--retrieve-title-or-filename (file type)
  "Return appropriate title for FILE given its TYPE."
  (if-let (((denote-file-is-note-p file))
           (title (denote-retrieve-title-value file type))
           ((not (string-blank-p title))))
      title
    (denote-retrieve-filename-title file :file-name-base-as-fallback)))

(defun denote--retrieve-location-in-xrefs (identifier)
  "Return list of xrefs for IDENTIFIER with their respective location.
Limit the search to text files, per `denote-directory-files' with
non-nil `text-only' parameter."
  (mapcar #'xref-match-item-location
          (xref-matches-in-files identifier
                                 (denote-directory-files nil nil :text-only))))

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
   #'string-collate-lessp))

;;;; New note

;;;;; Common helpers for new notes

(defun denote-format-file-name (dir-path id keywords title-slug extension signature-slug)
  "Format file name.
DIR-PATH, ID, KEYWORDS, TITLE-SLUG, EXTENSION and SIGNATURE-SLUG are
expected to be supplied by `denote' or equivalent command.

DIR-PATH is a string pointing to a directory.  It ends with a
forward slash (the function `denote-directory' makes sure this is
the case when returning the value of the variable `denote-directory').
DIR-PATH cannot be nil or an empty string.

ID is a string holding the identifier of the note.  It cannot be
nil or an empty string and must match `denote-id-regexp'.

DIR-PATH and ID form the base file name.

KEYWORDS is a list of strings that is reduced to a single string
by `denote-keywords-combine'.  KEYWORDS can be an empty string or
a nil value, in which case the relevant file name component is
not added to the base file name.

TITLE-SLUG and SIGNATURE-SLUG are strings which, in principle,
are sluggified before passed as arguments here (per
`denote-sluggify' and `denote-sluggify-signature').  They can be
an empty string or a nil value, in which case their respective
file name component is not added to the base file name.

EXTENSION is a string that contains a dot followed by the file
type extension.  It can be an empty string or a nil value, in
which case it is not added to the base file name."
  (cond
   ((null dir-path)
    (error "DIR-PATH must not be nil"))
   ((string-empty-p dir-path)
    (error "DIR-PATH must not be an empty string"))
   ((not (string-suffix-p "/" dir-path))
    (error "DIR-PATH does not end with a / as directories ought to"))
   ((null id)
    (error "ID must not be nil"))
   ((string-empty-p id)
    (error "ID must not be an empty string"))
   ((not (string-match-p denote-id-regexp id))
    (error "ID `%s' does not match `denote-id-regexp'" id)))
  (let ((file-name (concat dir-path id)))
    (when (and signature-slug (not (string-empty-p signature-slug)))
      (setq file-name (concat file-name "==" signature-slug)))
    (when (and title-slug (not (string-empty-p title-slug)))
      (setq file-name (concat file-name "--" title-slug)))
    (when (and keywords (or (listp keywords) (not (string-empty-p keywords))))
      (setq file-name (concat file-name "__" (denote-keywords-combine keywords))))
    (concat file-name extension)))

(defun denote--format-front-matter-title (title file-type)
  "Format TITLE according to FILE-TYPE for the file's front matter."
  (funcall (denote--title-value-function file-type) title))

(defun denote--format-front-matter-keywords (keywords file-type)
  "Format KEYWORDS according to FILE-TYPE for the file's front matter.
Apply `denote-letter-case' to KEYWORDS."
  (let ((kw (denote-sluggify-keywords keywords)))
    (funcall (denote--keywords-value-function file-type) kw)))

(defun denote--format-front-matter (title date keywords id filetype)
  "Front matter for new notes.

TITLE, DATE, and ID are all strings or functions that return a
string.  KEYWORDS is a list of strings.  FILETYPE is one of the
values of `denote-file-type'."
  (let* ((fm (denote--front-matter filetype))
         (title (denote--format-front-matter-title title filetype))
         (kws (denote--format-front-matter-keywords keywords filetype)))
    (if fm (format fm title date kws id) "")))

(defun denote--path (title keywords dir id file-type signature)
  "Return path to new file.
Use ID, TITLE, KEYWORDS, FILE-TYPE and SIGNATURE to construct
path to DIR."
  (denote-format-file-name
   dir id
   (denote-sluggify-keywords keywords)
   (denote-sluggify title 'title)
   (denote--file-extension file-type)
   (denote-sluggify-signature signature)))

;; Adapted from `org-hugo--org-date-time-to-rfc3339' in the `ox-hugo'
;; package: <https://github.com/kaushalmodi/ox-hugo>.
(defun denote-date-rfc3339 (date)
  "Format DATE using the RFC3339 specification."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" date)))

(defun denote-date-org-timestamp (date)
  "Format DATE using the Org inactive timestamp notation."
  (format-time-string "[%F %a %R]" date))

(defun denote-date-iso-8601 (date)
  "Format DATE according to ISO 8601 standard."
  (format-time-string "%F" date))

(defun denote--date (date file-type)
  "Expand DATE in an appropriate format for FILE-TYPE."
  (let ((format denote-date-format))
    (cond
     (format
      (format-time-string format date))
     ((when-let ((fn (denote--date-format-function file-type)))
        (funcall fn date)))
     (t
      (denote-date-org-timestamp date)))))

(defun denote--prepare-note (title keywords date id directory file-type template signature)
  "Prepare a new note file.

Arguments TITLE, KEYWORDS, DATE, ID, DIRECTORY, FILE-TYPE,
TEMPLATE, and SIGNATURE should be valid for note creation."
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
  "Return non-nil if DIRECTORY is in variable `denote-directory'."
  (and directory
       (string-prefix-p (denote-directory)
                        (expand-file-name directory))))

(defun denote--valid-file-type (filetype)
  "Return a valid filetype symbol given the argument FILETYPE.
If none is found, the first element of `denote-file-types' is
returned."
  (let ((type (cond
               ((stringp filetype) (intern filetype))
               ((symbolp filetype) filetype)
               (t (error "The `%s' is neither a string nor a symbol" filetype)))))
    (if (memq type (denote--file-type-keys))
        type
      (caar denote-file-types))))

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
                      ((denote-filename-is-note-p file)))
             file))
         (buffer-list))))

(defun denote--id-exists-p (identifier)
  "Return non-nil if IDENTIFIER already exists."
  (seq-some
   (lambda (file)
     (string-prefix-p identifier (file-name-nondirectory file)))
   (append (denote-directory-files) (denote--buffer-file-names))))

(defun denote--get-all-used-ids ()
  "Return a hash-table of all used identifiers.
It checks files in variable `denote-directory' and active buffer files."
  (let* ((ids (make-hash-table :test 'equal))
         (file-names (mapcar
                      (lambda (file) (file-name-nondirectory file))
                      (denote-directory-files)))
         (names (append file-names (denote--buffer-file-names))))
    (dolist (name names)
      (let ((id (when (string-match (concat "\\`" denote-id-regexp) name)
                  (match-string-no-properties 0 name))))
        (puthash id t ids)))
    ids))

(defun denote--find-first-unused-id (id used-ids)
  "Return the first unused id starting at ID from USED-IDS.
USED-IDS is a hash-table of all used IDs.  If ID is already used,
increment it 1 second at a time until an available id is found."
  (let ((current-id id))
    (while (gethash current-id used-ids)
      (setq current-id (format-time-string denote-id-format (time-add (date-to-time current-id) 1))))
    current-id))

(make-obsolete 'denote-barf-duplicate-id nil "2.1.0")

(defvar denote--command-prompt-history nil
  "Minibuffer history for `denote-command-prompt'.")

(defun denote-command-prompt ()
  "Prompt for command among `denote-commands-for-new-notes'."
  (let ((default (car denote--command-prompt-history)))
    (intern
     (completing-read
      (format-prompt "Run note-creating Denote command" default)
      denote-commands-for-new-notes nil :require-match
      nil 'denote--command-prompt-history default))))

;;;;; The `denote' command and its prompts

;;;###autoload
(defun denote (&optional title keywords file-type subdirectory date template signature)
  "Create a new note with the appropriate metadata and file name.

Run the `denote-after-new-note-hook' after creating the new note.

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
         (id (denote--find-first-unused-id
              (format-time-string denote-id-format date)
              (denote--get-all-used-ids)))
         (directory (if (denote--dir-in-denote-directory-p subdirectory)
                        (file-name-as-directory subdirectory)
                      (denote-directory)))
         (template (if (stringp template)
                       template
                     (or (alist-get template denote-templates) "")))
         (signature (or signature "")))
    (denote--prepare-note title kws date id directory file-type template signature)
    (denote--keywords-add-to-history keywords)
    (run-hooks 'denote-after-new-note-hook)))

(defvar denote--title-history nil
  "Minibuffer history of `denote-title-prompt'.")

(defvar denote-title-prompt-current-default nil
  "Currently bound default title for `denote-title-prompt'.
Set the value of this variable within the lexical scope of a
command that needs to supply a default title before calling
`denote-title-prompt' and use `unwind-protect' to set its value
back to nil.")

(defun denote-title-prompt (&optional default-title prompt-text)
  "Prompt for title string.

With optional DEFAULT-TITLE use it as the initial minibuffer
text.  With optional PROMPT-TEXT use it in the minibuffer instead
of the default prompt.

Previous inputs at this prompt are available for minibuffer
completion.  Consider `savehist-mode' to persist minibuffer
histories between sessions."
  ;; NOTE 2023-10-27: By default SPC performs completion in the
  ;; minibuffer.  We do not want that, as the user should be able to
  ;; input an arbitrary string, while still performing completion
  ;; against their input history.
  (minibuffer-with-setup-hook
      (lambda ()
        (use-local-map
         (let ((map (make-composed-keymap
                     nil (current-local-map))))
           (define-key map (kbd "SPC") nil)
           map)))
    (completing-read
     (format-prompt (or prompt-text "File title") denote-title-prompt-current-default)
     denote--title-history
     nil nil default-title 'denote--title-history denote-title-prompt-current-default)))

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

(defvar denote--signature-history nil
  "Minibuffer history of `denote-signature-prompt'.")

(defun denote-signature-prompt (&optional default-signature prompt-text)
  "Prompt for signature string.
With optional DEFAULT-SIGNATURE use it as the initial minibuffer
text.  With optional PROMPT-TEXT use it in the minibuffer instead
of the default prompt.

Previous inputs at this prompt are available for minibuffer
completion.  Consider `savehist-mode' to persist minibuffer
histories between sessions."
  (when (and default-signature (string-empty-p default-signature))
    (setq default-signature nil))
  ;; NOTE 2023-10-27: By default SPC performs completion in the
  ;; minibuffer.  We do not want that, as the user should be able to
  ;; input an arbitrary string, while still performing completion
  ;; against their input history.
  (minibuffer-with-setup-hook
      (lambda ()
        (use-local-map
         (let ((map (make-composed-keymap
                     nil (current-local-map))))
           (define-key map (kbd "SPC") nil)
           map)))
    (completing-read
     (format-prompt (or prompt-text "Provide signature") nil)
     denote--signature-history
     nil nil default-signature 'denote--signature-history)))

(defvar denote--files-matching-regexp-hist nil
  "Minibuffer history of `denote-files-matching-regexp-prompt'.")

(defun denote-files-matching-regexp-prompt (&optional prompt-text)
  "Prompt for REGEXP to filter Denote files by.
With optional PROMPT-TEXT use it instead of a generic prompt."
  (read-regexp
   (format-prompt (or prompt-text "Match files with the given REGEXP") nil)
   nil 'denote--files-matching-regexp-hist))

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

;;;###autoload
(defun denote-region ()
  "Call `denote' and insert therein the text of the active region.
Prompt for title and keywords.  With no active region, call
`denote' ordinarily (refer to its documentation for the
technicalities)."
  (declare (interactive-only t))
  (interactive)
  (if-let (((region-active-p))
           ;; We capture the text early, otherwise it will be empty
           ;; the moment `insert' is called.
           (text (buffer-substring-no-properties (region-beginning) (region-end))))
      (progn
        (denote (denote-title-prompt) (denote-keywords-prompt))
        (push-mark (point))
        (insert text)
        (run-hook-with-args 'denote-region-after-new-note-functions (mark) (point)))
    (call-interactively 'denote)))

;;;;; Other convenience commands

(defun denote--command-with-default-title (command)
  "Call COMMAND with last input at the `denote-file-prompt'.
Set the `denote-title-prompt-current-default' to the last input.
This is what makes commands such as `denote-open-or-create' or
`denote-link-or-create' get what the user initially typed as the
default value for the title of the new note to be created."
  (let ((last-input (when denote--file-history
                      (pop denote--file-history))))
    (unwind-protect
        (progn
          (setq denote-title-prompt-current-default last-input)
          (call-interactively command))
      (setq denote-title-prompt-current-default nil))))

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
    (denote--command-with-default-title #'denote)))

;;;###autoload
(defun denote-open-or-create-with-command ()
  "Visit TARGET file in variable `denote-directory'.
If file does not exist, invoke `denote' to create a file.

If TARGET file does not exist, add the user input that was used
to search for it to the minibuffer history of the
`denote-file-prompt'.  The user can then retrieve and possibly
further edit their last input, using it as the newly created
note's actual title.  At the `denote-file-prompt' type
\\<minibuffer-local-map>\\[previous-history-element]."
  (declare (interactive-only t))
  (interactive)
  (let ((target (denote-file-prompt)))
    (if (and target (file-exists-p target))
        (find-file target)
      (denote--command-with-default-title (denote-command-prompt)))))

;;;###autoload
(defun denote-keywords-add (keywords)
  "Prompt for KEYWORDS to add to the current note's front matter.
When called from Lisp, KEYWORDS is a list of strings.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'."
  (interactive (list (denote-keywords-prompt)))
  ;; A combination of if-let and let, as we need to take into account
  ;; the scenario in which there are no keywords yet.
  (if-let ((file (buffer-file-name))
           ((denote-file-is-note-p file))
           (file-type (denote-filetype-heuristics file)))
      (let* ((cur-keywords (denote-retrieve-keywords-value file file-type))
             (new-keywords (denote-keywords-sort
                            (seq-uniq (append keywords cur-keywords)))))
        (denote-rewrite-keywords file new-keywords file-type)
        (denote-rename-file-using-front-matter file t))
    (user-error "Buffer not visiting a Denote file")))

(defun denote--keywords-delete-prompt (keywords)
  "Prompt for one or more KEYWORDS.
In the case of multiple entries, those are separated by the
`crm-sepator', which typically is a comma.  In such a case, the
output is sorted with `string-collate-lessp'."
  (let ((choice (denote--keywords-crm keywords "Keywords to remove")))
    (if denote-sort-keywords
        (sort choice #'string-collate-lessp)
      choice)))

;;;###autoload
(defun denote-keywords-remove ()
  "Prompt for keywords in current note and remove them.
Keywords are retrieved from the file's front matter.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'."
  (declare (interactive-only t))
  (interactive)
  (if-let ((file (buffer-file-name))
           ((denote-file-is-note-p file))
           (file-type (denote-filetype-heuristics file)))
      (when-let ((cur-keywords (denote-retrieve-keywords-value file file-type))
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

(defun denote--file-type-org-capture-p ()
  "Return Org `denote-file-type' if this is an `org-capture' buffer."
  (and (bound-and-true-p org-capture-mode)
       (derived-mode-p 'org-mode)
       (string-match-p "\\`CAPTURE.*\\.org" (buffer-name))))

(defun denote-filetype-heuristics (file)
  "Return likely file type of FILE.
Use the file extension to detect the file type of the file.

If more than one file type correspond to this file extension, use
the first file type for which the key-title-kegexp matches in the
file or, if none matches, use the first type with this file
extension in `denote-file-type'.

If no file types in `denote-file-types' has the file extension,
the file type is assumed to be the first of `denote-file-types'."
  (if (denote--file-type-org-capture-p)
      'org
    (let* ((extension (denote-get-file-extension-sans-encryption file))
           (types (denote--file-types-with-extension extension)))
      (cond ((not types)
             (caar denote-file-types))
            ((= (length types) 1)
             (caar types))
            (t
             (or (car (seq-find
                       (lambda (type)
                         (denote--regexp-in-file-p (plist-get (cdr type) :title-key-regexp) file))
                       types))
                 (caar types)))))))

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
                  (denote--dir-in-denote-directory-p default-directory))
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
     ;; FIXME 2023-11-03: The `vc-rename-file' requires the file to be
     ;; saved, but our convention is to not save the buffer after
     ;; changing front matter unless we absolutely have to (allows
     ;; users to do `diff-buffer-with-file', for example).

     ;; ((vc-backend old-name)
     ;;  (vc-rename-file old-name new-name))
     (t
      (rename-file old-name new-name nil)))
    (denote--rename-buffer old-name new-name)))

(defun denote--add-front-matter (file title keywords id file-type)
  "Prepend front matter to FILE if `denote-file-is-note-p'.
The TITLE, KEYWORDS ID, and FILE-TYPE are passed from the
renaming command and are used to construct a new front matter
block if appropriate."
  (when-let ((date (denote--date (date-to-time id) file-type))
             (new-front-matter (denote--format-front-matter title date keywords id file-type)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (insert new-front-matter))))

(defun denote--regexp-in-file-p (regexp file)
  "Return t if REGEXP matches in the FILE."
  (denote--file-with-temp-buffer file
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
`denote-dired-rename-files', or related."
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

(defun denote-rewrite-front-matter (file title keywords file-type &optional no-confirm)
  "Rewrite front matter of note after `denote-rename-file'.
The FILE, TITLE, KEYWORDS, and FILE-TYPE are given by the
renaming command and are used to construct new front matter
values if appropriate.

With optional NO-CONFIRM, do not prompt to confirm the rewriting
of the front matter.  Otherwise produce a `y-or-n-p' prompt to
that effect."
  (when-let ((old-title-line (denote-retrieve-title-line file file-type))
             (old-keywords-line (denote-retrieve-keywords-line file file-type))
             (new-title-line (denote--get-title-line-from-front-matter title file-type))
             (new-keywords-line (denote--get-keywords-line-from-front-matter keywords file-type)))
    (with-current-buffer (find-file-noselect file)
      (when (or no-confirm
                (y-or-n-p (format
                           "Replace front matter?\n-%s\n+%s\n\n-%s\n+%s?"
                           (propertize old-title-line 'face 'denote-faces-prompt-old-name)
                           (propertize new-title-line 'face 'denote-faces-prompt-new-name)
                           (propertize old-keywords-line 'face 'denote-faces-prompt-old-name)
                           (propertize new-keywords-line 'face 'denote-faces-prompt-new-name))))
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
Throw error if FILE is not regular, else return FILE."
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
             (propertize (file-name-nondirectory old-name) 'face 'denote-faces-prompt-old-name)
             (propertize (file-name-nondirectory new-name) 'face 'denote-faces-prompt-new-name)))))

;; NOTE 2023-10-20: We do not need a user option for this, though it
;; can be useful to have it as a variable.
(defvar denote-rename-max-mini-window-height 0.33
  "How much to enlarge `max-mini-window-height' for renaming operations.")

;;;###autoload
(defun denote-rename-file (file title keywords signature &optional ask-date)
  "Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.  When called from Lisp, FILE
is a filesystem path represented as a string.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE, KEYWORDS, and SIGNATURE components of the
file name.

Else create an identifier based on the following conditions:

1. If optional ASK-DATE is non-nil (such as with a prefix
   argument), prompt for a date and use it to derive the
   identifier.

2. If optional ASK-DATE is nil (this is the case without a prefix
   argument), use the file attributes to determine the last
   modified date and format it as an identifier.

3. As a fallback, derive an identifier from the current time.

4. If the resulting identifier is not unique among the files in
   the variable `denote-directory', increment it such that it
   becomes unique.

Add TITLE to FILE.  In interactive use, prompt for user input and
retrieve the default TITLE value from a line starting with a
title field in the file's contents, depending on the given file
type (e.g. #+title for Org).  Else, use the file name as a
default value at the minibuffer prompt.  When called from Lisp,
TITLE is a string.

If TITLE is nil or an empty string, do not add it to a newly
renamed file or remove it from an existing file.

Add SIGNATURE to FILE.  In interactive use, prompt for SIGNATURE,
using an existing one as the default value at the minibuffer
prompt.  When called from Lisp, SIGNATURE is a string.

If SIGNATURE is nil or an empty string, do not add it to a newly
renamed file or remove it from an existing file.

Add KEYWORDS to FILE.  In interactive use, prompt for KEYWORDS.
More than one keyword can be inserted when separated by the
`crm-sepator' (normally a comma).  When called from Lisp,
KEYWORDS is a list of strings.

If KEYWORDS is nil or an empty string, do not add it to a newly
renamed file or remove it from an existing file.

Read the file type extension (like .txt) from the underlying file
and preserve it through the renaming process.  Files that have no
extension are left without one.

Renaming only occurs relative to the current directory.  Files
are not moved between directories.

As a final step after the FILE, TITLE, KEYWORDS, and SIGNATURE
are collected, ask for confirmation, showing the difference
between old and new file names.  Do not ask for confirmation if
the user option `denote-rename-no-confirm' is set to a non-nil
value.

If the FILE has Denote-style front matter for the TITLE and
KEYWORDS, ask to rewrite their values in order to reflect the new
input (this step always requires confirmation and the underlying
buffer is not saved, so consider invoking `diff-buffer-with-file'
to double-check the effect).  The rewrite of the TITLE and
KEYWORDS in the front matter should not affect the rest of the
front matter.

If the file does not have front matter but is among the supported
file types (per `denote-file-type'), add front matter at the top
of it and leave the buffer unsaved for further inspection.

For the front matter of each file type, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'

This command is intended to (i) rename existing Denote notes
while updating their title and keywords in the front matter, (ii)
convert existing supported file types to Denote notes, and (ii)
rename non-note files (e.g. PDF) that can benefit from Denote's
file-naming scheme."
  (interactive
   (let* ((file (denote--rename-dired-file-or-prompt))
          (file-type (denote-filetype-heuristics file))
          (file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name)))
     (list
      file
      (denote-title-prompt
       (denote--retrieve-title-or-filename file file-type)
       (format "Rename `%s' with title (empty to ignore/remove)" file-in-prompt))
      (denote-keywords-prompt
       (format "Rename `%s' with keywords (empty to ignore/remove)" file-in-prompt))
      (denote-signature-prompt
       (denote-retrieve-filename-signature file)
       (format "Rename `%s' with signature (empty to ignore/remove)" file-in-prompt))
      current-prefix-arg)))
  (let* ((dir (file-name-directory file))
         (id (or (denote-retrieve-filename-identifier file :no-error)
                 (denote-create-unique-file-identifier file (denote--get-all-used-ids) ask-date)))
         (extension (denote-get-file-extension file))
         (file-type (denote-filetype-heuristics file))
         (title (or title (denote--retrieve-title-or-filename file file-type)))
         (keywords (or keywords (denote-retrieve-keywords-value file file-type)))
         (signature (or signature (denote-retrieve-filename-signature file)))
         (new-name (denote-format-file-name dir id keywords (denote-sluggify title 'title) extension (denote-sluggify-signature signature)))
         (max-mini-window-height denote-rename-max-mini-window-height))
    (when (or denote-rename-no-confirm (denote-rename-file-prompt file new-name))
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (when (denote-file-is-writable-and-supported-p new-name)
        (if (denote--edit-front-matter-p new-name file-type)
            (denote-rewrite-front-matter new-name title keywords file-type denote-rename-no-confirm)
          (denote--add-front-matter new-name title keywords id file-type))))
    new-name))

;;;###autoload
(defun denote-dired-rename-files ()
  "Rename Dired marked files same way as `denote-rename-file'.
Rename each file in sequence, making all the relevant prompts.
Unlike `denote-rename-file', do not prompt for confirmation of
the changes made to the file: perform them outright."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files)))
      (let ((used-ids (when (seq-some
                             (lambda (m)
                               (not (denote-retrieve-filename-identifier m :no-error)))
                             marks)
                        (denote--get-all-used-ids))))
        (dolist (file marks)
          (let* ((file-type (denote-filetype-heuristics file))
                 (file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name))
                 (dir (file-name-directory file))
                 (id (or (denote-retrieve-filename-identifier file :no-error)
                         (denote-create-unique-file-identifier file used-ids)))
                 (title (denote-title-prompt
                         (denote--retrieve-title-or-filename file file-type)
                         (format "Rename `%s' with title (empty to ignore/remove)" file-in-prompt)))
                 (keywords (denote-keywords-prompt
                            (format "Rename `%s' with keywords (empty to ignore/remove)" file-in-prompt)))
                 (signature (denote-signature-prompt
                             (denote-retrieve-filename-signature file)
                             (format "Rename `%s' with signature (empty to ignore/remove)" file-in-prompt)))
                 (extension (denote-get-file-extension file))
                 (new-name (denote-format-file-name dir id keywords (denote-sluggify title 'title) extension (denote-sluggify-signature signature))))
            (denote-rename-file-and-buffer file new-name)
            (when (denote-file-is-writable-and-supported-p new-name)
              (if (denote--edit-front-matter-p new-name file-type)
                  (denote-rewrite-front-matter new-name title keywords file-type denote-rename-no-confirm)
                (denote--add-front-matter new-name title keywords id file-type)))
            (when used-ids
              (puthash id t used-ids))))
        (denote-update-dired-buffers))
    (user-error "No marked files; aborting")))

(make-obsolete
 'denote-dired-rename-marked-files
 'denote-dired-rename-marked-files-with-keywords
 "2.1.0")

;;;###autoload
(defun denote-dired-rename-marked-files-with-keywords ()
  "Rename marked files in Dired to a Denote file name by writing keywords.

Specifically, do the following:

- retain the file's existing name and make it the TITLE field,
  per Denote's file-naming scheme;

- `denote-letter-case' and sluggify the TITLE, according to our
  conventions (check the user option `denote-file-name-letter-casing');

- prepend an identifier to the TITLE;

- preserve the file's extension, if any;

- prompt once for KEYWORDS and apply the user's input to the
  corresponding field in the file name, rewriting any keywords
  that may exist while removing keywords that do exist if
  KEYWORDS is empty;

- add or rewrite existing front matter to the underlying file, if
  it is recognized as a Denote note (per `denote-file-type'),
  such that it includes the new keywords.

[ Note that the affected buffers are not saved.  Users can thus
  check them to confirm that the new front matter does not cause
  any problems (e.g. with the `diff-buffer-with-file' command).
  Multiple buffers can be saved in one go with the command
  `save-some-buffers' (read its doc string).  ]"
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files)))
      (let ((keywords (denote-keywords-prompt "Rename marked files with keywords, overwriting existing (empty to ignore/remove)"))
            (used-ids (when (seq-some
                             (lambda (m) (not (denote-retrieve-filename-identifier m :no-error)))
                             marks)
                        (denote--get-all-used-ids))))
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (or (denote-retrieve-filename-identifier file :no-error)
                         (denote-create-unique-file-identifier file used-ids)))
                 (signature (denote-retrieve-filename-signature file))
                 (file-type (denote-filetype-heuristics file))
                 (title (denote--retrieve-title-or-filename file file-type))
                 (extension (denote-get-file-extension file))
                 (new-name (denote-format-file-name dir id keywords (denote-sluggify title 'title) extension (denote-sluggify-signature signature))))
            (denote-rename-file-and-buffer file new-name)
            (when (denote-file-is-writable-and-supported-p new-name)
              (if (denote--edit-front-matter-p new-name file-type)
                  (denote-rewrite-keywords new-name keywords file-type)
                (denote--add-front-matter new-name title keywords id file-type)))
            (when used-ids
              (puthash id t used-ids))))
        (denote-update-dired-buffers))
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

If AUTO-CONFIRM is non-nil, then proceed with the renaming
operation without prompting for confirmation.  This is what the
command `denote-dired-rename-marked-files-using-front-matter'
does internally."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (unless (denote-file-is-writable-and-supported-p file)
    (user-error "The file is not writable or does not have a supported file extension"))
  (if-let ((file-type (denote-filetype-heuristics file))
           (title (denote-retrieve-title-value file file-type))
           (id (denote-retrieve-filename-identifier file :no-error)))
      (let* ((sluggified-title (denote-sluggify title 'title))
             (keywords (denote-retrieve-keywords-value file file-type))
             (signature (denote-retrieve-filename-signature file))
             (extension (denote-get-file-extension file))
             (dir (file-name-directory file))
             (new-name (denote-format-file-name dir id keywords sluggified-title extension (when signature (denote-sluggify-signature signature)))))
        (when (or auto-confirm
                  (denote-rename-file-prompt file new-name))
          (denote-rename-file-and-buffer file new-name)
          (denote-update-dired-buffers)))
    (user-error "No identifier or front matter for title")))

;;;###autoload
(defun denote-dired-rename-marked-files-using-front-matter ()
  "Call `denote-rename-file-using-front-matter' over the Dired marked files.
Refer to the documentation of that command for the technicalities.

Marked files must count as notes for the purposes of Denote,
which means that they at least have an identifier in their file
name and use a supported file type, per `denote-file-type'.
Files that do not meet this criterion are ignored because Denote
cannot know if they have front matter and what that may be."
  (interactive nil dired-mode)
  (if-let ((marks (seq-filter
                   (lambda (m)
                     (and (denote-file-is-writable-and-supported-p m)
                          (denote-retrieve-filename-identifier m :no-error)))
                   (dired-get-marked-files))))
      (progn
        (dolist (file marks)
          (denote-rename-file-using-front-matter file :auto-confirm))
        (denote-update-dired-buffers))
    (user-error "No marked Denote files; aborting")))

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
  (when (and (denote-file-is-writable-and-supported-p file)
             (denote-retrieve-filename-identifier file :no-error))
    (denote--add-front-matter
     file title keywords
     (denote-retrieve-filename-identifier file)
     (denote-filetype-heuristics file))))

(define-obsolete-function-alias
  'denote-change-file-type
  'denote-change-file-type-and-front-matter
  "2.1.0")

;;;###autoload
(defun denote-change-file-type-and-front-matter (file new-file-type)
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
         (id (or (denote-retrieve-filename-identifier file :no-error) ""))
         (title (denote--retrieve-title-or-filename file old-file-type))
         (keywords (denote-retrieve-keywords-value file old-file-type))
         (signature (denote-retrieve-filename-signature file))
         (old-extension (denote-get-file-extension file))
         (new-extension (denote--file-extension new-file-type))
         (new-name (denote-format-file-name
                    dir id keywords (denote-sluggify title 'title) new-extension signature))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (when (and (not (eq old-extension new-extension))
               (denote-rename-file-prompt file new-name))
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (when (denote-file-is-writable-and-supported-p new-name)
        (denote--add-front-matter new-name title keywords id new-file-type)))))

;;;; The Denote faces

(defgroup denote-faces ()
  "Faces for Denote."
  :group 'denote)

(defface denote-faces-link '((t :inherit link))
  "Face used to style Denote links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

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

(defface denote-faces-time-delimiter '((t :inherit shadow))
  "Face for the delimiter between date and time in Dired buffers."
  :group 'denote-faces
  :package-version '(denote . "2.1.0"))

;; For character classes, evaluate: (info "(elisp) Char Classes")
(defvar denote-faces--file-name-regexp
  (concat "\\(?1:[0-9]\\{8\\}\\)\\(?10:T\\)\\(?2:[0-9]\\{6\\}\\)"
          "\\(?:\\(?3:==\\)\\(?4:[[:alnum:][:nonascii:]=]*?\\)\\)?"
          "\\(?:\\(?5:--\\)\\(?6:[[:alnum:][:nonascii:]-]*?\\)\\)?"
          "\\(?:\\(?7:__\\)\\(?8:[[:alnum:][:nonascii:]_-]*?\\)\\)?"
          "\\(?9:\\..*\\)?$")
  "Regexp of file names for fontification.")

(defconst denote-faces-file-name-keywords
  `((,(concat "\\(?11:[\t\s]+\\|.*/\\)?" denote-faces--file-name-regexp)
     (11 'denote-faces-subdirectory nil t)
     (1 'denote-faces-date)
     (10 'denote-faces-time-delimiter nil t)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-signature nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-title nil t)
     (7 'denote-faces-delimiter nil t)
     (8 'denote-faces-keywords nil t)
     (9 'denote-faces-extension nil t )))
  "Keywords for fontification of file names.")

(make-obsolete-variable 'denote-faces-file-name-keywords-for-backlinks nil "2.2.0")

(defface denote-faces-prompt-old-name '((t :inherit error))
  "Face for the old name shown in the prompt of `denote-rename-file' etc."
  :group 'denote-faces
  :package-version '(denote . "2.2.0"))

(defface denote-faces-prompt-new-name '((t :inherit success))
  "Face for the new name shown in the prompt of `denote-rename-file' etc."
  :group 'denote-faces
  :package-version '(denote . "2.2.0"))

(defface denote-faces-prompt-current-name '((t :inherit denote-faces-prompt-old-name))
  "Face for the current file shown in the prompt of `denote-rename-file' etc."
  :group 'denote-faces
  :package-version '(denote . "2.2.0"))

;;;; Fontification in Dired

(defgroup denote-dired ()
  "Integration between Denote and Dired."
  :group 'denote)

(defcustom denote-dired-directories (list denote-directory)
  "List of directories where `denote-dired-mode' should apply to.
For this to take effect, add `denote-dired-mode-in-directories',
to the `dired-mode-hook'.

If `denote-dired-directories-include-subdirectories' is non-nil,
also apply the effect to all subdirectories of those specified in
the list."
  :type '(repeat directory)
  :package-version '(denote . "0.1.0")
  :link '(info-link "(denote) Fontification in Dired")
  :group 'denote-dired)

(defcustom denote-dired-directories-include-subdirectories nil
  "If non-nil `denote-dired-directories' also affects all subdirectories.
Otherwise `denote-dired-directories' works only with exact matches."
  :package-version '(denote . "2.2.0")
  :link '(info-link "(denote) Fontification in Dired")
  :type 'boolean
  :group 'denote-dired)

;; FIXME 2022-08-12: Make `denote-dired-mode' work with diredfl.  This
;; may prove challenging.

(defun denote-dired-add-font-lock (&rest _)
  "Append `denote-faces-file-name-keywords' to font lock keywords."
  ;; NOTE 2023-10-28: I tried to add the first argument and then
  ;; experimented with various combinations of keywords, such as
  ;; `(,@dired-font-lock-keywords ,@denote-faces-file-name-keywords).
  ;; None of them could be unset upon disabling `denote-dired-mode'.
  ;; As such, I am using the `when' here.
  (when (derived-mode-p 'dired-mode)
    (font-lock-add-keywords nil denote-faces-file-name-keywords t)))

(defun denote-dired-remove-font-lock (&rest _)
  "Remove `denote-faces-file-name-keywords' from font lock keywords."
  ;; See NOTE in `denote-dired-add-font-lock'.
  (when (derived-mode-p 'dired-mode)
    (font-lock-remove-keywords nil denote-faces-file-name-keywords)))

(declare-function wdired-change-to-wdired-mode "wdired")
(declare-function wdired-finish-edit "wdired")

;;;###autoload
(define-minor-mode denote-dired-mode
  "Fontify all Denote-style file names.
Add this or `denote-dired-mode-in-directories' to
`dired-mode-hook'."
  :global nil
  :group 'denote-dired
  (if denote-dired-mode
      (progn
        (denote-dired-add-font-lock)
        (advice-add #'wdired-change-to-wdired-mode :after #'denote-dired-add-font-lock)
        (advice-add #'wdired-finish-edit :after #'denote-dired-add-font-lock))
    (denote-dired-remove-font-lock)
    (advice-remove #'wdired-change-to-wdired-mode #'denote-dired-add-font-lock)
    (advice-remove #'wdired-finish-edit #'denote-dired-add-font-lock))
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
Add this function to `dired-mode-hook'.

If `denote-dired-directories-include-subdirectories' is non-nil,
also enable it in all subdirectories."
  (when-let ((dirs (denote-dired--modes-dirs-as-dirs))
             ;; Also include subdirs
             ((or (member (file-truename default-directory) dirs)
                  (and denote-dired-directories-include-subdirectories
                       (seq-some
                        (lambda (dir)
                          (string-prefix-p dir (file-truename default-directory)))
                        dirs)))))
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

(defvar denote-org-link-format "[[denote:%s][%s]]"
  "Format of Org link to note.
The value is passed to `format' with IDENTIFIER and TITLE
arguments, in this order.

Also see `denote-org-link-in-context-regexp'.")

(defvar denote-md-link-format "[%2$s](denote:%1$s)"
  "Format of Markdown link to note.
The %N$s notation used in the default value is for `format' as
the supplied arguments are IDENTIFIER and TITLE, in this order.

Also see `denote-md-link-in-context-regexp'.")

(defvar denote-id-only-link-format "[[denote:%s]]"
  "Format of identifier-only link to note.
The value is passed to `format' with IDENTIFIER as its sole
argument.

Also see `denote-id-only-link-in-context-regexp'.")

(defvar denote-org-link-in-context-regexp
  (concat "\\[\\[" "denote:"  "\\(?1:" denote-id-regexp "\\)" "]" "\\[.*?]]")
  "Regexp to match an Org link in its context.
The format of such links is `denote-org-link-format'.")

(defvar denote-md-link-in-context-regexp
  (concat "\\[.*?]" "(denote:"  "\\(?1:" denote-id-regexp "\\)" ")")
  "Regexp to match a Markdown link in its context.
The format of such links is `denote-md-link-format'.")

(defvar denote-id-only-link-in-context-regexp
  (concat "\\[\\[" "denote:"  "\\(?1:" denote-id-regexp "\\)" "]]")
  "Regexp to match an identifier-only link in its context.
The format of such links is `denote-id-only-link-format'."  )

(defun denote-link--file-type-format (file-type id-only)
  "Return link format based on FILE-TYPE.
With non-nil ID-ONLY, use the generic link format without a
title.

Fall back to `denote-org-link-format'."
  (cond
   (id-only denote-id-only-link-format)
   ((when-let ((link (denote--link-format file-type)))
      link))
   ;; Plain text also uses [[denote:ID][TITLE]]
   (t denote-org-link-format)))

(defun denote-format-link (file format description)
  "Prepare link to FILE using FORMAT and DESCRIPTION text.
FILE is the path to a file name.  FORMAT is the symbol of a
variable that specifies a string.  See the `:link' property of
`denote-file-types'.

DESCRIPTION is the text of the link.  If nil, DESCRIPTION is
retrieved from the FILE, unless the FORMAT is
`denote-id-only-link-format'."
  (let* ((file-id (denote-retrieve-filename-identifier file))
         (file-type (denote-filetype-heuristics file))
         (file-title (unless (string= format denote-id-only-link-format)
                       (or description (denote--retrieve-title-or-filename file file-type)))))
    (format format file-id file-title)))

(make-obsolete 'denote-link--format-link 'denote-format-link "2.1.0")

(defun denote--link-get-description (file file-type)
  "Return description for `denote-link'.
If the region is active, make the description the text within the
region's boundaries.  Else retrieve the title from FILE, given
FILE-TYPE.

Also see `denote--link-get-description-with-signature'."
  (if-let (((region-active-p))
           (beg (region-beginning))
           (end (region-end))
           (selected-text (string-trim (buffer-substring-no-properties beg end))))
      (progn
        (delete-region beg end)
        selected-text)
    (denote--retrieve-title-or-filename file file-type)))

;;;###autoload
(defun denote-link (file file-type description &optional id-only)
  "Create link to FILE note in variable `denote-directory' with DESCRIPTION.

When called interactively, prompt for FILE using completion.  In
this case, derive FILE-TYPE from the selected FILE, as well as
the DESCRIPTION from the title of FILE.  The title comes either
from the front matter or the file name.  With an active region,
the DESCRIPTION is the text of the region, despite the
aforementioned.  If active region is empty (i.e whitespace-only),
insert an ID-ONLY link.

With optional ID-ONLY as a non-nil argument, such as with a
universal prefix (\\[universal-argument]), insert links with just
the identifier and no further description.  In this case, the
link format is always [[denote:IDENTIFIER]].

When called from Lisp, FILE is a string representing a full file
system path.  FILE-TYPE is a symbol as described in
`denote-file-type'.  DESCRIPTION is a string.  Whether the caller
treats the active region specially, is up to it."
  (interactive
   (let ((file (denote-file-prompt))
         (type (denote-filetype-heuristics (buffer-file-name))))
     (list
      file
      type
      (denote--link-get-description file type)
      current-prefix-arg)))
  (let* ((beg (point))
         (identifier-only (or id-only (string-empty-p description))))
    (insert
     (denote-format-link
      file
      (denote-link--file-type-format file-type identifier-only)
      description))
    (unless (derived-mode-p 'org-mode)
      (make-button beg (point) 'type 'denote-link-button))))

(define-obsolete-function-alias
  'denote-link-insert-link
  'denote-insert-link
  "2.0.0")

(defalias 'denote-insert-link 'denote-link
  "Alias for `denote-link' command.")

;; NOTE 2023-12-05 04:16 +0200: This is a candidate for a user option,
;; subject to feedback.  I think the signature should be better
;; disambiguated in this context, although the double space is a good
;; start.
(defvar denote--link-signature-format "%s  %s"
  "Format of link description for `denote-link-with-signature'.")

(defun denote--link-get-description-with-signature (file file-type)
  "Return `denote-link-with-signature' description.
Retrieve the title and signature from FILE with FILE-TYPE.  If
the region is active, use it to describe the link instead of the
file's title.  Make the signature a prefix.  If there is no title
or text in the active region, return the signature on its own.

Also see `denote--link-get-description'."
  (let* ((signature (denote-retrieve-filename-signature file))
         (text (denote--link-get-description file file-type))
         (specifiers (if (and text
                              (not (string-empty-p text)))
                         denote--link-signature-format
                       "%s")))
    (format specifiers signature text)))

;;;###autoload
(defun denote-link-with-signature ()
  "Insert link to file with signature.
Prompt for file using minibuffer completion, limiting the list of
candidates to files with a signature in their file name.

The description of the link includes the signature followed by
the file's title, if any.  For this case, the signature is
assumed present.

For more advanced uses with Lisp, refer to the `denote-link'
function."
  (declare (interactive-only t))
  (interactive)
  (let ((file (denote-file-prompt "="))
        (type (denote-filetype-heuristics (buffer-file-name))))
    (denote-link file type (denote--link-get-description-with-signature file type))))

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
        found-files)
    (dolist (file files)
      (dolist (i (denote-link--collect-identifiers regexp))
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
  (when-let ((current-file (or file (buffer-file-name)))
             ((denote-file-has-supported-extension-p current-file))
             (file-type (denote-filetype-heuristics current-file))
             (regexp (denote--link-in-context-regexp file-type)))
    (with-current-buffer (find-file-noselect current-file)
      (denote-link--expand-identifiers regexp))))

(defalias 'denote-link-return-forelinks 'denote-link-return-links
  "Alias for `denote-link-return-links'.")

(define-obsolete-function-alias
  'denote-link-find-file
  'denote-find-link
  "2.0.0")

;;;###autoload
(defun denote-find-link ()
  "Use minibuffer completion to visit linked file."
  (declare (interactive-only t))
  (interactive)
  (find-file
   (denote-link--find-file-prompt
    (or (denote-link-return-links)
        (user-error "No links found")))))

(defun denote-link-return-backlinks (&optional file)
  "Return list of backlinks in current or optional FILE.
Also see `denote-link-return-links'."
  (when-let ((current-file (or file (buffer-file-name)))
             (id (denote-retrieve-filename-identifier current-file)))
    (delete current-file (denote--retrieve-files-in-xrefs id))))

(define-obsolete-function-alias
  'denote-link-find-backlink
  'denote-find-backlink
  "2.0.0")

;;;###autoload
(defun denote-find-backlink ()
  "Use minibuffer completion to visit backlink to current file.

Like `denote-find-link', but select backlink to follow."
  (declare (interactive-only t))
  (interactive)
  (find-file
   (denote-get-path-by-id
    (denote-extract-id-from-string
     (denote-link--find-file-prompt
      (or (denote-link-return-backlinks)
          (user-error "No backlinks found")))))))

(defun denote--link-after-creating-subr (command description-fn &optional id-only)
  "Subroutine for `denote-link-after-creating' and the like.
COMMAND is the symbol of a file-creating command to call, such as
`denote' or `denote-signature'.

DESCRIPTION-FN is the symbol of a function that returns the
description of a link, like `denote--link-get-description' or
`denote--link-get-description-with-signature'.

ID-ONLY has the same meaning as described in `denote-link'."
  (let (path)
    (save-window-excursion
      (call-interactively command)
      (save-buffer)
      (setq path (buffer-file-name)))
    (let ((type (denote-filetype-heuristics path)))
      (denote-link path type (funcall description-fn path type) id-only))))

;;;###autoload
(defun denote-link-after-creating (&optional id-only)
  "Create new note in the background and link to it directly.

Use `denote' interactively to produce the new note.  Its doc
string explains which prompts will be used and under what
conditions.

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'.

For a variant of this, see `denote-link-after-creating-with-command'.

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
  (denote--link-after-creating-subr #'denote #'denote--link-get-description id-only))

;;;###autoload
(defun denote-link-after-creating-with-command (command &optional id-only)
  "Like `denote-link-after-creating' but prompt for note-making COMMAND.
Use this to, for example, call `denote-signature' so that the
newly created note has a signature as part of its file name.

Optional ID-ONLY has the same meaning as in the command
`denote-link-after-creating'."
  (interactive
   (list
    (denote-command-prompt)
    current-prefix-arg))
  (denote--link-after-creating-subr
   command
   (if (eq command 'denote-signature)
       #'denote--link-get-description-with-signature
     #'denote--link-get-description)
   id-only))

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
      (let ((type (denote-filetype-heuristics target)))
        (denote-link
         target
         type
         (denote--link-get-description target type)
         id-only))
    (denote--command-with-default-title #'denote-link-after-creating)))

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
             buffer-file-name
             (denote-file-has-identifier-p buffer-file-name))
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

(defun denote-backlinks-next (n)
  "Use appropriate command for forward motion in backlinks buffer.
With N as a numeric argument, move to the Nth button from point.
A nil value of N is understood as 1.

When `denote-backlinks-show-context' is nil, move between files
in the backlinks buffer.

When `denote-backlinks-show-context' is non-nil move between
matching identifiers."
  (interactive "p" denote-backlinks-mode)
  (unless (derived-mode-p 'denote-backlinks-mode)
    (user-error "Only use this in a Denote backlinks buffer"))
  (if denote-backlinks-show-context
      (xref-next-line)
    (forward-button n)))

(defun denote-backlinks-prev (n)
  "Use appropriate command for backward motion in backlinks buffer.
With N as a numeric argument, move to the Nth button from point.
A nil value of N is understood as 1.

When `denote-backlinks-show-context' is nil, move between files
in the backlinks buffer.

When `denote-backlinks-show-context' is non-nil move between
matching identifiers."
  (interactive "p" denote-backlinks-mode)
  (unless (derived-mode-p 'denote-backlinks-mode)
    (user-error "Only use this in a Denote backlinks buffer"))
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

(define-derived-mode denote-backlinks-mode xref--xref-buffer-mode "Backlinks"
  "Major mode for backlinks buffers."
  (unless denote-backlinks-show-context
    (font-lock-add-keywords nil denote-faces-file-name-keywords t))
  (add-hook 'project-find-functions #'denote-project-find nil t))

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
      (when-let  ((title (denote-retrieve-title-value file file-type))
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
                                        (denote-directory-files nil :omit-current :text-only))
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
                          (denote-directory-files nil :omit-current :text-only))
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

(defun denote-link--prepare-links (files current-file-type id-only &optional no-sort)
  "Prepare links to FILES from CURRENT-FILE-TYPE.
When ID-ONLY is non-nil, use a generic link format.  See
`denote-link--file-type-format'.

With optional NO-SORT do not try to sort the inserted lines.
Otherwise sort lines while accounting for `denote-link-add-links-sort'."
  (with-temp-buffer
    (mapc
     (lambda (file)
       (insert
        (format
         denote-link--prepare-links-format
         (denote-format-link
          file
          (denote-link--file-type-format current-file-type id-only)
          (let ((type (denote-filetype-heuristics file)))
            (if (denote-file-has-signature-p file)
                (denote--link-get-description-with-signature file type)
              (denote--link-get-description file type)))))))
          files)
    (unless no-sort
      (sort-lines denote-link-add-links-sort (point-min) (point-max)))
    (buffer-string)))

(define-obsolete-function-alias
  'denote-link-add-links
  'denote-add-links
  "2.0.0")

(defun denote-link--insert-links (files current-file-type &optional id-only no-sort)
  "Insert at point a typographic list of links matching FILES.

With CURRENT-FILE-TYPE as a symbol among those specified in
`denote-file-type' (or the `car' of each element in
`denote-file-types'), format the link accordingly.  With a nil or
unknown non-nil value, default to the Org notation.

With ID-ONLY as a non-nil value, produce links that consist only
of the identifier, thus deviating from CURRENT-FILE-TYPE.

Optional NO-SORT is passed to `denote-link--prepare-links'."
  (insert (denote-link--prepare-links files current-file-type id-only no-sort)))

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
    (denote-files-matching-regexp-prompt "Insert links matching REGEXP")
    current-prefix-arg))
  (let ((file-type (denote-filetype-heuristics (buffer-file-name))))
    (if-let ((files (denote-directory-files regexp :omit-current))
             (beg (point)))
        (progn
          (denote-link--insert-links files file-type id-only)
          (denote-link-buttonize-buffer beg (point)))
      (message "No links matching `%s'" regexp))))

(defalias 'denote-link-insert-links-matching-regexp 'denote-add-links
  "Alias for `denote-add-links' command.")

(define-obsolete-function-alias
  'denote-link-add-missing-links
  'denote-add-missing-links
  "2.0.0")

(make-obsolete 'denote-add-missing-links nil "2.2.0")

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
  (when (denote--dir-in-denote-directory-p default-directory)
    (seq-filter #'denote-file-is-note-p (dired-get-marked-files))))

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
    ["Open a note or create it if missing" denote-open-or-create
     :help "Open an existing note in the `denote-directory' or create it if missing"]
    ["Open a note or create it with the chosen command" denote-open-or-create-with-command
     :help "Open an existing note or create it with the chosen command if missing"]
    "---"
    ["Rename a file" denote-rename-file
     :help "Rename file interactively"
     :enable (derived-mode-p 'dired-mode 'text-mode)]
    ["Rename this file using its front matter" denote-rename-file-using-front-matter
     :help "Rename the current file using its front matter as input"
     :enable (derived-mode-p 'text-mode)]
    ["Rename Dired marked files interactively" denote-dired-rename-files
     :help "Rename marked files in Dired by prompting for all file name components"
     :enable (derived-mode-p 'dired-mode)]
    ["Rename Dired marked files with keywords" denote-dired-rename-marked-files-with-keywords
     :help "Rename marked files in Dired by prompting for keywords"
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
    ["Link to existing note or newly created one" denote-link-or-create
     :help "Insert a link to an existing file, else create it and link to it"
     :enable (derived-mode-p 'text-mode)]
    ["Link to existing note or newly created one with the chosen command" denote-link-or-create-with-command
     :help "Insert a link to an existing file, else create it with the given command and link to it"
     :enable (derived-mode-p 'text-mode)]
    ["Create note in the background and link to it directly" denote-link-after-creating
     :help "Create new note and link to it from the current file"
     :enable (derived-mode-p 'text-mode)]
    ["Create note in the background with chosen command and link to it directly" denote-link-after-creating-with-command
     :help "Create new note with the chosen command and link to it from the current file"
     :enable (derived-mode-p 'text-mode)]
    "---"
    ["Highlight Dired file names" denote-dired-mode
     :help "Apply colors to Denote file name components in Dired"
     :enable (derived-mode-p 'dired-mode)
     :style toggle
     :selected (bound-and-true-p denote-dired-mode)])
  "Contents of the Denote menu.")

(easy-menu-define denote-global-menu nil
  "Menu with all Denote commands, each available in the right context."
  denote--menu-contents)

;; Add Denote menu at the end of global-map after Tools
(easy-menu-add-item global-map '(menu-bar)
                    denote-global-menu)

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
         (id (if (and search (not (string-empty-p search)))
                 (substring link 0 (match-beginning 0))
               link))
         (path (denote-get-path-by-id id)))
    (cond
     (path-id
      (cons (format "%s" path) (format "%s" id)))
     ((and search (not (string-empty-p search)))
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
  (when-let ((file (buffer-file-name))
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
                        (format-time-string denote-id-format) 'org ""))
    (denote--keywords-add-to-history keywords)
    (concat front-matter denote-org-capture-specifiers)))

;; TODO 2023-12-02: Maybe simplify `denote-org-capture-with-prompts'
;; by passing a single PROMPTS that is the same value as `denote-prompts'?

;; TODO 2023-12-02: The `denote-org-capture-with-prompts' is missing a
;; signature argument, but nobody has asked for it.  I think
;; refactoring it per the above TODO is better, anyway.  But maybe do
;; this after version 2.2.0 is out.

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
         (id (denote--find-first-unused-id
              (format-time-string denote-id-format date)
              (denote--get-all-used-ids)))
         (template (if template (denote-template-prompt) ""))
         (front-matter (denote--format-front-matter
                        title (denote--date date 'org) kws
                        (format-time-string denote-id-format date) 'org)))
    (setq denote-last-path
          (denote--path title kws directory id 'org ""))
    (denote--keywords-add-to-history kws)
    (concat front-matter template denote-org-capture-specifiers)))

(defun denote-org-capture-delete-empty-file ()
  "Delete file if capture with `denote-org-capture' is aborted."
  (when-let ((file denote-last-path)
             ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

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
text files, use the function `denote-directory-files' with a
non-nil `text-only' parameter."
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
  (when-let ((file (denote-get-path-by-id identifier)))
    (if (file-equal-p file (buffer-file-name (current-buffer)))
        (user-error "Identifier points to the current buffer")
      ;; Without the message, Xref will report that the ID does not
      ;; exist, which is incorrect in this case.
      (list (xref-make nil (xref-make-file-location file 0 0))))))

(cl-defmethod xref-backend-references ((_backend (eql 'denote)) identifier)
  "Return list of xrefs where IDENTIFIER is referenced.
This include the definition itself."
  (xref-matches-in-files identifier (denote-directory-files nil nil :text-only)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend
                                                         (eql 'denote)))
  "Return list of Denote identifers as completion table."

  (mapcar #'denote-retrieve-filename-identifier (denote-directory-files)))

(provide 'denote)
;;; denote.el ends here
