;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote
;; Version: 2.3.1
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

(define-obsolete-variable-alias
 'denote-user-enforced-denote-directory
 'denote-directory
 "2.3.0")

;;;###autoload (put 'denote-directory 'safe-local-variable (lambda (val) (or (stringp val) (eq val 'local) (eq val 'default-directory))))
(defcustom denote-directory (expand-file-name "~/Documents/notes/")
  "Directory for storing personal notes.

If you intend to reference this variable in Lisp, consider using
the function `denote-directory' instead."
  :group 'denote
  :safe (lambda (val) (or (stringp val) (eq val 'local) (eq val 'default-directory)))
  :package-version '(denote . "2.0.0")
  :link '(info-link "(denote) Maintain separate directories for notes")
  :type 'directory)

(defcustom denote-save-buffer-after-creation nil
  "Control whether commands that creeate new notes save their buffer outright.

The default behaviour of commands such as `denote' (or related)
is to not save the buffer they create.  This gives the user the
chance to review the text before writing it to a file.  The user
may choose to delete the unsaved buffer, thus not creating a new
note.

If this user option is set to a non-nil value, such buffers are
saved automatically."
  :group 'denote
  :package-version '(denote . "2.3.0")
  :type 'boolean)

;;;###autoload (put 'denote-known-keywords 'safe-local-variable #'listp)
(defcustom denote-known-keywords
  '("emacs" "philosophy" "politics" "economics")
  "List of strings with predefined keywords for `denote'.
Also see user options: `denote-infer-keywords',
`denote-sort-keywords', `denote-file-name-slug-functions'."
  :group 'denote
  :safe #'listp
  :package-version '(denote . "0.1.0")
  :type '(repeat string))

;;;###autoload (put 'denote-infer-keywords 'safe-local-variable (lambda (val) (or val (null val))))
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
  :safe (lambda (val) (or val (null val)))
  :package-version '(denote . "0.1.0")
  :type 'boolean)

(defcustom denote-prompts '(title keywords)
  "Specify the prompts followed by relevant Denote commands.

Commands that prompt for user input to construct a Denote file name
include, but are not limited to: `denote', `denote-signature',
`denote-type', `denote-date', `denote-subdirectory',
`denote-rename-file', `denote-dired-rename-files'.

The value of this user option is a list of symbols, which includes any
of the following:

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
`denote' or other relevant commands (advanced users can call it from
Lisp).  In Lisp usage, the behaviour is always what the caller
specifies, based on the supplied arguments.

Also see `denote-history-completion-in-prompts'."
  :group 'denote
  :package-version '(denote . "2.3.0")
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

(defcustom denote-org-store-link-to-heading t
  "Determine whether `org-store-link' links to the current Org heading.

When non-nil store link to the current Org heading inside the
Denote file.  If the heading does not have a CUSTOM_ID, create it
and include it in its PROPERTIES drawer.  If a CUSTOM_ID exists,
take it as-is.

Make the resulting link a combination of the `denote:' link type,
pointing to the identifier of the current file, plus the value of
the heading's CUSTOM_ID, such as:

- [[denote:20240118T060608][Some test]]
- [[denote:20240118T060608::#h:eed0fb8e-4cc7-478f][Some test::Heading text]]

Both lead to the same Denote file, but the latter jumps to the
heading with the given CUSTOM_ID.  Notice that the link to the
heading also has a different description, which includes the
heading text.

The value of the CUSTOM_ID is determined by the Org user option
`org-id-method'.  The sample shown above uses the default UUID
infrastructure (though I deleted a few characters to not get
complaints from the byte compiler about long lines in the doc
string...).

If this user option is set to nil, only store links to the Denote
file (using its identifier), but not to the given heading.  This
is what Denote was doing in versions prior to 2.3.0.

What `org-store-link' does is merely collect a link.  To actually insert
it, use the command `org-insert-link'.  Note tha `org-capture' uses
`org-store-link' internally when it needs to store a link.

[ This feature only works in Org mode files, as other file types
  do not have a linking mechanism that handles unique identifiers
  for headings or other patterns to jump to.  If `org-store-link'
  is invoked in one such file, it captures only the Denote
  identifier of the file, even if this user option is set to a
  non-nil value.  ]"
  :group 'denote
  :package-version '(denote . "2.3.0")
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
  "Make renaming commands not prompt for confirmation and save buffers outright.

This affects the behaviour of the commands `denote-rename-file',
`denote-dired-rename-files', `denote-rename-file-using-front-matter',
`denote-dired-rename-marked-files-with-keywords',
`denote-dired-rename-marked-files-using-front-matter',
`denote-keywords-add', `denote-keywords-remove', and any other
command that builds on top of them.

The default behaviour of the `denote-rename-file' command (and
others like it) is to ask for an affirmative answer as a final
step before changing the file name and, where relevant, inserting
or updating the corresponding front matter.  It also does not
save the affected file's buffer to let the user inspect and
confirm the changes (such as by invoking the command
`diff-buffer-with-file').

With this user option bound to a non-nil value, buffers are saved
as well.  The assumption is that the user who opts in to this
feature is familiar with the `denote-rename-file' operation (or
related) and knows it is reliable.

Specialised commands that build on top of `denote-rename-file' (or related)
may internally bind this user option to a non-nil value in order
to perform their operation (e.g. `denote-dired-rename-files' goes
through each marked Dired file, prompting for the information to
use, but carries out the renaming without asking for confirmation)."
  :group 'denote
  :package-version '(denote . "2.3.0")
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
  :link '(info-link "(denote) Standard note creation")
  :type 'hook)

(defcustom denote-after-rename-file-hook nil
  "Normal hook called after a succesful Denote rename operation.
This affects the behaviour of the commands `denote-rename-file',
`denote-dired-rename-files', `denote-rename-file-using-front-matter',
`denote-dired-rename-marked-files-with-keywords',
`denote-dired-rename-marked-files-using-front-matter',
`denote-keywords-add', `denote-keywords-remove', and any other
command that builds on top of them."
  :group 'denote
  :package-version '(denote . "2.3.0")
  :link '(info-link "(denote) Renaming files")
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

(defvar denote-prompts-with-history-as-completion
  '(denote-title-prompt denote-signature-prompt denote-files-matching-regexp-prompt)
  "Prompts that conditionally perform completion against their history.

These are minibuffer prompts that ordinarily accept a free form string
input, as opposed to matching against a predefined set.

These prompts can optionally perform completion against their own
minibuffer history when the user option `denote-history-completion-in-prompts'
is set to a non-nil value.")

(defcustom denote-history-completion-in-prompts t
  "Toggle history completion in all `denote-prompts-with-history-as-completion'.

When this user option is set to a non-nil value, use minibuffer history
entries as completion candidates in `denote-prompts-with-history-as-completion'.
Those will show previous inputs from their respective history as
possible values to select, either to (i) re-insert them verbatim or (ii)
with the intent to edit further (depending on the minibuffer user
interface, one can select a candidate with TAB without exiting the
minibuffer, as opposed to what RET normally does by selecting and
exiting).

When this user option is set to a nil value, all of the
`denote-prompts-with-history-as-completion' do not use minibuffer
completion: they just prompt for a string of characters.  Their
history is still available through all the standard ways of retrieving
minibuffer history, such as with the command `previous-history-element'.

History completion still allows arbitrary values to be provided as
input: they do not have to match the available minibuffer completion
candidates.

Note that some prompts, like `denote-keywords-prompt', always use
minibuffer completion, due to the specifics of their data.

[ Consider enabling the built-in `savehist-mode' to persist minibuffer
  histories between sessions.]

Also see `denote-prompts'."
  :type 'boolean
  :package-version '(denote . "2.3.0")
  :group 'denote)

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

(defcustom denote-file-name-slug-functions
  '((title . denote-sluggify-title)
    (signature . denote-sluggify-signature)
    (keyword . denote-sluggify-keyword))
  "Specify the method Denote uses to format the components of the file name.

The value is an alist where each element is a cons cell of the
form (COMPONENT . METHOD).

- The COMPONENT is an unquoted symbol among `title', `signature',
  `keyword' (notice the absence of `s', see below), which
  refers to the corresponding component of the file name.

- The METHOD is the function to be used to format the given
  component.  This function should take a string as its parameter
  and return the string formatted for the file name.  In the case
  of the `keyword' component, the function receives a SINGLE
  string representing a single keyword and return it formatted
  for the file name.  Joining the keywords together is handled by
  Denote.

Note that the `keyword' function is also applied to the keywords
of the front matter.

By default, if a function is not specified for a component, we
use `denote-sluggify-title', `denote-sluggify-keyword' and
`denote-sluggify-signature'.

Remember that deviating from the default file-naming scheme of Denote
will make things harder to search in the future, as files can/will have
permutations that create uncertainty.  The sluggification scheme and
concomitant restrictions we impose by default are there for a very good
reason: they are the distillation of years of experience.  Here we give
you what you wish, but bear in mind it may not be what you need.  You
have been warned."
  :group 'denote
  :package-version '(denote . "2.3.0")
  :link '(info-link "(denote) User-defined sluggification of file name components")
  :type '(alist :key (choice (const title)
                             (const signature)
                             (const keyword))
                :value function))

(make-obsolete
 'denote-file-name-letter-casing
 'denote-file-name-slug-functions
 "2.3.0")

;;;; Main variables

;; For character classes, evaluate: (info "(elisp) Char Classes")

(defconst denote-id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.
The note's ID is derived from the date and time of its creation.")

(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote-id-format'.")

(defconst denote-signature-regexp "==\\([^.]*?\\)\\(--.*\\|__.*\\|\\..*\\)*$"
  "Regular expression to match the SIGNATURE field in a file name.")

(defconst denote-title-regexp "--\\([^.]*?\\)\\(--.*\\|__.*\\|\\..*\\)*$"
  "Regular expression to match the TITLE field in a file name.")

(defconst denote-keywords-regexp "__\\([^.]*?\\)\\(--.*\\|__.*\\|\\..*\\)*$"
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

(defun denote--completion-table-no-sort (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.
Like `denote--completion-table' but also disable sorting."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (display-sort-function . ,#'identity))
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

(defun denote-directory ()
  "Return path of variable `denote-directory' as a proper directory.
Custom Lisp code can `let' bind the variable `denote-directory'
to override what this function returns."
  ;; NOTE 2024-02-09: We may want to remove this condition eventually.
  ;; The reason is that we want to stop supporting the dir-local
  ;; values of `default-directory' or `local' in favour of just
  ;; specifying a string.  I don't think we can delete this altogether
  ;; though, as it will break existing configurations.
  (if-let (((or (eq denote-directory 'default-directory) (eq denote-directory 'local)))
           (silo-dir (denote--default-directory-is-silo-p)))
      silo-dir
    (let ((denote-directory (file-name-as-directory (expand-file-name denote-directory))))
      (denote--make-denote-directory)
      denote-directory)))

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

(defun denote--slug-no-punct-for-signature (str &optional extra-characters)
  "Remove punctuation (except = signs) from STR.

This works the same way as `denote--slug-no-punct', except that =
signs are not removed from STR.

EXTRA-CHARACTERS is an optional string.  See
`denote--slug-no-punct' for its documentation."
  (dolist (regexp (list denote-excluded-punctuation-regexp
                        denote-excluded-punctuation-extra-regexp
                        extra-characters))
    (when (stringp regexp)
      (setq str (replace-regexp-in-string (string-replace "=" "" regexp) "" str))))
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

(defun denote--remove-dot-characters (str)
  "Remove the dot character from STR."
  (replace-regexp-in-string "\\." "" str))

(defun denote--trim-right-token-characters (str)
  "Remove =, - and _ from the end of STR."
  (string-trim-right str "[=_-]+"))

(defun denote--replace-consecutive-token-characters (str)
  "Replace consecutive characters with a single one in STR.
Spaces, underscores and equal signs are replaced with a single
one in str."
  (replace-regexp-in-string
   "-\\{2,\\}" "-"
   (replace-regexp-in-string
    "_\\{2,\\}" "_"
    (replace-regexp-in-string
     "=\\{2,\\}" "=" str))))

(defun denote-sluggify (component str)
  "Make STR an appropriate slug for file name COMPONENT.

Apply the function specified in `denote-file-name-slug-function'
to COMPONENT which is one of `title', `signature', `keyword'.  If
the resulting string still contains consecutive -,_ or =, they
are replaced by a single occurence of the character.  If
COMPONENT is `keyword', remove underscores from STR as they are
used as the keywords separator in file names."
  (let* ((slug-function (alist-get component denote-file-name-slug-functions))
         (str-slug (cond ((eq component 'title)
                          (funcall (or slug-function #'denote-sluggify-title) str))
                         ((eq component 'keyword)
                          (replace-regexp-in-string
                           "_" ""
                           (funcall (or slug-function #'denote-sluggify-keyword) str)))
                         ((eq component 'signature)
                          (funcall (or slug-function #'denote-sluggify-signature) str)))))
    (denote--trim-right-token-characters
     (denote--replace-consecutive-token-characters
      (denote--remove-dot-characters str-slug)))))

(make-obsolete
 'denote-letter-case
 'denote-sluggify
 "2.3.0")

(defun denote--slug-put-equals (str)
  "Replace spaces and underscores with equals signs in STR.
Also replace multiple equals signs with a single one and remove
any leading and trailing signs."
  (replace-regexp-in-string
   "^=\\|=$" ""
   (replace-regexp-in-string
    "=\\{2,\\}" "="
    (replace-regexp-in-string "_\\|\s+" "=" str))))

(defun denote-sluggify-title (str)
  "Make STR an appropriate slug for title."
  (downcase (denote--slug-hyphenate (denote--slug-no-punct str))))

(defun denote-sluggify-signature (str)
  "Make STR an appropriate slug for signature."
  (downcase (denote--slug-put-equals (denote--slug-no-punct-for-signature str "-+"))))

(defun denote-sluggify-keyword (str)
  "Sluggify STR while joining separate words."
  (downcase
   (replace-regexp-in-string
    "-" ""
    (denote--slug-hyphenate (denote--slug-no-punct str)))))

(make-obsolete
 'denote-sluggify-and-join
 'denote-sluggify-keyword
 "2.3.0")

(defun denote-sluggify-keywords (keywords)
  "Sluggify KEYWORDS, which is a list of strings."
  (mapcar (lambda (keyword)
            (denote-sluggify 'keyword keyword))
          keywords))

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun denote-file-has-identifier-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (denote-retrieve-filename-identifier file))

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
       (denote-file-has-identifier-p filename)
       (denote-file-has-supported-extension-p filename)))

(defun denote-file-is-note-p (file)
  "Return non-nil if FILE is an actual Denote note.
For our purposes, a note must not be a directory, must satisfy
`file-regular-p' and `denote-filename-is-note-p'."
  (and (not (file-directory-p file))
       (file-regular-p file)
       (denote-filename-is-note-p file)))

(defun denote-file-has-signature-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (denote-retrieve-filename-signature file))

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
    (match-string-no-properties 0 string)))

(define-obsolete-function-alias
  'denote--default-dir-has-denote-prefix
  'denote--dir-in-denote-directory-p
  "2.1.0")

(defun denote--exclude-directory-regexp-p (file)
  "Return non-nil if FILE matches `denote-excluded-directories-regexp'."
  (and denote-excluded-directories-regexp
       (string-match-p denote-excluded-directories-regexp file)))

(defun denote--directory-files-recursively-predicate (file)
  "Predicate used by `directory-files-recursively' on FILE.

Return t if FILE is valid, else return nil."
  (let ((rel (denote-get-file-name-relative-to-denote-directory file)))
    (cond
     ((string-match-p "\\`\\." rel) nil)
     ((string-match-p "/\\." rel) nil)
     ((denote--exclude-directory-regexp-p rel) nil)
     ((file-readable-p file)))))

(defun denote--directory-all-files-recursively ()
  "Return list of all files in variable `denote-directory'.
Avoids traversing dotfiles (unconditionally) and whatever matches
`denote-excluded-directories-regexp'."
  (directory-files-recursively
   (denote-directory)
   directory-files-no-dot-files-regexp
   :include-directories
   #'denote--directory-files-recursively-predicate
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
      (setq files (seq-filter #'denote-file-is-note-p files)))
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

(defvar denote-file-history nil
  "Minibuffer history of `denote-file-prompt'.")

(defalias 'denote--file-history 'denote-file-history
  "Compatibility alias for `denote-file-history'.")

;; NOTE 2024-02-29: Based on `project--read-file-cpd-relative' from
;; the built-in project.el
(defun denote-file-prompt (&optional files-matching-regexp prompt-text)
  "Prompt for file with identifier in variable `denote-directory'.
With optional FILES-MATCHING-REGEXP, filter the candidates per
the given regular expression.

With optional PROMPT-TEXT, use it instead of the default call to
\"Select NOTE\"."
  (when-let ((all-files (denote-directory-files files-matching-regexp :omit-current)))
    (let* ((common-parent-directory
            (let ((common-prefix (try-completion "" all-files)))
              (if (> (length common-prefix) 0)
                  (file-name-directory common-prefix))))
           (cpd-length (length common-parent-directory))
           (prompt-prefix (or prompt-text "Select NOTE"))
           (prompt (if (zerop cpd-length)
                       (format "%s: " prompt-prefix)
                     (format "%s in %s: " prompt-prefix common-parent-directory)))
           (included-cpd (when (member common-parent-directory all-files)
                           (setq all-files
                                 (delete common-parent-directory all-files))
                           t))
           (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files))
           (_ (when included-cpd
                (setq substrings (cons "./" substrings))))
           (new-collection (denote--completion-table 'file substrings))
           (relname (completing-read prompt new-collection nil nil nil 'denote-file-history))
           (absname (expand-file-name relname common-parent-directory)))
      ;; NOTE 2024-02-29: This delete and add feels awkward.  I wish
      ;; we could tell `completing-read' to just leave this up to us.
      (setq denote-file-history (delete relname denote-file-history))
      (add-to-history 'denote-file-history absname)
      absname)))

;;;; Keywords

(defun denote-extract-keywords-from-path (path)
  "Extract keywords from PATH and return them as a list of strings.
PATH must be a Denote-style file name where keywords are prefixed
with an underscore.

If PATH has no such keywords, return nil."
  (when-let ((kws (denote-retrieve-filename-keywords path)))
    (split-string kws "_")))

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

(defun denote-convert-file-name-keywords-to-crm (string)
  "Make STRING with keywords readable by `completing-read-multiple'.
STRING consists of underscore-separated words, as those appear in
the keywords component of a Denote file name.  STRING is the same
as the return value of `denote-retrieve-filename-keywords'."
  (string-join (split-string string "_" :omit-nulls "_") ","))

(defvar denote-keyword-history nil
  "Minibuffer history of inputted keywords.")

(defalias 'denote--keyword-history 'denote-keyword-history
  "Compatibility alias for `denote-keyword-history'.")

(defun denote--keywords-crm (keywords &optional prompt initial)
  "Use `completing-read-multiple' for KEYWORDS.
With optional PROMPT, use it instead of a generic text for file
keywords.  With optional INITIAL, add it to the minibuffer as
initial input."
  (delete-dups
   (completing-read-multiple
    (format-prompt (or prompt "New file KEYWORDS") nil)
    keywords nil nil initial 'denote-keyword-history)))

(defun denote-keywords-prompt (&optional prompt-text initial-keywords)
  "Prompt for one or more keywords.
Read entries as separate when they are demarcated by the
`crm-separator', which typically is a comma.

With optional PROMPT-TEXT, use it to prompt the user for
keywords.  Else use a generic prompt.  With optional
INITIAL-KEYWORDS use them as the initial minibuffer text.

Return an empty list if the minibuffer input is empty."
  (denote--keywords-crm (denote-keywords) prompt-text initial-keywords))

(defun denote-keywords-sort (keywords)
  "Sort KEYWORDS if `denote-sort-keywords' is non-nil.
KEYWORDS is a list of strings, per `denote-keywords-prompt'."
  (if denote-sort-keywords
      (sort (copy-sequence keywords) #'string-collate-lessp)
    keywords))

(define-obsolete-function-alias
  'denote--keywords-combine
  'denote-keywords-combine
  "2.1.0")

(defun denote-keywords-combine (keywords)
  "Combine KEYWORDS list of strings into a single string.
Keywords are separated by the underscore character, per the
Denote file-naming scheme."
  (string-join keywords "_"))

(defun denote--keywords-add-to-history (keywords)
  "Append KEYWORDS to `denote-keyword-history'."
  (mapc
   (lambda (kw)
     (add-to-history 'denote-keyword-history kw))
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

(define-obsolete-function-alias
  'denote-surround-with-quotes
  'denote-format-string-for-md-front-matter
  "2.3.0")

(defun denote-format-string-for-md-front-matter (s)
  "Surround string S with quotes.
If S is not a string, return a literal emptry string.

This can be used in `denote-file-types' to format front mattter."
  (if (stringp s)
      (format "%S" s)
    "\"\""))

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

(defun denote-format-string-for-org-front-matter (s)
  "Return string S as-is for Org or plain text front matter.
If S is not a string, return an empty string."
  (if (stringp s) s ""))

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
     :title-value-function denote-format-string-for-org-front-matter
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
     :title-value-function denote-format-string-for-md-front-matter
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
     :title-value-function denote-format-string-for-md-front-matter
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
     :title-value-function denote-format-string-for-org-front-matter
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

(defun denote--format-front-matter (title date keywords id filetype)
  "Front matter for new notes.

TITLE, DATE, and ID are all strings or functions that return a
string.  KEYWORDS is a list of strings.  FILETYPE is one of the
values of `denote-file-type'."
  (let* ((fm (denote--front-matter filetype))
         (title (denote--format-front-matter-title title filetype))
         (kws (denote--format-front-matter-keywords keywords filetype)))
    (if fm (format fm title date kws id) "")))

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
  "Extract identifier from FILE name, if present, else return nil.

To create a new one, refer to the function
`denote-create-unique-file-identifier'."
  (let ((filename (file-name-nondirectory file)))
    (if (string-match (concat "\\`" denote-id-regexp) filename)
        (match-string-no-properties 0 filename))))

;; TODO 2023-12-08: Maybe we can only use
;; `denote-retrieve-filename-identifier' and remove this function.
(defun denote-retrieve-filename-identifier-with-error (file)
  "Extract identifier from FILE name, if present, else signal an error."
  (or (denote-retrieve-filename-identifier file)
      (error "Cannot find `%s' as a file with a Denote identifier" file)))

(defun denote-get-identifier (&optional date)
  "Convert DATE into a Denote identifier using `denote-id-format'.
DATE is parsed by `denote-valid-date-p'.  If DATE is nil, use the
current time."
  (format-time-string
   denote-id-format
   (when date (denote-valid-date-p date))))

(defun denote-create-unique-file-identifier (file used-ids &optional date)
  "Generate a unique identifier for FILE not in USED-IDS hash-table.

The conditions are as follows:

- If optional DATE is non-nil pass it to `denote-get-identifier'.
  DATE will have to conform with `denote-valid-date-p'.  If it
  does not, return an error.

- If optional DATE is nil, use the file attributes to determine
  the last modified date and format it as an identifier.

- As a fallback, derive an identifier from the current time.

To only return an existing identifier, refer to the function
`denote-retrieve-filename-identifier'."
  (let ((id (cond
              (date (denote-get-identifier date))
              ((denote--file-attributes-time file))
              (t (denote-get-identifier)))))
    (denote--find-first-unused-id id used-ids)))

(define-obsolete-function-alias
  'denote-retrieve-or-create-file-identifier
  'denote-retrieve-filename-identifier
  "2.1.0")

(defun denote-retrieve-filename-keywords (file)
  "Extract keywords from FILE name, if present, else return nil.
Return matched keywords as a single string."
  (let ((filename (file-name-nondirectory file)))
    (when (string-match denote-keywords-regexp filename)
      (match-string 1 filename))))

(defun denote-retrieve-filename-signature (file)
  "Extract signature from FILE name, if present, else return nil."
  (let ((filename (file-name-nondirectory file)))
    (when (string-match denote-signature-regexp filename)
      (match-string 1 filename))))

(defun denote-retrieve-filename-title (file)
  "Extract Denote title component from FILE name, else return nil."
  (let ((filename (file-name-nondirectory file)))
    (when (string-match denote-title-regexp filename)
      (match-string 1 filename))))

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

(defun denote-retrieve-front-matter-title-value (file file-type)
  "Return title value from FILE front matter per FILE-TYPE."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
      (funcall (denote--title-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(defun denote-retrieve-front-matter-title-line (file file-type)
  "Return title line from FILE front matter per FILE-TYPE."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun denote-retrieve-front-matter-keywords-value (file file-type)
  "Return keywords value from FILE front matter per FILE-TYPE.
The return value is a list of strings.  To get a combined string
the way it would appear in a Denote file name, use
`denote-retrieve-front-matter-keywords-value-as-string'."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
      (funcall (denote--keywords-value-reverse-function file-type)
               (buffer-substring-no-properties (point) (line-end-position))))))

(defun denote-retrieve-front-matter-keywords-value-as-string (file file-type)
  "Return keywords value from FILE front matter per FILE-TYPE.
The return value is a string, with the underscrore as a separator
between individual keywords.  To get a list of strings instead,
use `denote-retrieve-front-matter-keywords-value' (the current function uses
that internally)."
  (denote-keywords-combine (denote-retrieve-front-matter-keywords-value file file-type)))

(defun denote-retrieve-front-matter-keywords-line (file file-type)
  "Return keywords line from FILE front matter per FILE-TYPE."
  (denote--file-with-temp-buffer file
    (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defalias 'denote-retrieve-title-value 'denote-retrieve-front-matter-title-value
 "Alias for `denote-retrieve-front-matter-title-value'.")

(defalias 'denote-retrieve-title-line 'denote-retrieve-front-matter-title-line
 "Alias for `denote-retrieve-front-matter-title-line'.")

(defalias 'denote-retrieve-keywords-value 'denote-retrieve-front-matter-keywords-value
 "Alias for `denote-retrieve-front-matter-keywords-value'.")

(defalias 'denote-retrieve-keywords-line 'denote-retrieve-front-matter-keywords-line
 "Alias for `denote-retrieve-front-matter-keywords-line'.")

(defalias 'denote-retrieve-keywords-value-as-string 'denote-retrieve-front-matter-keywords-value-as-string
 "Alias for `denote-retrieve-front-matter-keywords-value-as-string'.")

(define-obsolete-function-alias
  'denote--retrieve-title-or-filename
  'denote-retrieve-title-or-filename
  "2.3.0")

(defun denote-retrieve-title-or-filename (file type)
  "Return appropriate title for FILE given its TYPE.
Try to find the value of the title in the front matter of FILE,
otherwise use its file name.

This is a wrapper for `denote-retrieve-front-matter-title-value' and
`denote-retrieve-filename-title'."
  (if-let (((denote-file-is-note-p file))
           (title (denote-retrieve-front-matter-title-value file type))
           ((not (string-blank-p title))))
      title
    (or (denote-retrieve-filename-title file)
        (file-name-base file))))

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

(defun denote-format-file-name (dir-path id keywords title extension signature)
  "Format file name.
DIR-PATH, ID, KEYWORDS, TITLE, EXTENSION and SIGNATURE are
expected to be supplied by `denote' or equivalent command.

DIR-PATH is a string pointing to a directory.  It ends with a
forward slash (the function `denote-directory' makes sure this is
the case when returning the value of the variable `denote-directory').
DIR-PATH cannot be nil or an empty string.

ID is a string holding the identifier of the note.  It cannot be
nil or an empty string and must match `denote-id-regexp'.

DIR-PATH and ID form the base file name.

KEYWORDS is a list of strings that is reduced to a single string
by `denote-keywords-combine'.  KEYWORDS can be an empty list or
a nil value, in which case the relevant file name component is
not added to the base file name.

TITLE and SIGNATURE are strings.  They can be an empty string, in
which case their respective file name component is not added to
the base file name.

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
    (when (and signature (not (string-empty-p signature)))
      (setq file-name (concat file-name "==" (denote-sluggify 'signature signature))))
    (when (and title (not (string-empty-p title)))
      (setq file-name (concat file-name "--" (denote-sluggify 'title title))))
    (when keywords
      (setq file-name (concat file-name "__" (denote-keywords-combine (denote-sluggify-keywords keywords)))))
    (concat file-name extension)))

(defun denote--format-front-matter-title (title file-type)
  "Format TITLE according to FILE-TYPE for the file's front matter."
  (funcall (denote--title-value-function file-type) title))

(defun denote--format-front-matter-keywords (keywords file-type)
  "Format KEYWORDS according to FILE-TYPE for the file's front matter.
Apply `denote-sluggify' to KEYWORDS."
  (let ((kws (denote-sluggify-keywords keywords)))
    (funcall (denote--keywords-value-function file-type) kws)))

(defun denote--path (title keywords dir id file-type signature)
  "Return path to new file.
Use ID, TITLE, KEYWORDS, FILE-TYPE and SIGNATURE to construct
path to DIR."
  (denote-format-file-name
   dir id keywords title (denote--file-extension file-type) signature))

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
                  id
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

(define-obsolete-function-alias
  'denote--valid-date
  'denote-valid-date-p
  "2.3.0")

(defun denote-valid-date-p (date)
  "Return DATE as a valid date.
A valid DATE is a value that can be parsed by either
`decode-time' or `date-to-time'.  Those functions signal an error
if DATE is a value they do not recognise.

If DATE is nil, return nil."
  (if (and (or (numberp date) (listp date))
           (decode-time date))
      date
    (date-to-time (denote--date-add-current-time date))))

(defun denote-parse-date (date)
  "Return DATE as an appropriate value for the `denote' command.
Pass DATE through `denote-valid-date-p' and use its return value.
If either that or DATE is nil, return `current-time'."
  (or (denote-valid-date-p date) (current-time)))

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
      (let ((id (denote-retrieve-filename-identifier name)))
        (puthash id t ids)))
    ids))

(defun denote--find-first-unused-id (id used-ids)
  "Return the first unused id starting at ID from USED-IDS.
USED-IDS is a hash-table of all used IDs.  If ID is already used,
increment it 1 second at a time until an available id is found."
  (let ((current-id id))
    (while (gethash current-id used-ids)
      (setq current-id (denote-get-identifier (time-add (date-to-time current-id) 1))))
    current-id))

(make-obsolete 'denote-barf-duplicate-id nil "2.1.0")

(defvar denote-command-prompt-history nil
  "Minibuffer history for `denote-command-prompt'.")

(defalias 'denote--command-prompt-history 'denote-command-prompt-history
  "Compatibility alias for `denote-command-prompt-history'.")

(defun denote-command-prompt ()
  "Prompt for command among `denote-commands-for-new-notes'."
  (let ((default (car denote-command-prompt-history)))
    (intern
     (completing-read
      (format-prompt "Run note-creating Denote command" default)
      denote-commands-for-new-notes nil :require-match
      nil 'denote-command-prompt-history default))))

;;;;; The `denote' command and its prompts

(defun denote--prompt-with-completion-p (fn)
  "Return non-nil if FN prompt should perform completion.
FN is one among `denote-prompts-with-history-as-completion' and performs
completion when the user option `denote-history-completion-in-prompts'
is non-nil."
  (and denote-history-completion-in-prompts
       (memq fn denote-prompts-with-history-as-completion)))

(defvar denote-ignore-region-in-denote-command nil
  "If non-nil, the region is ignored by the `denote' command.

The `denote' command uses the region as the default title when
prompted for a title.  When this variable is non-nil, the
`denote' command ignores the region.  This variable is useful in
commands that have their own way of handling the region.")

(defvar denote-title-prompt-current-default nil
  "Currently bound default title for `denote-title-prompt'.
Set the value of this variable within the lexical scope of a
command that needs to supply a default title before calling
`denote-title-prompt'.")

(defun denote--command-with-features (command force-use-file-prompt-as-default-title force-ignore-region force-save in-background)
  "Execute file-creating COMMAND with specified features.

COMMAND is the symbol of a file-creating command to call, such as
`denote' or `denote-signature'.

With non-nil FORCE-USE-FILE-PROMPT-AS-DEFAULT-TITLE, use the last
item of `denote-file-history' as the default title of the title
prompt.  This is useful in a command such as `denote-link' where
the entry of the file prompt can be reused as the default title.

With non-nil FORCE-IGNORE-REGION, the region is ignore when
creating the note, i.e. it will not be used as the initial title
in a title prompt.  Else, the value of
`denote-ignore-region-in-denote-command' is respected.

With non-nil FORCE-SAVE, the file is saved at the end of the note
creation.  Else, the value of `denote-save-buffer-after-creation'
is respected.

With non-nil IN-BACKGROUND, the note creation happens in the
background, i.e. the note's buffer will not be displayed after
the note is created.

Note that if all parameters except COMMAND are nil, this is
equivalent to `(call-interactively command)'.

The path of the newly created file is returned."
  (let ((denote-save-buffer-after-creation
         (or force-save denote-save-buffer-after-creation))
        (denote-ignore-region-in-denote-command
         (or force-ignore-region denote-ignore-region-in-denote-command))
        (denote-title-prompt-current-default
         (if force-use-file-prompt-as-default-title
             (when denote-file-history
               (file-name-nondirectory (pop denote-file-history)))
           denote-title-prompt-current-default))
        (path))
    (if in-background
        (save-window-excursion
          (call-interactively command)
          (setq path (buffer-file-name)))
      (call-interactively command)
      (setq path (buffer-file-name)))
    path))

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
                               (when (and (not denote-ignore-region-in-denote-command)
                                          (use-region-p))
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
         (kws (denote-keywords-sort keywords))
         (date (denote-parse-date date))
         (id (denote--find-first-unused-id
              (denote-get-identifier date)
              (denote--get-all-used-ids)))
         (directory (if (denote--dir-in-denote-directory-p subdirectory)
                        (file-name-as-directory subdirectory)
                      (denote-directory)))
         (template (if (stringp template)
                       template
                     (or (alist-get template denote-templates) "")))
         (signature (or signature "")))
    (denote--prepare-note title kws date id directory file-type template signature)
    (when denote-save-buffer-after-creation (save-buffer))
    (denote--keywords-add-to-history keywords)
    (run-hooks 'denote-after-new-note-hook)))

(defvar denote-title-history nil
  "Minibuffer history of `denote-title-prompt'.")

(defalias 'denote--title-history 'denote-title-history
  "Compatibility alias for `denote-title-history'.")

(defmacro denote--with-conditional-completion (fn prompt history &optional initial-value default-value)
  "Produce body of FN that may perform completion.
Use PROMPT, HISTORY, INITIAL-VALUE, and DEFAULT-VALUE as arguments for
the given minibuffer prompt."
  `(if (denote--prompt-with-completion-p ,fn)
       ;; NOTE 2023-10-27: By default SPC performs completion in the
       ;; minibuffer.  We do not want that, as the user should be able to
       ;; input an arbitrary string, while still performing completion
       ;; against their input history.
       (minibuffer-with-setup-hook
           (lambda ()
             (use-local-map
              (let ((map (make-composed-keymap nil (current-local-map))))
                (define-key map (kbd "SPC") nil)
                map)))
         (completing-read ,prompt ,history nil nil ,initial-value ',history ,default-value))
     (read-string ,prompt ,initial-value ',history ,default-value)))

(defun denote-title-prompt (&optional initial-title prompt-text)
  "Prompt for title string.

With optional INITIAL-TITLE use it as the initial minibuffer
text.  With optional PROMPT-TEXT use it in the minibuffer instead
of the default prompt.

Previous inputs at this prompt are available for minibuffer completion
if the user option `denote-history-completion-in-prompts' is set to a
non-nil value."
  (denote--with-conditional-completion
   'denote-title-prompt
   (format-prompt (or prompt-text "New file TITLE") denote-title-prompt-current-default)
   denote-title-history
   initial-title
   denote-title-prompt-current-default))

(defvar denote-file-type-history nil
  "Minibuffer history of `denote-file-type-prompt'.")

(defalias 'denote--file-type-history 'denote-file-type-history
  "Compatibility alias for `denote-file-type-history'.")

(defun denote-file-type-prompt ()
  "Prompt for `denote-file-type'.
Note that a non-nil value other than `text', `markdown-yaml', and
`markdown-toml' falls back to an Org file type.  We use `org'
here for clarity."
  (completing-read
   "Select file type: " (denote--file-type-keys) nil t
   nil 'denote-file-type-history))

(defvar denote-date-history nil
  "Minibuffer history of `denote-date-prompt'.")

(defalias 'denote--date-history 'denote-date-history
  "Compatibility alias for `denote-date-history'.")

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
     nil 'denote-date-history)))

(defun denote-prompt-for-date-return-id ()
  "Use `denote-date-prompt' and return it as `denote-id-format'."
  (denote-get-identifier (denote-date-prompt)))

(defvar denote-subdirectory-history nil
  "Minibuffer history of `denote-subdirectory-prompt'.")

(defalias 'denote--subdir-history 'denote-subdirectory-history
  "Compatibility alias for `denote-subdirectory-history'.")

;; Making it a completion table is useful for packages that read the
;; metadata, such as `marginalia' and `embark'.
(defun denote--subdirs-completion-table (dirs)
  "Match DIRS as a completion table."
  (let* ((def (car denote-subdirectory-history))
         (table (denote--completion-table 'file dirs))
         (prompt (if def
                     (format "Select subdirectory [%s]: " def)
                   "Select subdirectory: ")))
    (completing-read prompt table nil t nil 'denote-subdirectory-history def)))

(defun denote-subdirectory-prompt ()
  "Prompt for subdirectory of the variable `denote-directory'.
The table uses the `file' completion category (so it works with
packages such as `marginalia' and `embark')."
  (let* ((root (directory-file-name (denote-directory)))
         (subdirs (denote-directory-subdirectories))
         (dirs (push root subdirs)))
    (denote--subdirs-completion-table dirs)))

(defvar denote-template-history nil
  "Minibuffer history of `denote-template-prompt'.")

(defalias 'denote--template-history 'denote-template-history
  "Compatibility alias for `denote-template-history'.")

(defun denote-template-prompt ()
  "Prompt for template key in `denote-templates' and return its value."
  (let ((templates denote-templates))
    (alist-get
     (intern
      (completing-read
       "Select template KEY: " (mapcar #'car templates)
       nil t nil 'denote-template-history))
     templates)))

(defvar denote-signature-history nil
  "Minibuffer history of `denote-signature-prompt'.")

(defalias 'denote--signature-history 'denote-signature-history
  "Compatibility alias for `denote-signature-history'.")

(defun denote-signature-prompt (&optional initial-signature prompt-text)
  "Prompt for signature string.
With optional INITIAL-SIGNATURE use it as the initial minibuffer
text.  With optional PROMPT-TEXT use it in the minibuffer instead
of the default prompt.

Previous inputs at this prompt are available for minibuffer completion
if the user option `denote-history-completion-in-prompts' is set to a
non-nil value."
  (when (and initial-signature (string-empty-p initial-signature))
    (setq initial-signature nil))
  (denote--with-conditional-completion
   'denote-signature-prompt
   (format-prompt (or prompt-text "New file SIGNATURE") nil)
   denote-signature-history
   initial-signature))

(defvar denote-files-matching-regexp-history nil
  "Minibuffer history of `denote-files-matching-regexp-prompt'.")

(defalias 'denote--files-matching-regexp-hist 'denote-files-matching-regexp-history
  "Compatibility alias for `denote-files-matching-regexp-history'.")

(defun denote-files-matching-regexp-prompt (&optional prompt-text)
  "Prompt for REGEXP to filter Denote files by.
With optional PROMPT-TEXT use it instead of a generic prompt."
  (denote--with-conditional-completion
   'denote-files-matching-regexp-prompt
   (format-prompt (or prompt-text "Match files with the given REGEXP") nil)
   denote-files-matching-regexp-history))

;;;;; Convenience commands as `denote' variants

(defalias 'denote-create-note 'denote
  "Alias for `denote' command.")

(defun denote--add-prompts (additional-prompts)
  "Add all the elements in the ADDITIONAL-PROMPTS list to `denote-prompts'."
  (seq-union additional-prompts denote-prompts))

;;;###autoload
(defun denote-type ()
  "Create note while prompting for a file type.

This is the equivalent of calling `denote' when `denote-prompts'
has the `file-type' prompt appended to its existing prompts."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts (denote--add-prompts '(file-type))))
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

This is the equivalent of calling `denote' when `denote-prompts'
has the `date' prompt appended to its existing prompts."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts (denote--add-prompts '(date))))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-date 'denote-date
  "Alias for `denote-date' command.")

;;;###autoload
(defun denote-subdirectory ()
  "Create note while prompting for a subdirectory.

Available candidates include the value of the variable
`denote-directory' and any subdirectory thereof.

This is the equivalent of calling `denote' when `denote-prompts'
has the `subdirectory' prompt appended to its existing prompts."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts (denote--add-prompts '(subdirectory))))
    (call-interactively #'denote)))

(defalias 'denote-create-note-in-subdirectory 'denote-subdirectory
  "Alias for `denote-subdirectory' command.")

;;;###autoload
(defun denote-template ()
  "Create note while prompting for a template.

Available candidates include the keys in the `denote-templates'
alist.  The value of the selected key is inserted in the newly
created note after the front matter.

This is the equivalent of calling `denote' when `denote-prompts'
has the `template' prompt appended to its existing prompts."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts (denote--add-prompts '(template))))
    (call-interactively #'denote)))

(defalias 'denote-create-note-with-template 'denote-template
  "Alias for `denote-template' command.")

;;;###autoload
(defun denote-signature ()
  "Create note while prompting for a file signature.

This is the equivalent of calling `denote' when `denote-prompts'
has the `signature' prompt appended to its existing prompts."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts (denote--add-prompts '(signature))))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-signature 'denote-signature
  "Alias for `denote-signature' command.")

;;;###autoload
(defun denote-region ()
  "Call `denote' and insert therein the text of the active region."
  (declare (interactive-only t))
  (interactive)
  (if-let (((region-active-p))
           ;; We capture the text early, otherwise it will be empty
           ;; the moment `insert' is called.
           (text (buffer-substring-no-properties (region-beginning) (region-end))))
      (progn
        (let ((denote-ignore-region-in-denote-command t))
          (call-interactively 'denote))
        (push-mark (point))
        (insert text)
        (run-hook-with-args 'denote-region-after-new-note-functions (mark) (point)))
    (call-interactively 'denote)))

;;;;; Other convenience commands

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
    (denote--command-with-features #'denote :use-file-prompt-as-def-title nil nil nil)))

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
      (denote--command-with-features (denote-command-prompt) :use-file-prompt-as-def-title nil nil nil))))

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
If in the process of `org-capture', consider the file type to be that of
Org.  Otherwise, use the file extension to detect the file type of FILE.

If more than one file type correspond to this file extension, use the
first file type for which the :title-key-regexp in `denote-file-types'
matches in the file.

If no file type in `denote-file-types' has the file extension,
the file type is assumed to be the first one in `denote-file-types'."
  (cond
   ((denote--file-type-org-capture-p) 'org)
   (file
    (let* ((extension (denote-get-file-extension-sans-encryption file))
           (types (denote--file-types-with-extension extension)))
      (cond ((null types)
             (caar denote-file-types))
            ((= (length types) 1)
             (caar types))
            (t
             (or (car (seq-find
                       (lambda (type)
                         (denote--regexp-in-file-p (plist-get (cdr type) :title-key-regexp) file))
                       types))
                 (caar types))))))))

(defun denote--file-attributes-time (file)
  "Return `file-attribute-modification-time' of FILE as identifier."
  (denote-get-identifier (file-attribute-modification-time (file-attributes file))))

(defun denote--revert-dired (buf)
  "Revert BUF if appropriate.
Do it if BUF is in Dired mode and is either part of the variable
`denote-directory' or the `current-buffer'."
  (let ((current (current-buffer)))
    (with-current-buffer buf
      (when (and (eq major-mode 'dired-mode)
                 (or (denote--dir-in-denote-directory-p default-directory)
                     (eq current buf)))
        (revert-buffer)))))

(defun denote-update-dired-buffers ()
  "Update Dired buffers of variable `denote-directory'.
Also revert the current Dired buffer even if it is not inside the
variable `denote-directory'."
  (mapc #'denote--revert-dired (buffer-list)))

(defun denote-rename-file-and-buffer (old-name new-name)
  "Rename file named OLD-NAME to NEW-NAME, updating buffer name."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (cond
     ((derived-mode-p 'dired-mode)
      (dired-rename-file old-name new-name nil))
     ;; NOTE 2024-02-25: The `vc-rename-file' requires the file to be
     ;; saved, but our convention is to not save the buffer after
     ;; changing front matter unless we absolutely have to (allows
     ;; users to do `diff-buffer-with-file', for example).
     ((and denote-save-buffer-after-creation (not (buffer-modified-p)) (vc-backend old-name))
      (vc-rename-file old-name new-name))
     (t
      (rename-file old-name new-name nil)))
    (when-let ((buffer (find-buffer-visiting old-name)))
      (with-current-buffer buffer
        (set-visited-file-name new-name nil t)))))

(defun denote--add-front-matter (file title keywords id file-type &optional save-buffer)
  "Prepend front matter to FILE if `denote-file-is-note-p'.
The TITLE, KEYWORDS ID, and FILE-TYPE are passed from the
renaming command and are used to construct a new front matter
block if appropriate.

With optional SAVE-BUFFER, save the buffer corresponding to FILE."
  (when-let ((date (denote--date (date-to-time id) file-type))
             (new-front-matter (denote--format-front-matter title date keywords id file-type)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (insert new-front-matter)
      (when save-buffer (save-buffer)))))

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
  (and (denote--front-matter file-type)
       (or (denote--regexp-in-file-p (denote--title-key-regexp file-type) file)
           (denote--regexp-in-file-p (denote--keywords-key-regexp file-type) file))))

(defun denote-rewrite-keywords (file keywords file-type &optional save-buffer)
  "Rewrite KEYWORDS in FILE outright according to FILE-TYPE.

Do the same as `denote-rewrite-front-matter' for keywords,
but do not ask for confirmation.

With optional SAVE-BUFFER, save the buffer corresponding to FILE.

This function is for use in the commands `denote-keywords-add',
`denote-keywords-remove', `denote-dired-rename-files', or
related."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
          (goto-char (line-beginning-position))
          (insert (denote--get-keywords-line-from-front-matter keywords file-type))
          (delete-region (point) (line-end-position))
          (when save-buffer (save-buffer)))))))

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
that effect.

With optional NO-CONFIRM, save the buffer after performing the
rewrite.  Otherwise leave it unsaved for furthter review by the
user."
  (when-let ((old-title-line (denote-retrieve-front-matter-title-line file file-type))
             (old-keywords-line (denote-retrieve-front-matter-keywords-line file file-type))
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
            (delete-region (point) (line-end-position))
            (when no-confirm (save-buffer))))))))

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
                         (format "Rename FILE Denote-style [%s]: " file)
                       "Rename FILE Denote-style: "))
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
(defun denote-rename-file (file &optional title keywords signature date)
  "Rename file and update existing front matter if appropriate.

Always rename the file where it is located in the file system:
never move it to another directory.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.  When called from Lisp, FILE
is a file system path represented as a string.

If FILE has a Denote-compliant identifier, retain it while
updating components of the file name referenced by the user
option `denote-prompts'.  By default, these are the TITLE and
KEYWORDS.  The SIGNATURE is another one.  When called from Lisp,
TITLE and SIGNATURE are strings, while KEYWORDS is a list of
strings.

If there is no identifier, create an identifier based on the
following conditions:

1. If the `denote-prompts' includes an entry for date prompts,
   then prompt for DATE and take its input to produce a new
   identifier.  For use in Lisp, DATE must conform with
   `denote-valid-date-p'.

2. If DATE is nil (e.g. when `denote-prompts' does not include a
   date entry), use the file attributes to determine the last
   modified date of FILE and format it as an identifier.

3. As a fallback, derive an identifier from the current date and
   time.

4. At any rate, if the resulting identifier is not unique among
   the files in the variable `denote-directory', increment it
   such that it becomes unique.

In interactive use, and assuming `denote-prompts' includes a
title entry, make the TITLE prompt have prefilled text in the
minibuffer that consists of the current title of FILE.  The
current title is either retrieved from the front matter (such as
the #+title in Org) or from the file name.

Do the same for the SIGNATURE prompt, subject to `denote-prompts',
by prefilling the minibuffer with the current signature of FILE,
if any.

Same principle for the KEYWORDS prompt: convert the keywords in
the file name into a comma-separated string and prefill the
minibuffer with it (the KEYWORDS prompt accepts more than one
keywords, each separated by a comma, else the `crm-separator').

For all prompts, interpret an empty input as an instruction to
remove that file name component.  For example, if a TITLE prompt
is available and FILE is 20240211T093531--some-title__keyword1.org
then rename FILE to 20240211T093531__keyword1.org.

If a file name component is present, but there is no entry for it in
`denote-prompts', keep it as-is.

[ NOTE: Please check with your minibuffer user interface how to
  provide an empty input.  The Emacs default setup accepts the
  empty minibuffer contents as they are, though popular packages
  like `vertico' use the first available completion candidate
  instead.  For `vertico', the user must either move one up to
  select the prompt and then type RET there with empty contents,
  or use the command `vertico-exit-input' with empty contents.
  That Vertico command is bound to M-RET as of this writing on
  2024-02-13 08:08 +0200. ]

When renaming FILE, read its file type extension (like .org) and
preserve it through the renaming process.  Files that have no
extension are left without one.

As a final step, ask for confirmation, showing the difference
between old and new file names.  Do not ask for confirmation if
the user option `denote-rename-no-confirm' is set to a non-nil
value.

If FILE has front matter for TITLE and KEYWORDS, ask to rewrite
their values in order to reflect the new input, unless
`denote-rename-no-confirm' is non-nil.  When the
`denote-rename-no-confirm' is nil (the default), do not save the
underlying buffer, thus giving the user the option to
double-check the result, such as by invokling the command
`diff-buffer-with-file'.  The rewrite of the TITLE and KEYWORDS
in the front matter should not affect the rest of the front
matter.

If the file does not have front matter but is among the supported
file types (per `denote-file-type'), add front matter to the top
of it and leave the buffer unsaved for further inspection.  Save
the buffer if `denote-rename-no-confirm' is non-nil.

For the front matter of each file type, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'

Run the `denote-after-rename-file-hook' after renaming FILE.

This command is intended to (i) rename Denote files, (ii) convert
existing supported file types to Denote notes, and (ii) rename
non-note files (e.g. PDF) that can benefit from Denote's
file-naming scheme.

For a version of this command that works with multiple files
one-by-one, use `denote-dired-rename-files'."
  (interactive
   (let* ((file (denote--rename-dired-file-or-prompt))
          (file-type (denote-filetype-heuristics file))
          (file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name))
          (date nil)
          (title (denote-retrieve-title-or-filename file file-type))
          (keywords (denote-convert-file-name-keywords-to-crm (or (denote-retrieve-filename-keywords file) "")))
          (signature (or (denote-retrieve-filename-signature file) "")))
     (dolist (prompt denote-prompts)
       (pcase prompt
         ('title
          (setq title (denote-title-prompt
                       title
                       (format "Rename `%s' with TITLE (empty to remove)" file-in-prompt))))
         ('keywords
          (setq keywords (denote-keywords-prompt
                          (format "Rename `%s' with KEYWORDS (empty to remove)" file-in-prompt)
                          keywords)))
         ('signature
          (setq signature (denote-signature-prompt
                           signature
                           (format "Rename `%s' with SIGNATURE (empty to remove)" file-in-prompt))))
         ('date
          (unless (denote-file-has-identifier-p file)
            (setq date (denote-date-prompt))))))
     (list file title keywords signature date)))
  (setq keywords (denote-keywords-sort
                  (if (stringp keywords)
                      (split-string keywords "," :omit-nulls)
                    keywords)))
  (let* ((dir (file-name-directory file))
         (id (or (denote-retrieve-filename-identifier file)
                 (denote-create-unique-file-identifier file (denote--get-all-used-ids) date)))
         ;; TODO 2024-02-13: Should we derive the extension from the
         ;; `denote-file-type-prompt' if we are conforming with the
         ;; `denote-prompts'?
         (extension (denote-get-file-extension file))
         (file-type (denote-filetype-heuristics file))
         (new-name (denote-format-file-name dir id keywords title extension signature))
         (max-mini-window-height denote-rename-max-mini-window-height))
    (when (or denote-rename-no-confirm (denote-rename-file-prompt file new-name))
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (when (denote-file-is-writable-and-supported-p new-name)
        (if (denote--edit-front-matter-p new-name file-type)
            (denote-rewrite-front-matter new-name title keywords file-type denote-rename-no-confirm)
          (denote--add-front-matter new-name title keywords id file-type denote-rename-no-confirm)))
      (run-hooks 'denote-after-rename-file-hook))
    new-name))

;;;###autoload
(defun denote-dired-rename-files ()
  "Rename Dired marked files same way as `denote-rename-file'.
Rename each file in sequence, making all the relevant prompts.
Unlike `denote-rename-file', do not prompt for confirmation of
the changes made to the file: perform them outright (same as
setting `denote-rename-no-confirm' to a non-nil value)."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files)))
      (let ((used-ids (unless (seq-every-p #'denote-file-has-identifier-p marks)
                        (denote--get-all-used-ids))))
        (dolist (file marks)
          (let* ((file-type (denote-filetype-heuristics file))
                 (file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name))
                 (dir (file-name-directory file))
                 (id (or (denote-retrieve-filename-identifier file)
                         (denote-create-unique-file-identifier file used-ids)))
                 (title (denote-retrieve-title-or-filename file file-type))
                 (keywords (denote-convert-file-name-keywords-to-crm (or (denote-retrieve-filename-keywords file) "")))
                 (signature (or (denote-retrieve-filename-signature file) ""))
                 (extension (denote-get-file-extension file)))
            (dolist (prompt denote-prompts)
              (pcase prompt
                ('title
                 (setq title (denote-title-prompt
                              title
                              (format "Rename `%s' with TITLE (empty to remove)" file-in-prompt))))
                ('keywords
                 (setq keywords (denote-keywords-prompt
                                 (format "Rename `%s' with KEYWORDS (empty to remove)" file-in-prompt)
                                 keywords)))
                ('signature
                 (setq signature (denote-signature-prompt
                                  signature
                                  (format "Rename `%s' with SIGNATURE (empty to remove)" file-in-prompt))))
                ('date
                 (setq id (denote-prompt-for-date-return-id)))))
            (setq keywords (denote-keywords-sort
                            (if (stringp keywords)
                                (split-string keywords "," :omit-nulls)
                              keywords)))
            (let ((new-name (denote-format-file-name dir id keywords title extension signature)))
              (denote-rename-file-and-buffer file new-name)
              (when (denote-file-is-writable-and-supported-p new-name)
                (if (denote--edit-front-matter-p new-name file-type)
                    (denote-rewrite-front-matter new-name title keywords file-type :no-confirm)
                  (denote--add-front-matter new-name title keywords id file-type :save-buffer)))
              (run-hooks 'denote-after-rename-file-hook)
              (when used-ids
                (puthash id t used-ids)))))
        (denote-update-dired-buffers))
    (user-error "No marked files; aborting")))

(make-obsolete
 'denote-dired-rename-marked-files
 'denote-dired-rename-marked-files-with-keywords
 "2.1.0")

(defalias 'denote-dired-rename-marked-files 'denote-dired-rename-files
  "Alias for `denote-dired-rename-files'.")

;;;###autoload
(defun denote-dired-rename-marked-files-with-keywords ()
  "Rename marked files in Dired to a Denote file name by writing keywords.

Specifically, do the following:

- retain the file's existing name and make it the TITLE field,
  per Denote's file-naming scheme;

- sluggify the TITLE, according to our conventions (check the
  user option `denote-file-name-slug-functions');

- prepend an identifier to the TITLE;

- preserve the file's extension, if any;

- prompt once for KEYWORDS and apply the user's input to the
  corresponding field in the file name, rewriting any keywords
  that may exist while removing keywords that do exist if
  KEYWORDS is empty;

- add or rewrite existing front matter to the underlying file, if
  it is recognized as a Denote note (per `denote-file-type'),
  such that it includes the new keywords.

Run the `denote-after-rename-file-hook' after renaming is done.

[ Note that the affected buffers are not saved, unless the user
  option `denote-rename-no-confirm' is non-nil.  Users can thus
  check them to confirm that the new front matter does not cause
  any problems (e.g. with the `diff-buffer-with-file' command).
  Multiple buffers can be saved in one go with the command
  `save-some-buffers' (read its doc string).  ]"
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files)))
      (let ((keywords (denote-keywords-sort
                       (denote-keywords-prompt "Rename marked files with KEYWORDS, overwriting existing (empty to ignore/remove)")))
            (used-ids (unless (seq-every-p #'denote-file-has-identifier-p marks)
                        (denote--get-all-used-ids))))
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (or (denote-retrieve-filename-identifier file)
                         (denote-create-unique-file-identifier file used-ids)))
                 (signature (or (denote-retrieve-filename-signature file) ""))
                 (file-type (denote-filetype-heuristics file))
                 (title (denote-retrieve-title-or-filename file file-type))
                 (extension (denote-get-file-extension file))
                 (new-name (denote-format-file-name dir id keywords title extension signature)))
            (denote-rename-file-and-buffer file new-name)
            (when (denote-file-is-writable-and-supported-p new-name)
              (if (denote--edit-front-matter-p new-name file-type)
                  (denote-rewrite-keywords new-name keywords file-type denote-rename-no-confirm)
                (denote--add-front-matter new-name title keywords id file-type denote-rename-no-confirm)))
            (run-hooks 'denote-after-rename-file-hook)
            (when used-ids
              (puthash id t used-ids))))
        (denote-update-dired-buffers))
    (user-error "No marked files; aborting")))

;;;###autoload
(defun denote-rename-file-using-front-matter (file &optional no-confirm save-buffer)
  "Rename FILE using its front matter as input.
When called interactively, FILE is the return value of the
function `buffer-file-name' which is subsequently inspected for
the requisite front matter.  It is thus implied that the FILE has
a file type that is supported by Denote, per `denote-file-type'.

Unless NO-CONFIRM is non-nil (such as with a prefix argument),
ask for confirmation, showing the difference between the old and
the new file names.

Never modify the identifier of the FILE, if any, even if it is
edited in the front matter.  Denote considers the file name to be
the source of truth in this case to avoid potential breakage with
typos and the like.

If NO-CONFIRM is non-nil (such as with a prefix argument) do not
prompt for confirmation while renaming the file.  Do it outright.

If optional SAVE-BUFFER is non-nil (such as with a double prefix
argument), save the corresponding buffer.

If the user option `denote-rename-no-confirm' is non-nil,
interpret it the same way as a combination of NO-CONFIRM and
SAVE-BUFFER.

The identifier of the file, if any, is never modified even if it
is edited in the front matter: Denote considers the file name to
be the source of truth in this case, to avoid potential breakage
with typos and the like."
  (interactive
   (let (no-confirm save-buffer)
     (cond
      ((and current-prefix-arg (> (prefix-numeric-value current-prefix-arg) 4))
       (setq no-confirm t
             save-buffer t))
      (current-prefix-arg
       (setq no-confirm t)))
     (list buffer-file-name no-confirm save-buffer)))
  (unless (denote-file-is-writable-and-supported-p file)
    (user-error "The file is not writable or does not have a supported file extension"))
  (if-let ((file-type (denote-filetype-heuristics file))
           (title (denote-retrieve-front-matter-title-value file file-type))
           (id (denote-retrieve-filename-identifier file)))
      (let* ((keywords (denote-retrieve-front-matter-keywords-value file file-type))
             (signature (or (denote-retrieve-filename-signature file) ""))
             (extension (denote-get-file-extension file))
             (dir (file-name-directory file))
             (new-name (denote-format-file-name dir id keywords title extension signature)))
        (when (or denote-rename-no-confirm
                  no-confirm
                  (denote-rename-file-prompt file new-name))
          (denote-rename-file-and-buffer file new-name)
          (denote-update-dired-buffers)
          (when (or denote-rename-no-confirm save-buffer)
            (save-buffer))
          (run-hooks 'denote-after-rename-file-hook)))
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
                          (denote-file-has-identifier-p m)))
                   (dired-get-marked-files))))
      (progn
        (dolist (file marks)
          (denote-rename-file-using-front-matter file :no-confirm denote-rename-no-confirm))
        (denote-update-dired-buffers))
    (user-error "No marked Denote files; aborting")))

;;;;;; Interactively modify keywords and rename accordingly

;;;###autoload
(defun denote-keywords-add (keywords &optional save-buffer)
  "Prompt for KEYWORDS to add to the current note's front matter.
When called from Lisp, KEYWORDS is a list of strings.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'.

With an optional SAVE-BUFFER (such as a prefix argument when
called interactively), save the buffer outright.  Otherwise leave
the buffer unsaved for further review.

If the user option `denote-rename-no-confirm' is non-nil,
interpret it the same way as SAVE-BUFFER, making SAVE-BUFFER
reduntant.

Run `denote-after-rename-file-hook' as a final step."
  (interactive (list (denote-keywords-prompt "Add KEYWORDS") current-prefix-arg))
  ;; A combination of if-let and let, as we need to take into account
  ;; the scenario in which there are no keywords yet.
  (if-let ((file (buffer-file-name))
           ((denote-file-is-note-p file))
           (file-type (denote-filetype-heuristics file)))
      (let* ((cur-keywords (denote-retrieve-front-matter-keywords-value file file-type))
             (new-keywords (denote-keywords-sort
                            (seq-uniq (append keywords cur-keywords)))))
        (denote-rewrite-keywords file new-keywords file-type)
        (denote-rename-file-using-front-matter file :no-confirm (or denote-rename-no-confirm save-buffer))
        (run-hooks 'denote-after-rename-file-hook))
    (user-error "Buffer not visiting a Denote file")))

(defalias 'denote-rename-add-keywords 'denote-keywords-add
  "Alias for `denote-keywords-add'.")

(defun denote--keywords-delete-prompt (keywords)
  "Prompt for one or more KEYWORDS.
In the case of multiple entries, those are separated by the
`crm-separator', which typically is a comma.  In such a case, the
output is sorted with `string-collate-lessp'."
  (let ((choice (denote--keywords-crm keywords "Keywords to remove")))
    (if denote-sort-keywords
        (sort choice #'string-collate-lessp)
      choice)))

;;;###autoload
(defun denote-keywords-remove (&optional save-buffer)
  "Prompt for keywords in current note and remove them.
Keywords are retrieved from the file's front matter.

Rename the file without further prompt so that its name reflects
the new front matter, per `denote-rename-file-using-front-matter'.

With an optional SAVE-BUFFER as a prefix argument, save the
buffer outright.  Otherwise leave the buffer unsaved for further
review.

If the user option `denote-rename-no-confirm' is non-nil,
interpret it the same way as SAVE-BUFFER, making SAVE-BUFFER
reduntant.

Run `denote-after-rename-file-hook' as a final step."
  (declare (interactive-only t))
  (interactive "P")
  (if-let ((file (buffer-file-name))
           ((denote-file-is-note-p file))
           (file-type (denote-filetype-heuristics file)))
      (when-let ((cur-keywords (denote-retrieve-front-matter-keywords-value file file-type))
                 (del-keyword (denote--keywords-delete-prompt cur-keywords)))
        (denote-rewrite-keywords
         file
         (seq-difference cur-keywords del-keyword)
         file-type)
        (denote-rename-file-using-front-matter file :no-confirm (or denote-rename-no-confirm save-buffer))
        (run-hooks 'denote-after-rename-file-hook))
    (user-error "Buffer not visiting a Denote file")))

(defalias 'denote-rename-remove-keywords 'denote-keywords-remove
  "Alias for `denote-keywords-remove'.")

;;;;;; Interactively add or remove file name signature

;;;###autoload
(defun denote-rename-add-signature (file signature)
  "Add to FILE name the SIGNATURE.
In interactive use, prompt for FILE, defaulting either to the current
buffer's file or the one at point in a Dired buffer.  Also prompt for
SIGNATURE, using the existing one, if any, as the initial value.

When called from Lisp, FILE is a string pointing to a file system path
and SIGNATURE is a string.

Ask for confirmation before renaming the file to include the new
signature.  Do it unless the user option `denote-rename-no-confirm' is
set to a non-nil value.

Once the operation is done, reload any Dired buffers and run the
`denote-after-rename-file-hook'.

Also see `denote-rename-remove-signature'."
  (interactive
   (let* ((file (denote--rename-dired-file-or-prompt))
          (file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name)))
     (list
      file
      (denote-signature-prompt
       (or (denote-retrieve-filename-signature file) "")
       (format "Rename `%s' with SIGNATURE (empty to remove)" file-in-prompt)))))
  (let* ((type (denote-filetype-heuristics file))
         (title (denote-retrieve-title-or-filename file type))
         (keywords-string (denote-retrieve-filename-keywords file))
         (keywords (when keywords-string (split-string keywords-string "_" :omit-nulls "_")))
         (dir (file-name-directory file))
         (id (or (denote-retrieve-filename-identifier file)
                 (denote-create-unique-file-identifier file (denote--get-all-used-ids))))
         (extension (denote-get-file-extension file))
         (new-name (denote-format-file-name dir id keywords title extension signature)))
    (when (or denote-rename-no-confirm (denote-rename-file-prompt file new-name))
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (run-hooks 'denote-after-rename-file-hook))))

;;;###autoload
(defun denote-rename-remove-signature (file)
  "Remove the signature of FILE.
In interactive use, prompt for FILE, defaulting either to the current
buffer's file or the one at point in a Dired buffer.  When called from
Lisp, FILE is a string pointing to a file system path.

Ask for confirmation before renaming the file to remove its signature.
Do it unless the user option `denote-rename-no-confirm' is set to a
non-nil value.

Once the operation is done, reload any Dired buffers and run the
`denote-after-rename-file-hook'.

Also see `denote-rename-add-signature'."
  (interactive (list (denote--rename-dired-file-or-prompt)))
  (when (denote-retrieve-filename-signature file)
    (let* ((type (denote-filetype-heuristics file))
           (title (denote-retrieve-title-or-filename file type))
           (keywords-string (denote-retrieve-filename-keywords file))
           (keywords (when keywords-string (split-string keywords-string "_" :omit-nulls "_")))
           (dir (file-name-directory file))
           (id (or (denote-retrieve-filename-identifier file)
                   (denote-create-unique-file-identifier file (denote--get-all-used-ids))))
           (extension (denote-get-file-extension file))
           (new-name (denote-format-file-name dir id keywords title extension nil)))
      (when (or denote-rename-no-confirm (denote-rename-file-prompt file new-name))
        (denote-rename-file-and-buffer file new-name)
        (denote-update-dired-buffers)
        (run-hooks 'denote-after-rename-file-hook)))))

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
relevant front matter.

[ NOTE: Please check with your minibuffer user interface how to
  provide an empty input.  The Emacs default setup accepts the
  empty minibuffer contents as they are, though popular packages
  like `vertico' use the first available completion candidate
  instead.  For `vertico', the user must either move one up to
  select the prompt and then type RET there with empty contents,
  or use the command `vertico-exit-input' with empty contents.
  That Vertico command is bound to M-RET as of this writing on
  2024-02-29 09:24 +0200. ]"
  (interactive
   (list
    (buffer-file-name)
    (denote-title-prompt nil "Add TITLE (empty to ignore)")
    (denote-keywords-sort (denote-keywords-prompt "Add KEYWORDS (empty to ignore)"))))
  (when-let ((denote-file-is-writable-and-supported-p file)
             (id (denote-retrieve-filename-identifier file))
             (file-type (denote-filetype-heuristics file)))
    (denote--add-front-matter file title keywords id file-type)))

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
         (id (or (denote-retrieve-filename-identifier file) ""))
         (title (denote-retrieve-title-or-filename file old-file-type))
         (keywords (denote-retrieve-front-matter-keywords-value file old-file-type))
         (signature (or (denote-retrieve-filename-signature file) ""))
         (old-extension (denote-get-file-extension file))
         (new-extension (denote--file-extension new-file-type))
         (new-name (denote-format-file-name dir id keywords title new-extension signature))
         (max-mini-window-height denote-rename-max-mini-window-height))
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

(defface denote-faces-year '((t :inherit denote-faces-date))
  "Face for file name year in Dired buffers.
This is the part of the identifier that covers the year, month, and day."
  :group 'denote-faces
  :package-version '(denote . "2.3.0"))

(defface denote-faces-month '((t :inherit denote-faces-date))
  "Face for file name month in Dired buffers.
This is the part of the identifier that covers the year, month, and day."
  :group 'denote-faces
  :package-version '(denote . "2.3.0"))

(defface denote-faces-day '((t :inherit denote-faces-date))
  "Face for file name day in Dired buffers.
This is the part of the identifier that covers the year, month, and day."
  :group 'denote-faces
  :package-version '(denote . "2.3.0"))

(defface denote-faces-hour '((t :inherit denote-faces-date))
  "Face for file name hours in Dired buffers.
This is the part of the identifier that covers the hours, minutes,
and seconds."
  :group 'denote-faces
  :package-version '(denote . "2.3.0"))

(defface denote-faces-minute '((t :inherit denote-faces-date))
  "Face for file name minutes in Dired buffers.
This is the part of the identifier that covers the hours, minutes,
and seconds."
  :group 'denote-faces
  :package-version '(denote . "2.3.0"))

(defface denote-faces-second '((t :inherit denote-faces-date))
  "Face for file name seconds in Dired buffers.
This is the part of the identifier that covers the hours, minutes,
and seconds."
  :group 'denote-faces
  :package-version '(denote . "2.3.0"))

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

(defvar denote-faces--file-name-regexp
  (concat "\\(?11:[\t\s]+\\|.*/\\)?"
          "\\(?1:[0-9]\\{4\\}\\)\\(?12:[0-9]\\{2\\}\\)\\(?13:[0-9]\\{2\\}\\)"
          "\\(?10:T\\)"
          "\\(?2:[0-9]\\{2\\}\\)\\(?14:[0-9]\\{2\\}\\)\\(?15:[0-9]\\{2\\}\\)"
          "\\(?:\\(?3:==\\)\\(?4:[^.]*?\\)\\)?"
          "\\(?:\\(?5:--\\)\\(?6:[^.]*?\\)\\)?"
          "\\(?:\\(?7:__\\)\\(?8:[^.]*?\\)\\)?"
          "\\(?9:\\..*\\)?$")
  "Regexp of file names for fontification.")

(defconst denote-faces-file-name-keywords
  `((,denote-faces--file-name-regexp
     (11 'denote-faces-subdirectory nil t)
     (1 'denote-faces-year nil t)
     (12 'denote-faces-month nil t)
     (13 'denote-faces-day nil t)
     (10 'denote-faces-time-delimiter nil t)
     (2 'denote-faces-hour nil t)
     (14 'denote-faces-minute nil t)
     (15 'denote-faces-second nil t)
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

(defun denote-format-link (file description file-type id-only)
  "Prepare link to FILE using DESCRIPTION.

FILE-TYPE and ID-ONLY are used to get the format of the link.
See the `:link' property of `denote-file-types'."
  (format
   (if (or id-only (null description) (string-empty-p description))
       denote-id-only-link-format
     (denote--link-format file-type))
   (denote-retrieve-filename-identifier file)
   description))

(make-obsolete 'denote-link--format-link 'denote-format-link "2.1.0")

;; NOTE 2023-12-05 04:16 +0200: This is a candidate for a user option,
;; subject to feedback.  I think the signature should be better
;; disambiguated in this context, although the double space is a good
;; start.
(define-obsolete-variable-alias
  'denote--link-signature-format
  'denote-link-signature-format
  "2.3.0")

(defvar denote-link-signature-format "%s  %s"
  "Format of link description for `denote-link-with-signature'.")

;; TODO 2024-02-22: Consider documenting this, such as:
;; <https://github.com/protesilaos/denote/issues/255#issuecomment-1949634482>.
(defvar denote-link-description-function #'denote-link-description-with-signature-and-title
  "Function to use to create the description of links.

The function specified should take a FILE argument and should
return the description as a string.  By default, the title of the
file is returned as the description.")

(defun denote--link-get-description (file)
  "Return link description for FILE."
  (funcall
   (or denote-link-description-function #'denote-link-description-with-signature-and-title)
   file))

(defun denote-link-description-with-signature-and-title (file)
  "Return link description for FILE.

- If the region is active, use it as the description.

- If FILE as a signature, then use the `denote-link-signature-format'.
  By default, this looks like \"signature   title\".

- If FILE does not have a signature, then use its title as the
  description."
  (let* ((file-type (denote-filetype-heuristics file))
         (signature (denote-retrieve-filename-signature file))
         (title (denote-retrieve-title-or-filename file file-type))
         (region-text (denote--get-active-region-content)))
    (cond (region-text
           region-text)
          (signature
           (format denote-link-signature-format signature title))
          (t
           (format "%s" title)))))

(defun denote--get-active-region-content ()
  "Return the text of the active region, else nil."
  (when-let (((region-active-p))
             (beg (region-beginning))
             (end (region-end)))
    (string-trim (buffer-substring-no-properties beg end))))

(defun denote--delete-active-region-content ()
  "Delete the content of the active region, if any."
  (when-let (((region-active-p))
             (beg (region-beginning))
             (end (region-end)))
    (delete-region beg end)))

;;;###autoload
(defun denote-link (file file-type description &optional id-only)
  "Create link to FILE note in variable `denote-directory' with DESCRIPTION.

When called interactively, prompt for FILE using completion.  In
this case, derive FILE-TYPE from the current buffer.

The DESCRIPTION is returned by the function specified in variable
`denote-link-description-function'.  If the region is active, its
content is deleted and can be used as the description of the
link.  The default value of `denote-link-description-function'
returns the content of the active region, if any, else the title
of the linked file is used as the description.  The title comes
either from the front matter or the file name.  Note that if you
change the default value of `denote-link-description-function',
make sure to use the `region-text' parameter.  Regardless of the
value of this user option, `denote-link' will always replace the
content of the active region.

With optional ID-ONLY as a non-nil argument, such as with a
universal prefix (\\[universal-argument]), insert links with just
the identifier and no further description.  In this case, the
link format is always [[denote:IDENTIFIER]].  If the DESCRIPTION
is empty, the link is also as if ID-ONLY were non-nil.  The
default value of `denote-link-description-function' returns an
empty string when the region is empty.  Thus, the link will have
no description in this case.

When called from Lisp, FILE is a string representing a full file
system path.  FILE-TYPE is a symbol as described in
`denote-file-type'.  DESCRIPTION is a string.  Whether the caller
treats the active region specially, is up to it."
  (interactive
   (let* ((file (denote-file-prompt nil "Link to FILE"))
          (file-type (denote-filetype-heuristics buffer-file-name))
          (description (when (file-exists-p file)
                         (denote--link-get-description file))))
       (list file file-type description current-prefix-arg)))
  (unless (or file-type (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (unless (file-exists-p file)
    (user-error "The linked file does not exist"))
  (let* ((beg (point)))
    (denote--delete-active-region-content)
    (insert (denote-format-link file description file-type id-only))
    (unless (derived-mode-p 'org-mode)
      (make-button beg (point) 'type 'denote-link-button))))

(define-obsolete-function-alias
  'denote-link-insert-link
  'denote-insert-link
  "2.0.0")

(defalias 'denote-insert-link 'denote-link
  "Alias for `denote-link' command.")

;;;###autoload
(defun denote-link-with-signature ()
  "Insert link to file with signature.
Prompt for file using minibuffer completion, limiting the list of
candidates to files with a signature in their file name.

By default, the description of the link includes the signature,
if present, followed by the file's title, if any.

For more advanced uses with Lisp, refer to the `denote-link'
function."
  (declare (interactive-only t))
  (interactive)
  (unless (and (buffer-file-name) (denote-file-has-supported-extension-p (buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let* ((file (denote-file-prompt "="))
         (type (denote-filetype-heuristics (buffer-file-name)))
         (description (denote--link-get-description file)))
    (denote-link file type description)))

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

(defvar denote-link-find-file-history nil
  "History for `denote-find-link'.")

(defalias 'denote-link--find-file-history 'denote-link-find-file-history
  "Compatibility alias for `denote-link-find-file-history'.")

(defun denote-link--find-file-prompt (files)
  "Prompt for linked file among FILES."
  (let ((file-names (mapcar #'denote-get-file-name-relative-to-denote-directory
                            files)))
    (completing-read
     "Find linked file: "
     (denote--completion-table 'file file-names)
     nil t nil 'denote-link-find-file-history)))

(defun denote-link-return-links (&optional file)
  "Return list of links in current or optional FILE.
Also see `denote-link-return-backlinks'."
  (when-let ((current-file (or file (buffer-file-name)))
             ((denote-file-has-supported-extension-p current-file))
             (file-type (denote-filetype-heuristics current-file))
             (regexp (denote--link-in-context-regexp file-type)))
    (with-temp-buffer
      (insert-file-contents current-file)
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
   (concat
    (denote-directory)
    (denote-link--find-file-prompt
     (or (denote-link-return-links)
         (user-error "No links found"))))))

(defun denote-link-return-backlinks (&optional file)
  "Return list of backlinks in current or optional FILE.
Also see `denote-link-return-links'."
  (when-let ((current-file (or file (buffer-file-name)))
             (id (denote-retrieve-filename-identifier-with-error current-file)))
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
  (unless (and (buffer-file-name) (denote-file-has-supported-extension-p (buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let* ((type (denote-filetype-heuristics (buffer-file-name)))
         (path (denote--command-with-features #'denote nil nil :save :in-background))
         (description (denote--link-get-description path)))
    (denote-link path type description id-only)))

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
  (unless (and (buffer-file-name) (denote-file-has-supported-extension-p (buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let* ((type (denote-filetype-heuristics (buffer-file-name)))
         (path (denote--command-with-features command nil nil :save :in-background))
         (description (denote--link-get-description path)))
    (denote-link path type description id-only)))

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
  (interactive
   (let* ((target (denote-file-prompt)))
     (unless (and target (file-exists-p target))
       (setq target (denote--command-with-features #'denote :use-file-prompt-as-def-title :ignore-region :save :in-background)))
     (list target current-prefix-arg)))
  (unless (and (buffer-file-name) (denote-file-has-supported-extension-p (buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (denote-link target
               (denote-filetype-heuristics (buffer-file-name))
               (denote--link-get-description target)
               id-only))

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

(defun denote-link-markdown-follow (link)
  "Function to open Denote file present in LINK.
To be assigned to `markdown-follow-link-functions'."
  (when (ignore-errors (string-match denote-id-regexp link))
    (funcall denote-link-button-action
             (denote-get-path-by-id (match-string 0 link)))))

(eval-after-load 'markdown-mode
  '(add-hook 'markdown-follow-link-functions #'denote-link-markdown-follow))

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

(define-obsolete-function-alias
  'denote-backlinks-next
  'denote-backlinks-mode-next
  "2.3.0")

(defun denote-backlinks-mode-next (n)
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

(define-obsolete-function-alias
  'denote-backlinks-prev
  'denote-backlinks-mode-previous
  "2.3.0")

(defun denote-backlinks-mode-previous (n)
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
    (define-key m "n" #'denote-backlinks-mode-next)
    (define-key m "p" #'denote-backlinks-mode-previous)
    (define-key m "g" #'revert-buffer)
    m)
  "Keymap for `denote-backlinks-mode'.")

(define-derived-mode denote-backlinks-mode xref--xref-buffer-mode "Backlinks"
  :interactive nil
  "Major mode for backlinks buffers."
  (unless denote-backlinks-show-context
    (font-lock-add-keywords nil denote-faces-file-name-keywords t)))

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
         (id (denote-retrieve-filename-identifier-with-error file))
         (buf (format "*denote-backlinks to %s*" id))
         ;; We retrieve results in absolute form and change the absolute
         ;; path to a relative path a few lines below. We could add a
         ;; suitable function to project-find-functions and the results
         ;; would be automatically in relative form, but eventually
         ;; notes may not be all under a common directory (or project).
         (xref-file-name-display 'abs)
         (xref-alist (xref--analyze (funcall fetcher)))
         (dir (denote-directory)))
    ;; Change the GROUP of each item in xref-alist to a relative path
    (mapc (lambda (x)
            (setf (car x) (denote-get-file-name-relative-to-denote-directory (car x))))
          xref-alist)
    (with-current-buffer (get-buffer-create buf)
      (setq-local default-directory dir)
      (erase-buffer)
      (setq overlay-arrow-position nil)
      (denote-backlinks-mode)
      (goto-char (point-min))
      (when-let  ((title (denote-retrieve-front-matter-title-value file file-type))
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
      (let* ((id (denote-retrieve-filename-identifier-with-error file))
             (xref-show-xrefs-function #'denote-link--prepare-backlinks))
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
When ID-ONLY is non-nil, use a generic link format.

With optional NO-SORT do not try to sort the inserted lines.
Otherwise sort lines while accounting for `denote-link-add-links-sort'."
  (with-temp-buffer
    (mapc
     (lambda (file)
       (let ((description (denote--link-get-description file)))
         (insert
          (format
           denote-link--prepare-links-format
           (denote-format-link file description current-file-type id-only)))))
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

(defun denote--menu-bar-enable ()
  "Enable Denote menu bar."
  (define-key-after global-map [menu-bar denote]
    (easy-menu-binding
     (easy-menu-create-menu "Denote" denote--menu-contents) "Denote")
    "Tools"))

;; Enable Denote menu bar by default
(denote--menu-bar-enable)

;;;###autoload
(define-minor-mode denote-menu-bar-mode "Show Denote menu bar."
  :global t
  :init-value t
  (if denote-menu-bar-mode
      (denote--menu-bar-enable)
    (define-key global-map [menu-bar denote] nil)))

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

(defun denote-link--ol-resolve-link-to-target (link &optional full-data)
  "Resolve LINK to target file, with or without additioanl search terms.
With optional FULL-DATA return a list in the form of (path id search)."
  (let* ((search (and (string-match "::\\(.*\\)\\'" link)
                      (match-string 1 link)))
         (id (if (and search (not (string-empty-p search)))
                 (substring link 0 (match-beginning 0))
               link))
         (path (denote-get-path-by-id id)))
    (cond
     (full-data
      (list path id search))
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

(declare-function org-entry-put "org" (pom property value))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-id-new "org-id" (&optional prefix))

(defun denote-link-ol-get-id ()
  "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else
create a new one."
  (let* ((pos (point))
         (id (org-entry-get pos "CUSTOM_ID")))
    (if (and id (stringp id) (string-match-p "\\S-" id))
        id
      (setq id (org-id-new "h"))
      (org-entry-put pos "CUSTOM_ID" id)
      id)))

(declare-function org-get-heading "org" (no-tags no-todo no-priority no-comment))

(defun denote-link-ol-get-heading ()
  "Get current Org heading text."
  (org-get-heading :no-tags :no-todo :no-priority :no-comment))

(defun denote-link-format-heading-description (file-text heading-text)
  "Return description for FILE-TEXT with HEADING-TEXT at the end."
  (format "%s::%s" file-text heading-text))

;;;###autoload
(defun denote-link-ol-store ()
  "Handler for `org-store-link' adding support for denote: links.
Also see the user option `denote-org-store-link-to-heading'."
  (when-let ((file (buffer-file-name))
             ((denote-file-is-note-p file))
             (file-id (denote-retrieve-filename-identifier file))
             (description (denote--link-get-description file)))
    (let ((heading-links (and denote-org-store-link-to-heading (derived-mode-p 'org-mode))))
      (org-link-store-props
       :type "denote"
       :description (if heading-links
                        (denote-link-format-heading-description
                         description
                         (denote-link-ol-get-heading))
                      description)
       :link (if heading-links
                 (format "denote:%s::#%s" file-id (denote-link-ol-get-id))
               (concat "denote:" file-id)))
      org-store-link-plist)))

;;;###autoload
(defun denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :full-data))
         (path (file-relative-name (nth 0 path-id)))
         (id (nth 1 path-id))
         (search (nth 2 path-id))
         (anchor (file-name-sans-extension path))
         (desc (cond
                (description)
                (search (format "denote:%s::%s" id search))
                (t (concat "denote:" id)))))
    (cond
     ((eq format 'html)
      (if search
          (format "<a href=\"%s.html%s\">%s</a>" anchor search desc)
        (format "<a href=\"%s.html\">%s</a>" anchor desc)))
     ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
     ((eq format 'md) (format "[%s](%s)" desc path))
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

This function obeys `denote-prompts', but it ignores `file-type',
if present: it always sets the Org file extension for the created
note to ensure that the capture process works as intended,
especially for the desired output of the
`denote-org-capture-specifiers' (which can include arbitrary
text).

Consult the manual for template samples."
  (let (title keywords subdirectory date template signature)
    (dolist (prompt denote-prompts)
      (pcase prompt
        ('title (setq title (denote-title-prompt
                              (when (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning)
                                 (region-end))))))
        ('keywords (setq keywords (denote-keywords-prompt)))
        ('subdirectory (setq subdirectory (denote-subdirectory-prompt)))
        ('date (setq date (denote-date-prompt)))
        ('template (setq template (denote-template-prompt)))
        ('signature (setq signature (denote-signature-prompt)))))
  (let* ((title (or title ""))
         (date (if (or (null date) (string-empty-p date))
                   (current-time)
                 (denote-valid-date-p date)))
         (id (denote--find-first-unused-id
              (denote-get-identifier date)
              (denote--get-all-used-ids)))
         (keywords (denote-keywords-sort keywords))
         (directory (if (denote--dir-in-denote-directory-p subdirectory)
                        (file-name-as-directory subdirectory)
                      (denote-directory)))
         (template (or (alist-get template denote-templates) ""))
         (signature (or signature ""))
         (front-matter (denote--format-front-matter
                        title (denote--date nil 'org) keywords
                        (denote-get-identifier) 'org)))
    (setq denote-last-path
          (denote--path title keywords directory id 'org signature))
    (denote--keywords-add-to-history keywords)
    (concat front-matter template denote-org-capture-specifiers))))

;; TODO 2023-12-02: Maybe simplify `denote-org-capture-with-prompts'
;; by passing a single PROMPTS that is the same value as `denote-prompts'?

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
  (let ((denote-prompts '()))
    (when template (push 'template denote-prompts))
    (when date (push 'date denote-prompts))
    (when subdirectory (push 'subdirectory denote-prompts))
    (when keywords (push 'keywords denote-prompts))
    (when title (push 'title denote-prompts))
    (denote-org-capture)))

(defun denote-org-capture-delete-empty-file ()
  "Delete file if capture with `denote-org-capture' is aborted."
  (when-let ((file denote-last-path)
             ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

(provide 'denote)
;;; denote.el ends here
