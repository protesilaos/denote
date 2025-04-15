;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote
;; Version: 4.0.0
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

(define-obsolete-variable-alias 'denote-save-buffer-after-creation 'denote-save-buffers "3.0.0")

(defcustom denote-save-buffers nil
  "Control whether commands that handle new notes save their buffer outright.

The default behaviour of commands such as `denote' (or related)
is to not save the buffer they create.  This gives the user the
chance to review the text before writing it to a file.  The user
may choose to delete the unsaved buffer, thus not creating a new
note.

This option also applies to notes affected by the renaming
commands (`denote-rename-file' and related).

If this user option is set to a non-nil value, such buffers are
saved automatically.  The assumption is that the user who opts in
to this feature is familiar with the `denote-rename-file'
operation (or related) and knows it is reliable.  Data loss may
occur if the file is modified externally.

Also see `denote-kill-buffers'."
  :group 'denote
  :package-version '(denote . "3.0.0")
  :type 'boolean)

(defcustom denote-kill-buffers nil
  "Control whether creation or renaming commands kill their buffer.

The default behaviour of creation or renaming commands such as
`denote' or `denote-rename-file' is to not kill the buffer they
create or modify at the end of their operation.

If this user option is nil (the default), buffers affected by a
creation or renaming command are not automatically killed.

If set to `on-creation', new notes are automatically killed.

If set to `on-rename', renamed notes are automatically killed.

If set to t, new and renamed notes are killed.

If a buffer is killed, it is also saved, as if `denote-save-buffers'
were t. See its documentation.

In all cases, if the buffer already existed before the Denote operation
it is NOT automatically killed."
  :group 'denote
  :package-version '(denote . "3.1.0")
  :type '(choice
          (const :tag "Do not kill buffers" nil)
          (const :tag "Kill after creation" on-creation)
          (const :tag "Kill after rename" on-rename)
          (const :tag "Kill after creation and rename" t)))

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

- `keywords': Prompts with completion for the keywords of the new note.
  Available candidates are those specified in the user option
  `denote-known-keywords'.  If the user option `denote-infer-keywords'
  is non-nil, keywords in existing note file names are included in the
  list of candidates.  The `keywords' prompt uses `completing-read-multiple',
  meaning that it can accept multiple keywords separated by a comma (or
  whatever the value of `crm-separator' is).

- `file-type': Prompts with completion for the file type of the new
  note.  Available candidates are those specified in the user option
  `denote-file-type'.  Without this prompt, `denote' uses the value of
  the variable `denote-file-type'.

- `subdirectory': Prompts with completion for a subdirectory in which to
  create the note.  Available candidates are the value of the user
  option `denote-directory' and all of its subdirectories.  Any
  subdirectory must already exist: Denote will not create it.

- `date': Prompts for the date of the new note.  It will expect an input
  like 2022-06-16 or a date plus time: 2022-06-16 14:30.  Without the
  `date' prompt, the `denote' command uses the `current-time'.  (To
  leverage the more sophisticated Org method, see the
  `denote-date-prompt-use-org-read-date'.)

- `template': Prompts for a KEY among `denote-templates'.  The value of
  that KEY is used to populate the new note with content, which is added
  after the front matter.

- `signature': Prompts for an arbitrary string that can be used for any
  kind of workflow, such as a special tag to label the part1 and part2
  of a large file that is split in half, or to add special contexts like
  home and work, or even priorities like a, b, c. One other use-case is
  to implement a sequencing scheme that makes notes have hierarchical
  relationships.  This is handled by our optional extension
  denote-sequence.el, which is part of the denote package (read the
  manual).

The prompts occur in the given order.

If the value of this user option is nil, no prompts are used.  The
resulting file name will consist of an identifier (i.e. the date and
time) and a supported file type extension (per the variable
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

Also see `denote-history-completion-in-prompts'.

To change the order of the file name components, refer to
`denote-file-name-components-order'."
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

(defcustom denote-file-name-components-order '(identifier signature title keywords)
  "Specify the order of the file name components.

The value is a list of the following symbols:

- `identifier': This is the combination of the date and time.  When it
  is the first on the list, it looks like \"20240519T073456\" and does
  not have a component separator of its own due its unambiguous format.
  When it is placed anywhere else in the file name, it is prefixed with
  \"@@\", so it looks like \"@@20240519T073456\".

- `signature': This is an arbitrary string that can be used to qualify
  the file in some way, according to the user's methodology (e.g. to add
  a sequence to notes).  The string is always prefixed with the \"==\"
  to remain unambiguous.

- `title': This is an arbitrary string which describes the file.  It is
  always prefixed with \"--\" to be unambiguous.

- `keywords': This is a series of one or more words that succinctly
  group the file.  Multiple keywords are separated by an underscore
  prefixed to each of them.  The file name component is always prefixed
  with \"__\".

All four symbols must appear exactly once.  Duplicates are ignored.  Any
missing symbol is added automatically.

Some examples:

    (setq denote-file-name-components-order
       \\='(identifier signature title keywords))
    => 20240519T07345==hello--this-is-the-title__denote_testing.org

    (setq denote-file-name-components-order
       \\='(signature identifier title keywords))
    => ==hello@@20240519T07345--this-is-the-title__denote_testing.org

    (setq denote-file-name-components-order
       \\='(title signature identifier keywords))
    => --this-is-the-title==hello@@20240519T07345__denote_testing.org

    (setq denote-file-name-components-order
       \\='(keywords title signature identifier))
    => __denote_testing--this-is-the-title==hello@@20240519T07345.org

Also see the user option `denote-prompts', which affects which
components are actually used in the order specified herein.

Before deciding on this, please consider the longer-term implications
of file names with varying patterns. Consistency makes things
predictable and thus easier to find. So pick one order and never touch
it again. When in doubt, leave the default file-naming scheme as-is.

This user option should only be used to build a file name.  Custom code
should not have behaviors that depend on its value.  The reason is that
its value can change over time and Denote should be able to handle past
and current notes."
  :group 'denote
  :package-version '(denote . "3.0.0")
  ;; FIXME 2024-05-19: This technically works to display the user
  ;; option in the Custom buffer and to show its current value, though
  ;; it does not allow the user to modify it graphically: they have to
  ;; switch to the Lisp expression.  Find a way to present an
  ;; interface that lets the user reorder those elements.
  ;;
  ;; Still, making this a defcustom helps with discoverability, as
  ;; well as with the use of `setopt' and related.
  :type '(list
          (const :tag "Identifier component (date and time)" identifier)
          (const :tag "File signature (text to qualify a file)" signature)
          (const :tag "The title of the file" title)
          (const :tag "Keywords of the file" keywords)))

(defcustom denote-front-matter-components-present-even-if-empty-value '(title keywords date identifier)
  "The components that are always present in front matter even when empty.

Components are `title', `keywords', `signature', `date', `identifier'.

Note that even though a component may be listed in this variable, it
will not be present in the front matter if the corresponding line is not
in the front matter template."
  :group 'denote
  :package-version '(denote . "4.0.0")
  :type '(list
          (const :tag "Title" title)
          (const :tag "Keywords" keywords)
          (const :tag "Signature" signature)
          (const :tag "Date" date)
          (const :tag "Identifier" identifier)))

(defcustom denote-sort-keywords t
  "Whether to sort keywords in new files.

When non-nil, the keywords of `denote' are sorted with
`string-collate-lessp' regardless of the order they were inserted at the
minibuffer prompt.

If nil, show the keywords in their given order."
  :group 'denote
  :package-version '(denote . "0.1.0")
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
format (also check the user option `denote-file-type'):

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

(defcustom denote-org-store-link-to-heading nil
  "Determine whether `org-store-link' links to the current Org heading.

[ Remember that what `org-store-link' does is merely collect a link.  To
  actually insert it, use the command `org-insert-link'.  Note that
  `org-capture' uses `org-store-link' internally when it needs to store
  a link.  ]

When the value is nil, the Denote handler for `org-store-link' produces
links only to the current file (by using the file's identifier).  For
example:

    [[denote:20240118T060608][Some test]]

If the value is `context', the link consists of the file's identifier
and the text of the current heading, like this:

    [[denote:20240118T060608::*Heading text][Some test::Heading text]].

However, if there already exists a CUSTOM_ID property for the current
heading, this is always given priority and is used instead of the
context.

If the value is `id' or, for backward-compatibility, any other non-nil
value, then Denote will use the standard Org mechanism of the CUSTOM_ID
property to create a unique link to the heading.  If the heading does
not have a CUSTOM_ID, it creates it and includes it in its PROPERTIES
drawer.  If a CUSTOM_ID exists, it takes it as-is.  The result is like
this:

    [[denote:20240118T060608::#h:eed0fb8e-4cc7-478f][Some test::Heading text]]

The value of the CUSTOM_ID is determined by the Org user option
`org-id-method'.  The sample shown above uses the default UUID
infrastructure (though I deleted a few characters to not get
complaints from the byte compiler about long lines in the doc
string...).

Note that this option does not affect how Org behaves with regard to
`org-id-link-to-org-use-id'.  If that user option is set to create ID
properties, then those will be created by Org even if the Denote link
handler will take care to not use/store the ID value.  Concretely, users
who never want ID properties under their headings should keep
`org-id-link-to-org-use-id' in its nil value.

Context links are easier to break than those with a CUSTOM_ID in cases
where either the heading text changes or there is another heading that
matches that text.  The potential advantage of context links is that
they do not require a PROPERTIES drawer.

When visiting a link to a heading, Org opens the Denote file and then
navigates to that heading.

[ This feature only works in Org mode files, as other file types
  do not have a linking mechanism that handles unique identifiers
  for headings or other patterns to jump to.  If `org-store-link'
  is invoked in one such file, it captures only the Denote
  identifier of the file, even if this user option is set to a
  non-nil value.  ]"
  :group 'denote
  :package-version '(denote . "4.0.0")
  :type '(choice (const :tag "No link to heading (default)" nil)
                 (const :tag "Link to the context" context)
                 (const :tag "Link wtih CUSTOM_ID, creating it if needed" id)))

(defcustom denote-templates nil
  "Alist of content templates for new notes.
A template is arbitrary text that Denote will add to a newly
created note right below the front matter.

Templates are expressed as a (KEY . VALUE) association.

- The KEY is the name which identifies the template.  It is an
  arbitrary symbol, such as `report', `memo', `statement'.

- The VALUE is either a string or the symbol of a function.

  - If it is a string, it is ordinary text that Denote will insert
    as-is.  It can contain newline characters to add spacing.  The
    manual of Denote contains examples on how to use the `concat'
    function, beside writing a generic string.

  - If it is a function, it is called without arguments and is expected
    to return a string.  Denote will call the function and insert the
    result in the buffer.

The user can choose a template either by invoking the command
`denote-template' or by changing the user option `denote-prompts'
to always prompt for a template when calling the `denote'
command."
  :type '(alist :key-type symbol :value-type (choice string function))
  :package-version '(denote . "3.1.0")
  :link '(info-link "(denote) The denote-templates option")
  :group 'denote)

(make-obsolete-variable 'denote-rename-no-confirm 'denote-rename-confirmations "3.0.0")

(defcustom denote-rename-confirmations '(rewrite-front-matter modify-file-name)
  "Make renaming commands prompt for confirmations.

This affects the behaviour of renaming commands.  The value is either
nil, in which case no confirmation is ever requested, or a list of
symbols among the following:

- `modify-file-name' means that renaming commands will ask for
  confirmation before modifying the file name.

- `rewrite-front-matter' means that renaming commands will ask for
  confirmation before rewritting the front matter.

- `add-front-matter' means that renaming commands will ask for
  confirmation before adding new front matter to the file.

The default behaviour of the `denote-rename-file' command (and others
like it) is to ask for an affirmative answer as a final step before
changing the file name and, where relevant, inserting or updating the
corresponding front matter.

Specialized commands that build on top of `denote-rename-file' (or
related) may internally bind this user option to a non-nil value in
order to perform their operation (e.g. `denote-dired-rename-files' goes
through each marked Dired file, prompting for the information to use,
but carries out the renaming without asking for confirmation)."
  :group 'denote
  :type '(radio (const :tag "Disable all confirmations" nil)
                (set :tag "Available confirmations" :greedy t
                     (const :tag "Add front matter" add-front-matter)
                     (const :tag "Rewrite front matter" rewrite-front-matter)
                     (const :tag "Modify file name" modify-file-name))))

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

(defcustom denote-excluded-files-regexp nil
  "Regular expression of files that are excluded from Denote file prompts.
Files are provided for completion when using commands like `denote-link'
and `denote-open-or-create'.

The match is performed with `string-match-p' on the full file path."
  :group 'denote
  :package-version '(denote . "3.0.0")
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
  '(denote-title-prompt denote-signature-prompt denote-files-matching-regexp-prompt denote-query-link-prompt)
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

(define-obsolete-variable-alias
  'denote-link-button-action
  'denote-open-link-function
  "4.0.0")

(defcustom denote-open-link-function #'find-file-other-window
  "Function to find the file of a Denote link.

The default value is `find-file-other-window', with `find-file' because
another common option.  Users can provide a custom function which
behaves like the other two.

This is used in all non-Org buffers that have a link created by Denote.
Org has its own mechanism, which you can learn more about by reading the
documentation of the `org-open-at-point' command."
  :group 'denote
  :type '(choice (function :tag "Other window" find-file-other-window)
                 (function :tag "Current window" find-file)
                 (function :tag "Custom function"))
  :package-version '(denote . "4.0.0"))

(define-obsolete-variable-alias
  'denote-link-description-function
  'denote-link-description-format
  "4.0.0")

(defcustom denote-link-description-format #'denote-link-description-with-signature-and-title
  "The format of a link description text.
This determines how `denote-link' and related functions create a link
description by default.

The value can be either a function or a string.  If it is a function, it
is called with one argument, the file, and should return a string
representing the link description.

The default is a function that returns the active region or the title of
the note (with the signature if present).

If the value is a string, it treats specially the following specifiers:

- The %t is the Denote TITLE in the front matter or the file name.
- The %T is the Denote TITLE in the file name.
- The %i is the Denote IDENTIFIER of the file.
- The %I is the identifier converted to DAYNAME, DAYNUM MONTHNUM YEAR.
- The %d is the same as %i (DATE mnemonic).
- The %D is a \"do what I mean\" which behaves the same as %t and if
  that returns nothing, it falls back to %I, then %i.
- The %s is the Denote SIGNATURE of the file.
- The %k is the Denote KEYWORDS of the file.
- The %% is a literal percent sign.

In addition, the following flags are available for each of the specifiers:

- 0 :: Pad to the width, if given, with zeros instead of spaces.
- - :: Pad to the width, if given, on the right instead of the left.
- < :: Truncate to the width and precision, if given, on the left.
- > :: Truncate to the width and precision, if given, on the right.
- ^ :: Convert to upper case.
- _ :: Convert to lower case.

When combined all together, the above are written thus:

    %<flags><width><precision>SPECIFIER-CHARACTER

Any other text in the string it taken as-is.  Users may want, for
example, to include some text that makes Denote links stand out, such as
a [D] prefix.

If the region is active, its text is used as the link's description."
  :type '(choice
          (string :tag "String with treats format specifiers specially")
          (function :tag "Custom function like `denote-link-description-with-signature-and-title'"))
  :package-version '(denote . "4.0.0")
  :group 'denote)

;;;; Main variables

;; For character classes, evaluate: (info "(elisp) Char Classes")

(defconst denote-id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.
The note's ID is derived from the date and time of its creation.")

(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote-id-format'.")

(defconst denote-signature-regexp "==\\([^.]*?\\)\\(==.*\\|--.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the SIGNATURE field in a file name.")

(defconst denote-title-regexp "--\\([^.]*?\\)\\(==.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the TITLE field in a file name.")

(defconst denote-keywords-regexp "__\\([^.]*?\\)\\(==.*\\|--.*\\|__.*\\|@@.*\\|\\..*\\)*$"
  "Regular expression to match the KEYWORDS field in a file name.")

(make-obsolete-variable
 'denote-excluded-punctuation-extra-regexp
 'denote-file-name-slug-functions
 "4.0.0")

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
  (when-let* ((dir-locals (dir-locals-find-file default-directory))
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
  (if-let* (((or (eq denote-directory 'default-directory) (eq denote-directory 'local)))
            (silo-dir (denote--default-directory-is-silo-p)))
      (progn
        (display-warning
         'denote
         "Silo value must be a string; `local' or `default-directory'are obsolete"
         :error)
        silo-dir)
    (let ((denote-directory (file-name-as-directory (expand-file-name denote-directory))))
      (denote--make-denote-directory)
      denote-directory)))

;; TODO: Review and fix the features listed in the docstring below before
;; making this a user option.
(defvar denote-generate-identifier-automatically t
  "Make creation and renaming commands automatically create and identifier.

This applies when a note is created or renamed.  The default is to
always create an identifier automatically.

Valid values are: t, nil, `on-creation', and `on-rename'.

IMPORTANT: Some features may not work with notes that do not have an
identifier.  For example, backlinks do not contain files without an
identifier.")

;;;;; Sluggification functions

(defun denote-slug-keep-only-ascii (str)
  "Remove all non-ASCII characters from STR and replace them with spaces.
This is useful as a helper function to construct
`denote-file-name-slug-functions'."
  (let* ((ascii-range (seq-map
                       (lambda (character)
                         (if (and (>= character 33) (<= character 126))
                             character
                           32)) ; empty space
                       str))
         (characters (seq-filter #'characterp ascii-range)))
    (mapconcat #'string characters)))

(define-obsolete-function-alias
  'denote--slug-hyphenate
  'denote-slug-hyphenate
  "4.0.0")

(defun denote-slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun denote-slug-put-equals (str)
  "Replace spaces and underscores with equals signs in STR.
Also replace multiple equals signs with a single one and remove
any leading and trailing signs."
  (replace-regexp-in-string
   "^=\\|=$" ""
   (replace-regexp-in-string
    "=\\{2,\\}" "="
    (replace-regexp-in-string "_\\|\s+" "=" str))))

(defun denote--remove-dot-characters (str)
  "Remove dot characters from STR."
  (replace-regexp-in-string "\\." "" str))

(defun denote--trim-right-token-characters (str component)
  "Remove =, -, _ and @ from the end of STR.
The removal is done only if necessary according to COMPONENT."
  (if (eq component 'title)
      (string-trim-right str "[=@_]+")
    (string-trim-right str "[=@_-]+")))

(defun denote--replace-consecutive-token-characters (str component)
  "Replace consecutive characters with a single one in STR.
Hyphens, underscores, equal signs and at signs are replaced with
a single one in str, if necessary according to COMPONENT."
  (let ((str (replace-regexp-in-string
              "_\\{2,\\}" "_"
              (replace-regexp-in-string
               "=\\{2,\\}" "="
               (replace-regexp-in-string
                "@\\{2,\\}" "@" str)))))
    ;; -- are allowed in titles when the default sluggification is disabled
    (if (eq component 'title)
        str
      (replace-regexp-in-string
       "-\\{2,\\}" "-" str))))

(defun denote-sluggify (component str)
  "Make STR an appropriate slug for file name COMPONENT.

Apply the function specified in `denote-file-name-slug-function' to
COMPONENT which is one of `title', `signature', `keyword'.  If the
resulting string still contains consecutive -, _, =, or @, they are
replaced by a single occurence of the character, if necessary according
to COMPONENT.  If COMPONENT is `keyword', remove underscores from STR as
they are used as the keywords separator in file names."
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
      (denote--remove-dot-characters str-slug) component) component)))

(defun denote-sluggify-title (str)
  "Make STR an appropriate slug for title."
  (downcase
   (denote-slug-hyphenate
    (replace-regexp-in-string "[][{}!@#$%^&*()+'\"?,.\|;:~`‘’“”/=]*" "" str))))

(defun denote-sluggify-signature (str)
  "Make STR an appropriate slug for signature."
  (downcase
   (denote-slug-put-equals
    (replace-regexp-in-string "[][{}!@#$%^&*()+'\"?,.\|;:~`‘’“”/-]*" "" str))))

(defun denote-sluggify-keyword (str)
  "Sluggify STR while joining separate words."
  (downcase
   (replace-regexp-in-string "[][{}!@#$%^&*()+'\"?,.\|;:~`‘’“”/_ =-]*" "" str)))

(defun denote-sluggify-keywords (keywords)
  "Sluggify KEYWORDS, which is a list of strings."
  (mapcar (lambda (keyword)
            (denote-sluggify 'keyword keyword))
          keywords))

;;;;; Common helper functions

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun denote-identifier-p (identifier)
  "Return non-nil if IDENTIFIER string is a Denote identifier."
  (string-match-p (format "\\`%s\\'" denote-id-regexp) identifier))

(defun denote-file-has-identifier-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (denote-retrieve-filename-identifier file))

(defun denote-file-has-supported-extension-p (file)
  "Return non-nil if FILE has supported extension.
Also account for the possibility of an added .gpg suffix.  Supported
extensions are those implied by the variable `denote-file-type'."
  (seq-some (lambda (e)
              (string-suffix-p e file))
            (denote-file-type-extensions-with-encryption)))

(defun denote-file-is-in-denote-directory-p (file)
  "Return non-nil if FILE is in the variable `denote-directory'."
  (string-prefix-p (denote-directory) (expand-file-name file)))

(defun denote-filename-is-note-p (filename)
  "Return non-nil if FILENAME is a valid name for a Denote note.
For our purposes, its path must be part of the variable
`denote-directory', it must have a Denote identifier in its name, and
use one of the extensions implied by the variable `denote-file-type'."
  (and (denote-file-is-in-denote-directory-p filename)
       (denote-file-has-identifier-p filename)
       (denote-file-has-supported-extension-p filename)))

(defun denote-file-is-note-p (file)
  "Return non-nil if FILE is an actual Denote note.
For our purposes, a note must satisfy `file-regular-p' and
`denote-filename-is-note-p'."
  (and (file-regular-p file) (denote-filename-is-note-p file)))

(defun denote-file-has-denoted-filename-p (file)
  "Return non-nil if FILE respects the file-naming scheme of Denote.

This tests the rules of Denote's file-naming scheme.  Sluggification is
ignored.  It is done by removing all file name components and validating
what remains."
  (let* ((initial-filename (file-name-nondirectory file))
         (filename initial-filename)
         (title (denote-retrieve-filename-title file))
         (keywords-string (denote-retrieve-filename-keywords file))
         (signature (denote-retrieve-filename-signature file))
         (identifier (denote-retrieve-filename-identifier file)))
    (when title
      (setq filename (replace-regexp-in-string (concat "\\(--" (regexp-quote title) "\\).*\\'") "" filename nil nil 1)))
    (when keywords-string
      (setq filename (replace-regexp-in-string (concat "\\(__" (regexp-quote keywords-string) "\\).*\\'") "" filename nil nil 1)))
    (when signature
      (setq filename (replace-regexp-in-string (concat "\\(==" (regexp-quote signature) "\\).*\\'") "" filename nil nil 1)))
    (when identifier
      (if (string-match-p "@@" filename)
          (setq filename (replace-regexp-in-string (concat "\\(@@" (regexp-quote identifier) "\\).*\\'") "" filename nil nil 1))
        (setq filename (replace-regexp-in-string (concat "\\(" (regexp-quote identifier) "\\).*\\'") "" filename nil nil 1))))
    ;; What remains should be the empty string or the file extension.
    (and (not (string-prefix-p "." initial-filename))
         (or (string-empty-p filename)
             (string-prefix-p "." filename)))))

(defun denote-file-has-signature-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (denote-retrieve-filename-signature file))

(defun denote-file-is-writable-and-supported-p (file)
  "Return non-nil if FILE is writable and has supported extension."
  ;; We do not want to test that the file is regular (exists) because we want
  ;; this function to return t on files that are still unsaved.
  (and (file-writable-p file)
       (denote-file-has-supported-extension-p file)))

(defun denote-get-file-name-relative-to-denote-directory (file)
  "Return name of FILE relative to the variable `denote-directory'.
FILE must be an absolute path."
  (when-let* ((dir (denote-directory))
              ((file-name-absolute-p file))
              (file-name (expand-file-name file))
              ((string-prefix-p dir file-name)))
    (substring-no-properties file-name (length dir))))

(defun denote-extract-id-from-string (string)
  "Return existing Denote identifier in STRING, else nil."
  (when (string-match denote-id-regexp string)
    (match-string-no-properties 0 string)))

(defun denote--exclude-directory-regexp-p (file)
  "Return non-nil if FILE matches `denote-excluded-directories-regexp'."
  (and (stringp denote-excluded-directories-regexp)
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

(defun denote--file-excluded-p (file)
  "Return non-file if FILE matches `denote-excluded-files-regexp'."
  (and denote-excluded-files-regexp
       (string-match-p denote-excluded-files-regexp file)))

(defun denote--directory-get-files ()
  "Return list with full path of valid files in variable `denote-directory'.
Consider files that satisfy `denote-file-has-identifier-p' and
are not backups."
  (mapcar
   #'expand-file-name
   (seq-filter
    (lambda (file)
      (and (file-regular-p file)
           (denote-file-has-identifier-p file)
           (not (denote--file-excluded-p file))
           (not (backup-file-name-p file))))
    (denote--directory-all-files-recursively))))

(defun denote-directory-files (&optional files-matching-regexp omit-current text-only exclude-regexp)
  "Return list of absolute file paths in variable `denote-directory'.
Files that match `denote-excluded-files-regexp' are excluded from the
list.

Files only need to have an identifier.  The return value may thus
include file types that are not implied by the variable
`denote-file-type'.

With optional FILES-MATCHING-REGEXP, restrict files to those
matching the given regular expression.

With optional OMIT-CURRENT as a non-nil value, do not include the
current Denote file in the returned list.

With optional TEXT-ONLY as a non-nil value, limit the results to
text files that satisfy `denote-file-has-supported-extension-p'.

With optional EXCLUDE-REGEXP exclude the files that match the given
regular expression.  This is done after FILES-MATCHING-REGEXP and
OMIT-CURRENT have been applied."
  (let ((files (denote--directory-get-files)))
    (when (and omit-current buffer-file-name (denote-file-has-identifier-p buffer-file-name))
      (setq files (delete buffer-file-name files)))
    (when files-matching-regexp
      (setq files (seq-filter
                   (lambda (f)
                     (string-match-p files-matching-regexp (denote-get-file-name-relative-to-denote-directory f)))
                   files)))
    (when text-only
      (setq files (seq-filter #'denote-file-has-supported-extension-p files)))
    (when exclude-regexp
      (setq files (seq-remove
                   (lambda (file)
                     (string-match-p exclude-regexp file))
                   files)))
    files))

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

;; TODO 2023-01-24: Perhaps there is a good reason to make this a user
;; option, but I am keeping it as a generic variable for now.
(defvar denote-encryption-file-extensions '(".gpg" ".age")
  "List of strings specifying file extensions for encryption.")

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
    (if-let* (((member outer-extension denote-encryption-file-extensions))
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
            (string= id (denote-retrieve-filename-identifier file)))
          (denote-directory-files))))
    (if (length< files 2)
        (car files)
      (seq-find
       (lambda (file)
         (let ((file-extension (denote-get-file-extension-sans-encryption file)))
           (and (denote-file-has-supported-extension-p file)
                (or (string= (denote--file-extension denote-file-type)
                             file-extension)
                    (string= ".org" file-extension)
                    (member file-extension (denote-file-type-extensions))))))
       files))))

(defun denote-get-relative-path-by-id (id &optional directory)
  "Return relative path of ID string in `denote-directory-files'.
The path is relative to DIRECTORY (default: ‘default-directory’)."
  (when-let* ((path (denote-get-path-by-id id)))
    (file-relative-name path directory)))

(defvar denote-file-history nil
  "Minibuffer history of `denote-file-prompt'.")

(defalias 'denote--file-history 'denote-file-history
  "Compatibility alias for `denote-file-history'.")

(defvar denote-file-prompt-latest-input nil
  "Latest input passed to `denote-file-prompt'.
This is used for retrieving a value that is used to set a new default at
the title prompt of `denote-open-or-create' and related commands.")

(defvar denote-file-prompt-use-files-matching-regexp nil
  "The `denote-file-prompt' FILES-MATCHING-REGEXP value.
Only ever `let' bind this, otherwise the restriction will always be
there.")

(defun denote-file-prompt (&optional files-matching-regexp prompt-text no-require-match)
  "Prompt for file in variable `denote-directory'.
Files that match `denote-excluded-files-regexp' are excluded from the
list.

With optional FILES-MATCHING-REGEXP, filter the candidates per
the given regular expression.

With optional PROMPT-TEXT, use it instead of the default call to
select a file.

With optional NO-REQUIRE-MATCH, accept the given input as-is.

Return the absolute path to the matching file."
  (let* ((default-directory (denote-directory))
         (relative-files (mapcar
                          #'denote-get-file-name-relative-to-denote-directory
                          (denote-directory-files
                           (or denote-file-prompt-use-files-matching-regexp files-matching-regexp)
                           :omit-current)))
         (prompt (format "%s in %s: "
                         (or prompt-text "Select FILE")
                         (propertize (denote-directory) 'face 'denote-faces-prompt-current-name)))
         (input (completing-read
                 prompt
                 (denote--completion-table 'file relative-files)
                 nil (unless no-require-match :require-match)
                 nil 'denote-file-history))
         (absolute-file (concat (denote-directory) input)))
    ;; NOTE: This block is executed when no-require-match is t. It is useful
    ;; for commands such as `denote-open-or-create` or similar.
    (unless (file-exists-p absolute-file)
      (setq denote-file-prompt-latest-input input)
      (setq denote-file-history (delete input denote-file-history)))
    ;; NOTE: We must always return an absolute path, even if it does not
    ;; exist, because callers expect one.  They handle a non-existent file
    ;; appropriately.
    absolute-file))

;;;; The sort mechanism

(defgroup denote-sort nil
  "Sort Denote files based on a file name component."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defconst denote-sort-comparison-fallback-function #'string-collate-lessp
  "String comparison function used by `denote-sort-files' subroutines.")

(defconst denote-sort-components '(title keywords signature identifier)
  "List of sorting keys applicable for `denote-sort-files' and related.")

(defcustom denote-sort-identifier-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the DATE/IDENTIFIER component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "4.0.0")
  :group 'denote-sort)

(defcustom denote-sort-title-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the TITLE component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-keywords-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the KEYWORDS component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-signature-comparison-function denote-sort-comparison-fallback-function
  "Function to sort the SIGNATURE component in file names.
The function accepts two arguments and must return a non-nil value if
the first argument is smaller than the second one."
  :type 'function
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-dired-extra-prompts '(sort-by-component reverse-sort)
  "Determine what `denote-sort-dired' prompts for beside a search query.
This concerns the additional prompts issued by `denote-sort-dired' about
whether to sort by a given file name component and to then reverse the
sort.

The value is a list of symbols, which can include the symbols
`sort-by-component', `reverse-sort', and `exclude-regexp'.  The order is
significant, with the leftmost symbol coming first.

These symbols correspond to the following:

- A choice to select the file name component to sort by.
- A yes or no prompt on whether to reverse the sorting.
- A string (or regular expression) of files to be excluded from the results.

If the value is nil, skip all prompts.  In this scenario, the sorting is
done according to `denote-sort-dired-default-sort-component' and
`denote-sort-dired-default-reverse-sort'."
  :type '(radio (const :tag "Do not prompt for anything" nil)
                (set :tag "Available prompts" :greedy t
                     (const :tag "Sort by file name component" sort-by-component)
                     (const :tag "Reverse the sort" reverse-sort)
                     (const :tag "Exclude files matching regexp" exclude-regexp)))
  :package-version '(denote . "4.0.0")
  :group 'denote-sort)

(defcustom denote-sort-dired-default-sort-component 'identifier
  "Set the default file name component to sort by.
This is used only if `denote-sort-dired-extra-prompts' omits the
minibuffer prompt for which file name component to sort by."
  :type '(radio
          (const :tag "Sort by identifier (default)" identifier)
          (const :tag "Sort by title" title)
          (const :tag "Sort by keywords" keywords)
          (const :tag "Sort by signature" signature))
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

(defcustom denote-sort-dired-default-reverse-sort nil
  "If non-nil, reverse the sorting order by default.
This is used only if `denote-sort-dired-extra-prompts' omits the
minibuffer prompt that asks for a reverse sort or not."
  :type 'boolean
  :package-version '(denote . "3.1.0")
  :group 'denote-sort)

;; NOTE 2023-12-04: We can have compound sorting algorithms such as
;; title+signature, but I want to keep this simple for the time being.
;; Let us first hear from users to understand if there is a real need
;; for such a feature.
(defmacro denote-sort--define-lessp (component)
  "Define function to sort by COMPONENT."
  (let ((retrieve-fn (intern (format "denote-retrieve-filename-%s" component)))
        (comparison-fn (intern (format "denote-sort-%s-comparison-function" component))))
    `(defun ,(intern (format "denote-sort-%s-lessp" component)) (file1 file2)
       ,(format
         "Return smallest among FILE1, FILE2 based on their %s.
The `%s' performs the comparison."
         component comparison-fn)
       (let* ((one (,retrieve-fn file1))
              (two (,retrieve-fn file2))
              (one-empty-p (or (null one) (string-empty-p one)))
              (two-empty-p (or (null two) (string-empty-p two))))
         (cond
          (one-empty-p nil)
          ((and (not one-empty-p) two-empty-p) one)
          (t (funcall (or ,comparison-fn denote-sort-comparison-fallback-function) one two)))))))

;; TODO 2023-12-04: Subject to the above NOTE, we can also sort by
;; directory and by file length.
(denote-sort--define-lessp identifier)
(denote-sort--define-lessp title)
(denote-sort--define-lessp keywords)
(denote-sort--define-lessp signature)

;;;###autoload
(defun denote-sort-files (files component &optional reverse)
  "Returned sorted list of Denote FILES.

With COMPONENT as a symbol among `denote-sort-components',
sort files based on the corresponding file name component.

With COMPONENT as the symbol of a function, use it to perform the
sorting.  In this case, the function is called with two arguments, as
described by `sort'.

With COMPONENT as a nil value keep the original date-based
sorting which relies on the identifier of each file name.

With optional REVERSE as a non-nil value, reverse the sort order."
  (let* ((files-to-sort (copy-sequence files))
         (sort-fn (pcase component
                    ((pred functionp) component)
                    ('identifier #'denote-sort-identifier-lessp)
                    ('title #'denote-sort-title-lessp)
                    ('keywords #'denote-sort-keywords-lessp)
                    ('signature #'denote-sort-signature-lessp)))
         (sorted-files (if sort-fn (sort files sort-fn) files-to-sort)))
    (if reverse
        (reverse sorted-files)
      sorted-files)))

(defun denote-sort-get-directory-files (files-matching-regexp sort-by-component &optional reverse omit-current exclude-regexp)
  "Return sorted list of files in variable `denote-directory'.

With FILES-MATCHING-REGEXP as a string limit files to those
matching the given regular expression.

With SORT-BY-COMPONENT as a symbol among `denote-sort-components',
pass it to `denote-sort-files' to sort by the corresponding file
name component.

With optional REVERSE as a non-nil value, reverse the sort order.

With optional OMIT-CURRENT, do not include the current file in
the list.

With optional EXCLUDE-REGEXP exclude the files that match the given
regular expression.  This is done after FILES-MATCHING-REGEXP and
OMIT-CURRENT have been applied."
  (denote-sort-files
   (denote-directory-files files-matching-regexp omit-current nil exclude-regexp)
   sort-by-component
   reverse))

(defun denote-sort-get-links (files-matching-regexp sort-by-component current-file-type id-only &optional reverse exclude-regexp)
  "Return sorted typographic list of links for FILES-MATCHING-REGEXP.

With FILES-MATCHING-REGEXP as a string, match files stored in the
variable `denote-directory'.

With SORT-BY-COMPONENT as a symbol among `denote-sort-components',
sort FILES-MATCHING-REGEXP by the given Denote file name
component.  If SORT-BY-COMPONENT is nil or an unknown non-nil
value, default to the identifier-based sorting.

With CURRENT-FILE-TYPE as a symbol among those specified in
the variable `denote-file-type' (or the `car' of each element in
`denote-file-types'), format the link accordingly.  With a nil or
unknown non-nil value, default to the Org notation.

With ID-ONLY as a non-nil value, produce links that consist only
of the identifier, thus deviating from CURRENT-FILE-TYPE.

With optional REVERSE as a non-nil value, reverse the sort order.

With optional EXCLUDE-REGEXP exclude the files that match the given
regular expression.  This is done after FILES-MATCHING-REGEXP and
OMIT-CURRENT have been applied."
  (denote-link--prepare-links
   (denote-sort-get-directory-files files-matching-regexp sort-by-component reverse exclude-regexp)
   current-file-type
   id-only))

(defvar denote-sort-component-history nil
  "Minibuffer history of `denote-sort-component-prompt'.")

(defalias 'denote-sort--component-hist 'denote-sort-component-history
  "Compatibility alias for `denote-sort-component-history'.")

(defun denote-sort-component-prompt ()
  "Prompt for sorting key among `denote-sort-components'."
  (let ((default (car denote-sort-component-history)))
    (intern
     (completing-read
      (format-prompt "Sort by file name component" default)
      denote-sort-components nil :require-match
      nil 'denote-sort-component-history default))))

(defvar denote-sort-exclude-files-history nil
  "Minibuffer history for `denote-sort-exclude-files-prompt'.")

(defun denote-sort-exclude-files-prompt ()
  "Prompt for regular expression of files to exclude."
  ;; TODO 2024-12-03: Maybe use `read-regexp'?  We do not use it
  ;; elsewhere, so maybe this is fine.
  (let ((default (car denote-sort-exclude-files-history)))
    (read-string
     (format-prompt "Exclude files matching REGEXP" default)
     default 'denote-sort-exclude-files-history)))

(defvar-local denote-sort--dired-buffer nil
  "Buffer object of current `denote-sort-dired'.")

(defun denote-sort-dired--prompts ()
  "Return list of prompts per `denote-sort-dired-extra-prompts'."
  (let (sort-by-component reverse-sort exclude-rx)
    (dolist (prompt denote-sort-dired-extra-prompts)
      (pcase prompt
        ('sort-by-component (setq sort-by-component (denote-sort-component-prompt)))
        ('reverse-sort (setq reverse-sort (y-or-n-p "Reverse sort? ")))
        ('exclude-regexp (setq exclude-rx (denote-sort-exclude-files-prompt)))))
    (list sort-by-component reverse-sort exclude-rx)))

;;;###autoload
(defun denote-sort-dired (files-matching-regexp sort-by-component reverse exclude-regexp)
  "Produce Dired buffer with sorted files from variable `denote-directory'.
When called interactively, prompt for FILES-MATCHING-REGEXP and,
depending on the value of the user option `denote-sort-dired-extra-prompts',
also prompt for SORT-BY-COMPONENT, REVERSE, and EXCLUDE-REGEXP.

1. FILES-MATCHING-REGEXP limits the list of Denote files to
   those matching the provided regular expression.

2. SORT-BY-COMPONENT sorts the files by their file name component (one
   among `denote-sort-components').  If it is nil, sorting is performed
   according to the user option `denote-sort-dired-default-sort-component',
   falling back to the identifier.

3. REVERSE is a boolean to reverse the order when it is a non-nil value.
   If `denote-sort-dired-extra-prompts' is configured to skip this
   prompt, then the sorting is done according to the user option
   `denote-sort-dired-default-reverse-sort', falling back to
   nil (i.e. no reverse sort).

4. EXCLUDE-REGEXP excludes the files that match the given regular
   expression.  This is done after FILES-MATCHING-REGEXP and
   OMIT-CURRENT have been applied.

When called from Lisp, the arguments are a string, a symbol among
`denote-sort-components', a non-nil value, and a string, respectively."
  (interactive
   (append (list (denote-files-matching-regexp-prompt)) (denote-sort-dired--prompts)))
  (let ((component (or sort-by-component
                       denote-sort-dired-default-sort-component
                       'identifier))
        (reverse-sort (or reverse
                          denote-sort-dired-default-reverse-sort
                          nil))
        (exclude-rx (or exclude-regexp nil)))
    (if-let* ((default-directory (denote-directory))
              (files (denote-sort-get-directory-files files-matching-regexp component reverse-sort nil exclude-rx))
              ;; NOTE 2023-12-04: Passing the FILES-MATCHING-REGEXP as
              ;; buffer-name produces an error if the regexp contains a
              ;; wildcard for a directory. I can reproduce this in emacs
              ;; -Q and am not sure if it is a bug. Anyway, I will report
              ;; it upstream, but even if it is fixed we cannot use it
              ;; for now (whatever fix will be available for Emacs 30+).
              (denote-sort-dired-buffer-name (format "Denote sort `%s' by `%s'" files-matching-regexp component))
              (buffer-name (format "Denote sort by `%s' at %s" component (format-time-string "%T"))))
        (let ((dired-buffer (dired (cons buffer-name (mapcar #'file-relative-name files)))))
          (setq denote-sort--dired-buffer dired-buffer)
          (with-current-buffer dired-buffer
            (setq-local revert-buffer-function
                        (lambda (&rest _)
                          ;; FIXME 2025-01-04: Killing the buffer has
                          ;; the unintended side effect of affecting the
                          ;; window configuration when we call
                          ;; `denote-update-dired-buffers'.
                          (kill-buffer dired-buffer)
                          (denote-sort-dired files-matching-regexp component reverse-sort exclude-rx))))
          buffer-name)
      (message "No matching files for: %s" files-matching-regexp))))

(defalias 'denote-dired 'denote-sort-dired
  "Alias for `denote-sort-dired' command.")

;;;; Keywords

(defun denote-extract-keywords-from-path (path)
  "Extract keywords from PATH and return them as a list of strings.
PATH must be a Denote-style file name where keywords are prefixed
with an underscore.

If PATH has no such keywords, return nil.

Also see `denote-retrieve-filename-keywords'."
  (when-let* ((kws (denote-retrieve-filename-keywords path)))
    (split-string kws "_" :omit-nulls)))

(defalias 'denote-retrieve-filename-keywords-as-list 'denote-extract-keywords-from-path
  "Alias for the function `denote-extract-keywords-from-path'")

(define-obsolete-function-alias
  'denote--inferred-keywords
  'denote-infer-keywords-from-files
  "4.0.0")

(defun denote-infer-keywords-from-files (&optional files-matching-regexp)
  "Return list of keywords in `denote-directory-files'.
With optional FILES-MATCHING-REGEXP, only extract keywords from the
matching files.  Otherwise, do it for all files.

Keep any duplicates.  Users who do not want duplicates should refer to
the functions `denote-keywords'."
  (when-let* ((files (denote-directory-files files-matching-regexp))
              (keywords (mapcan #'denote-extract-keywords-from-path files)))
    (if-let* ((regexp denote-excluded-keywords-regexp))
        (seq-remove
         (lambda (k)
           (string-match-p regexp k))
         keywords)
      keywords)))

(defun denote-keywords (&optional files-matching-regexp)
  "Return appropriate list of keyword candidates.
If `denote-infer-keywords' is non-nil, infer keywords from existing
notes and combine them into a list with `denote-known-keywords'.  Else
use only the latter.

In the case of keyword inferrence, use optional FILES-MATCHING-REGEXP,
to extract keywords only from the matching files.  Otherwise, do it for
all files.

Filter inferred keywords with the user option `denote-excluded-keywords-regexp'."
  (delete-dups
   (if denote-infer-keywords
       (append (denote-infer-keywords-from-files files-matching-regexp) denote-known-keywords)
     denote-known-keywords)))

(defvar denote-keyword-history nil
  "Minibuffer history of inputted keywords.")

(defalias 'denote--keyword-history 'denote-keyword-history
  "Compatibility alias for `denote-keyword-history'.")

(make-obsolete
 'denote-convert-file-name-keywords-to-crm
 nil
 "3.0.0: Keywords are always returned as a list")

(defun denote--keywords-crm (keywords &optional prompt initial)
  "Use `completing-read-multiple' for KEYWORDS.
With optional PROMPT, use it instead of a generic text for file
keywords.  With optional INITIAL, add it to the minibuffer as
initial input."
  (delete-dups
   (completing-read-multiple
    (format-prompt (or prompt "New file KEYWORDS") nil)
    keywords nil nil initial 'denote-keyword-history)))

(defun denote-keywords-prompt (&optional prompt-text initial-keywords infer-from-files-matching-regexp)
  "Prompt for one or more keywords.
Read entries as separate when they are demarcated by the
`crm-separator', which typically is a comma.

With optional PROMPT-TEXT, use it to prompt the user for keywords.  Else
use a generic prompt.  With optional INITIAL-KEYWORDS use them as the
initial minibuffer text.

With optional INFER-FROM-FILES-MATCHING-REGEXP, only infer keywords from
files that match the given regular expression, per the function
`denote-keywords'.

Return an empty list if the minibuffer input is empty."
  (denote--keywords-crm (denote-keywords infer-from-files-matching-regexp) prompt-text initial-keywords))

(defun denote-keywords-sort (keywords)
  "Sort KEYWORDS if `denote-sort-keywords' is non-nil.
KEYWORDS is a list of strings, per `denote-keywords-prompt'."
  (if denote-sort-keywords
      (sort (copy-sequence keywords) #'string-collate-lessp)
    keywords))

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
#+signature:  %s
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
identifier: %s
signature:  %s
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
identifier = %s
signature  = %s
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
signature:  %s
---------------------------\n\n"
  "Plain text front matter.
It is passed to `format' with arguments TITLE, DATE, KEYWORDS,
ID.  Advanced users are advised to consult Info node `(denote)
Change the front matter format'.")

(defun denote-format-string-for-md-front-matter (s)
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

(defun denote-format-string-for-org-front-matter (s)
  "Return string S as-is for Org or plain text front matter."
  s)

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

(defun denote-extract-date-from-front-matter (date-string)
  "Extract date object from front matter DATE-STRING.

Consult the `denote-file-types' for how this is used."
  (let ((date-string (denote-trim-whitespace date-string)))
    (if (string-empty-p date-string)
        nil
      (date-to-time date-string))))

(defvar denote-file-types
  '((org
     :extension ".org"
     :front-matter denote-org-front-matter
     :title-key-regexp "^#\\+title\\s-*:"
     :title-value-function denote-format-string-for-org-front-matter
     :title-value-reverse-function denote-trim-whitespace
     :keywords-key-regexp "^#\\+filetags\\s-*:"
     :keywords-value-function denote-format-keywords-for-org-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :signature-key-regexp "^#\\+signature\\s-*:"
     :signature-value-function denote-format-string-for-org-front-matter
     :signature-value-reverse-function denote-trim-whitespace
     :identifier-key-regexp "^#\\+identifier\\s-*:"
     :identifier-value-function denote-format-string-for-org-front-matter
     :identifier-value-reverse-function denote-trim-whitespace
     :date-key-regexp "^#\\+date\\s-*:"
     :date-value-function denote-date-org-timestamp
     :date-value-reverse-function denote-extract-date-from-front-matter
     :link denote-org-link-format
     :link-in-context-regexp denote-org-link-in-context-regexp)
    (markdown-yaml
     :extension ".md"
     :front-matter denote-yaml-front-matter
     :title-key-regexp "^title\\s-*:"
     :title-value-function denote-format-string-for-md-front-matter
     :title-value-reverse-function denote-trim-whitespace-then-quotes
     :keywords-key-regexp "^tags\\s-*:"
     :keywords-value-function denote-format-keywords-for-md-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :signature-key-regexp "^signature\\s-*:"
     :signature-value-function denote-format-string-for-md-front-matter
     :signature-value-reverse-function denote-trim-whitespace-then-quotes
     :identifier-key-regexp "^identifier\\s-*:"
     :identifier-value-function denote-format-string-for-md-front-matter
     :identifier-value-reverse-function denote-trim-whitespace-then-quotes
     :date-key-regexp "^date\\s-*:"
     :date-value-function denote-date-rfc3339
     :date-value-reverse-function denote-extract-date-from-front-matter
     :link denote-md-link-format
     :link-in-context-regexp denote-md-link-in-context-regexp)
    (markdown-toml
     :extension ".md"
     :front-matter denote-toml-front-matter
     :title-key-regexp "^title\\s-*="
     :title-value-function denote-format-string-for-md-front-matter
     :title-value-reverse-function denote-trim-whitespace-then-quotes
     :keywords-key-regexp "^tags\\s-*="
     :keywords-value-function denote-format-keywords-for-md-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :signature-key-regexp "^signature\\s-*="
     :signature-value-function denote-format-string-for-md-front-matter
     :signature-value-reverse-function denote-trim-whitespace-then-quotes
     :identifier-key-regexp "^identifier\\s-*="
     :identifier-value-function denote-format-string-for-md-front-matter
     :identifier-value-reverse-function denote-trim-whitespace-then-quotes
     :date-key-regexp "^date\\s-*="
     :date-value-function denote-date-rfc3339
     :date-value-reverse-function denote-extract-date-from-front-matter
     :link denote-md-link-format
     :link-in-context-regexp denote-md-link-in-context-regexp)
    (text
     :extension ".txt"
     :front-matter denote-text-front-matter
     :title-key-regexp "^title\\s-*:"
     :title-value-function denote-format-string-for-org-front-matter
     :title-value-reverse-function denote-trim-whitespace
     :keywords-key-regexp "^tags\\s-*:"
     :keywords-value-function denote-format-keywords-for-text-front-matter
     :keywords-value-reverse-function denote-extract-keywords-from-front-matter
     :signature-key-regexp "^signature\\s-*:"
     :signature-value-function denote-format-string-for-org-front-matter
     :signature-value-reverse-function denote-trim-whitespace
     :identifier-key-regexp "^identifier\\s-*:"
     :identifier-value-function denote-format-string-for-org-front-matter
     :identifier-value-reverse-function denote-trim-whitespace
     :date-key-regexp "^date\\s-*:"
     :date-value-function denote-date-iso-8601
     :date-value-reverse-function denote-extract-date-from-front-matter
     :link denote-org-link-format
     :link-in-context-regexp denote-org-link-in-context-regexp))
  "Alist of variable `denote-file-type' and their format properties.

Each element is of the form (SYMBOL PROPERTY-LIST).  SYMBOL is one of
those specified in the user option `denote-file-type' or an arbitrary
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

If the user option `denote-file-type' is nil, use the first element of
this list for new note creation.  The default is `org'.")

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
  (or (plist-get
       (alist-get file-type denote-file-types)
       :title-key-regexp)
      "^DenoteUserWantsEmptyFieldSoHandleIt")) ; Will not be found

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
  (or (plist-get
       (alist-get file-type denote-file-types)
       :keywords-key-regexp)
      "^DenoteUserWantsEmptyFieldSoHandleIt")) ; Will not be found

(defun denote--keywords-value-function (file-type)
  "Convert keywords' list to front matter keywords, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :keywords-value-function))

(defun denote--keywords-value-reverse-function (file-type)
  "Convert front matter keywords to keywords' list, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :keywords-value-reverse-function))

(defun denote--signature-key-regexp (file-type)
  "Return the signature key regexp associated to FILE-TYPE."
  (or (plist-get
       (alist-get file-type denote-file-types)
       :signature-key-regexp)
      "^DenoteUserWantsEmptyFieldSoHandleIt")) ; Will not be found

(defun denote--signature-value-function (file-type)
  "Convert signature string to front matter signature, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :signature-value-function))

(defun denote--signature-value-reverse-function (file-type)
  "Convert front matter signature to signature string, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :signature-value-reverse-function))

(defun denote--identifier-key-regexp (file-type)
  "Return the identifier key regexp associated to FILE-TYPE."
  (or (plist-get
       (alist-get file-type denote-file-types)
       :identifier-key-regexp)
      "^DenoteUserWantsEmptyFieldSoHandleIt")) ; Will not be found

(defun denote--identifier-value-function (file-type)
  "Convert identifier string to front matter identifier, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :identifier-value-function))

(defun denote--identifier-value-reverse-function (file-type)
  "Convert front matter identifier to identifier string, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :identifier-value-reverse-function))

(defun denote--date-key-regexp (file-type)
  "Return the date key regexp associated to FILE-TYPE."
  (or (plist-get
       (alist-get file-type denote-file-types)
       :date-key-regexp)
      "^DenoteUserWantsEmptyFieldSoHandleIt")) ; Will not be found

(defun denote--date-value-function (file-type)
  "Convert date object to front matter date, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :date-value-function))

(defun denote--date-value-reverse-function (file-type)
  "Convert front matter date to date object, per FILE-TYPE."
  (plist-get
   (alist-get file-type denote-file-types)
   :date-value-reverse-function))

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

(defun denote-file-type-extensions ()
  "Return all file type extensions in `denote-file-types'."
  (delete-dups
   (mapcar (lambda (type)
             (plist-get (cdr type) :extension))
           denote-file-types)))

(defun denote--file-type-keys ()
  "Return all `denote-file-types' keys."
  (delete-dups (mapcar #'car denote-file-types)))

(defun denote--get-component-key-regexp-function (component)
  "Return COMPONENT's key regexp function.

COMPONENT can be one of `title', `keywords', `identifier', `date', `signature'."
  (pcase component
    ('title #'denote--title-key-regexp)
    ('keywords #'denote--keywords-key-regexp)
    ('signature #'denote--signature-key-regexp)
    ('date #'denote--date-key-regexp)
    ('identifier #'denote--identifier-key-regexp)))

(defun denote--format-front-matter (title date keywords id signature filetype)
  "Front matter for new notes.

TITLE, SIGNATURE, and ID are strings.  DATE is a date object.  KEYWORDS
is a list of strings.  FILETYPE is one of the values of variable
`denote-file-type'."
  (let* ((fm (denote--front-matter filetype))
         (title-value-function (denote--title-value-function filetype))
         (keywords-value-function (denote--keywords-value-function filetype))
         (id-value-function (denote--identifier-value-function filetype))
         (signature-value-function (denote--signature-value-function filetype))
         (title-string (if title-value-function (funcall title-value-function title) ""))
         (date-string (denote--format-front-matter-date date filetype))
         (keywords-string (if keywords-value-function (funcall keywords-value-function (denote-sluggify-keywords keywords)) ""))
         (id-string (if id-value-function (funcall id-value-function id) ""))
         (signature-string (if signature-value-function (funcall signature-value-function (denote-sluggify-signature signature)) ""))
         (new-front-matter (if fm (format fm title-string date-string keywords-string id-string signature-string) "")))
    ;; Remove lines with empty values if the corresponding component
    ;; is not in `denote-front-matter-components-present-even-if-empty-value'.
    (with-temp-buffer
      (insert new-front-matter)
      (dolist (component '(title date keywords signature identifier))
        (let ((value (pcase component ('title title) ('keywords keywords) ('signature signature) ('date date) ('identifier id)))
              (component-key-regexp-function (denote--get-component-key-regexp-function component)))
          (goto-char (point-min))
          (when (and (not (denote--component-has-value-p component value))
                     (not (memq component denote-front-matter-components-present-even-if-empty-value))
                     (re-search-forward (funcall component-key-regexp-function filetype) nil t 1))
              (goto-char (line-beginning-position))
              (delete-region (line-beginning-position) (line-beginning-position 2)))))
      (buffer-string))))

;;;; Front matter or content retrieval functions

(defun denote-retrieve-filename-identifier (file)
  "Extract identifier from FILE name, if present, else return nil.

To create a new one from a date, refer to the function
`denote-get-identifier'."
  (let ((filename (file-name-nondirectory file)))
    (cond ((string-match (concat "\\`" denote-id-regexp) filename)
           (match-string-no-properties 0 filename))
          ((string-match (concat "@@\\(?1:" denote-id-regexp "\\)") filename)
           (match-string-no-properties 1 filename)))))

;; TODO 2023-12-08: Maybe we can only use
;; `denote-retrieve-filename-identifier' and remove this function.
(defun denote-retrieve-filename-identifier-with-error (file)
  "Extract identifier from FILE name, if present, else signal an error."
  (or (denote-retrieve-filename-identifier file)
      (error "Cannot find `%s' as a file with a Denote identifier" file)))

(defun denote-get-identifier (date)
  "Convert DATE into a Denote identifier using `denote-id-format'.
If DATE is nil, return an empty string as the identifier."
  (if date
      (format-time-string denote-id-format date)
    ""))

(defvar denote--used-ids nil
  "Hash table of used identifiers.
This variable should be set only for the duration of a command.
It should stay nil otherwise.")

(define-obsolete-function-alias
  'denote-create-unique-file-identifier
  'denote-get-identifier
  "4.0.0")

(defun denote-retrieve-filename-keywords (file)
  "Extract keywords from FILE name, if present, else return nil.
Return matched keywords as a single string.

Also see `denote-extract-keywords-from-path' (alias
`denote-retrieve-filename-keywords-as-list')."
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
  `(when-let* ((file-and-function (denote--file-with-temp-buffer-subr ,file)))
     (with-temp-buffer
       (funcall (car file-and-function) (cdr file-and-function))
       (goto-char (point-min))
       ,@body)))

(defmacro denote--define-retrieve-front-matter (component scope)
  "Define a function to retrieve front matter for COMPONENT given SCOPE.
The COMPONENT is one of the file name components that has a
corresponding front matter entry.  SCOPE is a symbol of either `value'
or `line', referring to what the function should retrieve."
  (declare (indent 1))
  `(defun ,(intern (format "denote-retrieve-front-matter-%s-%s" component scope)) (file file-type)
     (when file-type
       (denote--file-with-temp-buffer file
         (when (re-search-forward (,(intern (format "denote--%s-key-regexp" component)) file-type) nil t 1)
           ,(cond
             ((eq scope 'value)
              `(funcall (,(intern (format "denote--%s-value-reverse-function" component)) file-type)
                        (buffer-substring-no-properties (point) (line-end-position))))
             ((eq scope 'line)
              '(buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (t (error "`%s' is not a known scope" scope))))))))

(denote--define-retrieve-front-matter title value)
(denote--define-retrieve-front-matter title line)
(denote--define-retrieve-front-matter keywords value)
(denote--define-retrieve-front-matter keywords line)
(denote--define-retrieve-front-matter signature value)
(denote--define-retrieve-front-matter signature line)
(denote--define-retrieve-front-matter identifier value)
(denote--define-retrieve-front-matter identifier line)
(denote--define-retrieve-front-matter date value)
(denote--define-retrieve-front-matter date line)

;; These are private front matter retrieval functions, working with a content parameter

(defmacro denote--define-retrieve-front-matter-from-content (component scope)
  "Define a function to retrieve front matter for COMPONENT given SCOPE.
The COMPONENT is one of the file name components that has a
corresponding front matter entry.  SCOPE is a symbol of either `value'
or `line', referring to what the function should retrieve."
  (declare (indent 1))
  `(defun ,(intern (format "denote--retrieve-front-matter-%s-%s-from-content" component scope)) (content file-type)
     (when file-type
       (with-temp-buffer
         (insert content)
         (goto-char (point-min))
         (when (re-search-forward (,(intern (format "denote--%s-key-regexp" component)) file-type) nil t 1)
           ,(cond
             ((eq scope 'value)
              `(funcall (,(intern (format "denote--%s-value-reverse-function" component)) file-type)
                        (buffer-substring-no-properties (point) (line-end-position))))
             ((eq scope 'line)
              '(buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (t (error "`%s' is not a known scope" scope))))))))

(denote--define-retrieve-front-matter-from-content title value)
(denote--define-retrieve-front-matter-from-content title line)
(denote--define-retrieve-front-matter-from-content keywords value)
(denote--define-retrieve-front-matter-from-content keywords line)
(denote--define-retrieve-front-matter-from-content signature value)
(denote--define-retrieve-front-matter-from-content signature line)
(denote--define-retrieve-front-matter-from-content identifier value)
(denote--define-retrieve-front-matter-from-content identifier line)
(denote--define-retrieve-front-matter-from-content date value)
(denote--define-retrieve-front-matter-from-content date line)

(defalias 'denote-retrieve-title-value 'denote-retrieve-front-matter-title-value
  "Alias for `denote-retrieve-front-matter-title-value'.")

(defalias 'denote-retrieve-title-line 'denote-retrieve-front-matter-title-line
  "Alias for `denote-retrieve-front-matter-title-line'.")

(defalias 'denote-retrieve-keywords-value 'denote-retrieve-front-matter-keywords-value
  "Alias for `denote-retrieve-front-matter-keywords-value'.")

(defalias 'denote-retrieve-keywords-line 'denote-retrieve-front-matter-keywords-line
  "Alias for `denote-retrieve-front-matter-keywords-line'.")

(defun denote-retrieve-title-or-filename (file type)
  "Return appropriate title for FILE given its TYPE.
This is a wrapper for `denote-retrieve-front-matter-title-value' and
`denote-retrieve-filename-title'."
  (let ((has-denoted-filename (denote-file-has-denoted-filename-p file))
        (has-supported-extension (denote-file-has-supported-extension-p file)))
    (cond ((and has-denoted-filename has-supported-extension)
           (or (denote-retrieve-front-matter-title-value file type)
               (denote-retrieve-filename-title file)
               ""))
          (has-denoted-filename
           (or (denote-retrieve-filename-title file) ""))
          (t
           (file-name-base file)))))

(make-obsolete 'denote--retrieve-location-in-xrefs 'denote-retrieve-groups-xref-query "4.0.0")

(define-obsolete-function-alias
  'denote--retrieve-group-in-xrefs
  'denote-retrieve-groups-xref-query
  "4.0.0")

(defun denote-retrieve-groups-xref-query (query &optional files-matching-regexp)
  "Access location of xrefs for QUERY and group them per file.
Limit the search to text files.  With optional FILES-MATCHING-REGEXP,
pass it to `denote-directory-files'."
  (when-let* ((files (denote-directory-files files-matching-regexp nil :text-only))
              (locations (mapcar #'xref-match-item-location (xref-matches-in-files query files))))
    (mapcar #'xref-location-group locations)))

(define-obsolete-function-alias
  'denote--retrieve-files-in-xrefs
  'denote-retrieve-files-xref-query
  "4.0.0")

(defun denote-retrieve-files-xref-query (query &optional files-matching-regexp)
  "Return sorted, deduplicated file names with matches for QUERY in their contents.
Limit the search to text files.  With optional FILES-MATCHING-REGEXP,
pass it to `denote-directory-files'."
  (sort
   (delete-dups
    (denote-retrieve-groups-xref-query query files-matching-regexp))
   #'string-collate-lessp))

(defvar denote-query--last-files nil
  "List of files matched by the last call to `denote-make-links-buffer'.")

(defvar denote-query--last-query nil
  "String of the last call to `denote-make-links-buffer'.")

(defvar denote-query--omit-current t
  "When non-nil `denote-make-links-buffer' omits the current file.")

(defun denote-retrieve-xref-alist (query &optional files)
  "Return xref alist of absolute file paths with location of matches for QUERY.
Optional FILES can be a list of files to search for.  It can also be a
regular expression, which means to use the text files in the variable
`denote-directory' that match that regexp.

If FILES is not given, use all text files as returned by
`denote-directory-files'."
  (let ((xref-file-name-display 'abs))
    (xref--analyze
     (xref-matches-in-files
      query
      (if (and files (listp files))
          files
        (denote-directory-files files denote-query--omit-current :text-only))))))

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

ID is a string holding the identifier of the note.  It can be an
empty string, in which case its respective file name component is
not added to the base file name.

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
    (error "DIR-PATH does not end with a / as directories ought to")))
  (let ((file-name "")
        (components (seq-union denote-file-name-components-order
                               '(identifier signature title keywords))))
    (dolist (component components)
      (cond ((and (eq component 'identifier) id (not (string-empty-p id)))
             (setq file-name (concat file-name "@@" id)))
            ((and (eq component 'title) title (not (string-empty-p title)))
             (setq file-name (concat file-name "--" (denote-sluggify 'title title))))
            ((and (eq component 'keywords) keywords)
             (setq file-name (concat file-name "__" (denote-keywords-combine (denote-sluggify-keywords keywords)))))
            ((and (eq component 'signature) signature (not (string-empty-p signature)))
             (setq file-name (concat file-name "==" (denote-sluggify 'signature signature))))))
    (when (string-empty-p file-name)
      (error "There should be at least one file name component"))
    (setq file-name (concat file-name extension))
    ;; Do not prepend identifier with @@ if it is the first component and has the format 00000000T000000.
    (when (and (string-prefix-p "@@" file-name)
               (string-match-p (concat "\\`" denote-id-regexp "\\'") id)) ; This is always true for now.
      (setq file-name (substring file-name 2)))
    (concat dir-path file-name)))

;; Adapted from `org-hugo--org-date-time-to-rfc3339' in the `ox-hugo'
;; package: <https://github.com/kaushalmodi/ox-hugo>.
(defun denote-date-rfc3339 (date)
  "Format DATE using the RFC3339 specification."
  (if date
      (replace-regexp-in-string
       "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
       (format-time-string "%FT%T%z" date))
    ""))

(defun denote-date-org-timestamp (date)
  "Format DATE using the Org inactive timestamp notation."
  (if date
      (format-time-string "[%F %a %R]" date)
    ""))

(defun denote-date-iso-8601 (date)
  "Format DATE according to ISO 8601 standard."
  (if date
      (format-time-string "%F" date)
    ""))

(defun denote--format-front-matter-date (date file-type)
  "Expand DATE in an appropriate format for FILE-TYPE."
  (let ((format denote-date-format))
    (cond
     (format
      (if date (format-time-string format date) ""))
     ((when-let* ((fn (denote--date-value-function file-type)))
        (funcall fn date)))
     (t
      (denote-date-org-timestamp date)))))

(defun denote--prepare-note (title keywords date id directory file-type template signature)
  "Prepare a new note file and return its path.

Arguments TITLE, KEYWORDS, DATE, ID, DIRECTORY, FILE-TYPE,
TEMPLATE, and SIGNATURE should be valid for note creation."
  (let* ((path (denote-format-file-name
                directory id keywords title (denote--file-extension file-type) signature))
         (buffer (find-file path))
         (header (denote--format-front-matter title date keywords id signature file-type)))
    (when (file-regular-p path)
      (user-error "A file named `%s' already exists" path))
    (with-current-buffer buffer
      (insert header)
      (insert (cond
               ((stringp template) template)
               ((functionp template) (funcall template))
               (t (user-error "Invalid template")))))
    path))

(defun denote--dir-in-denote-directory-p (directory)
  "Return non-nil if DIRECTORY is in variable `denote-directory'."
  (string-prefix-p (denote-directory) (expand-file-name directory)))

(defun denote--valid-file-type (filetype)
  "Return a valid filetype symbol given the argument FILETYPE.
If none is found, the first element of `denote-file-types' is
returned."
  (let ((type (cond
               ((stringp filetype) (intern filetype))
               ((symbolp filetype) filetype)
               (t (error "The `%s' is neither a string nor a symbol" filetype)))))
    (cond ((memq type (denote--file-type-keys))
           type)
          ((null denote-file-types)
           (user-error "At least one file type must be defined in `denote-file-types' to create a note"))
          (t
           (caar denote-file-types)))))

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
  'denote-parse-date
  'denote-valid-date-p
  "4.0.0")

(defun denote-valid-date-p (date)
  "Return DATE as a valid date.
A valid DATE is a value that can be parsed by either
`decode-time' or `date-to-time'.  Those functions signal an error
if DATE is a value they do not recognise.

If DATE is nil or an empty string, return nil."
  (cond ((null date)
         nil)
        ((and (stringp date) (string-empty-p date))
         nil)
        ((and (or (numberp date) (listp date))
              (decode-time date))
         date)
        (t ; non-empty strings (e.g. "2024-01-01", "2024-01-01 12:00", etc.)
         (date-to-time (denote--date-add-current-time date)))))

(defun denote--id-to-date (identifier)
  "Convert IDENTIFIER string to YYYY-MM-DD."
  (if (denote-identifier-p identifier)
      (replace-regexp-in-string
       "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\).*"
       "\\1-\\2-\\3"
       identifier)
    (error "`%s' does not look like a Denote identifier per `denote-id-regexp'" identifier)))

(defun denote--buffer-file-names ()
  "Return file names of Denote buffers."
  (delq nil
        (mapcar
         (lambda (buffer)
           (when-let* (((buffer-live-p buffer))
                       (file (buffer-file-name buffer))
                       ((denote-filename-is-note-p file)))
             file))
         (buffer-list))))

(defun denote--get-all-used-ids ()
  "Return a hash-table of all used identifiers.
It checks files in variable `denote-directory' and active buffer files."
  (let* ((ids (make-hash-table :test 'equal))
         (file-names (mapcar
                      (lambda (file) (file-name-nondirectory file))
                      (denote-directory-files)))
         (names (append file-names (denote--buffer-file-names))))
    (dolist (name names)
      (when-let* ((id (denote-retrieve-filename-identifier name)))
        (puthash id t ids)))
    ids))

(defun denote--find-first-unused-id (id)
  "Return the first unused id starting at ID.
If ID is already used, increment it 1 second at a time until an
available id is found."
  (let ((used-ids (or denote--used-ids (denote--get-all-used-ids)))
        (current-id id)
        (iteration 0))
    (while (gethash current-id used-ids)
      ;; Prevent infinite loop if `denote-id-format' is misconfigured
      (setq iteration (1+ iteration))
      (when (>= iteration 10000)
        (user-error "A unique identifier could not be found"))
      (setq current-id (denote-get-identifier (time-add (date-to-time current-id) 1))))
    current-id))

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
creation.  Else, the value of `denote-save-buffers' is respected.

With non-nil IN-BACKGROUND, the note creation happens in the
background, i.e. the note's buffer will not be displayed after
the note is created.

Note that if all parameters except COMMAND are nil, this is
equivalent to `(call-interactively command)'.

The path of the newly created file is returned."
  (let ((denote-save-buffers
         (or force-save denote-save-buffers))
        (denote-ignore-region-in-denote-command
         (or force-ignore-region denote-ignore-region-in-denote-command))
        (denote-title-prompt-current-default
         (if force-use-file-prompt-as-default-title
             denote-file-prompt-latest-input
           denote-title-prompt-current-default))
        (path))
    (if in-background
        (save-window-excursion
          (setq path (call-interactively command)))
      (setq path (call-interactively command)))
    path))

(defun denote--handle-save-and-kill-buffer (mode file initial-state)
  "Save and kill buffer of FILE according to MODE and INITIAL-STATE.

The values of `denote-save-buffers' and `denote-kill-buffers' are
used to decide whether to save and/or kill the buffer visiting
FILE.

MODE is one of the symbols `creation' or `rename'.

INITIAL-STATE is nil or one of the following symbols:
`not-visited', `visited'.  If a buffer was already visited at the
beginning of a rename operation, it is NOT killed automatically.

If a buffer needs to be killed, it is also automatically saved,
no matter the value of `denote-save-buffers'."
  (let* ((do-kill-buffer (and (not (eq initial-state 'visited))
                              (or (eq denote-kill-buffers t)
                                  (and (eq mode 'creation)
                                       (eq denote-kill-buffers 'on-creation))
                                  (and (eq mode 'rename)
                                       (eq denote-kill-buffers 'on-rename)))))
         (do-save-buffer (or do-kill-buffer denote-save-buffers)))
    (when-let* ((buffer (find-buffer-visiting file)))
      (when do-save-buffer (with-current-buffer buffer (save-buffer)))
      (when do-kill-buffer (kill-buffer buffer)))))

(defvar denote-current-data nil
  "Store the current unprocessed data passed to `denote'.
This is an alist where each `car' is one among `title', `keywords',
`signature', `directory', `date', `id', `file-type', `template'.  The
value each of them contains is the unprocessed input (e.g. the title
before it is sluggified).

This may be used by the hooks `denote-after-new-note-hook' and
`denote-after-rename-file-hook' to access the relevant data.")

(defvar denote-use-title nil
  "The title to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the title will always be the same
and the title prompt will be skipped.")

(defvar denote-use-keywords 'default
  "The keywords to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if `default'.

Only ever `let' bind this, otherwise the keywords will always be the same
and the keywords prompt will be skipped.")

(defvar denote-use-signature nil
  "The signature to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the signaturew will always be the same
and the signature prompt will be skipped.")

(defvar denote-use-file-type nil
  "The title to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the file type will always be the
same.")

(defvar denote-use-directory nil
  "The directory to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the directory will always be the
same.")

(defvar denote-use-date nil
  "The date to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the date will always be the same
and the date prompt will be skipped.")

(defvar denote-use-template nil
  "The template to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the template will always be the same
and the template prompt will be skipped.")

(defun denote--creation-get-note-data-from-prompts ()
  "Retrieve the data necessary for note creation.

The data elements are: title, keywords, file-type, directory,
date, template and signature.

It is retrieved from prompts according to `denote-prompts' and
from `denote-use-*' variables.  For example, if
`denote-use-title' is set to a title, then no prompts happen for
the title and the value of `denote-use-title' will be used
instead."
  (let (title keywords file-type directory date template signature)
    (dolist (prompt denote-prompts)
      (pcase prompt
        ('title (unless denote-use-title
                  (setq title (denote-title-prompt
                               (when (and (not denote-ignore-region-in-denote-command)
                                          (use-region-p))
                                 (buffer-substring-no-properties
                                  (region-beginning)
                                  (region-end)))))))
        ('keywords (when (eq denote-use-keywords 'default)
                     (setq keywords (denote-keywords-prompt))))
        ('file-type (unless denote-use-file-type
                      (setq file-type (denote-file-type-prompt))))
        ('subdirectory (unless denote-use-directory
                         (setq directory (denote-subdirectory-prompt))))
        ('date (unless denote-use-date
                 (setq date (denote-date-prompt))))
        ('template (unless denote-use-template
                     (setq template (denote-template-prompt))))
        ('signature (unless denote-use-signature
                      (setq signature (denote-signature-prompt))))))
    (list title keywords file-type directory date template signature)))

(defun denote--creation-prepare-note-data (title keywords file-type directory date template signature)
  "Return parameters in a valid form for file creation.

The data is: TITLE, KEYWORDS, FILE-TYPE, DIRECTORY, DATE,
TEMPLATE and SIGNATURE.  The identifier is also returned.

If a `denote-use-*' variable is set for a data, its value is used
instead of that of the parameter."
  (let* (;; Handle the `denote-use-*' variables
         (title (or denote-use-title title))
         (keywords (if (eq denote-use-keywords 'default) keywords denote-use-keywords))
         (file-type (or denote-use-file-type file-type))
         (directory (or denote-use-directory directory))
         (date (or denote-use-date date))
         (template (or denote-use-template template))
         (signature (or denote-use-signature signature))
         ;; Make the data valid
         (title (or title ""))
         (file-type (denote--valid-file-type (or file-type denote-file-type)))
         (keywords (denote-keywords-sort keywords))
         (date (denote-valid-date-p date))
         (date (cond (date date)
                     ((or (eq denote-generate-identifier-automatically t)
                          (eq denote-generate-identifier-automatically 'on-creation))
                      (current-time))))
         (id (denote-get-identifier date))
         (id (if (string-empty-p id) id (denote--find-first-unused-id id)))
         (date (if (string-empty-p id) nil (date-to-time id)))
         (directory (if (and directory (denote--dir-in-denote-directory-p directory))
                        (file-name-as-directory directory)
                      (denote-directory)))
         (template (if (or (stringp template) (functionp template))
                       template
                     (or (alist-get template denote-templates) "")))
         (signature (or signature "")))
    (list title keywords file-type directory date id template signature)))

;;;###autoload
(defun denote (&optional title keywords file-type directory date template signature)
  "Create a new note with the appropriate metadata and file name.

Run the `denote-after-new-note-hook' after creating the new note and
return its path.  Before returning the path, determine what needs to be
done to the buffer, in accordance with the user option `denote-kill-buffers'.

When called interactively, the metadata and file name are prompted
according to the value of `denote-prompts'.

When called from Lisp, all arguments are optional.

- TITLE is a string or a function returning a string.

- KEYWORDS is a list of strings.  The list can be empty or the
  value can be set to nil.

- FILE-TYPE is a symbol among those described in the user option
  `denote-file-type'.

- DIRECTORY is a string representing the path to either the
  value of the variable `denote-directory' or a subdirectory
  thereof.  The subdirectory must exist: Denote will not create
  it.  If DIRECTORY does not resolve to a valid path, the
  variable `denote-directory' is used instead.

- DATE is a string representing a date like 2022-06-30 or a date
  and time like 2022-06-16 14:30.  A nil value or an empty string
  is interpreted as the `current-time'.

- TEMPLATE is a symbol which represents the key of a cons cell in
  the user option `denote-templates'.  The value of that key is
  inserted to the newly created buffer after the front matter.

- SIGNATURE is a string or a function returning a string."
  (interactive (denote--creation-get-note-data-from-prompts))
  (pcase-let* ((`(,title ,keywords ,file-type ,directory ,date ,id ,template ,signature)
                (denote--creation-prepare-note-data title keywords file-type directory date template signature))
               (note-path (denote--prepare-note title keywords date id directory file-type template signature)))
    (denote--keywords-add-to-history keywords)
    (setq denote-current-data
          (list
           (cons 'title title)
           (cons 'keywords keywords)
           (cons 'signature signature)
           (cons 'directory directory)
           (cons 'date date)
           (cons 'id id)
           (cons 'file-type file-type)
           (cons 'template template)))
    (run-hooks 'denote-after-new-note-hook)
    (denote--handle-save-and-kill-buffer 'creation note-path nil)
    note-path))

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
   (or initial-title denote-title-prompt-current-default)
   denote-title-prompt-current-default))

(defvar denote-file-type-history nil
  "Minibuffer history of `denote-file-type-prompt'.")

(defalias 'denote--file-type-history 'denote-file-type-history
  "Compatibility alias for `denote-file-type-history'.")

(defun denote-file-type-prompt ()
  "Prompt for variable `denote-file-type'.
Note that a non-nil value other than `text', `markdown-yaml', and
`markdown-toml' falls back to an Org file type.  We use `org'
here for clarity."
  (completing-read
   "Select file TYPE: " (denote--file-type-keys) nil t
   nil 'denote-file-type-history))

(defvar denote-date-history nil
  "Minibuffer history of `denote-date-prompt'.")

(defalias 'denote--date-history 'denote-date-history
  "Compatibility alias for `denote-date-history'.")

(declare-function org-read-date "org" (&optional with-time to-time from-string prompt default-time default-input inactive))

(defun denote--date-convert (date prefer-type)
  "Determine how to convert DATE to PREFER-TYPE `:list' or `:string'."
  (unless (memq prefer-type '(:list :string))
    (error "The PREFER-TYPE must be either `:list' or `:string'"))
  (cond ((eq prefer-type :list)
         date)
        ((eq prefer-type :string)
         (if date (format-time-string "%F %T" date) ""))))

(defun denote-date-prompt (&optional initial-date prompt-text)
  "Prompt for date, expecting YYYY-MM-DD or that plus HH:MM.
Use Org's more advanced date selection utility if the user option
`denote-date-prompt-use-org-read-date' is non-nil.

With optional INITIAL-DATE use it as the initial minibuffer
text.  With optional PROMPT-TEXT use it in the minibuffer instead
of the default prompt.

INITIAL-DATE is a string that can be processed by `denote-valid-date-p',
a value that can be parsed by `decode-time' or nil."
  (let ((initial-date (denote-valid-date-p initial-date)))
    (if (and denote-date-prompt-use-org-read-date
             (require 'org nil :no-error))
        (let* ((time (org-read-date nil t nil prompt-text (denote--date-convert initial-date :list)))
               (org-time-seconds (format-time-string "%S" time))
               (cur-time-seconds (format-time-string "%S" (current-time))))
          ;; When the user does not input a time, org-read-date defaults to 00 for seconds.
          ;; When the seconds are 00, we add the current seconds to avoid identifier collisions.
          (when (string-equal "00" org-time-seconds)
            (setq time (time-add time (string-to-number cur-time-seconds))))
          (format-time-string "%Y-%m-%d %H:%M:%S" time))
      (read-string
       (or prompt-text "DATE and TIME for note (e.g. 2022-06-16 14:30): ")
       (denote--date-convert initial-date :string)
       'denote-date-history))))

(defun denote-prompt-for-date-return-id (&optional initial-date prompt-text)
  "Use `denote-date-prompt' and return it as `denote-id-format'.
Optional INITIAL-DATE and PROMPT-TEXT have the same meaning as
`denote-date-prompt'."
  (denote-get-identifier
   (denote-valid-date-p
    (denote-date-prompt (denote-valid-date-p initial-date) prompt-text))))

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
         (prompt (format-prompt "Select SUBDIRECTORY" def)))
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
       "Select TEMPLATE key: " (mapcar #'car templates)
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

(define-obsolete-function-alias
  'denote--add-prompts
  'denote-add-prompts
  "3.0.0")

(defun denote-add-prompts (additional-prompts)
  "Add list of ADDITIONAL-PROMPTS to `denote-prompts'.
This is best done inside of a `let' to create a wrapper function around
`denote', `denote-rename-file', and generally any command that consults
the value of `denote-prompts'."
  (seq-union additional-prompts denote-prompts))

;;;###autoload
(defun denote-type ()
  "Create note while prompting for a file type.

This is the equivalent of calling `denote' when `denote-prompts'
has the `file-type' prompt appended to its existing prompts."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts (denote-add-prompts '(file-type))))
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
  (let ((denote-prompts (denote-add-prompts '(date))))
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
  (let ((denote-prompts (denote-add-prompts '(subdirectory))))
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
  (let ((denote-prompts (denote-add-prompts '(template))))
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
  (let ((denote-prompts (denote-add-prompts '(signature))))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-signature 'denote-signature
  "Alias for `denote-signature' command.")

;;;###autoload
(defun denote-region ()
  "Call `denote' and insert therein the text of the active region.

Note that, currently, `denote-save-buffers' and
`denote-kill-buffers' are NOT respected.  The buffer is not
saved or killed at the end of `denote-region'."
  (declare (interactive-only t))
  (interactive)
  (if-let* (((region-active-p))
            ;; We capture the text early, otherwise it will be empty
            ;; the moment `insert' is called.
            (text (buffer-substring-no-properties (region-beginning) (region-end))))
      (progn
        (let ((denote-ignore-region-in-denote-command t)
              ;; FIXME: Find a way to insert the region before the buffer is
              ;; saved/killed by the creation command.
              (denote-save-buffers nil)
              (denote-kill-buffers nil))
          (call-interactively 'denote))
        (push-mark (point))
        (insert text)
        (run-hook-with-args 'denote-region-after-new-note-functions (mark) (point)))
    (call-interactively 'denote)))

;;;;; Other convenience commands

;;;###autoload
(defun denote-open-or-create (target)
  "Visit TARGET file in variable `denote-directory'.
If file does not exist, invoke `denote' to create a file.  In that case,
use the last input at the file prompt as the default value of the title
prompt."
  (interactive (list (denote-file-prompt nil "Select file (RET on no match to create it)" :no-require-match)))
  (if (and target (file-exists-p target))
      (find-file target)
    (denote--command-with-features #'denote :use-last-input-as-def-title nil nil nil)))

;;;###autoload
(defun denote-open-or-create-with-command ()
  "Like `denote-open-or-create' but use one of the `denote-commands-for-new-notes'."
  (declare (interactive-only t))
  (interactive)
  (let ((target (denote-file-prompt nil "Select file (RET on no match to create it)" :no-require-match)))
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

(defun denote--file-type-org-extra-p ()
  "Return non-nil if this is an `org-capture' or Org Note buffer."
  (and (derived-mode-p 'org-mode)
       (or (and (bound-and-true-p org-capture-mode)
                (string-match-p "\\`CAPTURE-.*" (buffer-name)))
           (string-match-p "\\`\\*Org Note\\*" (buffer-name))
           (null buffer-file-name))))

(defun denote-file-type (file)
  "Use the file extension to detect the file type of FILE.

If more than one file type correspond to this file extension, use the
first file type for which the :title-key-regexp in `denote-file-types'
matches in the file.

Return nil if the file type is not recognized."
  (when-let* ((extension (denote-get-file-extension-sans-encryption file))
              (types (denote--file-types-with-extension extension)))
    (if (= (length types) 1)
        (caar types)
      (or (car (seq-find
                (lambda (type)
                  (denote--regexp-in-file-p (plist-get (cdr type) :title-key-regexp) file))
                types))
          (caar types)))))

(defun denote-filetype-heuristics (file)
  "Return likely file type of FILE.
If in the process of `org-capture', consider the file type to be that of
Org.  Otherwise, use the function `denote-file-type' to return the type."
  (if (denote--file-type-org-extra-p)
      'org
    (denote-file-type file)))

(defun denote--revert-dired (buf)
  "Revert BUF if appropriate.
Do it if BUF is in Dired mode and is either part of the variable
`denote-directory' or the `current-buffer'."
  (let ((current (current-buffer)))
    (with-current-buffer buf
      (when (and (eq major-mode 'dired-mode)
                 (or (and default-directory (denote--dir-in-denote-directory-p default-directory))
                     (eq current buf)))
        (revert-buffer)))))

(defun denote-update-dired-buffers ()
  "Update Dired buffers of variable `denote-directory'.
Also revert the current Dired buffer even if it is not inside the
variable `denote-directory'."
  (mapc #'denote--revert-dired (buffer-list)))

(defun denote-rename-file-and-buffer (old-name new-name)
  "Rename file named OLD-NAME to NEW-NAME, updating buffer name.

If the file exists on the file system, it is renamed.  This
function may be called when creating a new note and the file does
not exist yet.

If a buffer is visiting the file, its name is updated."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (when (and (file-regular-p old-name)
               (file-writable-p new-name))
      (cond
       ((derived-mode-p 'dired-mode)
        (dired-rename-file old-name new-name nil))
       ;; NOTE 2024-02-25: The `vc-rename-file' requires the file to be
       ;; saved, but our convention is to not save the buffer after
       ;; changing front matter unless we absolutely have to (allows
       ;; users to do `diff-buffer-with-file', for example).
       ((and denote-save-buffers (not (buffer-modified-p)) (vc-backend old-name))
        (vc-rename-file old-name new-name))
       (t
        (rename-file old-name new-name nil))))
    (when-let* ((buffer (find-buffer-visiting old-name)))
      (with-current-buffer buffer
        (set-visited-file-name new-name nil t)))))

(define-obsolete-function-alias
  'denote--add-front-matter
  'denote-prepend-front-matter
  "4.0.0")

(defun denote-prepend-front-matter (file title keywords signature date id file-type)
  "Prepend front matter to FILE.
The TITLE, KEYWORDS, DATE, ID, SIGNATURE, and FILE-TYPE are passed from
the renaming command and are used to construct a new front matter block
if appropriate."
  (when-let* ((new-front-matter (denote--format-front-matter title date keywords id signature file-type)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (insert new-front-matter))))

(defun denote--regexp-in-file-p (regexp file)
  "Return t if REGEXP matches in the FILE."
  (denote--file-with-temp-buffer file
    (re-search-forward regexp nil t 1)))

(defun denote-rewrite-keywords (file keywords file-type &optional save-buffer)
  "Rewrite KEYWORDS in FILE outright according to FILE-TYPE.

Do the same as `denote-rewrite-front-matter' for keywords,
but do not ask for confirmation.

With optional SAVE-BUFFER, save the buffer corresponding to FILE.

This function is for use in the commands `denote-keywords-add',
`denote-keywords-remove', `denote-dired-rename-files', or
related."
  (let* ((new-front-matter (denote--format-front-matter "" (current-time) keywords "" "" file-type))
         (new-keywords-line (denote--retrieve-front-matter-keywords-line-from-content new-front-matter file-type)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
            (goto-char (line-beginning-position))
            (insert new-keywords-line)
            (delete-region (point) (line-end-position))
            (when save-buffer (save-buffer))))))))

(defun denote--component-has-value-p (component value)
  "Return non-nil if COMPONENT has a non-nil/non-empty VALUE.

COMPONENT can be one of `title', `keywords', `signature', `date',
`identifier'.

VALUE is the corresponding value to test.

This function returns nil given an empty string title, signature or
identifier.  It also returns nil given a nil date or nil keywords."
  (pcase component
    ('title (not (string-empty-p value)))
    ('keywords (not (null value)))
    ('signature (not (string-empty-p (denote-sluggify-signature value))))
    ('date (not (null value)))
    ('identifier (not (string-empty-p value)))))

(defun denote--get-old-and-new-front-matter-lines (file new-front-matter file-type)
  "Return an alist of the old and new front-matter lines for each component.

The FILE contains the old front matter lines.

NEW-FRONT-MATTER is a the front matter with the new values, with the
format given by FILE-TYPE."
  `((title . ((old . ,(denote-retrieve-front-matter-title-line file file-type))
              (new . ,(denote--retrieve-front-matter-title-line-from-content new-front-matter file-type))))
    (keywords . ((old . ,(denote-retrieve-front-matter-keywords-line file file-type))
                 (new . ,(denote--retrieve-front-matter-keywords-line-from-content new-front-matter file-type))))
    (signature . ((old . ,(denote-retrieve-front-matter-signature-line file file-type))
                  (new . ,(denote--retrieve-front-matter-signature-line-from-content new-front-matter file-type))))
    (date . ((old . ,(denote-retrieve-front-matter-date-line file file-type))
             (new . ,(denote--retrieve-front-matter-date-line-from-content new-front-matter file-type))))
    (identifier . ((old . ,(denote-retrieve-front-matter-identifier-line file file-type))
                   (new . ,(denote--retrieve-front-matter-identifier-line-from-content new-front-matter file-type))))))

(defun denote--get-front-matter-components-order (content file-type)
  "Return the components in the order they appear in CONTENT given FILE-TYPE.

Return a list containing the symbols `title', `signature', `keywords',
`identifier' and `date' in the order that they appear in TEXT.  TEXT can
be any string.  For example, it can be a front matter template or an
entire file content."
  (let ((components-with-line-numbers '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
        (push `(,(line-number-at-pos) . title) components-with-line-numbers))
      (goto-char (point-min))
      (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
        (push `(,(line-number-at-pos) . keywords) components-with-line-numbers))
      (goto-char (point-min))
      (when (re-search-forward (denote--signature-key-regexp file-type) nil t 1)
        (push `(,(line-number-at-pos) . signature) components-with-line-numbers))
      (goto-char (point-min))
      (when (re-search-forward (denote--date-key-regexp file-type) nil t 1)
        (push `(,(line-number-at-pos) . date) components-with-line-numbers))
      (goto-char (point-min))
      (when (re-search-forward (denote--identifier-key-regexp file-type) nil t 1)
        (push `(,(line-number-at-pos) . identifier) components-with-line-numbers)))
    (mapcar #'cdr
            (sort components-with-line-numbers (lambda (x y) (< (car x) (car y)))))))

(defun denote--file-has-front-matter-p (file file-type)
  "Return non-nil if FILE has at least one front-matter line, given FILE-TYPE.

This is checked against its front matter definition.  If the front matter
definition has no lines, this function returns non-nil."
  (let* ((front-matter (denote--front-matter file-type))
         (file-content (with-current-buffer (find-file-noselect file) (buffer-string)))
         (components-in-template (denote--get-front-matter-components-order front-matter file-type))
         (components-in-file (denote--get-front-matter-components-order file-content file-type)))
    (or (null components-in-template)
        (seq-intersection components-in-template components-in-file))))

(defun denote--get-front-matter-rewrite-prompt (final-components to-add to-remove to-modify old-and-new-front-matter-lines)
  "Return the prompt for the front matter rewrite operation.

FINAL-COMPONENTS is the list of components to handle at the end of the
rewrite operation.

TO-ADD, TO-REMOVE, and TO-MODIFY are the list of components that needs
to be added, removed or modified.

OLD-AND-NEW-FRONT-MATTER-LINES is an alist containing the old and new
front matter lines."
  (let ((prompt "Replace front matter?"))
    (dolist (component final-components)
      (let ((old-line (alist-get 'old (alist-get component old-and-new-front-matter-lines)))
            (new-line (alist-get 'new (alist-get component old-and-new-front-matter-lines)))
            (next-prompt ""))
        (cond ((memq component to-remove)
               (setq next-prompt (format "\n-%s\n"
                                         (propertize old-line 'face 'denote-faces-prompt-old-name))))
              ((memq component to-add)
               (setq next-prompt (format "\n-%s\n"
                                         (propertize new-line 'face 'denote-faces-prompt-new-name))))
              ((memq component to-modify)
               (setq next-prompt (format "\n-%s\n-%s\n"
                                         (propertize old-line 'face 'denote-faces-prompt-old-name)
                                         (propertize new-line 'face 'denote-faces-prompt-new-name)))))
        (setq prompt (concat prompt next-prompt))))
    (concat prompt "?")))

(defun denote--get-final-components-for-rewrite (components-in-file components-in-template components-to-add)
  "Return the final components to handle by a front matter rewrite operation.

COMPONENTS-TO-ADD is the list of components that have to be added to
COMPONENTS-IN-FILE to build the list of components that will need to be
handled during a front matter rewrite operation.

COMPONENTS-IN-TEMPLATE is the list of components in a front matter
template.  They are used to determine how the COMPONENTS-TO-ADD are
added to COMPONENTS-IN-FILE.

Example:
          file = (title signature)
      template = (title keywords date id signature)

The date line is missing from the file.  From the template, we find out
that it needs to be added *after* a keywords line.  Since we don't have
one in the file, we keep looking for a line to add it *after* and find a
title line.  Had we not found the title line in the file, we would have
searched for a line to insert it *before*.  We would have inserted the
date line before the signature line, for example.

This is repeated until all missing components are added."
  (let ((final-components (copy-sequence components-in-file)))
    (dolist (component components-to-add)
      (if-let* ((previous-components-in-template
                 (seq-take-while (lambda (x) (not (eq x component))) components-in-template))
                (first-previous-component-in-file
                 (seq-find (lambda (x) (memq x final-components)) (reverse previous-components-in-template))))
          ;; Insert after the existing element.
          (let ((sublist final-components))
            (while sublist
              (if (not (eq (car sublist) first-previous-component-in-file))
                  (setq sublist (cdr sublist))
                (push component (cdr sublist))
                (setq sublist nil))))
        (let* ((next-components-in-template
                (cdr (seq-drop-while (lambda (x) (not (eq x component))) components-in-template)))
               (first-next-component-in-file
                (seq-find (lambda (x) (memq x final-components)) next-components-in-template)))
          ;; Insert before the existing element.  The intention is to
          ;; modify final-components, but it does not work when push
          ;; is called on sublist on the first iteration of the loop.
          (if (eq (car final-components) first-next-component-in-file)
              (push component final-components)
            (let ((sublist final-components))
              (while sublist
                (if (not (eq (car sublist) first-next-component-in-file))
                    (setq sublist (cdr sublist))
                  (push component sublist)
                  (setq sublist nil))))))))
    final-components))

(defun denote-rewrite-front-matter (file title keywords signature date identifier file-type)
  "Rewrite front matter of note after `denote-rename-file'.
The FILE, TITLE, KEYWORDS, SIGNATURE, DATE, IDENTIFIER, and FILE-TYPE
are given by the renaming command and are used to construct new front
matter values if appropriate.

If `denote-rename-confirmations' contains `rewrite-front-matter',
prompt to confirm the rewriting of the front matter."
  (let* ((front-matter (denote--front-matter file-type))
         (file-content (with-current-buffer (find-file-noselect file) (buffer-string)))
         (components-in-template (denote--get-front-matter-components-order front-matter file-type))
         (components-in-file (denote--get-front-matter-components-order file-content file-type))
         (components-to-add '())
         (components-to-remove '())
         (components-to-modify '())
         (new-front-matter (denote--format-front-matter title date keywords identifier signature file-type))
         (old-and-new-front-matter-lines (denote--get-old-and-new-front-matter-lines file new-front-matter file-type)))
    ;; Build the lists of components to add, remove, modify.
    (dolist (component '(title keywords signature identifier date))
      ;; Ignore the component if it is not in the template.  It is not added, removed or modified.
      (when (memq component components-in-template)
        (let ((value (pcase component ('title title) ('keywords keywords) ('signature signature) ('date date) ('identifier identifier))))
          (cond ((and (not (memq component components-in-file))
                      (denote--component-has-value-p component value))
                 (push component components-to-add))
                ((and (memq component components-in-file)
                      ;; The component can still be marked for modification.
                      (not (memq component denote-front-matter-components-present-even-if-empty-value))
                      (not (denote--component-has-value-p component value)))
                 (push component components-to-remove))
                ((and (memq component components-in-file)
                      (not (string= (alist-get 'old (alist-get component old-and-new-front-matter-lines))
                                    (alist-get 'new (alist-get component old-and-new-front-matter-lines)))))
                 (push component components-to-modify))))))
    ;; There should be at least one component in the file and the template.
    (when (and (seq-intersection components-in-file components-in-template)
               (or components-to-add components-to-remove components-to-modify))
      (when-let* ((final-components (denote--get-final-components-for-rewrite
                                     components-in-file components-in-template components-to-add)))
        (with-current-buffer (find-file-noselect file)
          (when (or (not (memq 'rewrite-front-matter denote-rename-confirmations))
                    (y-or-n-p (denote--get-front-matter-rewrite-prompt
                               final-components
                               components-to-add components-to-remove components-to-modify
                               old-and-new-front-matter-lines)))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                ;; Position point at the beginning of the first front matter line
                (let ((first-component (car (seq-difference final-components components-to-add))))
                  (re-search-forward
                   (funcall (denote--get-component-key-regexp-function first-component) file-type) nil t 1)
                  (goto-char (line-beginning-position)))
                ;; Do the modifications
                (dolist (component final-components)
                  (let ((component-key-regexp-function (denote--get-component-key-regexp-function component))
                        (new-line (alist-get 'new (alist-get component old-and-new-front-matter-lines))))
                    (cond ((memq component components-to-remove)
                           (re-search-forward (funcall component-key-regexp-function file-type) nil t 1)
                           (delete-region (line-beginning-position) (line-beginning-position 2)))
                          ((memq component components-to-add)
                           (insert (concat new-line "\n")))
                          ((memq component components-to-modify)
                           (re-search-forward (funcall component-key-regexp-function file-type) nil t 1)
                           (goto-char (line-beginning-position))
                           (insert new-line)
                           (delete-region (point) (line-end-position))
                           (goto-char (line-beginning-position 2)))
                          (t
                           (goto-char (line-beginning-position 2))))))))))))))

;;;;; The renaming commands and their prompts

(defun denote--rename-dired-file-or-current-file-or-prompt ()
  "Return Dired file at point or the current file, else prompt for one.
Throw error if FILE is not regular, else return FILE."
  (or (dired-get-filename nil t)
      buffer-file-name
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
  "Prompt to rename file named OLD-NAME to NEW-NAME.
Return non-nil if the file should be renamed.

If `denote-rename-confirmations' does not contain
`modify-file-name', return t without prompting."
  (or (not (memq 'modify-file-name denote-rename-confirmations))
      (unless (string= (expand-file-name old-name) (expand-file-name new-name))
        (y-or-n-p
         (format "Rename %s to %s?"
                 (propertize (file-name-nondirectory old-name) 'face 'denote-faces-prompt-old-name)
                 (propertize (file-name-nondirectory new-name) 'face 'denote-faces-prompt-new-name))))))

(defun denote-add-front-matter-prompt (file)
  "Prompt to add a front-matter to FILE.
Return non-nil if a new front matter should be added.

If `denote-rename-confirmations' does not contain
`add-front-matter', return t without prompting."
  (or (not (memq 'add-front-matter denote-rename-confirmations))
      (y-or-n-p
       (format "Add new front matter to %s?"
               (propertize (file-name-nondirectory file) 'face 'denote-faces-prompt-new-name)))))

;; NOTE 2023-10-20: We do not need a user option for this, though it
;; can be useful to have it as a variable.
(defvar denote-rename-max-mini-window-height 0.33
  "How much to enlarge `max-mini-window-height' for renaming operations.")

(defun denote--generate-date-for-rename (file)
  "Generate a date for FILE.

Respect `denote-generate-identifier-automatically'."
  (if (or (eq denote-generate-identifier-automatically t)
          (eq denote-generate-identifier-automatically 'on-rename))
      (or (file-attribute-modification-time (file-attributes file))
          (current-time))
    nil))

(defun denote--rename-file (file title keywords signature date)
  "Rename FILE according to the other parameters.
Parameters TITLE, KEYWORDS, SIGNATURE and DATE are as described
in `denote-rename-file' and are assumed to be valid (TITLE and
SIGNATURE are strings, KEYWORDS is a list, etc.).

This function only does the work necessary to rename a file
according to its parameters.  In particular, it does not prompt
for anything.  It is meant to be combined with
`denote--rename-get-file-info-from-prompts-or-existing' to create
a renaming command.

Respect `denote-rename-confirmations', `denote-save-buffers' and
`denote-kill-buffers'."
  (let* ((initial-state (if (find-buffer-visiting file) 'visited 'not-visited))
         (file-type (denote-filetype-heuristics file))
         (keywords (denote-keywords-sort keywords))
         (directory (file-name-directory file))
         (extension (denote-get-file-extension file))
         (date (or date (denote--generate-date-for-rename file)))
         (old-id (or (denote-retrieve-filename-identifier file) ""))
         (id (denote-get-identifier date))
         (id (cond ((or (string-empty-p id) (string= old-id id))
                    id)
                   ((and (not (string-empty-p old-id)) (denote--file-has-backlinks-p file))
                    (user-error "The date cannot be modified because the file has backlinks"))
                   (t
                    (denote--find-first-unused-id id))))
         (date (if (string-empty-p id) nil (date-to-time id)))
         (new-name (denote-format-file-name directory id keywords title extension signature))
         (max-mini-window-height denote-rename-max-mini-window-height))
    (when (and (file-regular-p new-name)
               (not (string= (expand-file-name file) (expand-file-name new-name))))
      (user-error "The destination file `%s' already exists" new-name))
    ;; Modify file name, buffer name, or both
    (when (denote-rename-file-prompt file new-name)
      (denote-rename-file-and-buffer file new-name))
    ;; Handle front matter if new-name is of a supported type (rewrite or add front matter)
    (when (and (denote-file-has-supported-extension-p file)
               (denote-file-is-writable-and-supported-p new-name))
      (if (denote--file-has-front-matter-p new-name file-type)
          (denote-rewrite-front-matter new-name title keywords signature date id file-type)
        (when (denote-add-front-matter-prompt new-name)
          (denote-prepend-front-matter new-name title keywords signature date id file-type))))
    (when (and denote--used-ids (not (string-empty-p id)))
      (puthash id t denote--used-ids))
    (denote--handle-save-and-kill-buffer 'rename new-name initial-state)
    (setq denote-current-data
          (list
           (cons 'title title)
           (cons 'keywords keywords)
           (cons 'signature signature)
           (cons 'directory directory)
           (cons 'date date)
           (cons 'id id)
           (cons 'file-type file-type)
           (cons 'template "")))
    (run-hooks 'denote-after-rename-file-hook)
    new-name))

(defun denote--rename-get-file-info-from-prompts-or-existing (file)
  "Retrieve existing info from FILE and prompt according to `denote-prompts'.

It is meant to be combined with `denote--rename-file' to create
renaming commands."
  (let* ((file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name))
         (file-type (denote-filetype-heuristics file))
         (id (or (denote-retrieve-filename-identifier file) ""))
         (date (or (denote-valid-date-p id) (denote--generate-date-for-rename file)))
         (title (or (denote-retrieve-title-or-filename file file-type) ""))
         (keywords (denote-extract-keywords-from-path file))
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
                         (string-join keywords ","))))
        ('signature
         (setq signature (denote-signature-prompt
                          signature
                          (format "Rename `%s' with SIGNATURE (empty to remove)" file-in-prompt))))
        ('date
         (setq date (denote-valid-date-p (denote-date-prompt
                                          date
                                          (format "Rename `%s' with DATE" file-in-prompt)))))))
    (list title keywords signature date)))

;;;###autoload
(defun denote-rename-file (file title keywords signature date)
  "Rename file and update existing front matter if appropriate.

Always rename the file where it is located in the file system:
never move it to another directory.

If in Dired, consider FILE to be the one at point, else the
current file, else prompt with minibuffer completion for one.
When called from Lisp, FILE is a file system path represented as
a string.

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

In interactive use, if there is no entry for a file name
component in `denote-prompts', keep it as-is.

When called from Lisp, the special symbol `keep-current' can be
used for the TITLE, KEYWORDS, SIGNATURE and DATE parameters to
keep them as-is.

[ NOTE: Please check with your minibuffer user interface how to
  provide an empty input.  The Emacs default setup accepts the
  empty minibuffer contents as they are, though popular packages
  like `vertico' use the first available completion candidate
  instead.  For `vertico', the user must either move one up to
  select the prompt and then type RET there with empty contents,
  or use the command `vertico-exit-input' with empty contents.
  That Vertico command is bound to M-RET as of this writing on
  2024-02-13 08:08 +0200. ]

As a final step, ask for confirmation, showing the difference
between old and new file names.  Do not ask for confirmation if
the user option `denote-rename-confirmations' does not contain
the symbol `modify-file-name'.

If FILE has front matter for TITLE and KEYWORDS, ask to rewrite
their values in order to reflect the new input, unless
`denote-rename-confirmations' lacks `rewrite-front-matter'.  When
the `denote-save-buffers' is nil (the default), do not save the
underlying buffer, thus giving the user the option to
double-check the result, such as by invoking the command
`diff-buffer-with-file'.  The rewrite of the TITLE and KEYWORDS
in the front matter should not affect the rest of the front
matter.

If the file does not have front matter but is among the supported file
types (per the user option `denote-file-type'), add front matter to the
top of it and leave the buffer unsaved for further inspection.  Save the
buffer if `denote-save-buffers' is non-nil.

When `denote-kill-buffers' is t or `on-rename', kill the buffer
if it was not already being visited before the rename operation.

For the front matter of each file type, refer to the variables:

- `denote-org-front-matter'
- `denote-text-front-matter'
- `denote-toml-front-matter'
- `denote-yaml-front-matter'

Construct the file name in accordance with the user option
`denote-file-name-components-order'.

Run the `denote-after-rename-file-hook' after renaming FILE.

This command is intended to (i) rename Denote files, (ii) convert
existing supported file types to Denote notes, and (ii) rename
non-note files (e.g. PDF) that can benefit from Denote's
file-naming scheme.

For a version of this command that works with multiple files
one-by-one, use `denote-dired-rename-files'."
  (interactive
   (let* ((file (denote--rename-dired-file-or-current-file-or-prompt)))
     (append (list file) (denote--rename-get-file-info-from-prompts-or-existing file))))
  (let* ((file-type (denote-filetype-heuristics file))
         (title (if (eq title 'keep-current)
                    (or (denote-retrieve-title-or-filename file file-type) "")
                  title))
         (keywords (if (eq keywords 'keep-current)
                       (denote-extract-keywords-from-path file)
                     keywords))
         (signature (if (eq signature 'keep-current)
                        (or (denote-retrieve-filename-signature file) "")
                      signature))
         (date (if (eq date 'keep-current)
                   (denote-retrieve-filename-identifier file)
                 date))
         ;; Make the data valid
         (date (denote-valid-date-p date))
         (new-name (denote--rename-file file title keywords signature date)))
    (denote-update-dired-buffers)
    new-name))

(defun denote-rename-file-title ()
  "Convenience command to change the title of a file.
Like `denote-rename-file', but prompts only for the title.

Add or remove a title in one go.  Do this by prepopulating the
minibuffer prompt with the existing title.  The user can then modify it
accordingly.  An empty input means to remove the title altogether.

Please check the documentation of `denote-rename-file' with regard to
how a completion User Interface may accept an empty input."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(title)))
    (call-interactively #'denote-rename-file)))

(defun denote-rename-file-keywords ()
  "Convenience command to change the keywords of a file.
Like `denote-rename-file', but prompts only for keywords.

Add or remove keywords in one go.  Do this by prepopulating the
minibuffer prompt with the existing keywords.  The user can then insert
the `crm-separator' (normally a comma), to write new keywords or edit
what is in the prompt to rewrite them accordingly.  An empty input means
to remove all keywords.

Please check the documentation of `denote-rename-file' with regard to
how a completion User Interface may accept an empty input."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(keywords)))
    (call-interactively #'denote-rename-file)))

(defun denote-rename-file-date ()
  "Convenience command to change the date of a file.
Like `denote-rename-file', but prompts only for the date.

Modify a date in one go."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(date)))
    (call-interactively #'denote-rename-file)))

(define-obsolete-function-alias 'denote-keywords-add 'denote-rename-file-keywords "3.0.0")
(define-obsolete-function-alias 'denote-rename-add-keywords 'denote-rename-file-keywords "3.0.0")
(define-obsolete-function-alias 'denote-keywords-remove 'denote-rename-file-keywords "3.0.0")
(define-obsolete-function-alias 'denote-rename-rename-keywords 'denote-rename-file-keywords "3.0.0")

(defun denote-rename-file-signature ()
  "Convenience command to change the signature of a file.
Like `denote-rename-file', but prompts only for the signature.

Add or remove a signature in one go.  Do this by prepopulating the
minibuffer prompt with the existing signature.  The user can then modify
it accordingly.  An empty input means to remove the signature
altogether.

Please check the documentation of `denote-rename-file' with regard to
how a completion User Interface may accept an empty input."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(signature)))
    (call-interactively #'denote-rename-file)))

(define-obsolete-function-alias 'denote-add-signature 'denote-rename-file-signature "3.0.0")
(define-obsolete-function-alias 'denote-remove-signature 'denote-rename-file-signature "3.0.0")

;;;###autoload
(defun denote-dired-rename-files ()
  "Rename Dired marked files same way as `denote-rename-file'.
Rename each file in sequence, making all the relevant prompts.
Unlike `denote-rename-file', do not prompt for confirmation of
the changes made to the file: perform them outright (same as
setting `denote-rename-confirmations' to a nil value)."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote--used-ids (denote--get-all-used-ids))
        (denote-rename-confirmations nil))
    (if-let* ((marks (dired-get-marked-files)))
        (progn
          (dolist (file marks)
            (pcase-let ((`(,title ,keywords ,signature ,date)
                         (denote--rename-get-file-info-from-prompts-or-existing file)))
              (denote--rename-file file title keywords signature date)))
          (denote-update-dired-buffers))
      (user-error "No marked files; aborting"))))

(defalias 'denote-dired-rename-marked-files 'denote-dired-rename-files
  "Alias for `denote-dired-rename-files'.")

(defun denote-keywords--combine (combination-type user-input-keywords keywords)
  "COMBINATION-TYPE is either `:add', `:remove' or `:replace'.

USER-INPUT-KEYWORDS are new keywords collected from the end-user.
KEYWORDS are the existing keywords for the underlying file.

This function is an internal implementation function."
  (cond
   ((eq combination-type :add)
    (seq-union keywords user-input-keywords))
   ((eq combination-type :replace)
    user-input-keywords)
   ((eq combination-type :remove)
    (seq-difference keywords user-input-keywords))
   (t
    (error "Unknown operation in denote-keywords--combine: %s"
           combination-type))))

(defun denote-dired-rename-marked-files--change-keywords (combination-type keywords-prompt)
  "COMBINATION-TYPE is either `:add', `:remove' or `:replace'.

KEYWORDS-PROMPT is the prompt we show the end-user, when taking keywords
as input.

This function is an internal implementation function."
  (if-let* ((marks (dired-get-marked-files)))
      (let ((denote-prompts '())
            (denote-rename-confirmations nil)
            (user-input-keywords (denote-keywords-prompt keywords-prompt))
            (denote--used-ids (denote--get-all-used-ids)))
        (dolist (file marks)
          (pcase-let* ((`(,title ,keywords ,signature ,date)
                        (denote--rename-get-file-info-from-prompts-or-existing file))
                       (new-keywords (denote-keywords-sort (denote-keywords--combine combination-type user-input-keywords keywords))))
            (denote--rename-file file title new-keywords signature date)))
        (denote-update-dired-buffers))
    (user-error "No marked files; aborting")))

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

- add or rewrite existing front matter to the underlying file, if it is
  recognized as a Denote note (per the user option `denote-file-type'),
  such that it includes the new keywords.

Construct the file name in accordance with the user option
`denote-file-name-components-order'.

Run the `denote-after-rename-file-hook' after renaming is done.

Also see the specialized commands to only add or remove keywords:

- `denote-dired-rename-marked-files-add-keywords'.
- `denote-dired-rename-marked-files-remove-keywords'."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (denote-dired-rename-marked-files--change-keywords
   :replace "Rename marked files with KEYWORDS, overwriting existing (empty to ignore/remove)"))

;;;###autoload
(defun denote-dired-rename-marked-files-add-keywords ()
  "Like `denote-dired-rename-marked-files-with-keywords' to only add keywords."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (denote-dired-rename-marked-files--change-keywords
   :add "Add KEYWORDS to marked files"))

;;;###autoload
(defun denote-dired-rename-marked-files-remove-keywords ()
  "Like `denote-dired-rename-marked-files-with-keywords' to only remove keywords."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (denote-dired-rename-marked-files--change-keywords
   :remove "Remove KEYWORDS from marked files"))

;;;###autoload
(defun denote-rename-file-using-front-matter (file)
  "Rename FILE using its front matter as input.
When called interactively, FILE is the variable `buffer-file-name' or
the Dired file at point, which is subsequently inspected for the
requisite front matter.  It is thus implied that the FILE has a file
type that is supported by Denote, per the user option `denote-file-type'.

The values of `denote-rename-confirmations',
`denote-save-buffers' and `denote-kill-buffers' are respected.

Only the front matter lines that appear in the front matter template (as
defined in `denote-file-types') will be handled.

To change the identifier (date) of the note with this command, the
identifier line (if present) of the front matter must be modified.
Modifying the date line has no effect.

While this command generally does not modify the front matter, there are
exceptions.  The value of the `date' line will follow that of the
`identifier' line.  If they are both in the front matter template and
the `date' line is missing, it will be added again.  Similarly, if they
are both in the front matter template and the `date' line is present and
the `identifier' line has been removed, the `date' line will be removed
as well.  Also, if the keywords are out of order and
`denote-sort-keywords' is non-nil, they will be sorted.  There will be a
prompt for this if `denote-rename-confirmations' contains
`rewrite-front-matter'.

Construct the file name in accordance with the user option
`denote-file-name-components-order'."
  (interactive (list (or (dired-get-filename nil t) buffer-file-name)))
  (unless (denote-file-is-writable-and-supported-p file)
    (user-error "The file is not writable or does not have a supported file extension"))
  (let ((file-type (denote-filetype-heuristics file)))
    (unless (denote--file-has-front-matter-p file file-type)
      (user-error "The file does not appear to have a front matter"))
    (let* ((front-matter-template (denote--front-matter file-type))
           (components-in-template (denote--get-front-matter-components-order front-matter-template file-type))
           (title (if (memq 'title components-in-template)
                      (or (denote-retrieve-front-matter-title-value file file-type) "")
                    (or (denote-retrieve-filename-title file) "")))
           (keywords (if (memq 'keywords components-in-template)
                         (denote-retrieve-front-matter-keywords-value file file-type)
                       (denote-retrieve-filename-keywords-as-list file)))
           (signature (if (memq 'signature components-in-template)
                          (or (denote-retrieve-front-matter-signature-value file file-type) "")
                        (or (denote-retrieve-filename-signature file) "")))
           ;; We need to use the identifier because the date line may
           ;; not contain all the information.  For example,
           ;; "2024-01-01" does not have the time of the note.
           (date (if (memq 'identifier components-in-template)
                     (when-let* ((id-value (denote-retrieve-front-matter-identifier-value file file-type)))
                       (denote-valid-date-p id-value))
                   (denote-valid-date-p (or (denote-retrieve-filename-identifier file) "")))))
      (denote--rename-file file title keywords signature date)
      (denote-update-dired-buffers))))

;;;###autoload
(defun denote-dired-rename-marked-files-using-front-matter ()
  "Call `denote-rename-file-using-front-matter' over the Dired marked files.
Refer to the documentation of that command for the technicalities.

Marked files must count as notes for the purposes of Denote, which means
that they at least have an identifier in their file name and use a
supported file type, per the user option `denote-file-type'.  Files that
do not meet this criterion are ignored because Denote cannot know if
they have front matter and what that may be."
  (interactive nil dired-mode)
  (if-let* ((marks (seq-filter
                    (lambda (m)
                      (and (file-regular-p m)
                           (denote-file-is-writable-and-supported-p m)
                           (denote-file-has-identifier-p m)))
                    (dired-get-marked-files))))
      (let ((denote--used-ids (denote--get-all-used-ids)))
        (dolist (file marks)
          (denote-rename-file-using-front-matter file))
        (denote-update-dired-buffers))
    (user-error "No marked Denote files; aborting")))

;;;;; Creation of front matter

(make-obsolete 'denote-add-front-matter nil "Use `denote-rename-file' or related. Starting with version 4.0.0.")

;;;###autoload
(defun denote-change-file-type-and-front-matter (file new-file-type)
  "Change file type of FILE and add an appropriate front matter.

If in Dired, consider FILE to be the one at point, else the
current file, else prompt with minibuffer completion for one.

Add a front matter in the format of the NEW-FILE-TYPE at the
beginning of the file.

Retrieve the title of FILE from a line starting with a title
field in its front matter, depending on the previous file
type (e.g.  #+title for Org).  The same process applies for
keywords.

As a final step, ask for confirmation, showing the difference
between old and new file names.

Important note: No attempt is made to modify any other elements
of the file.  This needs to be done manually.

Construct the file name in accordance with the user option
`denote-file-name-components-order'."
  (interactive
   (list
    (denote--rename-dired-file-or-current-file-or-prompt)
    (denote--valid-file-type (or (denote-file-type-prompt) denote-file-type))))
  (let* ((initial-state (if (find-buffer-visiting file) 'visited 'not-visited))
         (dir (file-name-directory file))
         (old-file-type (denote-filetype-heuristics file))
         (id (or (denote-retrieve-filename-identifier file) ""))
         (date (if (string-empty-p id) nil (date-to-time id)))
         (title (or (denote-retrieve-title-or-filename file old-file-type) ""))
         (keywords (denote-retrieve-front-matter-keywords-value file old-file-type))
         (signature (or (denote-retrieve-filename-signature file) ""))
         (new-extension (denote--file-extension new-file-type))
         (new-name (denote-format-file-name dir id keywords title new-extension signature))
         (max-mini-window-height denote-rename-max-mini-window-height))
    (when (denote-rename-file-prompt file new-name)
      (denote-rename-file-and-buffer file new-name)
      (denote-update-dired-buffers)
      (when (and (denote-file-is-writable-and-supported-p new-name)
                 (denote-add-front-matter-prompt new-name))
        (denote-prepend-front-matter new-name title keywords signature date id new-file-type)
        (denote--handle-save-and-kill-buffer 'rename new-name initial-state)))))

;;;; The Denote faces

(defgroup denote-faces ()
  "Faces for Denote."
  :group 'denote)

(defface denote-faces-link '((t :inherit link))
  "Face used to style Denote links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(defface denote-faces-query-link '((t :inherit link-visited))
  "Face used to style Denote query links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "4.0.0"))

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

;; The following matchers must obey the doc of `font-lock-keywords':
;;   - Have one parameter, the limit of the search
;;   - Set match-data (and restore it on failure)
;;   - Move point after the match (or restore it on failure).
;;   - Return t on success and nil on failure. re-search-forward returns (point) on success. It may be better to do the same.

(defun denote-faces-dired-file-name-matcher (limit)
  "Find the file name in a Dired line, not looking beyond LIMIT."
  (let ((initial-match-data (match-data))
        (initial-point (point))
        (line-found nil))
    ;; Find the next non empty line that contains a Dired file name
    (while (and (not line-found)
                (re-search-forward "^.+$" limit t))
      ;; dired-move-to-filename moves the point even if it returns nil
      (let ((saved-point (point)))
        (if (and (dired-move-to-filename)
                 (save-match-data
                   (denote-file-has-denoted-filename-p (buffer-substring (point) (line-end-position)))))
            (setq line-found t)
          (goto-char saved-point))))
    (if line-found
        (let ((beginning-point (point)))
          (goto-char (match-end 0))
          (set-match-data (list beginning-point (match-end 0)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-faces-directory-matcher (limit)
  "Match the directory in a Dired line, not looking beyond LIMIT."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (re-search-forward "\\(?1:.*/\\)[^/]*$" limit t)
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-faces-signature-matcher (limit)
  "Match the signature in a Dired line, not looking beyond LIMIT."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "==\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "==\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-faces-identifier-matcher (limit)
  "Match a general identifier in a Dired line, not looking beyond LIMIT."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "@@\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "@@\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-faces-title-matcher (limit)
  "Match the title in a Dired line, not looking beyond LIMIT."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "--\\(?1:[^/]*?\\)\\(@@\\|__\\|==\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "--\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-faces-keywords-matcher (limit)
  "Match the keywords in a Dired line, not looking beyond LIMIT."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "__\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "__\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defconst denote-faces-matchers
  `((denote-faces-directory-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-subdirectory nil t))
    ;; Identifier with format 00000000T000000
    ("\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9]\\{2\\}\\)\\(?3:[0-9]\\{2\\}\\)\\(?7:T\\)\\(?4:[0-9]\\{2\\}\\)\\(?5:[0-9]\\{2\\}\\)\\(?6:[0-9]\\{2\\}\\)"
     (goto-char (match-beginning 0)) ; pre-form, executed before looking for the first identifier
     (goto-char (match-end 0))       ; post-form, executed after all matches (identifiers here) are found
     (1 'denote-faces-year nil t)
     (2 'denote-faces-month nil t)
     (3 'denote-faces-day nil t)
     (4 'denote-faces-hour nil t)
     (5 'denote-faces-minute nil t)
     (6 'denote-faces-second nil t)
     (7 'denote-faces-delimiter nil t))
    ;; Identifier with general format (not yet possible)
    (denote-faces-identifier-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-date nil t))
    ;; Title
    (denote-faces-title-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-title nil t))
    ;; Keywords
    (denote-faces-keywords-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-keywords nil t))
    ;; Signature
    (denote-faces-signature-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-signature nil t))
    ;; Delimiters
    ("\\(@@\\|--\\|__\\|==\\)"
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-delimiter nil t))
    ;; Extension
    ("\\..*$"
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-extension nil t)))
  "Matchers for fontification of file names.")

(defconst denote-faces-file-name-keywords-for-dired
  `((denote-faces-dired-file-name-matcher ,@denote-faces-matchers))
  "Keywords for fontification of file names.")

(make-obsolete-variable 'denote-faces-file-name-keywords-for-backlinks nil "4.0.0")

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
    (font-lock-add-keywords nil denote-faces-file-name-keywords-for-dired t)))

(defun denote-dired-remove-font-lock (&rest _)
  "Remove `denote-faces-file-name-keywords' from font lock keywords."
  ;; See NOTE in `denote-dired-add-font-lock'.
  (when (derived-mode-p 'dired-mode)
    (font-lock-remove-keywords nil denote-faces-file-name-keywords-for-dired)))

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
  (when-let* ((dirs (denote-dired--modes-dirs-as-dirs))
              ;; Also include subdirs
              ((or (member (file-truename default-directory) dirs)
                   (and denote-dired-directories-include-subdirectories
                        (seq-some
                         (lambda (dir)
                           (string-prefix-p dir (file-truename default-directory)))
                         dirs)))))
    (denote-dired-mode 1)))

;;;; The linking facility

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
  (concat "\\[\\[" "denote:"
          "\\(?1:[^][]*?\\)"
          "\\(?:::.*\\)?" "]"
          "\\[" "\\(?2:" ".*?" "\\)" "]]")
  "Regexp to match an Org link in its context.
The format of such links is `denote-org-link-format'.")

(defvar denote-md-link-in-context-regexp
  (concat "\\[" "\\(?2:" ".*?" "\\)" "]"
          "(denote:"  "\\(?1:[^][]*?\\)" ")")
  "Regexp to match a Markdown link in its context.
The format of such links is `denote-md-link-format'.")

(defvar denote-id-only-link-in-context-regexp
  (concat "\\[\\[" "denote:"  "\\(?1:[^][]*?\\)" "]]")
  "Regexp to match an identifier-only link in its context.
The format of such links is `denote-id-only-link-format'."  )

(defun denote-format-link (file description file-type id-only &optional include-date)
  "Prepare link to FILE using DESCRIPTION.

FILE-TYPE and ID-ONLY are used to get the format of the link.
See the `:link' property of `denote-file-types'.

With optional INCLUDE-DATE, convert the identifier using
`denote--id-to-date' and append it to DESCRIPTION."
  (let* ((identifier (denote-retrieve-filename-identifier file))
         (desc (if include-date
                   (format "%s (%s)" description (denote--id-to-date identifier))
                 description)))
    (format
     (cond
      ((or id-only (null description) (string-empty-p description))
       denote-id-only-link-format)
      ;; NOTE 2024-05-20: If there is no file type, we want to use the
      ;; Org format because it is still a usable link with the help of
      ;; the command `org-open-at-point-global'.
      ((null file-type)
       (denote--link-format 'org))
      (t
       (denote--link-format file-type)))
     identifier
     desc)))

(defun denote-link-description-with-signature-and-title (file)
  "Return link description for FILE.

- If the region is active, use it as the description.

- If FILE has a signature, then format the description as a sequence of
  the signature text and the title with two spaces between them.

- If FILE does not have a signature, then use its title as the
  description.

This is useful as the value of the user option
`denote-link-description-function'."
  (let* ((file-type (denote-filetype-heuristics file))
         (signature (denote-retrieve-filename-signature file))
         (title (denote-retrieve-title-or-filename file file-type))
         (region-text (denote--get-active-region-content)))
    (cond
     (region-text region-text)
     ((and signature title) (format "%s  %s" signature title))
     (title (format "%s" title))
     (signature (format "%s" signature))
     (t ""))))

(defun denote--get-active-region-content ()
  "Return the text of the active region, else nil."
  (when-let* (((region-active-p))
              (beg (region-beginning))
              (end (region-end)))
    (string-trim (buffer-substring-no-properties beg end))))

(defun denote--delete-active-region-content ()
  "Delete the content of the active region, if any."
  (when-let* (((region-active-p))
              (beg (region-beginning))
              (end (region-end)))
    (delete-region beg end)))

(defun denote-get-link-description (file)
  "Return a link description for FILE.

If `denote-link-description-format' is a function, call it with FILE as
an argument.  The function should return a string, representing the link
description.

If the user option `denote-link-description-format' is a string, parse
it to substitute any format specifiers therein with their respective
values (see the documentation of that user option).  If the region is
active, use it as the description."
  (cond
   ((functionp denote-link-description-format)
    (funcall denote-link-description-format file))
   ((stringp denote-link-description-format)
    (if-let* ((region (denote--get-active-region-content)))
        region
      (let ((type (denote-filetype-heuristics file)))
        (string-trim
         (format-spec denote-link-description-format
                      (list (cons ?t (cond
                                      ((denote-retrieve-front-matter-title-value file (denote-filetype-heuristics file)))
                                      ((denote-retrieve-filename-title file))
                                      (t  "")))
                            (cons ?T (or (denote-retrieve-filename-title file) ""))
                            (cons ?i (or (denote-retrieve-filename-identifier file) ""))
                            ;; TODO 2025-04-03: Maybe we can have something like `denote-date-format' here,
                            ;; but I think we are okay with a hardcoded value.
                            (cons ?I (or (when-let* ((id (denote-retrieve-filename-identifier file))
                                                     (_ (denote-valid-date-p id)))
                                           (format-time-string "%A, %e %B %Y" (date-to-time (denote--id-to-date id))))
                                         ""))
                            (cons ?D (cond
                                      ((denote-retrieve-front-matter-title-value file type))
                                      ((denote-retrieve-filename-title file))
                                      ((when-let* ((id (denote-retrieve-filename-identifier file)))
                                         (if (denote-valid-date-p id)
                                             (format-time-string "%A, %e %B %Y" (date-to-time (denote--id-to-date id)))
                                           id)))
                                      (t  "")))
                            (cons ?d (or (denote-retrieve-filename-identifier file) ""))
                            (cons ?s (or (denote-retrieve-filename-signature file) ""))
                            (cons ?k (or (denote-retrieve-filename-keywords file) ""))
                            (cons ?% "%"))
                      'delete)))))
   (t
    (error "The `denote-link-description-format' must be a function or string"))))

(define-obsolete-function-alias
  'denote--link-get-description
  'denote-get-link-description
  "4.0.0")

;;;###autoload
(defun denote-link (file file-type description &optional id-only)
  "Create link to FILE note in variable `denote-directory' with DESCRIPTION.

When called interactively, prompt for FILE using completion.  In this
case, derive FILE-TYPE from the current buffer.  FILE-TYPE is used to
determine the format of the link.

Return the DESCRIPTION of the link in the format specified by
`denote-link-description-format'.  The default is to return the text of
the active region or the title of the note (plus the signature if
present).

With optional ID-ONLY as a non-nil argument, such as with a universal
prefix (\\[universal-argument]), insert links with just the identifier
and no further description.  In this case, the link format is always
[[denote:IDENTIFIER]].

If the DESCRIPTION is empty, format the link the same as with ID-ONLY.

When called from Lisp, FILE is a string representing a full file system
path.  FILE-TYPE is a symbol as described in the user option
`denote-file-type'.  DESCRIPTION is a string.  Whether the caller treats
the active region specially, is up to it."
  (interactive
   (let* ((file (denote-file-prompt nil "Link to FILE"))
          (file-type (denote-filetype-heuristics buffer-file-name))
          (description (when (file-exists-p file)
                         (denote-get-link-description file))))
     (list file file-type description current-prefix-arg)))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (unless (file-exists-p file)
    (user-error "The linked file does not exist"))
  (denote--delete-active-region-content)
  (insert (denote-format-link file description file-type id-only)))

(defalias 'denote-insert-link 'denote-link
  "Alias for `denote-link' command.")

(make-obsolete 'denote-link-with-signature nil " 4.0.0: Use the `denote-link-description-format'.")

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
        (when (string= i (denote-retrieve-filename-identifier file))
          (push file found-files))))
    found-files))

(defvar denote-link-find-file-history nil
  "History for `denote-find-link'.")

(defalias 'denote-link--find-file-history 'denote-link-find-file-history
  "Compatibility alias for `denote-link-find-file-history'.")

(defun denote-select-linked-file-prompt (files)
  "Prompt for linked file among FILES."
  (let ((file-names (mapcar #'denote-get-file-name-relative-to-denote-directory files)))
    (completing-read
     "Find linked file: "
     (denote--completion-table 'file file-names)
     nil t nil 'denote-link-find-file-history)))

(define-obsolete-function-alias
  'denote-link--find-file-prompt
  'denote-select-linked-file-prompt
  "3.0.0")

(defun denote-link-return-links (&optional file)
  "Return list of links in current or optional FILE.
Also see `denote-link-return-backlinks'."
  (when-let* ((current-file (or file (buffer-file-name)))
              ((denote-file-has-supported-extension-p current-file))
              (file-type (denote-filetype-heuristics current-file))
              (regexp (denote--link-in-context-regexp file-type))
              (files (denote-directory-files))
              (file-identifiers
               (with-temp-buffer
                 (insert-file-contents current-file)
                 (denote-link--collect-identifiers regexp)))
              (file-identifiers-hash-table (make-hash-table :test 'equal)))
    (dolist (id file-identifiers)
      (puthash id t file-identifiers-hash-table))
    (let ((found-files))
      (dolist (file files)
        (when (gethash (denote-retrieve-filename-identifier file) file-identifiers-hash-table)
          (push file found-files)))
      found-files)))

(defalias 'denote-link-return-forelinks 'denote-link-return-links
  "Alias for `denote-link-return-links'.")

;;;###autoload
(defun denote-find-link ()
  "Use minibuffer completion to visit linked file.
Also see `denote-find-backlink'."
  (declare (interactive-only t))
  (interactive)
  (find-file
   (concat
    (denote-directory)
    (denote-select-linked-file-prompt
     (or (denote-link-return-links)
         (user-error "No links found"))))))

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
file.  Though see `denote-save-buffer-after-creation'."
  (interactive "P")
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let* ((type (denote-filetype-heuristics (buffer-file-name)))
         (path (denote--command-with-features #'denote nil nil :save :in-background))
         (description (denote-get-link-description path)))
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
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let* ((type (denote-filetype-heuristics (buffer-file-name)))
         (path (denote--command-with-features command nil nil :save :in-background))
         (description (denote-get-link-description path)))
    (denote-link path type description id-only)))

;;;###autoload
(defun denote-link-or-create (target &optional id-only)
  "Use `denote-link' on TARGET file, creating it if necessary.

If TARGET file does not exist, call `denote-link-after-creating' which
runs the `denote' command interactively to create the file.  The
established link will then be targeting that new file.  In that case,
use the last input at the file prompt as the default value of the title
prompt.

With optional ID-ONLY as a prefix argument create a link that
consists of just the identifier.  Else try to also include the
file's title.  This has the same meaning as in `denote-link'."
  (interactive
   (let* ((target (denote-file-prompt nil "Select file (RET on no match to create it)" :no-require-match)))
     (unless (and target (file-exists-p target))
       (setq target (denote--command-with-features #'denote :use-file-prompt-as-def-title :ignore-region :save :in-background)))
     (list target current-prefix-arg)))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (denote-link target
               (denote-filetype-heuristics (buffer-file-name))
               (denote-get-link-description target)
               id-only))

(defalias 'denote-link-to-existing-or-new-note 'denote-link-or-create
  "Alias for `denote-link-or-create' command.")

;;;;; Links' buffer (query links and backlinks using `denote-query-mode')

(define-obsolete-function-alias
  'denote-backlinks-mode
  'denote-query-mode
  "4.0.0")

(declare-function outline-cycle "outline" (&optional event))
(declare-function outline-cycle-buffer "outline" (&optional level))
(declare-function outline-next-heading "outline" ())
(declare-function outline-previous-heading "outline" ())

(defvar denote-query-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'outline-cycle-buffer)
    (define-key map "f" #'denote-query-focus-last-search)
    (define-key map "k" #'outline-previous-heading)
    (define-key map "j" #'outline-next-heading)
    (define-key map "o" #'delete-other-windows)
    (define-key map "s" #'denote-grep)
    (define-key map "v" #'outline-cycle)
    (define-key map "x" #'denote-query-exclude-files)
    (define-key map "i" #'denote-query-only-include-files)
    (define-key map "l" #'recenter-current-error)
    (define-key map "X" #'denote-query-exclude-files-with-keywords)
    (define-key map "I" #'denote-query-only-include-files-with-keywords)
    (define-key map "G" #'denote-query-clear-all-filters)
    map)
  "Keymap for `denote-query-mode' buffers.")

(define-derived-mode denote-query-mode xref--xref-buffer-mode "Denote Query"
  "Major mode for queries found in the variable `denote-directory'.
This is used by the commands `denote-backlinks', `denote-grep',
`denote-query-contents-link', among others."
  :interactive nil
  (setq-local outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode 1))

(make-obsolete 'denote-link--backlink-find-file nil "4.0.0")
(make-obsolete 'denote-link--display-buffer nil "4.0.0")
(make-obsolete 'denote-backlinks-mode-next nil "4.0.0")
(make-obsolete 'denote-backlinks-mode-previous nil "4.0.0")
(make-obsolete 'denote-backlinks-toggle-context nil "4.0.0")
(make-obsolete-variable 'denote-backlinks-mode-map nil "4.0.0")

(define-obsolete-function-alias
  'denote-link--prepare-backlinks
  'denote-make-links-buffer
  "4.0.0")

(make-obsolete-variable 'denote-backlinks-show-context nil "4.0.0")

(define-obsolete-variable-alias
  'denote-link-backlinks-display-buffer-action
  'denote-backlinks-display-buffer-action
  "3.1.0")

(defgroup denote-query ()
  "Integration between Denote and Xref for grep/query/backlink buffers."
  :group 'denote)

(defcustom denote-backlinks-display-buffer-action
  '((display-buffer-reuse-mode-window display-buffer-below-selected)
    (mode . denote-query-mode)
    (window-height . fit-window-to-buffer))
  "The action used to display the current file's backlinks buffer.

The value has the form (FUNCTION . ALIST), where FUNCTION is
either an \"action function\", a list thereof, or possibly an
empty list.  ALIST is a list of \"action alist\" which may be
omitted (or be empty).

Sample configuration to display the buffer in a side window on
the left of the Emacs frame:

    (setq denote-backlinks-display-buffer-action
          (quote ((display-buffer-reuse-window display-buffer-in-side-window)
                  (side . left)
                  (slot . 99)
                  (window-width . 0.3)
                  (dedicated . t)
                  (preserve-size . (t . t)))))

See Info node `(elisp) Displaying Buffers' for more details
and/or the documentation string of `display-buffer'."
  :risky t
  :type `(choice
          (alist :key-type
                 (choice :tag "Condition"
                         regexp
                         (function :tag "Matcher function"))
                 :value-type ,display-buffer--action-custom-type)
          (function :tag "Custom function to return an action alist"))
  :package-version '(denote . "3.1.0")
  :group 'denote-query)

(defcustom denote-query-links-display-buffer-action
  '((display-buffer-reuse-mode-window display-buffer-below-selected)
    (mode . (denote-query-mode dired))
    (window-height . 0.3)
    (preserve-size . (t . t)))
  "The action used to display query links.
This is the same as `denote-backlinks-display-buffer-action'.  Refer to
its documentation for the technicalities."
  :risky t
  :type `(choice
          (alist :key-type
                 (choice :tag "Condition"
                         regexp
                         (function :tag "Matcher function"))
                 :value-type ,display-buffer--action-custom-type)
          (function :tag "Custom function to return an action alist"))
  :package-version '(denote . "4.0.0")
  :group 'denote-query)

(defcustom denote-query-format-heading-function #'identity
  "Function used to construct headings for files matched by a query.

It is called with a single argument, the path to the note file, and it
should always return a string."
  :package-version '(denote . "4.0.0")
  :link '(info-link "(denote) Use denote-grep to search inside files")
  :group 'denote-query
  :type 'function)

(defcustom denote-query-untitled-string "[Untitled]"
  "String to use as heading for untitled notes in links' buffer.

Used only by `denote-query-extract-title'."
  :package-version '(denote . "4.0.0")
  :link '(info-link "(denote) Use denote-grep to search inside files")
  :group 'denote-query
  :type 'string)

(defun denote-query-extract-title (file)
  "Extract note title from FILE front matter.

When no title is found, return title found in FILE name.

When that doesn't work, return `denote-grep-untitled-string'.

Intended to be used as `denote-query-format-heading-function'."
  (if-let* ((type (denote-filetype-heuristics file))
            (title (denote-retrieve-title-or-filename file type))
            (_ (not (string-blank-p title))))
      title
    denote-query-untitled-string))

;; NOTE 2025-03-24: The `&rest' is there because we used to have an
;; extra SHOW-CONTEXT parameter.  This way we do not break anybody's
;; code, even if we slightly modify the behaviour.
(defun denote-make-links-buffer (query &optional files buffer-name display-buffer-action &rest _)
  "Create links' buffer called BUFFER-NAME for QUERY.

Optional FILES can be a list of files to search for.  It can also be a
regexp, which limits the files accordingly per `denote-directory-files'.

Optional DISPLAY-BUFFER-ACTION is a `display-buffer' action and
concomitant alist, such as `denote-backlinks-display-buffer-action'."
  (let* ((inhibit-read-only t)
         (file buffer-file-name)
         (buffer (or buffer-name (format-message "Denote query for `%s'" query)))
         ;; We retrieve results in absolute form and change the
         ;; absolute path to a relative path below. We could add a
         ;; suitable function and the results would be automatically
         ;; in relative form, but eventually notes may not be all
         ;; under a common directory (or project).
         (xref-alist (denote-retrieve-xref-alist query files))
         (dir (denote-directory)))
    (unless xref-alist
      (error "No matches for query `%s'" query))
    ;; Update internal variables
    (setq denote-query--last-files nil)
    (setq denote-query--last-query query)
    (dolist (x xref-alist)
      (let* ((file-xref (car x))
             (file
              ;; NOTE: Unfortunately, the car of the xref construct is
              ;; not reliable; sometimes it's absolute, sometimes it
              ;; is not
              (if (file-name-absolute-p file-xref)
                  file-xref
                (xref-location-group
                 (xref-match-item-location (car (last x)))))))
        ;; Add to current set of files
        (push file denote-query--last-files)
        ;; Format heading
        (setf (car x) (funcall denote-query-format-heading-function file))))
    (delete-dups denote-query--last-files)
    ;; Insert results
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (denote-query-mode)
      ;; In the links' buffer, the values of variables set in a
      ;; `.dir-locals.el` do not apply.  We need to set
      ;; `denote-directory' here because the buttons depend on it.
      ;; Moreover, its value is overwritten after enabling the major
      ;; mode, so it needs to be set after.
      (setq-local denote-directory dir)
      (setq overlay-arrow-position nil)
      (goto-char (point-min))
      (xref--insert-xrefs xref-alist)
      (goto-char (point-min))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (when-let* ((buffer-file-name file))
                      (denote-make-links-buffer query files buffer-name display-buffer-action)))))
    (display-buffer buffer display-buffer-action)))

(defvar denote-query-links-buffer-function #'denote-make-links-buffer
  "Function to make an Xref buffer showing query link results.
It accepts the same arguments as `denote-make-links-buffer'.")

(defun denote-query-focus-last-search (query)
  "Search QUERY in the content of files which matched the last search.
\"Last search\" here means any call to `denote-grep',
`denote-backlinks', `denote-query-contents-link', or, generally, any
command that relies on the `denote-make-links-buffer'."
  (interactive (list (denote-grep-query-prompt :focused)) denote-query-mode)
  (unless (derived-mode-p 'denote-query-mode)
    (user-error "Only use this command inside the `denote-query-mode'"))
  (denote-make-links-buffer
   query denote-query--last-files
   nil '(display-buffer-same-window))
  (message "Searching `%s' in files matched previously" query))

(defun denote-query-exclude-files (regexp)
  "Exclude files whose name matches REGEXP from current search buffer.

This is useful even if you don't know regular expressions, given the
Denote file-naming scheme.  For instance, to exclude notes with the
keyword \"philosophy\" from current search buffer, type
‘\\<denote-query-mode-map>\\[denote-query-exclude-files] _philosophy
RET’.

Internally, this works by generating a new call to
`denote-make-links-buffer' with the same QUERY as the last one, but with
a set of files gotten from checking REGEXP against last matched files.

When called from Lisp, REGEXP can be a list; in that case, it should be
a list of fixed strings (NOT regexps) to check against last matched
files.  Files that match any of the strings get excluded.  Internally,
the list is processed using `regexp-opt'.  For an example of this usage,
see `denote-query-exclude-files-with-keywords'."
  (interactive (list (denote-grep-file-regexp-prompt)) denote-query-mode)
  (unless (derived-mode-p 'denote-query-mode)
    (user-error "Only use this command inside the `denote-query-mode'"))
  (let (final-files)
    (dolist (file denote-query--last-files)
      (unless (string-match
               ;; Support list of strings as REGEXP
               (if (listp regexp)
                   (regexp-opt regexp)
                 regexp)
               file)
        (push file final-files)))
    (if final-files
        (denote-make-links-buffer denote-query--last-query final-files)
      (user-error "No remaining files when applying that filter"))
    (message "Excluding files matching `%s'" regexp)))

(defun denote-query-only-include-files (regexp)
  "Exclude file names not matching REGEXP from current query buffer.

See `denote-query-exclude-files' for details, including the behaviour
when REGEXP is a list."
  (interactive (list (denote-grep-file-regexp-prompt :include)) denote-query-mode)
  (unless (derived-mode-p 'denote-query-mode)
    (user-error "Only use this command inside the `denote-query-mode'"))
  (let (final-files)
    (dolist (file denote-query--last-files)
      (when (string-match
             ;; Support list of strings as REGEXP
             (if (listp regexp)
                 (regexp-opt regexp)
               regexp)
             file)
        (push file final-files)))
    (if final-files
        (denote-make-links-buffer denote-query--last-query final-files)
      (user-error "No remaining files when applying that filter"))
    (message "Only including files matching `%s'" regexp)))

(defun denote-query-exclude-files-with-keywords (keywords)
  "Exclude files with KEYWORDS from current query buffer.

KEYWORDS should be a list of keywords (without underscore).

Interactively, KEYWORDS are read from the minibuffer using
`completing-read-multiple', which see."
  (interactive
   (list (denote-keywords-prompt "Exclude files with keywords"))
   denote-query-mode)
  (unless (derived-mode-p 'denote-query-mode)
    (user-error "Only use this command inside the `denote-query-mode'"))
  (denote-query-exclude-files
   (mapcar (lambda (kw) (concat "_" kw)) keywords)))

(defun denote-query-only-include-files-with-keywords (keywords)
  "Exclude files without KEYWORDS from current query buffer.

See `denote-query-exclude-files-with-keywords' for details."
  (interactive
   (list (denote-keywords-prompt "Only include files with keywords"))
   denote-query-mode)
  (unless (derived-mode-p 'denote-query-mode)
    (user-error "Only use this command inside the `denote-query-mode'"))
  (denote-query-only-include-files
   (mapcar (lambda (kw) (concat "_" kw)) keywords)))

(defun denote-query-clear-all-filters ()
  "Run last search with the full set of files in the variable `denote-directory'.

This effectively gets ride of any interactive filter applied (by the
means of e.g. `denote-query-exclude-files')."
  (interactive nil denote-query-mode)
  (unless (derived-mode-p 'denote-query-mode)
    (user-error "Only use this command inside the `denote-query-mode'"))
  (denote-make-links-buffer denote-query--last-query)
  (message "Cleared all filters"))

;;;;;; Additional features for searching file contents

(defvar denote-grep-history nil
  "Minibuffer history of content searches performed by `denote-grep'.
Also see `denote-grep-file-regexp-history'.")

(defcustom denote-grep-display-buffer-action
  '((display-buffer-same-window)
    (mode . denote-query-mode))
  "The action used to display search results from `denote-grep'.
This is the same as `denote-backlinks-display-buffer-action'.  Refer to
its documentation for the technicalities."
  :risky t
  :type `(choice
          (alist :key-type
                 (choice :tag "Condition"
                         regexp
                         (function :tag "Matcher function"))
                 :value-type ,display-buffer--action-custom-type)
          (function :tag "Custom function to return an action alist"))
  :package-version '(denote . "4.0.0")
  :group 'denote-query)

(defun denote-grep-query-prompt (&optional type)
  "Prompt for a grep query in the minibuffer.

The prompt assumes a search in all files, unless TYPE is non-nil.

TYPE can be one of :focused (for a focused search (a search among
matching files), see `denote-query-focus-last-search'), :dired (for a
search in marked Dired files, see `denote-grep-marked-dired-files') or
:region (for a search in files referenced in region, see
`denote-grep-files-referenced-in-region').

TYPE only affects the prompt, not the returned value."
  (read-string
   (cond ((eq type :focused)
          "Search (only files matched last): ")
         ((eq type :dired)
          "Search (only marked dired files): ")
         ((eq type :region)
          "Search (only files referenced in region): ")
         (t "Search (all Denote files): "))
   nil 'denote-grep-history))

(defvar denote-grep-file-regexp-history nil
  "Minibuffer history for `denote-grep' commands asking for a file regexp.
Also see `denote-grep-history'.")

(defun denote-grep-file-regexp-prompt (&optional include)
  "Prompt for a file regexp in the minibuffer.

The prompt assumes the user wants to exclude files, unless INCLUDE is
non-nil."
  (read-string
   (if (not include)
       "Exclude file names matching: "
     "Only include file names matching: ")
   nil 'denote-grep-file-regexp-history))

;;;###autoload
(defun denote-grep (query)
  "Search QUERY in the content of Denote files.
QUERY should be a regular expression accepted by `xref-search-program'.

The files to search for are those returned by `denote-directory-files'
with a non-nil TEXT-ONLY argument.

Results are put in a buffer which allows folding and further
filtering (see the manual for details).

You can insert a link to a grep search in any note by using the command
`denote-query-contents-link'."
  (interactive (list (denote-grep-query-prompt)))
  (let (denote-query--omit-current)
    (denote-make-links-buffer query nil nil denote-grep-display-buffer-action)))

;;;###autoload
(defun denote-grep-marked-dired-files (query)
  "Do the equivalent of `denote-grep' for QUERY in marked Dired files."
  (interactive (list (denote-grep-query-prompt :dired)))
  (if-let* ((files (dired-get-marked-files)))
      (denote-make-links-buffer query files nil denote-grep-display-buffer-action)
    (user-error "No marked files")))

(defun denote-grep--get-files-referenced-in-region (start end)
  "Return a list with all Denote files referenced between START and END.
START and END are buffer positions, as integers.  A reference to a file
is the mere presence of its identifier.

Return a list with the absoulte path of referenced files."
  (let (id-list)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward denote-id-regexp nil t)
          (push (denote-get-path-by-id (match-string 0)) id-list))))
    id-list))

;;;###autoload
(defun denote-grep-files-referenced-in-region (query start end)
  "Perform `denote-grep' QUERY in files referenced between START and END.
When called interactively, prompt for QUERY.  Also get START and END as
the buffer positions that delimit the marked region.  When called from
Lisp, QUERY is a string, while START and END are buffer positions, as
integers.

Find references to files by their identifier.  This includes links with
just the identifier (as described in `denote-link' and related), links
written by an Org dynamic block (see the `denote-org' package), or even
file listings such as those of `dired' and the command-line `ls' program."
  (interactive
   (if (region-active-p)
       (list
        (denote-grep-query-prompt :region)
        (region-beginning)
        (region-end))
     (user-error "No region is active; aborting")))
  (if-let* ((files (denote-grep--get-files-referenced-in-region start end)))
      (denote-make-links-buffer query files nil denote-grep-display-buffer-action)
    (user-error "No files referenced in region")))

;;;;;; Backlinks

(defun denote--backlinks-get-buffer-name (file id)
  "Format a buffer name for `denote-backlinks'.
Use FILE to detect a suitable title with which to name the buffer.  Else
use the ID."
  (if-let* ((type (denote-filetype-heuristics file))
            (title (denote-retrieve-front-matter-title-value file type)))
      (format "*Denote FILE backlinks for %S*" title)
    (format "*Denote FILE backlinks for %s*" id)))

;;;###autoload
(defun denote-backlinks ()
  "Produce a buffer with backlinks to the current note.

Show the names of files linking to the current file.  Include the
context of each link if the user option `denote-backlinks-show-context'
is non-nil.

Place the buffer below the current window or wherever the user option
`denote-backlinks-display-buffer-action' specifies."
  (interactive)
  (if-let* ((file buffer-file-name))
      (when-let* ((identifier (denote-retrieve-filename-identifier-with-error file)))
        (funcall denote-query-links-buffer-function
                 identifier nil
                 (denote--backlinks-get-buffer-name file identifier)
                 denote-backlinks-display-buffer-action))
    (user-error "Buffer `%s' is not associated with a file" (current-buffer))))

(defalias 'denote-show-backlinks-buffer 'denote-backlinks
  "Alias for `denote-backlinks' command.")


(defun denote-link-return-backlinks (&optional file)
  "Return list of backlinks in current or optional FILE.
Also see `denote-link-return-links'."
  (when-let* ((current-file (or file (buffer-file-name)))
              (id (denote-retrieve-filename-identifier-with-error current-file)))
    (delete current-file (denote-retrieve-files-xref-query id))))

;; TODO 2024-09-04: Instead of using `denote-link-return-backlinks' we
;; should have a function that does not try to find all backlinks but
;; simply exits as soon as it finds one.
(defun denote--file-has-backlinks-p (file)
  "Return non-nil if FILE has backlinks."
  (not (zerop (length (denote-link-return-backlinks file)))))

;;;###autoload
(defun denote-find-backlink ()
  "Use minibuffer completion to visit backlink to current file.
Alo see `denote-find-link'."
  (declare (interactive-only t))
  (interactive)
  (find-file
   (denote-get-path-by-id
    (denote-extract-id-from-string
     (denote-select-linked-file-prompt
      (or (denote-link-return-backlinks)
          (user-error "No backlinks found")))))))

;;;;;; Query links

(defvar denote-query-link-history nil
  "Minibuffer history of `denote-query-link-prompt'.")

(defun denote-query-link-prompt (&optional initial-query prompt-text)
  "Prompt for query string.
With optional INITIAL-QUERY use it as the initial minibuffer text.  With
optional PROMPT-TEXT use it in the minibuffer instead of the default
prompt.

Previous inputs at this prompt are available for minibuffer completion
if the user option `denote-history-completion-in-prompts' is set to a
non-nil value."
  (when (and initial-query (string-empty-p initial-query))
    (setq initial-query nil))
  (denote--with-conditional-completion
   'denote-query-link-prompt
   (format-prompt (or prompt-text "Query for") nil)
   denote-query-link-history
   initial-query))

(defconst denote-query-link-types '(query-contents query-filenames)
  "Types of query links.")

;; NOTE 2025-03-27: Should we expose a user option for this?  And/or
;; should we add a DESCRIPTION parameter to `denote--format-query-link'?
;;
;; What would make for a good default description in that scenario?
;; Maybe "QC:query text here" and "QF:query text here" for
;; `query-contents' and `query-filenames' respectively.
(defvar denote-query-description-prefix ""
  "Prefix string for query links to format their description text.
The description text constists of the value of this variable followed by
the query")

(defun denote--format-query-link (type query file-type)
  "Format QUERY link of TYPE for the given FILE-TYPE.
Return an error if TYPE is not one among the symbols specified in
`denote-query-link-types'.

If FILE-TYPE is nil, use that of Org."
  (unless (memq type denote-query-link-types)
    (error "Type `%s' is not one among `denote-query-link-types'" type))
  (format (or (denote--link-format file-type) (denote--link-format 'org))
          (format "%s:%s" type query)
          (format "%s%s" denote-query-description-prefix query)))

;;;###autoload
(defun denote-query-contents-link (query)
  "Insert query link for file contents.
Prompt for QUERY or use the text of the active region.  When the user
follows this link, place any matches in a separate buffer (using the
built-in Xref mechanism).  This is the equivalent of a Unix grep command
across the variable `denote-directory'."
  (interactive
   (list
    (or (denote--get-active-region-content)
        (denote-query-link-prompt nil "Query in file CONTENTS"))))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (denote--delete-active-region-content)
  (insert (denote--format-query-link 'query-contents query (denote-filetype-heuristics buffer-file-name))))

;;;###autoload
(defun denote-query-filenames-link (query)
  "Insert query link for file names.
Prompt for QUERY or use the text of the active region.  When the user
follows this link, place any matches in a separate buffer (using the
built-in Dired mechanism).  This is the equivalent of a Unix find
command across the variable `denote-directory'."
  (interactive
   (list
    (or (denote--get-active-region-content)
        (denote-query-link-prompt nil "Query in file NAMES"))))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (denote--delete-active-region-content)
  (insert (denote--format-query-link 'query-filenames query (denote-filetype-heuristics buffer-file-name))))

(defvar denote--query-last-dired-buffer nil
  "Buffer object produced by the last query for file names.")

(defun denote--act-on-query-link (query)
  "Act on QUERY link.
QUERY is a string of the form TYPE:SEARCH, where TYPE is one among
`denote-query-link-types' while SEARCH is the regular expression to
search for."
  (cond
   ((string-prefix-p "query-contents:" query)
    (setq query (replace-regexp-in-string "query-contents:" "" query))
    (funcall denote-query-links-buffer-function query nil nil denote-query-links-display-buffer-action))
   ((string-prefix-p "query-filenames:" query)
    (setq query (replace-regexp-in-string "query-filenames:" "" query))
    ;; NOTE 2025-03-27: I do not think we need to add another
    ;; parameter to `denote-sort-dired' for handling the
    ;; `display-buffer'.  This is a special case, but we can always
    ;; change it later if the need arises.
    ;;
    ;; Here we handle the buffer and window state to make it behave
    ;; like the Xref buffer.  Otherwise, Dired does not reuse its
    ;; buffer (which is generally okay).
    (let ((buffer (save-window-excursion (denote-sort-dired query nil nil nil))))
      (when (bufferp denote--query-last-dired-buffer)
        (when-let* ((window (get-buffer-window denote--query-last-dired-buffer))
                    (_ (window-live-p window)))
          (delete-window window))
        (kill-buffer denote--query-last-dired-buffer))
      (display-buffer buffer denote-query-links-display-buffer-action)
      (setq denote--query-last-dired-buffer buffer)))
   (t
    (error "Cannot open `%s' of unknown link type" query))))

;;;;; Link buttons

(make-obsolete 'denote-link--find-file-at-button nil "4.0.0")

(make-obsolete
 'denote-link-buttonize-buffer
 'denote-fontify-links-mode
 "Use the `denote-fontify-links-mode', as it works better than buttonization. Since 3.0.0")

;; NOTE 2025-03-24: This does not work for query links because of how
;; `markdown-follow-link-at-point' is implemented to always check for
;; links.
(defun denote-link-markdown-follow (link)
  "Function to open Denote file present in LINK.
To be assigned to `markdown-follow-link-functions'."
  (when (ignore-errors (string-match denote-id-regexp link))
    (funcall denote-open-link-function
             (denote-get-path-by-id (match-string 0 link)))))

(eval-after-load 'markdown-mode
  '(add-hook 'markdown-follow-link-functions #'denote-link-markdown-follow))

;;;;; Link fontification

;; TODO 2024-06-19: We need to bind RET and maybe even C-c C-o to a
;; command that opens the link at point.  Then we may also rename this
;; keymap.
(defvar denote-link-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'denote-link-open-at-mouse)
    (define-key map [mouse-3] #'denote-link-open-at-mouse)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap for mouse actions over fontified Denote links.")

(defun denote--link-open-at-point-subr ()
  "Open link at point."
  (let ((query (get-text-property (point) 'denote-link-query-part)))
    (if-let* ((path (denote-get-path-by-id query)))
        (funcall denote-open-link-function path)
      (denote--act-on-query-link query))))

(defun denote-link-open-at-point ()
  "Open Denote link at point."
  (interactive)
  (denote--link-open-at-point-subr))

(defun denote-link-open-at-mouse (ev)
  "Open Denote link for mouse EV click."
  (interactive "e")
  (mouse-set-point ev)
  (denote--link-open-at-point-subr))

(defun denote-get-link-face (query)
  "Return appropriate face for QUERY."
  (if (denote-identifier-p (string-trim-right query ":[^/]+.*"))
      'denote-faces-link
    'denote-faces-query-link))

(defun denote--fontify-links-subr (query limit)
  "Do the work of the font-lock match for QUERY up to LIMIT.
Implementation based on the function `org-activate-links'."
  (catch :exit
    (while (re-search-forward query limit t)
      (save-match-data  ; to return the matches to font-lock
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (visible-start (or (match-beginning 2) start))
               (visible-end (or (match-end 2) end))
               (query (match-string-no-properties 1)))
          (let* ((properties `( face ,(denote-get-link-face query)
                                mouse-face highlight
                                keymap ,denote-link-mouse-map
                                denote-link-query-part ,query
                                help-echo query
                                htmlize-link (:uri ,query)
                                font-lock-multiline t))
                 (non-sticky-props
                  '(rear-nonsticky (mouse-face highlight keymap invisible intangible help-echo htmlize-link)))
                 (face-property 'link)
                 (hidden (append '(invisible 'denote-link) properties)))
            (remove-text-properties start end '(invisible nil))
            (add-text-properties start visible-start hidden)
            (add-face-text-property start end face-property)
            (add-text-properties visible-start visible-end properties)
            (add-text-properties visible-end end hidden)
            (dolist (pos (list end visible-start visible-end))
              (add-text-properties (1- pos) pos non-sticky-props)))
          (throw :exit t))))      ; signal success
    nil))

(defun denote-fontify-links (limit)
  "Provide font-lock matcher to fontify links up to LIMIT."
  (when-let* ((type (denote-filetype-heuristics (buffer-file-name))))
    (denote--fontify-links-subr (denote--link-in-context-regexp type) limit)))

(define-obsolete-function-alias
  'denote-get-identifier-at-point
  'denote-get-link-identifier-or-query-term-at-point
  "4.0.0")

(defun denote-get-link-identifier-or-query-term-at-point (&optional point)
  "Return the Denote identifier or query term at point or optional POINT."
  (when-let* ((position (or point (point)))
              (face-at-point (get-text-property position 'face))
              ((or (eq face-at-point 'denote-faces-link)
                   (member 'denote-faces-link face-at-point))))
    (or (get-text-property position 'denote-link-query-part)
        (when-let* ((link-data (get-text-property position 'htmlize-link))
                    (link (cadr link-data)))
          (string-match denote-id-regexp link)
          (match-string-no-properties 0 link)))))

(defun denote--get-link-file-path-at-point (&optional point)
  "Return link to the Denote file path at point or optional POINT.
To be used as a `thing-at' provider."
  (when-let* ((position (or point (point)))
              (id (get-text-property position 'denote-link-query-part))
              (path (denote-get-path-by-id id)))
    (concat "file:" path)))

(defvar thing-at-point-provider-alist)

;;;###autoload
(defun denote-fontify-links-mode-maybe ()
  "Enable `denote-fontify-links-mode' in a denote file unless in `org-mode'."
  (when (and buffer-file-name
             (not (derived-mode-p 'org-mode))
             (denote-file-is-note-p buffer-file-name))
    (denote-fontify-links-mode)))

;;;###autoload
(define-minor-mode denote-fontify-links-mode
  "A minor mode to fontify and fold Denote links.

Enabled this mode only when the current buffer is a Denote note and the
major mode is not `org-mode' (or derived therefrom).  Consider using
`denote-fontify-links-mode-maybe' for this purpose."
  :init-value nil
  :global nil
  :group 'denote
  (require 'thingatpt)
  (if denote-fontify-links-mode
      (progn
        (add-to-invisibility-spec 'denote-link)
        (font-lock-add-keywords nil '((denote-fontify-links)))
        (setq-local thing-at-point-provider-alist
                    (append thing-at-point-provider-alist
                            '((url . denote--get-link-file-path-at-point)))))
    (remove-from-invisibility-spec 'denote-link)
    (font-lock-remove-keywords nil '((denote-fontify-links)))
    (setq-local thing-at-point-provider-alist
                (delete
                 '(url . denote--get-link-file-path-at-point)
                 thing-at-point-provider-alist)))
  (font-lock-update))

;;;;; Add links matching regexp

(defvar denote-link--prepare-links-format "- %s\n"
  "Format specifiers for `denote-add-links'.")

(make-obsolete-variable 'denote-link-add-links-sort nil "3.1.0")

(defun denote-link--prepare-links (files current-file-type id-only &optional no-sort include-date)
  "Prepare links to FILES from CURRENT-FILE-TYPE.
When ID-ONLY is non-nil, use a generic link format.

With optional NO-SORT do not try to sort the inserted lines.
Otherwise sort lines while accounting for `denote-link-add-links-sort'.

Optional INCLUDE-DATE has the same meaning as in `denote-format-link'."
  (let ((links))
    (dolist (file files)
      (let* ((description (denote-get-link-description file))
             (link (denote-format-link file description current-file-type id-only include-date))
             (link-as-list-item (format denote-link--prepare-links-format link)))
        (push link-as-list-item links)))
    (if no-sort
        (nreverse links)
      (sort links #'string-collate-lessp))))

(defun denote-link--insert-links (files current-file-type &optional id-only no-sort include-date)
  "Insert at point a typographic list of links matching FILES.

With CURRENT-FILE-TYPE as a symbol among those specified in variable
`denote-file-type' (or the `car' of each element in `denote-file-types'),
format the link accordingly.  With a nil or unknown non-nil value,
default to the Org notation.

With ID-ONLY as a non-nil value, produce links that consist only
of the identifier, thus deviating from CURRENT-FILE-TYPE.

Optional NO-SORT is passed to `denote-link--prepare-links'.

Optional INCLUDE-DATE has the same meaning as in `denote-format-link'."
  (when-let* ((links (denote-link--prepare-links files current-file-type id-only no-sort include-date)))
    (dolist (link links)
      (insert link))))

;;;###autoload
(defun denote-add-links (regexp &optional id-only)
  "Insert links to all files whose file names match REGEXP.
Use this command to reference multiple files at once.  Particularly
useful for the creation of metanotes (read the manual for more on the
matter).

Optional ID-ONLY has the same meaning as in `denote-link': it
inserts links with just the identifier."
  (interactive
   (list
    (denote-files-matching-regexp-prompt "Insert links to files matching REGEXP")
    current-prefix-arg))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let ((file-type (denote-filetype-heuristics (buffer-file-name))))
    (if-let* ((files (denote-directory-files regexp :omit-current)))
        (denote-link--insert-links files file-type id-only)
      (message "No links matching `%s'" regexp))))

;;;;; Link to file with matching contents

;;;###autoload
(defun denote-link-to-file-with-contents (query &optional id-only)
  "Link to a file whose contents match QUERY.
This is similar to `denote-link', except that the file prompt is limited
to files matching QUERY.  Optional ID-ONLY has the same meaning as in
`denote-link'."
  (interactive
   (list (denote-query-link-prompt nil "Files whose contents include QUERY")))
  (if-let* ((files (denote-retrieve-files-xref-query query))
            ;; NOTE 2025-03-29: Maybe we should have a named prompt
            ;; for this case, but I think we do not need it right now.
            (file (completing-read
                   (format "Select FILE with contents `%s': "
                           (propertize query 'face 'denote-faces-prompt-current-name))
                   (denote--completion-table 'file files)
                   nil t nil 'denote-file-history)))
      (denote-link file
                   (denote-filetype-heuristics buffer-file-name)
                   (denote-get-link-description file)
                   id-only)
    (user-error "No files include the query `%s' in their contents" query)))

;;;###autoload
(defun denote-link-to-all-files-with-contents (query &optional id-only)
  "Link to all files whose contents match QUERY.
This is similar to `denote-add-links', except it searches inside file
contents, not file names.  Optional ID-ONLY has the same meaning as in
`denote-link' and `denote-add-links'."
  (interactive
   (list (denote-query-link-prompt nil "Files whose contents include QUERY")))
  (if-let* ((files (denote-retrieve-files-xref-query query)))
      (denote-link--insert-links files (denote-filetype-heuristics buffer-file-name) id-only)
    (user-error "No files include the query `%s' in their contents" query)))

;;;;; Links from Dired marks

;; NOTE 2022-07-21: I don't think we need a history for this one.
(defun denote-link--buffer-file-prompt (buffer-file-names)
  "Select file from BUFFER-FILE-NAMES of Denote notes."
  (let ((relative-buffer-file-names (mapcar #'denote-get-file-name-relative-to-denote-directory buffer-file-names)))
    (concat (denote-directory)
            (completing-read
             "Select open note to add links to: "
             (denote--completion-table 'file relative-buffer-file-names)
             nil t))))

(defun denote-link--map-over-notes ()
  "Return list of `denote-file-has-denoted-filename-p' from Dired marked items."
  (seq-filter (lambda (file) (and (denote-file-has-denoted-filename-p file)
                                  (denote-file-has-identifier-p file)))
              (dired-get-marked-files)))

;;;###autoload
(defun denote-link-dired-marked-notes (files buffer &optional id-only)
  "Insert Dired marked FILES as links in BUFFER.

FILES conform with the Denote file-naming scheme, such that they can be
linked to using the `denote:' link type.

The BUFFER is one which visits a Denote note file.  If there are
multiple BUFFER candidates in buffers, prompt with completion for
one among them.  If there is none, throw an error.

With optional ID-ONLY as a prefix argument, insert links with
just the identifier (same principle as with `denote-link').

This command is meant to be used from a Dired buffer."
  (interactive
   (if (derived-mode-p 'dired-mode)
       (list
        (denote-link--map-over-notes)
        (let ((file-names (denote--buffer-file-names)))
          (find-buffer-visiting
           (cond
            ((null file-names)
             (user-error "No buffers visiting Denote notes"))
            ((eq (length file-names) 1)
             (car file-names))
            (t
             (denote-link--buffer-file-prompt file-names)))))
        current-prefix-arg)
     (user-error "This command only works inside a Dired buffer"))
   dired-mode)
  (when (null files)
    (user-error "No note files to link to"))
  (unless (buffer-live-p buffer)
    (error "The buffer `%s' is not live" buffer))
  (let ((body (lambda ()
                (unless (or (denote--file-type-org-extra-p)
                            (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
                  (user-error "The target file's type is not recognized by Denote"))
                (when (y-or-n-p (format "Create links at point in `%s'?" buffer))
                  (denote-link--insert-links files (denote-filetype-heuristics buffer-file-name) id-only)
                  (message "Added links to `%s'; displaying it now"
                           ;; TODO 2024-12-26: Do we need our face here?  I think
                           ;; not, but let me keep a note of it.
                           (propertize (format "%s" buffer) 'face 'success))))))
    (if-let* ((window (get-buffer-window buffer))
              ((window-live-p window)))
        (with-selected-window window (funcall body))
      (with-current-buffer buffer (funcall body))
      (display-buffer-below-selected buffer nil))))

(defalias 'denote-dired-link-marked-notes 'denote-link-dired-marked-notes
  "Alias for `denote-link-dired-marked-notes' command.")

;;;; Define menu

(defvar denote--menu-contents
  '(["Create a note" denote
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
    ["Insert a direct link" denote-link
     :help "Insert link to a file in the `denote-directory'"
     :enable (derived-mode-p 'text-mode)]
    ["Insert a direct link to file with contents" denote-link-to-file-with-contents
     :help "Insert link to a file in the `denote-directory' whose contents include a query"
     :enable (derived-mode-p 'text-mode)]
    ["Insert a query link for file contents" denote-query-contents-link
     :help "Insert query link searching for file contents in the `denote-directory'"
     :enable (derived-mode-p 'text-mode)]
    ["Insert a query link for file names" denote-query-filenames-link
     :help "Insert query link searching for file names in the `denote-directory'"
     :enable (derived-mode-p 'text-mode)]
    "---"
    ["Insert links to file names matching regexp" denote-add-links
     :help "Insert links to file names in the `denote-directory' matching regexp"
     :enable (derived-mode-p 'text-mode)]
    ["Insert links to files whose contents match regexp" denote-link-to-all-files-with-contents
     :help "Insert links to file in the `denote-directory' whose contents match regexp"
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
    ["Create note in the background and link to it directly" denote-link-after-creating
     :help "Create new note and link to it from the current file"
     :enable (derived-mode-p 'text-mode)]
    ["Create note in the background with chosen command and link to it directly" denote-link-after-creating-with-command
     :help "Create new note with the chosen command and link to it from the current file"
     :enable (derived-mode-p 'text-mode)]
    "---"
    ["Generate sorted and filtered Dired listing" denote-sort-dired
     :help "Generate a sorted and filtered Dired listing of files in the `denote-directory'"]
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

;;;; Register `denote:' custom Org hyperlink

(declare-function org-link-open-as-file "ol" (path arg))

(defun denote-link--ol-resolve-link-to-target (link &optional full-data)
  "Resolve LINK to target file, with or without additioanl file-search terms.
With optional FULL-DATA return a list in the form of (path query file-search)."
  (let* ((file-search (and (string-match "::\\(.*\\)\\'" link)
                           (match-string 1 link)))
         (query (if (and file-search (not (string-empty-p file-search)))
                    (substring link 0 (match-beginning 0))
                  link))
         (path (denote-get-path-by-id query)))
    (cond
     (full-data
      (list path query file-search))
     ((and file-search (not (string-empty-p file-search)))
      (concat path "::" file-search))
     (t (or path query)))))

;;;###autoload
(defun denote-link-ol-follow (link)
  "Find file of type `denote:' matching LINK.
LINK is the identifier of the note, optionally followed by a file search
option akin to that of standard Org `file:' link types.  Read Info
node `(org) Query Options'.

If LINK is not an identifier, then it is not pointing to a file but to a
query of file contents or file names (see the commands
`denote-query-contents-link' and `denote-query-filenames-link').

Uses the function `denote-directory' to establish the path to the file."
  (if-let* ((match (denote-link--ol-resolve-link-to-target link))
            (_ (file-exists-p (string-trim-right match ":[^/]+.*"))))
      (org-link-open-as-file match nil)
    (denote--act-on-query-link match)))

;;;###autoload
(defun denote-link-ol-complete ()
  "Like `denote-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `denote:' hyperlink type."
  (if-let* ((file (denote-file-prompt)))
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
    (if (and (stringp id) (string-match-p "\\S-" id))
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
(defun denote-link-ol-store (&optional interactive?)
  "Handler for `org-store-link' adding support for denote: links.
Optional INTERACTIVE? is used by `org-store-link'.

Also see the user option `denote-org-store-link-to-heading'."
  (when interactive?
    (when-let* ((file (buffer-file-name))
                ((denote-file-is-note-p file))
                (file-id (denote-retrieve-filename-identifier file))
                (description (denote-get-link-description file)))
      (let ((heading-links (and denote-org-store-link-to-heading
                                (derived-mode-p 'org-mode)
                                (denote--org-capture-link-specifiers-p)))
            (heading (denote-link-ol-get-heading)))
        (org-link-store-props
         :type "denote"
         :description (if (and heading-links heading)
                          (denote-link-format-heading-description
                           description
                           heading)
                        description)
         :link (cond
                ((when-let* ((id (org-entry-get (point) "CUSTOM_ID")))
                   (format "denote:%s::#%s" file-id id)))
                ((and heading-links (eq denote-org-store-link-to-heading 'context) heading)
                 (format "denote:%s::*%s" file-id heading))
                ((and heading-links heading)
                 (format "denote:%s::#%s" file-id (denote-link-ol-get-id)))
                (t
                 (concat "denote:" file-id))))
        org-store-link-plist))))

;;;###autoload
(defun denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (pcase-let* ((`(,path ,query ,file-search) (denote-link--ol-resolve-link-to-target link :full-data))
               (anchor (when path (file-relative-name (file-name-sans-extension path))))
               (desc (cond
                      (description)
                      (file-search (format "denote:%s::%s" query file-search))
                      (t (concat "denote:" query)))))
    (if path
        (pcase format
          ('html (if file-search
                     (format "<a href=\"%s.html%s\">%s</a>" anchor file-search desc)
                   (format "<a href=\"%s.html\">%s</a>" anchor desc)))
          ('latex (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
          ('texinfo (format "@uref{%s,%s}" path desc))
          ('ascii (format "[%s] <denote:%s>" desc path))
          ('md (format "[%s](%s)" desc path))
          (_ path))
      (format-message "[[Denote query for `%s']]" query))))

(defun denote-link-ol-help-echo (_window _object position)
  "Echo the full file path of the identifier at POSITION."
  (when-let* ((htmlize-link (get-text-property position 'htmlize-link))
              (string (plist-get htmlize-link :uri))
              (identifier (replace-regexp-in-string "denote:\\(.*?\\)\\(#.*\\)?" "\\1" string))
              (path (denote-get-path-by-id identifier)))
    path))

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
           :face #'denote-get-link-face
           :help-echo #'denote-link-ol-help-echo
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

(defun denote--org-capture-link-specifiers-p ()
  "Return non-nil if `denote-org-capture-specifiers' uses link specifiers."
  (when (stringp denote-org-capture-specifiers)
    (string-match-p "%^?[aAlL]" denote-org-capture-specifiers)))

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
  (pcase-let* ((denote-prompts (remove 'file-type denote-prompts)) ; Do not prompt for file-type. We use org.
               (`(,title ,keywords _ ,directory ,date ,template ,signature)
                (denote--creation-get-note-data-from-prompts))
               (`(,title ,keywords _ ,directory ,date ,id ,template ,signature)
                (denote--creation-prepare-note-data title keywords 'org directory date template signature))
               (front-matter (denote--format-front-matter title date keywords id signature 'org))
               (template-string (cond ((stringp template) template)
                                      ((functionp template) (funcall template))
                                      (t (user-error "Invalid template")))))
    (setq denote-last-path
          (denote-format-file-name directory id keywords title ".org" signature))
    (when (file-regular-p denote-last-path)
      (user-error "A file named `%s' already exists" denote-last-path))
    (denote--keywords-add-to-history keywords)
    (concat front-matter template-string denote-org-capture-specifiers)))

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
  (when-let* ((file denote-last-path)
              ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

;;;; The `denote-rename-buffer-mode'

(defgroup denote-rename-buffer nil
  "Rename Denote buffers to be shorter and easier to read."
  :group 'denote
  :link '(info-link "(denote) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/denote"))

(defvaralias 'denote-buffer-has-backlinks-string 'denote-rename-buffer-backlinks-indicator
  "Alias for `denote-rename-buffer-backlinks-indicator'.")

(defcustom denote-rename-buffer-backlinks-indicator " <-->"
  "A string used to indicate that a buffer has backlinks pointing to it."
  :type 'string
  :package-version '(denote . "3.1.0")
  :group 'denote-rename-buffer)

(defcustom denote-rename-buffer-format "[D] %D%b"
  "The format of the buffer name `denote-rename-buffer' should use.
The value is a string that treats specially the following specifiers:

- The %t is the Denote TITLE in the front matter or the file name.
- The %T is the Denote TITLE in the file name.
- The %i is the Denote IDENTIFIER of the file.
- The %I is the identifier converted to DAYNAME, DAYNUM MONTHNUM YEAR.
- The %d is the same as %i (DATE mnemonic).
- The %D is a \"do what I mean\" which behaves the same as %t and if
  that returns nothing, it falls back to %I, then %i.
- The %s is the Denote SIGNATURE of the file.
- The %k is the Denote KEYWORDS of the file.
- The %b inserts `denote-rename-buffer-backlinks-indicator'.
- The %% is a literal percent sign.

In addition, the following flags are available for each of the specifiers:

- 0 :: Pad to the width, if given, with zeros instead of spaces.
- - :: Pad to the width, if given, on the right instead of the left.
- < :: Truncate to the width and precision, if given, on the left.
- > :: Truncate to the width and precision, if given, on the right.
- ^ :: Convert to upper case.
- _ :: Convert to lower case.

When combined all together, the above are written thus:

    %<flags><width><precision>SPECIFIER-CHARACTER

Any other string it taken as-is.  Users may want, for example, to
include some text that makes Denote buffers stand out, such as
a [D] prefix."
  :type 'string
  :package-version '(denote . "4.0.0")
  :group 'denote-rename-buffer)

(defcustom denote-rename-buffer-function #'denote-rename-buffer
  "Symbol of function that is called to rename the Denote file buffer.
The default `denote-rename-buffer' function uses the pattern
described in `denote-rename-buffer-format'.

Users can set this variable to an arbitrary function that does
something else.  The function is called without arguments from
the `find-file-hook' and `denote-after-new-note-hook'.

A nil value for this variable means that the title of the Denote
buffer will be used, if available."
  :type '(choice
          (const :tag "Rename using the `denote-rename-buffer-format'" denote-rename-buffer)
          (function :tag "Use a custom renaming function"))
  :package-version '(denote . "2.1.0")
  :group 'denote-rename-buffer)

(defun denote-rename-buffer--format (buffer)
  "Parse the BUFFER through the `denote-rename-buffer-format'."
  (when-let* ((file (buffer-file-name buffer)))
    (let ((type (denote-filetype-heuristics file))
          (should-show-backlink-indicator (and ; only do search if format contains "%b"
                                           (string-match-p "%b" denote-rename-buffer-format)
                                           (denote--file-has-backlinks-p file))))
      (string-trim
       (format-spec denote-rename-buffer-format
                    (list (cons ?t (cond
                                    ((denote-retrieve-front-matter-title-value file type))
                                    ((denote-retrieve-filename-title file))
                                    (t  "")))
                          (cons ?T (or (denote-retrieve-filename-title file) ""))
                          (cons ?b (if should-show-backlink-indicator denote-rename-buffer-backlinks-indicator ""))
                          (cons ?i (or (denote-retrieve-filename-identifier file) ""))
                          ;; TODO 2025-04-03: Maybe we can have something like `denote-date-format' here,
                          ;; but I think we are okay with a hardcoded value.
                          (cons ?I (or (when-let* ((id (denote-retrieve-filename-identifier file))
                                                   (_ (denote-valid-date-p id)))
                                         (format-time-string "%A, %e %B %Y" (date-to-time (denote--id-to-date id))))
                                       ""))
                          (cons ?d (or (denote-retrieve-filename-identifier file) ""))
                          (cons ?D (cond
                                    ((denote-retrieve-front-matter-title-value file type))
                                    ((denote-retrieve-filename-title file))
                                    ((when-let* ((id (denote-retrieve-filename-identifier file)))
                                       (if (denote-valid-date-p id)
                                           (format-time-string "%A, %e %B %Y" (date-to-time (denote--id-to-date id)))
                                         id)))
                                    (t  "")))
                          (cons ?s (or (denote-retrieve-filename-signature file) ""))
                          (cons ?k (or (denote-retrieve-filename-keywords file) ""))
                          (cons ?% "%"))
                    'delete)))))

(defun denote-rename-buffer (&optional buffer)
  "Rename current buffer or optional BUFFER with `denote-rename-buffer-format'.
The symbol of this function is the default value of the user
option `denote-rename-buffer-function' and is thus used by the
`denote-rename-buffer-mode'."
  (when-let* ((file (buffer-file-name buffer))
              ((denote-file-has-identifier-p file))
              (new-name (denote-rename-buffer--format (or buffer (current-buffer))))
              ((not (string-blank-p new-name))))
    (rename-buffer new-name :unique)))

(defun denote-rename-buffer--fallback (&optional buffer)
  "Fallback to rename BUFFER or `current-buffer'.
This is called if `denote-rename-buffer-rename-function' is nil."
  (let ((denote-rename-buffer-format "%t"))
    (denote-rename-buffer buffer)))

(defun denote-rename-buffer-rename-function-or-fallback ()
  "Call `denote-rename-buffer-function' or its fallback to rename with title.
Add this to `find-file-hook' and `denote-after-new-note-hook'."
  (funcall (or denote-rename-buffer-function #'denote-rename-buffer--fallback)))

;;;###autoload
(define-minor-mode denote-rename-buffer-mode
  "Automatically rename Denote buffers to be easier to read.
A buffer is renamed upon visiting the underlying file.  This
means that existing buffers are not renamed until they are
visited again in a new buffer (files are visited with the command
`find-file' or related)."
  :global t
  (if denote-rename-buffer-mode
      (progn
        (add-hook 'denote-after-new-note-hook #'denote-rename-buffer-rename-function-or-fallback)
        (add-hook 'denote-after-rename-file-hook #'denote-rename-buffer-rename-function-or-fallback)
        (add-hook 'find-file-hook #'denote-rename-buffer-rename-function-or-fallback))
    (remove-hook 'denote-after-new-note-hook #'denote-rename-buffer-rename-function-or-fallback)
    (remove-hook 'denote-after-rename-file-hook #'denote-rename-buffer-rename-function-or-fallback)
    (remove-hook 'find-file-hook #'denote-rename-buffer-rename-function-or-fallback)))

(provide 'denote)
;;; denote.el ends here
