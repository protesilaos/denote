;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote
;; Version: 3.1.0
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
  to qualify the note according to the user's methodology.
  Signatures have no strictly defined function and are up to the
  user to apply as they see fit.  One use-case is to implement
  Niklas Luhmann's Zettelkasten system for a sequence of notes
  (Folgezettel).  Signatures are not included in a file's front
  matter.  They are reserved solely for creating a structure in a
  file listing.  To insert a link that includes the signature,
  use the command `denote-link-with-signature'.

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
it again. When in doubt, leave the default file-naming scheme as-is."
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
it, use the command `org-insert-link'.  Note that `org-capture' uses
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

(make-obsolete-variable 'denote-rename-no-confirm 'denote-rename-confirmations "3.0.0")

(define-obsolete-variable-alias
  'denote-link-backlinks-display-buffer-action
  'denote-backlinks-display-buffer-action
  "3.1.0")

(defcustom denote-backlinks-display-buffer-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . fit-window-to-buffer)
    (dedicated . t))
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
  :type '(cons (choice (function :tag "Display Function")
                       (repeat :tag "Display Functions" function))
               alist)
  :package-version '(denote . "3.1.0")
  :group 'denote)

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

(defcustom denote-link-description-function #'denote-link-description-with-signature-and-title
  "Function to create the description of links.

The function specified takes a FILE argument and returns the description
as a string.

By default, the title of the file is returned as the description.  If
the file has a signature, it is prepended to the title."
  :group 'denote
  :type '(choice
          (function :tag "Link to title and include signature, if present" denote-link-description-with-signature-and-title)
          (function :tag "Custom function like `denote-link-description-with-signature-and-title'"))
  :package-version '(denote . "2.3.0"))

;;;; Main variables

;; For character classes, evaluate: (info "(elisp) Char Classes")

(defconst denote-id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.
The note's ID is derived from the date and time of its creation.")

(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote-id-format'.")

(defconst denote-signature-regexp "==\\([^.]*?\\)\\(==.*\\|--.*\\|__.*\\|@@\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)\\|\\|\\..*\\)*$"
  "Regular expression to match the SIGNATURE field in a file name.")

(defconst denote-title-regexp "--\\([^.]*?\\)\\(==.*\\|__.*\\|@@\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)\\|\\|\\..*\\)*$"
  "Regular expression to match the TITLE field in a file name.")

(defconst denote-keywords-regexp "__\\([^.]*?\\)\\(==.*\\|--.*\\|__.*\\|@@\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)\\|\\..*\\)*$"
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

;; TODO 2024-09-03: After Denote version 3.1.0 I want to make those
;; public because (i) they are stable and (ii) we want to encourage
;; people to use them as part of their `denote-file-name-slug-functions'.
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

;; See above TODO.
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

;; See above TODO.
(defun denote--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

;; See above TODO.
(defun denote--remove-dot-characters (str)
  "Remove dot characters from STR."
  (replace-regexp-in-string "\\." "" str))

;; See above TODO.
(defun denote--trim-right-token-characters (str component)
  "Remove =, -, _ and @ from the end of STR.
The removal is done only if necessary according to COMPONENT."
  (if (eq component 'title)
      (string-trim-right str "[=@_]+")
    (string-trim-right str "[=@_-]+")))

;; See above TODO.
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
For our purposes, a note must satisfy `file-regular-p' and
`denote-filename-is-note-p'."
  (and (file-regular-p file) (denote-filename-is-note-p file)))

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
  (when-let ((dir (denote-directory))
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

(defun denote-directory-files (&optional files-matching-regexp omit-current text-only)
  "Return list of absolute file paths in variable `denote-directory'.
Files that match `denote-excluded-files-regexp' are excluded from the
list.

Files only need to have an identifier.  The return value may thus
include file types that are not implied by `denote-file-type'.

With optional FILES-MATCHING-REGEXP, restrict files to those
matching the given regular expression.

With optional OMIT-CURRENT as a non-nil value, do not include the
current Denote file in the returned list.

With optional TEXT-ONLY as a non-nil value, limit the results to
text files that satisfy `denote-filename-is-note-p'."
  (let ((files (denote--directory-get-files)))
    (when (and omit-current buffer-file-name (denote-file-has-identifier-p buffer-file-name))
      (setq files (delete buffer-file-name files)))
    (when files-matching-regexp
      (setq files (seq-filter
                   (lambda (f)
                     (string-match-p files-matching-regexp (denote-get-file-name-relative-to-denote-directory f)))
                   files)))
    (when text-only
      (setq files (seq-filter #'denote-filename-is-note-p files)))
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
            (string= id (denote-retrieve-filename-identifier file)))
          (denote-directory-files))))
    (if (length< files 2)
        (car files)
      (seq-find
       (lambda (file)
         (let ((file-extension (denote-get-file-extension file)))
           (and (denote-filename-is-note-p file)
                (or (string= (denote--file-extension denote-file-type)
                             file-extension)
                    (string= ".org" file-extension)
                    (member file-extension (denote-file-type-extensions))))))
       files))))

(defun denote-get-relative-path-by-id (id &optional directory)
  "Return relative path of ID string in `denote-directory-files'.
The path is relative to DIRECTORY (default: ‘default-directory’)."
  (file-relative-name (denote-get-path-by-id id) directory))

(defvar denote-file-history nil
  "Minibuffer history of `denote-file-prompt'.")

(defalias 'denote--file-history 'denote-file-history
  "Compatibility alias for `denote-file-history'.")

(defvar denote-file-prompt-latest-input nil
  "Latest input passed to `denote-file-prompt'.
This is used for retrieving a value that is used to set a new default at
the title prompt of `denote-open-or-create' and related commands.")

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
  (let* ((relative-files (mapcar #'denote-get-file-name-relative-to-denote-directory
                                 (denote-directory-files files-matching-regexp :omit-current)))
         (prompt (format "%s in %s: " (or prompt-text "Select FILE") (denote-directory)))
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

;;;; Keywords

(defun denote-extract-keywords-from-path (path)
  "Extract keywords from PATH and return them as a list of strings.
PATH must be a Denote-style file name where keywords are prefixed
with an underscore.

If PATH has no such keywords, return nil."
  (when-let ((kws (denote-retrieve-filename-keywords path)))
    (split-string kws "_" :omit-nulls)))

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

(defun denote-get-identifier (&optional date)
  "Convert DATE into a Denote identifier using `denote-id-format'.
DATE is parsed by `denote-valid-date-p'.  If DATE is nil, use the
current time."
  (format-time-string
   denote-id-format
   (when date (denote-valid-date-p date))))

(defvar denote--used-ids nil
  "Hash table of used identifiers.
This variable should be set only for the duration of a command.
It should stay nil otherwise.")

(defun denote-create-unique-file-identifier (file &optional date)
  "Generate a unique identifier for FILE.

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
    (denote--find-first-unused-id id)))

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

(defalias 'denote-retrieve-title-value 'denote-retrieve-front-matter-title-value
  "Alias for `denote-retrieve-front-matter-title-value'.")

(defalias 'denote-retrieve-title-line 'denote-retrieve-front-matter-title-line
  "Alias for `denote-retrieve-front-matter-title-line'.")

(defalias 'denote-retrieve-keywords-value 'denote-retrieve-front-matter-keywords-value
  "Alias for `denote-retrieve-front-matter-keywords-value'.")

(defalias 'denote-retrieve-keywords-line 'denote-retrieve-front-matter-keywords-line
  "Alias for `denote-retrieve-front-matter-keywords-line'.")

(define-obsolete-function-alias
  'denote--retrieve-title-or-filename
  'denote-retrieve-title-or-filename
  "2.3.0")

(defun denote-retrieve-title-or-filename (file type)
  "Return appropriate title for FILE given its TYPE.
This is a wrapper for `denote-retrieve-front-matter-title-value' and
`denote-retrieve-filename-title'."
  (if-let (((denote-filename-is-note-p file))
           (title (denote-retrieve-front-matter-title-value file type))
           ((not (string-blank-p title))))
      title
    (or (denote-retrieve-filename-title file)
        (and (not (denote-file-has-identifier-p file))
             (file-name-base file)))))

(defun denote--retrieve-location-in-xrefs (identifier)
  "Return list of xrefs for IDENTIFIER with their respective location.
Limit the search to text files, per `denote-directory-files' with
non-nil `text-only' parameter."
  (when-let ((files (denote-directory-files nil nil :text-only)))
    (mapcar #'xref-match-item-location (xref-matches-in-files identifier files))))

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
    (setq file-name (concat file-name extension))
    ;; Do not prepend identifier with @@ if it is the first component and has the format 00000000T000000.
    (when (and (string-prefix-p "@@" file-name)
               (string-match-p (concat "\\`" denote-id-regexp "\\'") id)) ; This is always true for now.
      (setq file-name (substring file-name 2)))
    (concat dir-path file-name)))

(defun denote--format-front-matter-title (title file-type)
  "Format TITLE according to FILE-TYPE for the file's front matter."
  (funcall (denote--title-value-function file-type) title))

(defun denote--format-front-matter-keywords (keywords file-type)
  "Format KEYWORDS according to FILE-TYPE for the file's front matter.
Apply `denote-sluggify' to KEYWORDS."
  (let ((kws (denote-sluggify-keywords keywords)))
    (funcall (denote--keywords-value-function file-type) kws)))

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
  "Prepare a new note file and return its path.

Arguments TITLE, KEYWORDS, DATE, ID, DIRECTORY, FILE-TYPE,
TEMPLATE, and SIGNATURE should be valid for note creation."
  (let* ((path (denote-format-file-name
                directory id keywords title (denote--file-extension file-type) signature))
         (buffer (find-file path))
         (header (denote--format-front-matter
                  title (denote--date date file-type) keywords
                  id
                  file-type)))
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
  'denote--valid-date
  'denote-valid-date-p
  "2.3.0")

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
        (t
         (date-to-time (denote--date-add-current-time date)))))

(defun denote-parse-date (date)
  "Return DATE as an appropriate value for the `denote' command.
Pass DATE through `denote-valid-date-p' and use its return value.
If either that or DATE is nil or an empty string, return
`current-time'."
  (or (denote-valid-date-p date) (current-time)))

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
           (when-let (((buffer-live-p buffer))
                      (file (buffer-file-name buffer))
                      ((denote-filename-is-note-p file)))
             file))
         (buffer-list))))

(defun denote--id-exists-p (identifier)
  "Return non-nil if IDENTIFIER already exists."
  (seq-some
   (lambda (file)
     (string= identifier (denote-retrieve-filename-identifier file)))
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
    (when-let ((buffer (find-buffer-visiting file)))
      (when do-save-buffer (with-current-buffer buffer (save-buffer)))
      (when do-kill-buffer (kill-buffer buffer)))))

(defvar denote-use-title nil
  "The title to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

Only ever `let' bind this, otherwise the title will always be the same
and the title prompt will be skipped.")

(defvar denote-use-keywords nil
  "The keywords to be used in a note creation command.
See the documentation of `denote' for acceptable values.  This variable
is ignored if nil.

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
        ('keywords (unless denote-use-keywords
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
TEMPLATE and SIGNATURE.

If a `denote-use-*' variable is set for a data, its value is used
instead of that of the parameter."
  (let* (;; Handle the `denote-use-*' variables
         (title (or denote-use-title title))
         (keywords (or denote-use-keywords keywords))
         (file-type (or denote-use-file-type file-type))
         (directory (or denote-use-directory directory))
         (date (or denote-use-date date))
         (template (or denote-use-template template))
         (signature (or denote-use-signature signature))
         ;; Make the data valid
         (title (or title ""))
         (file-type (denote--valid-file-type (or file-type denote-file-type)))
         (keywords (denote-keywords-sort keywords))
         (date (denote-parse-date date))
         (directory (if (denote--dir-in-denote-directory-p directory)
                        (file-name-as-directory directory)
                      (denote-directory)))
         (template (if (or (stringp template) (functionp template))
                       template
                     (or (alist-get template denote-templates) "")))
         (signature (or signature "")))
    (list title keywords file-type directory date template signature)))

;;;###autoload
(defun denote (&optional title keywords file-type directory date template signature)
  "Create a new note with the appropriate metadata and file name.

Run the `denote-after-new-note-hook' after creating the new note
and return its path.

When called interactively, the metadata and file name are prompted
according to the value of `denote-prompts'.

When called from Lisp, all arguments are optional.

- TITLE is a string or a function returning a string.

- KEYWORDS is a list of strings.  The list can be empty or the
  value can be set to nil.

- FILE-TYPE is a symbol among those described in `denote-file-type'.

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
  (pcase-let* ((`(,title ,keywords ,file-type ,directory ,date ,template ,signature)
                (denote--creation-prepare-note-data title keywords file-type directory date template signature))
               (id (denote--find-first-unused-id (denote-get-identifier date)))
               (note-path (denote--prepare-note title keywords date id directory file-type template signature)))
    (denote--handle-save-and-kill-buffer 'creation note-path nil)
    (denote--keywords-add-to-history keywords)
    (run-hooks 'denote-after-new-note-hook)
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
   "Select file TYPE: " (denote--file-type-keys) nil t
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
                     (format "Select SUBDIRECTORY [%s]: " def)
                   "Select SUBDIRECTORY: ")))
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
  (if-let (((region-active-p))
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
  (interactive (list (denote-file-prompt nil nil :no-require-match)))
  (if (and target (file-exists-p target))
      (find-file target)
    (denote--command-with-features #'denote :use-last-input-as-def-title nil nil nil)))

;;;###autoload
(defun denote-open-or-create-with-command ()
  "Like `denote-open-or-create' but use one of the `denote-commands-for-new-notes'."
  (declare (interactive-only t))
  (interactive)
  (let ((target (denote-file-prompt nil nil :no-require-match)))
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

(defun denote-filetype-heuristics (file)
  "Return likely file type of FILE.
If in the process of `org-capture', consider the file type to be that of
Org.  Otherwise, use the file extension to detect the file type of FILE.

If more than one file type correspond to this file extension, use the
first file type for which the :title-key-regexp in `denote-file-types'
matches in the file.

Return nil if the file type is not recognized."
  (cond
   ((denote--file-type-org-extra-p) 'org)
   (file
    (when-let ((extension (denote-get-file-extension-sans-encryption file))
               (types (denote--file-types-with-extension extension)))
      (if (= (length types) 1)
          (caar types)
        (or (car (seq-find
                  (lambda (type)
                    (denote--regexp-in-file-p (plist-get (cdr type) :title-key-regexp) file))
                  types))
            (caar types)))))))

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
    (when-let ((buffer (find-buffer-visiting old-name)))
      (with-current-buffer buffer
        (set-visited-file-name new-name nil t)))))

(defun denote--add-front-matter (file title keywords id file-type)
  "Prepend front matter to FILE.
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

(defun denote-rewrite-front-matter (file title keywords file-type)
  "Rewrite front matter of note after `denote-rename-file'.
The FILE, TITLE, KEYWORDS, and FILE-TYPE are given by the
renaming command and are used to construct new front matter
values if appropriate.

If `denote-rename-confirmations' contains `rewrite-front-matter',
prompt to confirm the rewriting of the front matter.  Otherwise
produce a `y-or-n-p' prompt to that effect."
  (when-let ((old-title-line (denote-retrieve-front-matter-title-line file file-type))
             (old-keywords-line (denote-retrieve-front-matter-keywords-line file file-type))
             (new-title-line (denote--get-title-line-from-front-matter title file-type))
             (new-keywords-line (denote--get-keywords-line-from-front-matter keywords file-type)))
    (with-current-buffer (find-file-noselect file)
      (when (or (not (memq 'rewrite-front-matter denote-rename-confirmations))
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
         (current-title (or (denote-retrieve-front-matter-title-value file file-type) ""))
         (current-keywords (denote-extract-keywords-from-path file))
         (keywords (denote-keywords-sort keywords))
         (directory (file-name-directory file))
         (extension (file-name-extension file :include-period))
         ;; TODO: For now, we cannot change the identifier. We retrieve
         ;; the current one or generate a new one with DATE, if non-nil.
         (id (or (denote-retrieve-filename-identifier file)
                 (denote-create-unique-file-identifier file date)))
         (new-name (denote-format-file-name directory id keywords title extension signature))
         (max-mini-window-height denote-rename-max-mini-window-height))
    (when (file-regular-p new-name)
      (user-error "The destination file `%s' already exists" new-name))
    (when (denote-rename-file-prompt file new-name)
      ;; Modify file name, buffer name, or both
      (denote-rename-file-and-buffer file new-name)
      ;; Handle front matter if new-name is of a supported type (rewrite or add front matter)
      (when (and (denote-file-has-supported-extension-p file)
                 (denote-file-is-writable-and-supported-p new-name)
                 (or (not (string= title current-title))
                     (not (equal keywords current-keywords))))
        (if (denote--edit-front-matter-p new-name file-type)
            (denote-rewrite-front-matter new-name title keywords file-type)
          (when (denote-add-front-matter-prompt new-name)
            (denote--add-front-matter new-name title keywords id file-type))))
      ;; NOTE: Maybe offer to regenerate link descriptions in other
      ;; files on rename. This could be a distinct command.
      (when denote--used-ids
        (puthash id t denote--used-ids))
      (denote--handle-save-and-kill-buffer 'rename new-name initial-state)
      (run-hooks 'denote-after-rename-file-hook))
    new-name))

(defun denote--rename-get-file-info-from-prompts-or-existing (file)
  "Retrieve existing info from FILE and prompt according to `denote-prompts'.

It is meant to be combined with `denote--rename-file' to create
renaming commands."
  (let* ((file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name))
         (file-type (denote-filetype-heuristics file))
         (date (denote-retrieve-filename-identifier file))
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
         ;; TODO: We currently prompt only if the current file has no
         ;; identifier. Eventually, we may want to allow modifying the
         ;; date/id. Then, it will be better to prompt according to
         ;; `denote-prompts`, like other components (ie remove this
         ;; condition).
         (unless (denote-file-has-identifier-p file)
           (setq date (denote-date-prompt))))))
    (list title keywords signature date)))

;;;###autoload
(defun denote-rename-file (file &optional title keywords signature date)
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

If the file does not have front matter but is among the supported
file types (per `denote-file-type'), add front matter to the top
of it and leave the buffer unsaved for further inspection.  Save
the buffer if `denote-save-buffers' is non-nil.

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
  (let ((new-name (denote--rename-file file title keywords signature date)))
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
  (let ((denote--used-ids)
        (denote-rename-confirmations nil))
    (if-let ((marks (dired-get-marked-files)))
        (progn
          (unless (seq-every-p #'denote-file-has-identifier-p marks)
            (setq denote--used-ids (denote--get-all-used-ids)))
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
  (if-let ((marks (dired-get-marked-files)))
      (let ((denote-prompts '())
            (denote-rename-confirmations nil)
            (user-input-keywords (denote-keywords-prompt keywords-prompt))
            (denote--used-ids (unless (seq-every-p #'denote-file-has-identifier-p marks)
                                (denote--get-all-used-ids))))
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

- add or rewrite existing front matter to the underlying file, if
  it is recognized as a Denote note (per `denote-file-type'),
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
type that is supported by Denote, per `denote-file-type'.

The values of `denote-rename-confirmations',
`denote-save-buffers' and `denote-kill-buffers' are respected.
Though there is no prompt to confirm the rewrite of the front
matter, since this is already done by the user.

The identifier of the file, if any, is never modified even if it
is edited in the front matter: Denote considers the file name to
be the source of truth in this case, to avoid potential breakage
with typos and the like.

Construct the file name in accordance with the user option
`denote-file-name-components-order'."
  (interactive (list (or (dired-get-filename nil t) buffer-file-name)))
  (unless (denote-file-is-writable-and-supported-p file)
    (user-error "The file is not writable or does not have a supported file extension"))
  (if-let ((file-type (denote-filetype-heuristics file))
           (front-matter-title (denote-retrieve-front-matter-title-value file file-type))
           (id (denote-retrieve-filename-identifier file)))
      (let ((denote-rename-confirmations (delq 'rewrite-front-matter denote-rename-confirmations)))
        (pcase-let* ((denote-prompts '())
                     (front-matter-keywords (denote-retrieve-front-matter-keywords-value file file-type))
                     (`(_title _keywords ,signature ,date)
                      (denote--rename-get-file-info-from-prompts-or-existing file)))
          (denote--rename-file file front-matter-title front-matter-keywords signature date)
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
                     (and (file-regular-p m)
                          (denote-file-is-writable-and-supported-p m)
                          (denote-file-has-identifier-p m)))
                   (dired-get-marked-files))))
      (progn
        (dolist (file marks)
          (denote-rename-file-using-front-matter file))
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
        (denote--add-front-matter new-name title keywords id new-file-type)
        (denote--handle-save-and-kill-buffer 'rename new-name initial-state)))))

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
                   (denote-file-has-identifier-p (buffer-substring (point) (line-end-position)))))
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

(defconst denote-faces-file-name-keywords-for-backlinks
  `(("^.+$" ,@denote-faces-matchers))
  "Keywords for fontification of file names.")

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
  (concat "\\[\\[" "denote:"  "\\(?1:" denote-id-regexp "\\)" "]" "\\["
          "\\(?2:" ".*?" "\\)" "]]")
  "Regexp to match an Org link in its context.
The format of such links is `denote-org-link-format'.")

(defvar denote-md-link-in-context-regexp
  (concat "\\[" "\\(?2:" ".*?" "\\)" "]"
          "(denote:"  "\\(?1:" denote-id-regexp "\\)" ")")
  "Regexp to match a Markdown link in its context.
The format of such links is `denote-md-link-format'.")

(defvar denote-id-only-link-in-context-regexp
  (concat "\\[\\[" "denote:"  "\\(?1:" denote-id-regexp "\\)" "]]")
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

(make-obsolete 'denote-link-signature-format nil "2.3.0")

(defun denote--link-get-description (file)
  "Return link description for FILE."
  (funcall
   (or denote-link-description-function #'denote-link-description-with-signature-and-title)
   file))

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

When called interactively, prompt for FILE using completion.  In this
case, derive FILE-TYPE from the current buffer.  FILE-TYPE is used to
determine the format of the link.

Return the DESCRIPTION of the link in the format specified by
`denote-link-description-function'.  The default value of that variable,
`denote-link-description-with-signature-and-title', uses the active
region as the DESCRIPTION, or the FILE signature in addition to its
title, or the FILE title.

With optional ID-ONLY as a non-nil argument, such as with a universal
prefix (\\[universal-argument]), insert links with just the identifier
and no further description.  In this case, the link format is always
[[denote:IDENTIFIER]].

If the DESCRIPTION is empty, format the link the same as with ID-ONLY.

When called from Lisp, FILE is a string representing a full file system
path.  FILE-TYPE is a symbol as described in `denote-file-type'.
DESCRIPTION is a string.  Whether the caller treats the active region
specially, is up to it.

Also see `denote-link-with-signature'."
  (interactive
   (let* ((file (denote-file-prompt nil "Link to FILE"))
          (file-type (denote-filetype-heuristics buffer-file-name))
          (description (when (file-exists-p file)
                         (denote--link-get-description file))))
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
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
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
        (when (string= i (denote-retrieve-filename-identifier file))
          (push file found-files))))
    found-files))

(defvar denote-link-find-file-history nil
  "History for `denote-find-link'.")

(defalias 'denote-link--find-file-history 'denote-link-find-file-history
  "Compatibility alias for `denote-link-find-file-history'.")

(defun denote-select-linked-file-prompt (files)
  "Prompt for linked file among FILES."
  (let ((file-names (mapcar #'denote-get-file-name-relative-to-denote-directory
                            files)))
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
  (when-let ((current-file (or file (buffer-file-name)))
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
  "Use minibuffer completion to visit linked file."
  (declare (interactive-only t))
  (interactive)
  (find-file
   (concat
    (denote-directory)
    (denote-select-linked-file-prompt
     (or (denote-link-return-links)
         (user-error "No links found"))))))

(defun denote-link-return-backlinks (&optional file)
  "Return list of backlinks in current or optional FILE.
Also see `denote-link-return-links'."
  (when-let ((current-file (or file (buffer-file-name)))
             (id (denote-retrieve-filename-identifier-with-error current-file)))
    (delete current-file (denote--retrieve-files-in-xrefs id))))

;; TODO 2024-09-04: Instead of using `denote-link-return-backlinks' we
;; should have a function that does not try to find all backlinks but
;; simply exits as soon as it finds one.
(defun denote--file-has-backlinks-p (file)
  "Return non-nil if FILE has backlinks."
  (not (zerop (length (denote-link-return-backlinks file)))))

;;;###autoload
(defun denote-find-backlink ()
  "Use minibuffer completion to visit backlink to current file.

Like `denote-find-link', but select backlink to follow."
  (declare (interactive-only t))
  (interactive)
  (find-file
   (denote-get-path-by-id
    (denote-extract-id-from-string
     (denote-select-linked-file-prompt
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
file.  Though see `denote-save-buffer-after-creation'."
  (interactive "P")
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
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
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let* ((type (denote-filetype-heuristics (buffer-file-name)))
         (path (denote--command-with-features command nil nil :save :in-background))
         (description (denote--link-get-description path)))
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
   (let* ((target (denote-file-prompt nil nil :no-require-match)))
     (unless (and target (file-exists-p target))
       (setq target (denote--command-with-features #'denote :use-file-prompt-as-def-title :ignore-region :save :in-background)))
     (list target current-prefix-arg)))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (denote-link target
               (denote-filetype-heuristics (buffer-file-name))
               (denote--link-get-description target)
               id-only))

(defalias 'denote-link-to-existing-or-new-note 'denote-link-or-create
  "Alias for `denote-link-or-create' command.")

;;;;; Link buttons

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

(make-obsolete
 'denote-link-buttonize-buffer
 'denote-fontify-links-mode
 "Use the `denote-fontify-links-mode', as it works better than buttonization. Since 3.0.0")

(defun denote-link-markdown-follow (link)
  "Function to open Denote file present in LINK.
To be assigned to `markdown-follow-link-functions'."
  (when (ignore-errors (string-match denote-id-regexp link))
    (funcall denote-link-button-action
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

(defun denote-link-open-at-mouse (ev)
  "Open Denote link for mouse EV click."
  (interactive "e")
  (mouse-set-point ev)
  (if-let ((id (get-text-property (point) 'denote-link-id))
           (path (denote-get-path-by-id id)))
      (funcall denote-link-button-action path)
    (error "Cannot resolve the link at point")))

(defun denote-fontify-links (&optional limit)
  "Fontify Denote links up until optional LIMIT.

Implementation based on the function `org-activate-links'."
  (catch :exit
    (when-let ((type (denote-filetype-heuristics (buffer-file-name))))
      (while (re-search-forward (denote--link-in-context-regexp type) limit t)
        (save-match-data  ; to return the matches to font-lock
          (let* ((start (match-beginning 0))
                 (end (match-end 0))
                 (visible-start (match-beginning 2))
                 (visible-end (match-end 2))
                 (id (match-string-no-properties 1))
                 (path (denote-get-path-by-id id))
                 (file-link (concat "file:" path)))
            ;; FIXME 2024-06-19: Rewrite this (unless...let...if...)
            ;; because it is hard to reason about. But it works, so no
            ;; pressure.
            (unless (let ((face (get-text-property
                                 (max (1- start) (point-min)) 'face)))
                      (if (consp face)
                          (memq 'font-lock-comment-face face)
                        (eq 'font-lock-comment-face face)))
              (let* ((properties `(face denote-faces-link
                                   mouse-face highlight
                                              keymap ,denote-link-mouse-map
                                              denote-link-id ,id
                                              help-echo ,(or (denote-retrieve-title-or-filename path type)
                                                             (concat "denote:" id))
                                              htmlize-link (:uri ,file-link)
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
              (throw :exit t))))))      ; signal success
    nil))

(defun denote-get-identifier-at-point (&optional point)
  "Return the Denote identifier at point or optional POINT."
  (when-let ((position (or point (point)))
             (face-at-point (get-text-property position 'face))
             (_ (or (eq face-at-point 'denote-faces-link)
                    (member 'denote-faces-link face-at-point))))
    (or (get-text-property position 'denote-link-id)
        (let ((property (get-text-property position 'help-echo)))
          (string-match denote-id-regexp property)
          (match-string-no-properties 0 property)))))

(defun denote--get-link-file-path-at-point (&optional point)
  "Return link to the Denote file path at point or optional POINT.
To be used as a `thing-at' provider."
  (when-let ((position (or point (point)))
             (id (get-text-property position 'denote-link-id)))
    (concat "file:" (denote-get-path-by-id id))))

(defvar thing-at-point-provider-alist)

(defun denote-fontify-links-mode-maybe ()
  "Enable `denote-fontify-links-mode' in a denote file unless in `org-mode'."
  (when (and buffer-file-name
             (not (derived-mode-p 'org-mode))
             (denote-file-is-note-p buffer-file-name))
    (denote-fontify-links-mode)))

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
        (font-lock-add-keywords nil '(denote-fontify-links))
        (setq-local thing-at-point-provider-alist
                    (append thing-at-point-provider-alist
                            '((url . denote--get-link-file-path-at-point)))))
    (remove-from-invisibility-spec 'denote-link)
    (font-lock-remove-keywords nil '(denote-fontify-links))
    (setq-local thing-at-point-provider-alist
                (delete
                 '(url . denote--get-link-file-path-at-point)
                 thing-at-point-provider-alist)))
  (font-lock-update))

;;;;; Backlinks' buffer

(define-button-type 'denote-link-backlink-button
  'follow-link t
  'action #'denote-link--backlink-find-file
  'face nil)            ; we use this face though we style it later

(defun denote-link--backlink-find-file (button)
  "Action for BUTTON to `find-file'."
  (funcall denote-link-button-action
           (concat (denote-directory)
                   (buffer-substring (button-start button) (button-end button)))))

(defun denote-link--display-buffer (buf &optional action)
  "Run `display-buffer' on BUF using optional ACTION alist.
ACTION is an alist of the form described in the user option
`denote-backlinks-display-buffer-action'."
  (display-buffer
   buf
   `(,@(or action denote-backlinks-display-buffer-action))))

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

;; NOTE 2024-07-25: This can be a minor mode, though I do not like
;; that global minor modes have to be autoloaded.  We do not need to
;; autoload a secondary piece of functionality.
;;
;; NOTE 2024-07-25: We do not need the user option if we turn this
;; into a minor mode.
;;
;; NOTE 2024-07-25: I would prefer to have a buffer-local toggle which
;; does not affect the global user preference.  The trick is to make
;; this work with `revert-buffer'.
(defun denote-backlinks-toggle-context ()
  "Show or hide the context of links in backlinks buffers.
This is the same as toggling the `denote-backlinks-show-context' user
option.

When called inside of a backlinks buffer, also revert the buffer."
  (interactive)
  (let ((state))
    (if denote-backlinks-show-context
        (setq denote-backlinks-show-context nil
              state "compact")
      (setq denote-backlinks-show-context t
            state "detailed"))
    (message "Toggled the %s view for the backlinks buffer"
             (propertize state 'face 'error))
    (when (derived-mode-p 'denote-backlinks-mode)
      (revert-buffer)
      (fit-window-to-buffer))))

(defvar denote-backlinks-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "n" #'denote-backlinks-mode-next)
    (define-key m "p" #'denote-backlinks-mode-previous)
    (define-key m "c" #'denote-backlinks-toggle-context)
    (define-key m "g" #'revert-buffer)
    m)
  "Keymap for `denote-backlinks-mode'.")

(define-derived-mode denote-backlinks-mode xref--xref-buffer-mode "Backlinks"
  "Major mode for backlinks buffers."
  :interactive nil)

(defun denote-link--prepare-backlinks (query &optional files-matching-regexp buffer-name display-buffer-action show-context)
  "Create backlinks' buffer called BUFFER-NAME for the current file matching QUERY.

With optional FILES-MATCHING-REGEXP, limit the list of files
accordingly (per `denote-directory-files').

Optional DISPLAY-BUFFER-ACTION is a `display-buffer' action and
concomitant alist, such as `denote-backlinks-display-buffer-action'.

Optional SHOW-CONTEXT displays the lines where matches for QUERY
occur.  This is the same as setting `denote-backlinks-show-context' to a
non-nil value."
  (let* ((inhibit-read-only t)
         (file (buffer-file-name))
         (backlinks-buffer (or buffer-name (format "Backlinks for '%s'" query)))
         ;; We retrieve results in absolute form and change the
         ;; absolute path to a relative path a few lines below. We
         ;; could add a suitable function and the results would be
         ;; automatically in relative form, but eventually notes may
         ;; not be all under a common directory (or project).
         (xref-file-name-display 'abs)
         (xref-alist (xref--analyze
                      (xref-matches-in-files
                       query
                       (denote-directory-files files-matching-regexp :omit-current :text-only))))
         (dir (denote-directory)))
    (unless xref-alist
      (error "No backlinks for query `%s'" query))
    ;; Change the GROUP of each item in xref-alist to a relative path
    (mapc (lambda (x)
            (setf (car x) (denote-get-file-name-relative-to-denote-directory (car x))))
          xref-alist)
    (with-current-buffer (get-buffer-create backlinks-buffer)
      (erase-buffer)
      (denote-backlinks-mode)
      ;; In the backlinks buffer, the values of variables set in a
      ;; `.dir-locals.el` do not apply.  We need to set `denote-directory' in
      ;; the backlinks buffer because the buttons depend on it.  Moreover, its
      ;; value is overwritten after enabling the major mode, so it needs to be
      ;; set after.
      (setq-local denote-directory dir)
      (setq overlay-arrow-position nil)
      (goto-char (point-min))
      (if (or show-context denote-backlinks-show-context)
          (xref--insert-xrefs xref-alist)
        (mapc (lambda (x)
                (insert (car x))
                (make-button (line-beginning-position) (line-end-position) :type 'denote-link-backlink-button)
                (newline))
              xref-alist)
        (font-lock-add-keywords nil denote-faces-file-name-keywords-for-backlinks t))
      (goto-char (point-min))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (when-let ((buffer-file-name file))
                      (denote-link--prepare-backlinks query files-matching-regexp buffer-name display-buffer-action show-context)))))
    (denote-link--display-buffer backlinks-buffer display-buffer-action)))

(defun denote--backlinks-get-buffer-name (file id)
  "Format a buffer name for `denote-backlinks'.
Use FILE to detect a suitable title with which to name the buffer.  Else
use the ID."
  (if-let ((type (denote-filetype-heuristics file))
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
  (if-let ((file buffer-file-name))
      (when-let ((id (denote-retrieve-filename-identifier-with-error file)))
        (denote-link--prepare-backlinks id nil (denote--backlinks-get-buffer-name file id)))
    (user-error "Buffer `%s' is not associated with a file" (current-buffer))))

(defalias 'denote-show-backlinks-buffer 'denote-backlinks
  "Alias for `denote-backlinks' command.")

;;;;; Add links matching regexp

(defvar denote-link--prepare-links-format "- %s\n"
  "Format specifiers for `denote-link-add-links'.")

(make-obsolete-variable 'denote-link-add-links-sort nil "3.1.0")

(defun denote-link--prepare-links (files current-file-type id-only &optional no-sort include-date)
  "Prepare links to FILES from CURRENT-FILE-TYPE.
When ID-ONLY is non-nil, use a generic link format.

With optional NO-SORT do not try to sort the inserted lines.
Otherwise sort lines while accounting for `denote-link-add-links-sort'.

Optional INCLUDE-DATE has the same meaning as in `denote-format-link'."
  (let ((links))
    (dolist (file files)
      (let* ((description (denote--link-get-description file))
             (link (denote-format-link file description current-file-type id-only include-date))
             (link-as-list-item (format denote-link--prepare-links-format link)))
         (push link-as-list-item links)))
    (if no-sort
        (nreverse links)
      (sort links #'string-collate-lessp))))

(defun denote-link--insert-links (files current-file-type &optional id-only no-sort include-date)
  "Insert at point a typographic list of links matching FILES.

With CURRENT-FILE-TYPE as a symbol among those specified in
`denote-file-type' (or the `car' of each element in
`denote-file-types'), format the link accordingly.  With a nil or
unknown non-nil value, default to the Org notation.

With ID-ONLY as a non-nil value, produce links that consist only
of the identifier, thus deviating from CURRENT-FILE-TYPE.

Optional NO-SORT is passed to `denote-link--prepare-links'.

Optional INCLUDE-DATE has the same meaning as in `denote-format-link'."
  (when-let ((links (denote-link--prepare-links files current-file-type id-only no-sort include-date)))
    (dolist (link links)
      (insert link))))

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
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (let ((file-type (denote-filetype-heuristics (buffer-file-name))))
    (if-let ((files (denote-directory-files regexp :omit-current)))
        (denote-link--insert-links files file-type id-only)
      (message "No links matching `%s'" regexp))))

(defalias 'denote-link-insert-links-matching-regexp 'denote-add-links
  "Alias for `denote-add-links' command.")

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
  (when (null files)
    (user-error "No note files to link to"))
  (with-current-buffer buffer
    (unless (or (denote--file-type-org-extra-p)
                (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
      (user-error "The buffer's file type is not recognized by Denote")))
  (when (y-or-n-p (format "Create links at point in %s?" buffer))
    (with-current-buffer buffer
      (insert (denote-link--prepare-links
               files
               (denote-filetype-heuristics (buffer-file-name))
               id-only)))))

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
  "Resolve LINK to target file, with or without additioanl query terms.
With optional FULL-DATA return a list in the form of (path id query)."
  (let* ((query (and (string-match "::\\(.*\\)\\'" link)
                     (match-string 1 link)))
         (id (if (and query (not (string-empty-p query)))
                 (substring link 0 (match-beginning 0))
               link))
         (path (denote-get-path-by-id id)))
    (cond
     (full-data
      (list path id query))
     ((and query (not (string-empty-p query)))
      (concat path "::" query))
     (t path))))

;;;###autoload
(defun denote-link-ol-follow (link)
  "Find file of type `denote:' matching LINK.
LINK is the identifier of the note, optionally followed by a
query option akin to that of standard Org `file:' link types.
Read Info node `(org) Query Options'.

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
(defun denote-link-ol-store ()
  "Handler for `org-store-link' adding support for denote: links.
Also see the user option `denote-org-store-link-to-heading'."
  (when-let ((file (buffer-file-name))
             ((denote-file-is-note-p file))
             (file-id (denote-retrieve-filename-identifier file))
             (description (denote--link-get-description file)))
    (let ((heading-links (and denote-org-store-link-to-heading
                              (derived-mode-p 'org-mode)
                              (denote--org-capture-link-specifiers-p))))
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
         (query (nth 2 path-id))
         (anchor (file-name-sans-extension path))
         (desc (cond
                (description)
                (query (format "denote:%s::%s" id query))
                (t (concat "denote:" id)))))
    (cond
     ((eq format 'html)
      (if query
          (format "<a href=\"%s.html%s\">%s</a>" anchor query desc)
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
               (`(,title ,keywords _ ,directory ,date ,template ,signature)
                (denote--creation-prepare-note-data title keywords 'org directory date template signature))
               (id (denote--find-first-unused-id (denote-get-identifier date)))
               (front-matter (denote--format-front-matter
                              title (denote--date nil 'org) keywords id 'org))
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
  (when-let ((file denote-last-path)
             ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

(provide 'denote)
;;; denote.el ends here
