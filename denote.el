;;; denote.el --- Simple notes with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote
;; Version: 0.5.1
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
(require 'xdg)
(eval-when-compile (require 'subr-x))

(defgroup denote ()
  "Simple notes with an efficient file-naming scheme."
  :group 'files)

;;;; User options

;; About the autoload: (info "(elisp) File Local Variables")

;;;###autoload (put 'denote-directory 'safe-local-variable (lambda (val) (or (eq val 'local) (eq val 'default-directory))))
(defcustom denote-directory (expand-file-name "notes" (xdg-user-dir "DOCUMENTS"))
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
  :package-version '(denote . "0.5.0")
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
  `current-time'.

- `template': Prompts for a KEY among `denote-templates'.  The
  value of that KEY is used to populate the new note with
  content, which is added after the front matter.

The prompts occur in the given order.

If the value of this user option is nil, no prompts are used.
The resulting file name will consist of an identifier (i.e. the
date and time) and a supported file type extension (per
`denote-file-type').

Recall that Denote's standard file-naming scheme is defined as
follows (read the manual for the technicalities):

    DATE--TITLE__KEYWORDS.EXT

If either or both of the `title' and `keywords' prompts are not
included in the value of this variable, file names will be any of
those permutations:

    DATE.EXT
    DATE--TITLE.EXT
    DATE__KEYWORDS.EXT

When in doubt, always include the `title' and `keywords' prompts.

Finally, this user option only affects the interactive use of the
`denote' command (advanced users can call it from Lisp).  For
ad-hoc interactive actions that do not change the default
behaviour of the `denote' command, users can invoke these
convenience commands: `denote-type', `denote-subdirectory',
`denote-date'."
  :group 'denote
  :package-version '(denote . "0.5.0")
  :link '(info-link "(denote) The denote-prompts option")
  :type '(radio (const :tag "Use no prompts" nil)
                (set :tag "Available prompts" :greedy t
                     (const :tag "Title" title)
                     (const :tag "Keywords" keywords)
                     (const :tag "Date" date)
                     (const :tag "File type extension" file-type)
                     (const :tag "Subdirectory" subdirectory)
                     (const :tag "Template" template))))

(defcustom denote-sort-keywords t
  "Whether to sort keywords in new files.

When non-nil, the keywords of `denote' are sorted with
`string-lessp' regardless of the order they were inserted at the
minibuffer prompt.

If nil, show the keywords in their given order."
  :group 'denote
  :package-version '(denote . "0.1.0")
  :type 'boolean)

(defcustom denote-allow-multi-word-keywords t
  "If non-nil keywords can consist of multiple words.
Words are automatically separated by a hyphen when using the
`denote' command or related.  The hyphen is the only legal
character---no spaces, no other characters.  If, for example, the
user types <word1_word2> or <word1 word2>, it is converted to
<word1-word2>.

When nil, do not allow keywords to consist of multiple words.
Reduce them to a single word, such as by turning <word1_word2> or
<word1 word2> into <word1word2>."
  :group 'denote
  :package-version '(denote . "0.1.0")
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
  :package-version '(denote . "0.1.0")
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

;;;; Main variables

;; For character classes, evaluate: (info "(elisp) Char Classes")
(defconst denote--id-format "%Y%m%dT%H%M%S"
  "Format of ID prefix of a note's filename.")

(defconst denote--id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)"
  "Regular expression to match `denote--id-format'.")

(defconst denote--title-regexp "--\\([[:alnum:][:nonascii:]-]*\\)"
  "Regular expression to match the title field.")

(defconst denote--keywords-regexp "__\\([[:alnum:][:nonascii:]_-]*\\)"
  "Regular expression to match keywords.")

(defconst denote--extension-regexp "\\.\\(org\\|md\\|txt\\)"
  "Regular expression to match supported Denote extensions.")

(defconst denote--punctuation-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*"
  "Punctionation that is removed from file names.
We consider those characters illegal for our purposes.")

(defvar denote-punctuation-excluded-extra-regexp nil
  "Additional punctuation that is removed from file names.
This variable is for advanced users who need to extend the
`denote--punctuation-regexp'.  Once we have a better
understanding of what we should be omitting, we will update
things accordingly.")

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
    (file-name-as-directory (expand-file-name path))))

(defun denote--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string
   (concat denote--punctuation-regexp denote-punctuation-excluded-extra-regexp)
   "" str))

(defun denote--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
   "-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

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
  (mapcar (if denote-allow-multi-word-keywords
              #'denote--sluggify
            #'denote--sluggify-and-join)
          keywords))

(defun denote--file-empty-p (file)
  "Return non-nil if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

;; TODO 2022-08-11: In light of `denote--writable-and-supported-p', we
;; should either harden `denote--only-note-p' to also check for a
;; `denote-directory' or decide how to merge the two functions.  I think
;; hardening this one is more appropriate.
;;
;; There are two different needs:
;; - When converting a file to Denote, we need relaxed conditionality.
;; = When we truly need a  "note", we have to be more strict.
(defun denote--only-note-p (file)
  "Make sure FILE is an actual Denote note."
  (let ((file-name (file-name-nondirectory file)))
    (and (not (file-directory-p file))
         (file-regular-p file)
         (string-match-p (concat "\\`" denote--id-regexp
                                 ".*" denote--extension-regexp
                                 "\\(.gpg\\)?"
                                 "\\'")
                         file-name)
         ;; Can this ever be t given the above?
         (not (string-match-p "[#~]\\'" file)))))

(defun denote--file-has-identifier-p (file)
  "Return non-nil if FILE has a Denote identifier."
  (let ((file-name (file-name-nondirectory file)))
    (string-match-p denote--id-regexp (format "\\`%s" file-name))))

(defun denote--file-supported-extension-p (file)
  "Return non-nil if FILE has supported extension."
  (string-match-p (format "%s\\(.gpg\\)?\\'" denote--extension-regexp) file))

(defun denote--file-regular-writable-p (file)
  "Return non-nil if FILE is regular and writable."
  (and (file-regular-p file)
       (file-writable-p file)))

(defun denote--writable-and-supported-p (file)
  "Return non-nil if FILE is writable and has supported extension."
  (and (denote--file-regular-writable-p file)
       (denote--file-supported-extension-p file)))

(defun denote--file-name-relative-to-denote-directory (file)
  "Return file name of FILE relative to the variable `denote-directory'.
FILE must be an absolute path."
  (when-let* ((dir (denote-directory))
              ((file-name-absolute-p file))
              (file-name (expand-file-name file))
              ((string-prefix-p dir file-name)))
    (substring-no-properties file-name (length dir))))

(defun denote--default-dir-has-denote-prefix ()
  "Test `default-directory' for variable `denote-directory' prefix."
  (string-prefix-p (denote-directory)
                   (expand-file-name default-directory)))

(defun denote--current-file-is-note-p ()
  "Return non-nil if current file likely is a Denote note."
  (and (or (string-match-p denote--id-regexp (buffer-file-name))
           (string-match-p denote--id-regexp (buffer-name)))
       (denote--default-dir-has-denote-prefix)))

(defun denote--directory-files ()
  "List expanded note files."
  (mapcar
   (lambda (s) (expand-file-name s))
   (seq-remove
    (lambda (f)
      (not (denote--file-has-identifier-p f)))
    (directory-files-recursively (denote-directory) directory-files-no-dot-files-regexp t))))

(defun denote--get-note-path-by-id (id)
  "Return the absolute path of ID note in variable `denote-directory'."
  (seq-find
   (lambda (f)
     (string-prefix-p id (file-name-nondirectory f)))
   (denote--directory-files)))

(defun denote--directory-files-matching-regexp (regexp)
  "Return list of files matching REGEXP.
Files are those which satisfy `denote--file-has-identifier-p' and
`denote--file-name-relative-to-denote-directory'."
  (seq-filter
   (lambda (f)
     (string-match-p regexp (denote--file-name-relative-to-denote-directory f)))
   (denote--directory-files)))

;;;; Keywords

(defun denote--extract-keywords-from-path (path)
  "Extract keywords from PATH."
  (let* ((file-name (file-name-nondirectory path))
         (kws (when (string-match denote--keywords-regexp file-name)
                (match-string-no-properties 1 file-name))))
    (when kws
      (split-string kws "_"))))

(defun denote--inferred-keywords ()
  "Extract keywords from `denote--directory-files'.
This function returns duplicates.  The `denote-keywords' is the
one that doesn't."
  (mapcan (lambda (p)
            (denote--extract-keywords-from-path p))
          (denote--directory-files)))

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
  (delete-dups
   (completing-read-multiple
    "File keyword: " keywords
    nil nil nil 'denote--keyword-history)))

(defun denote--keywords-prompt ()
  "Prompt for one or more keywords.
In the case of multiple entries, those are separated by the
`crm-sepator', which typically is a comma.  In such a case, the
output is sorted with `string-lessp'."
  (let ((choice (denote--keywords-crm (denote-keywords))))
    (if denote-sort-keywords
        (sort choice #'string-lessp)
      choice)))

(defun denote--keywords-combine (keywords)
  "Format KEYWORDS output of `denote--keywords-prompt'."
  (mapconcat #'downcase keywords "_"))

(defun denote--keywords-add-to-history (keywords)
  "Append KEYWORDS to `denote--keyword-history'."
  (mapc (lambda (kw)
          (add-to-history 'denote--keyword-history kw))
        (delete-dups keywords)))

;;;; File types

;; TODO 2022-08-10: These are `defvar' and not `defcustom' because
;; tweaks to them need to be done with care.  Though there is demand for
;; modifying the front matter, so perhaps we should reconsider.

(defvar denote-toml-front-matter
  "+++
title      = %s
date       = %s
tags       = %s
identifier = %S
+++\n\n"
  "TOML front matter.")

(defvar denote-yaml-front-matter
  "---
title:      %s
date:       %s
tags:       %s
identifier: %S
---\n\n"
  "YAML front matter.")

(defvar denote-text-front-matter
  "title:      %s
date:       %s
tags:       %s
identifier: %s
---------------------------\n\n"
  "Plain text front matter.")

(defvar denote-org-front-matter
  "#+title:      %s
#+date:       %s
#+filetags:   %s
#+identifier: %s
\n"
  "Org front matter.")

(defun denote--surround-with-quotes (s)
  "Surround string S with quotes."
  (format "%S" s))

(defun denote--trim-whitespace (s)
  "Trim whitespace around string S."
  (let ((trims "[ \t\n\r]+"))
    (string-trim s trims trims)))

(defun denote--trim-quotes (s)
  "Trim quotes around string S."
  (let ((trims "[\"']+"))
    (string-trim s trims trims)))

(defun denote--trim-whitespace-then-quotes (s)
  "Trim whitespace then quotes around string S."
  (denote--trim-quotes (denote--trim-whitespace s)))

(defun denote--format-keywords-for-md-front-matter (keywords)
  "Format front matter KEYWORDS for markdown file type."
  (format "[%s]" (mapconcat (lambda (k) (format "%S" k)) keywords ", ")))

(defun denote--format-keywords-for-text-front-matter (keywords)
  "Format front matter KEYWORDS for text file type."
  (string-join keywords "  "))

(defun denote--format-keywords-for-org-front-matter (keywords)
  "Format front matter KEYWORDS for org file type."
  (format ":%s:" (string-join keywords ":")))

(defun denote--extract-keywords-from-front-matter (keywords-string)
  "Extract keywords list from front matter KEYWORDS-STRING."
  (split-string keywords-string "[:,\s]+" t "[][ \"']+"))

(defvar denote-file-types
  ;; If denote-file-type is nil, we use the first element
  ;; of denote-file-types for new note creation, which we want
  ;; to be org by default.
  `((org . (:extension ".org"
            :front-matter ,denote-org-front-matter
            :title-key-regexp "^#\\+title\\s-*:"
            :title-value-function identity
            :title-value-reverse-function denote--trim-whitespace
            :keywords-key-regexp "^#\\+filetags\\s-*:"
            :keywords-value-function denote--format-keywords-for-org-front-matter
            :keywords-value-reverse-function denote--extract-keywords-from-front-matter))
    (markdown-toml . (:extension ".md"
                      :front-matter ,denote-toml-front-matter
                      :title-key-regexp "^title\\s-*="
                      :title-value-function denote--surround-with-quotes
                      :title-value-reverse-function denote--trim-whitespace-then-quotes
                      :keywords-key-regexp "^tags\\s-*="
                      :keywords-value-function denote--format-keywords-for-md-front-matter
                      :keywords-value-reverse-function denote--extract-keywords-from-front-matter))
    (markdown-yaml . (:extension ".md"
                      :front-matter ,denote-yaml-front-matter
                      :title-key-regexp "^title\\s-*:"
                      :title-value-function denote--surround-with-quotes
                      :title-value-reverse-function denote--trim-whitespace-then-quotes
                      :keywords-key-regexp "^tags\\s-*:"
                      :keywords-value-function denote--format-keywords-for-md-front-matter
                      :keywords-value-reverse-function denote--extract-keywords-from-front-matter))
    (text . (:extension ".txt"
             :front-matter ,denote-yaml-front-matter
             :title-key-regexp "^title\\s-*:"
             :title-value-function identity
             :title-value-reverse-function denote--trim-whitespace
             :keywords-key-regexp "^tags\\s-*:"
             :keywords-value-function denote--format-keywords-for-text-front-matter
             :keywords-value-reverse-function denote--extract-keywords-from-front-matter)))
  "Alist for Denote's file types.
Each element is of the form (TYPE-SYMB . TYPE-INFO).

TYPE-INFO is a list of 8 elements:

  extension: The file extension, as a string.

  front-matter: The type's front matter, as a string.

  title-key-regexp: The regexp used to retrieve the title line in
    a file. The first line matching this regexp is considered the
    title line.

  title-value-function: The function used to format the raw title
    string for inclusion in the front matter.

  title-value-reverse-function: The function used to retrieve the raw title
    string from the string in the front matter.

  keywords-key-regexp: The regexp used to retrieve the keywords
    line in a file. The first line matching this regexp is
    considered the keywords line.

  keywords-value-function: The function used to format the
    keywords list for inclusion in the front matter.

  keywords-value-reverse-function: The function used to retrieve
    the keywords list from the string in the front matter.")

(defun denote--file-extension (file-type)
  "Return file type extension based on FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :extension))

(defun denote--front-matter (file-type)
  "Return front matter based on FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :front-matter))

(defun denote--title-key-regexp (file-type)
  "Return the title key regexp associated to FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :title-key-regexp))

(defun denote--title-value-function (file-type)
  "Function to convert the title string to a front matter title.
Based on FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :title-value-function))

(defun denote--title-value-reverse-function (file-type)
  "Function to convert a front matter title to the title string.
Based on FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :title-value-reverse-function))

(defun denote--keywords-key-regexp (file-type)
  "Return the keywords key regexp associated to FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :keywords-key-regexp))

(defun denote--keywords-value-function (file-type)
  "Function to convert the keywords string to a front matter keywords.
Based on FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :keywords-value-function))

(defun denote--keywords-value-reverse-function (file-type)
  "Function to convert a front matter keywords to the keywords list.
Based on FILE-TYPE."
  (plist-get (alist-get file-type denote-file-types) :keywords-value-reverse-function))

(defun denote--get-title-line-from-front-matter (title file-type)
  "Retrieve title line from front matter based on FILE-TYPE.
Format TITLE in the title line. The returned line does not
contain the newline."
  (let ((front-matter (denote--format-front-matter title "" nil "" file-type))
        (key-regexp (denote--title-key-regexp file-type)))
    (with-temp-buffer
      (insert front-matter)
      (goto-char (point-min))
      (when (re-search-forward key-regexp nil t 1)
        (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun denote--get-keywords-line-from-front-matter (keywords file-type)
  "Retrieve keywords line from front matter based on FILE-TYPE.
Format KEYWORDS in the keywords line. The returned line does not
contain the newline."
  (let ((front-matter (denote--format-front-matter "" "" keywords "" file-type))
        (key-regexp (denote--keywords-key-regexp file-type)))
    (with-temp-buffer
      (insert front-matter)
      (goto-char (point-min))
      (when (re-search-forward key-regexp nil t 1)
        (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

;;;; Front matter or content retrieval functions

(defun denote--retrieve-filename-identifier (file)
  "Extract identifier from FILE name."
  (if (file-exists-p file)
      (progn
        (string-match denote--id-regexp file)
        (match-string 0 file))
    (error "Cannot find `%s' as a file" file)))

(defun denote--retrieve-title-value (file file-type)
  "Return title value from FILE according to FILE-TYPE."
  ;; NOTE 2022-08-11: The `or' is superfluous, but I am keeping it as a
  ;; reminder.  See TODO comment above `denote--only-note-p'
  (when (or (denote--writable-and-supported-p file)
            (denote--only-note-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
        (funcall (denote--title-value-reverse-function file-type)
                 (buffer-substring-no-properties (point) (point-at-eol)))))))

(defun denote--retrieve-title-line (file file-type)
  "Return title line from FILE according to FILE-TYPE."
  ;; NOTE 2022-08-11: The `or' is superfluous, but I am keeping it as a
  ;; reminder.  See TODO comment above `denote--only-note-p'
  (when (or (denote--writable-and-supported-p file)
            (denote--only-note-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (denote--title-key-regexp file-type) nil t 1)
        (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun denote--retrieve-keywords-value (file file-type)
  "Return keywords value from FILE according to FILE-TYPE.
If optional KEY is non-nil, return the key instead."
  ;; NOTE 2022-08-11: The `or' is superfluous, but I am keeping it as a
  ;; reminder.  See TODO comment above `denote--only-note-p'
  (when (or (denote--writable-and-supported-p file)
            (denote--only-note-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
        (funcall (denote--keywords-value-reverse-function file-type)
                 (buffer-substring-no-properties (point) (point-at-eol)))))))

(defun denote--retrieve-keywords-line (file file-type)
  "Return keywords line from FILE according to FILE-TYPE."
  ;; NOTE 2022-08-11: The `or' is superfluous, but I am keeping it as a
  ;; reminder.  See TODO comment above `denote--only-note-p'
  (when (or (denote--writable-and-supported-p file)
            (denote--only-note-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
        (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun denote--retrieve-read-file-prompt ()
  "Prompt for regular file in variable `denote-directory'."
  (read-file-name "Select note: " (denote-directory) nil nil nil
                  (lambda (f)
                    (or (denote--file-has-identifier-p f)
                        (file-directory-p f)))))

(defun denote--retrieve-files-in-output (files)
  "Return list of FILES from `find' output."
  (seq-filter
   (lambda (f)
     (denote--only-note-p f))
   files))

(defun denote--retrieve-xrefs (identifier)
  "Return xrefs of IDENTIFIER in variable `denote-directory'.
The xrefs are returned as an alist."
  (xref--alistify
   (xref-matches-in-files identifier (denote--directory-files))
   (lambda (x)
     (xref-location-group (xref-item-location x)))))

(defun denote--retrieve-files-in-xrefs (xrefs)
  "Return sorted file names sans directory from XREFS.
Parse `denote--retrieve-xrefs'."
  (sort
   (delete-dups (mapcar #'car xrefs))
   #'string-lessp))

(defun denote--retrieve-process-grep (identifier)
  "Process lines matching IDENTIFIER and return list of files."
  (denote--retrieve-files-in-output
   (delete (buffer-file-name) (denote--retrieve-files-in-xrefs
                               (denote--retrieve-xrefs identifier)))))

;;;; New note

;;;;; Common helpers for new notes

(defun denote--format-file (path id keywords title-slug extension)
  "Format file name.
PATH, ID, KEYWORDS, TITLE-SLUG are expected to be supplied by
`denote' or equivalent: they will all be converted into a single
string.  EXTENSION is the file type extension, as a string."
  (let ((kws (denote--keywords-combine keywords))
        (file-name (concat path id)))
    (when (and title-slug (not (string-empty-p title-slug)))
      (setq file-name (concat file-name "--" title-slug)))
    (when keywords
      (setq file-name (concat file-name "__" kws)))
    (concat file-name extension)))

(defun denote--format-front-matter-title (title file-type)
  "Format TITLE according to FILE-TYPE for the file's front matter."
  (funcall (denote--title-value-function file-type) title))

(defun denote--format-front-matter-keywords (keywords file-type)
  "Format KEYWORDS according to FILE-TYPE for the file's front matter.
Apply `downcase' to KEYWORDS."
  (let ((kw (mapcar #'downcase (denote--sluggify-keywords keywords))))
    (funcall (denote--keywords-value-function file-type) kw)))

(make-obsolete-variable 'denote-text-front-matter-delimiter nil "0.6.0")

(defun denote--format-front-matter (title date keywords id filetype)
  "Front matter for new notes.

TITLE, DATE, KEYWORDS, FILENAME, ID are all strings which are
provided by `denote'.  FILETYPE is one of the values of
`denote-file-type'."
  (let ((title (denote--format-front-matter-title title filetype))
        (kws (denote--format-front-matter-keywords keywords filetype)))
    (format (denote--front-matter filetype) title date kws id)))

(defun denote--path (title keywords dir id file-type)
  "Return path to new file with ID, TITLE, KEYWORDS and FILE-TYPE in DIR."
  (denote--format-file
   dir id
   (denote--sluggify-keywords keywords)
   (denote--sluggify title)
   (denote--file-extension file-type)))

;; Adapted from `org-hugo--org-date-time-to-rfc3339' in the `ox-hugo'
;; package: <https://github.com/kaushalmodi/ox-hugo>.
(defun denote--date-rfc3339 (date)
  "Format DATE using the RFC3339 specification."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" date)))

(defun denote--date-org-timestamp (date)
  "Format DATE using the Org inactive timestamp notation."
  (format-time-string "[%F %a %R]" date))

(defun denote--date-iso-8601 (date)
  "Format DATE according to ISO 8601 standard."
  (format-time-string "%F" date))

(defun denote--date (date file-type)
  "Expand DATE in an appropriate format for FILE-TYPE."
  (let ((format denote-date-format))
    (cond
     ((stringp format)
      (format-time-string format date))
     ((or (eq file-type 'markdown-toml)
          (eq file-type 'markdown-yaml))
      (denote--date-rfc3339 date))
     ((eq file-type 'text)
      (denote--date-iso-8601 date))
     (t
      (denote--date-org-timestamp date)))))

(defun denote--prepare-note (title keywords date id directory file-type template)
  "Prepare a new note file.

Arguments TITLE, KEYWORDS, DATE, ID, DIRECTORY, FILE-TYPE,
and TEMPLATE should be valid for note creation."
  (let* ((path (denote--path title keywords directory id file-type))
         (buffer (find-file path))
         (header (denote--format-front-matter
                  title (denote--date date file-type) keywords
                  (format-time-string denote--id-format date)
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
  "Return file names of active buffers."
  (seq-filter
   (lambda (name) (denote--only-note-p name))
   (delq nil
         (mapcar
          (lambda (buf)
            (buffer-file-name buf))
          (buffer-list)))))

;; In normal usage, this should only be relevant for `denote-date',
;; otherwise the identifier is always unique (we trust that no-one
;; writes multiple notes within fractions of a second).  Though the
;; `denote' command does call `denote--barf-duplicate-id'.
(defun denote--id-exists-p (identifier)
  "Return non-nil if IDENTIFIER already exists."
  (seq-some (lambda (file)
              (string-prefix-p identifier (file-name-nondirectory file)))
            (append (denote--directory-files)
                    (denote--buffer-file-names))))

(defun denote--barf-duplicate-id (identifier)
  "Throw a user-error if IDENTIFIER already exists."
  (when (denote--id-exists-p identifier)
    (user-error "`%s' already exists; aborting new note creation" identifier)))

(defun denote--subdirs ()
  "Return list of subdirectories in variable `denote-directory'."
  (seq-remove
   (lambda (filename)
     (or (not (file-directory-p filename))
         (string-match-p "\\`\\." (denote--file-name-relative-to-denote-directory filename))
         (string-match-p "/\\." (denote--file-name-relative-to-denote-directory filename))))
   (directory-files-recursively (denote-directory) ".*" t t)))

;;;;; The `denote' command and its prompts

;;;###autoload
(defun denote (&optional title keywords file-type subdirectory date template)
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
  inserted to the newly created buffer after the front matter."
  (interactive
   (let ((args (make-vector 6 nil)))
     (dolist (prompt denote-prompts)
       (pcase prompt
         ('title (aset args 0 (denote--title-prompt)))
         ('keywords (aset args 1 (denote--keywords-prompt)))
         ('file-type (aset args 2 (denote--file-type-prompt)))
         ('subdirectory (aset args 3 (denote--subdirs-prompt)))
         ('date (aset args 4 (denote--date-prompt)))
         ('template (aset args 5 (denote--template-prompt)))))
     (append args nil)))
  (let* ((title (or title ""))
         (file-type (denote--valid-file-type (or file-type denote-file-type)))
         (date (if (or (null date) (string-empty-p date))
                   (current-time)
                 (denote--valid-date date)))
         (id (format-time-string denote--id-format date))
         (directory (if (denote--dir-in-denote-directory-p subdirectory)
                        (file-name-as-directory subdirectory)
                      (denote-directory)))
         (template (if (stringp template)
                       template
                     (or (alist-get template denote-templates) ""))))
    (denote--barf-duplicate-id id)
    (denote--prepare-note title keywords date id directory file-type template)
    (denote--keywords-add-to-history keywords)))

(defvar denote--title-history nil
  "Minibuffer history of `denote--title-prompt'.")

(defun denote--title-prompt (&optional default-title)
  "Read file title for `denote'.
With optional DEFAULT-TITLE use it as the default value."
  (let ((format (if default-title
                    (format "File title [%s]: " default-title)
                  "File title: ")))
    (read-string format nil 'denote--title-history default-title)))

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

(defvar denote--date-history nil
  "Minibuffer history of `denote--date-prompt'.")

(defun denote--date-prompt ()
  "Prompt for date."
  (read-string
   "DATE and TIME for note (e.g. 2022-06-16 14:30): "
   nil 'denote--date-history))

(defvar denote--subdir-history nil
  "Minibuffer history of `denote--subdirs-prompt'.")

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

(defun denote--subdirs-prompt ()
  "Handle user input on choice of subdirectory."
  (let* ((root (directory-file-name (denote-directory)))
         (subdirs (denote--subdirs))
         (dirs (push root subdirs)))
    (denote--subdirs-completion-table dirs)))

(defvar denote--template-history nil
  "Minibuffer history of `denote--template-prompt'.")

(defun denote--template-prompt ()
  "Prompt for template KEY from `denote-templates'."
  (let ((templates denote-templates))
    (alist-get
     (intern
      (completing-read
       "Select template KEY: " (mapcar #'car templates)
       nil t nil 'denote--template-history))
     templates)))

;;;;; Convenience commands as `denote' variants

(defalias 'denote-create-note (symbol-function 'denote))

;;;###autoload
(defun denote-type ()
  "Create note while prompting for a file type.

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(file-type title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(file-type title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-type (symbol-function 'denote-type))

;;;###autoload
(defun denote-date ()
  "Create note while prompting for a date.

The date can be in YEAR-MONTH-DAY notation like 2022-06-30 or
that plus the time: 2022-06-16 14:30

This is the equivalent to calling `denote' when `denote-prompts'
is set to \\='(date title keywords)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(date title keywords)))
    (call-interactively #'denote)))

(defalias 'denote-create-note-using-date (symbol-function 'denote-date))

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

(defalias 'denote-create-note-in-subdirectory (symbol-function 'denote-subdirectory))

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

(defalias 'denote-create-note-with-template (symbol-function 'denote-template))

;;;; Note modification

;;;;; Common helpers for note modifications

(defun denote--file-types-with-extension (extension)
  "Return only the entries of `denote-file-types' with EXTENSION.
See the format of `denote-file-types'."
  (seq-filter (lambda (type)
                (string-equal (plist-get (cdr type) :extension) extension))
              denote-file-types))

(defun denote--filetype-heuristics (file)
  "Return likely file type of FILE.
Use the file extension to detect the file type of the file.
If more than one file type correspond to this file extension,
use the first file type for which the key-title-kegexp matches
in the file.
Else, if nothing works, the file type is assumed to be the first
in `denote-file-types'."
  (let* ((file-type)
         (extension (file-name-extension file t))
         (types (denote--file-types-with-extension extension)))
    (if (= (length types) 1)
        (setq file-type (caar types))
      (when-let ((found-type (seq-find
                              (lambda (type)
                                (denote--regexp-in-file-p (plist-get (cdr type) :title-key-regexp) file))
                              types)))
        (setq file-type (car found-type))))
    (unless file-type
      (setq file-type (caar denote-file-types)))
    file-type))

(defun denote--file-attributes-time (file)
  "Return `file-attribute-modification-time' of FILE as identifier."
  (format-time-string
   denote--id-format
   (file-attribute-modification-time (file-attributes file))))

(defun denote--file-name-id (file)
  "Return FILE identifier, else generate one."
  (cond
   ((string-match denote--id-regexp file)
    (substring file (match-beginning 0) (match-end 0)))
   ((denote--file-attributes-time file))
   (t (format-time-string denote--id-format))))

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

(defun denote--rename-file (old-name new-name)
  "Rename file named OLD-NAME to NEW-NAME.
Update Dired buffers if the file is renamed."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (rename-file old-name new-name nil)
    (denote--rename-buffer old-name new-name)))

(defun denote--add-front-matter (file title keywords id file-type)
  "Prepend front matter to FILE if `denote--only-note-p'.
The TITLE, KEYWORDS ID, and FILE-TYPE are passed from the
renaming command and are used to construct a new front matter
block if appropriate."
  (when-let* (((denote--only-note-p file))
              (date (denote--date (date-to-time id) file-type))
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
Use FILE-TYPE to look for the front matter lines. This is
relevant for operations that insert or rewrite the front matter
in a Denote note.

For the purposes of this test, FILE is a Denote note when it (i)
is a regular file, (ii) is writable, (iii) has a supported file
type extension per `denote-file-type', and (iv) is stored in the
variable `denote-directory'."
  (and (denote--writable-and-supported-p file)
       (not (denote--file-empty-p file))
       ;; Heuristic to check if this is one of our notes
       (string-prefix-p (denote-directory) (expand-file-name file)) ; FIXME 2022-08-11: Why do we need this?
       (denote--regexp-in-file-p (denote--title-key-regexp file-type) file)
       (denote--regexp-in-file-p (denote--keywords-key-regexp file-type) file)))

(defun denote--rewrite-keywords (file keywords file-type)
  "Rewrite KEYWORDS in FILE outright according to FILE-TYPE.

Do the same as `denote--rewrite-front-matter' for keywords,
but do not ask for confirmation.

This is for use in `denote-dired-rename-marked-files' or related.
Those commands ask for confirmation once before performing an
operation on multiple files."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
          (goto-char (point-at-bol))
          (insert (denote--get-keywords-line-from-front-matter keywords file-type))
          (delete-region (point) (point-at-eol)))))))

;; FIXME 2022-07-25: We should make the underlying regular expressions
;; that `denote--retrieve-title-value' targets more refined, so that we
;; capture eveyrhing at once.
(defun denote--rewrite-front-matter (file title keywords file-type)
  "Rewrite front matter of note after `denote-dired-rename-file'.
The FILE, TITLE, KEYWORDS, and FILE-TYPE are passed from the
renaming command and are used to construct new front matter
values if appropriate."
  (when-let* ((old-title-line (denote--retrieve-title-line file file-type))
              (old-keywords-line (denote--retrieve-keywords-line file file-type))
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
            (goto-char (point-at-bol))
            (insert new-title-line)
            (delete-region (point) (point-at-eol))
            (goto-char (point-min))
            (re-search-forward (denote--keywords-key-regexp file-type) nil t 1)
            (goto-char (point-at-bol))
            (insert new-keywords-line)
            (delete-region (point) (point-at-eol))))))))

(make-obsolete 'denote-dired-rename-expert nil "0.5.0")
(make-obsolete 'denote-dired-post-rename-functions nil "0.4.0")

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

(defun denote--rename-file-prompt (old-name new-name)
  "Prompt to rename file named OLD-NAME to NEW-NAME."
  (unless (string= (expand-file-name old-name) (expand-file-name new-name))
    (y-or-n-p
     (format "Rename %s to %s?"
             (propertize (file-name-nondirectory old-name) 'face 'error)
             (propertize (file-name-nondirectory new-name) 'face 'success)))))

;;;###autoload
(defun denote-rename-file (file title keywords)
  "Rename file and update existing front matter if appropriate.

If in Dired, consider FILE to be the one at point, else prompt
with minibuffer completion for one.

If FILE has a Denote-compliant identifier, retain it while
updating the TITLE and KEYWORDS fields of the file name.  Else
create an identifier based on the file's attribute of last
modification time.  If such attribute cannot be found, the
identifier falls back to the `current-time'.

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
          (file-type (denote--filetype-heuristics file)))
     (list
      file
      (denote--title-prompt
       (or (denote--retrieve-title-value file file-type) (file-name-base file)))
      (denote--keywords-prompt))))
  (let* ((dir (file-name-directory file))
         (id (denote--file-name-id file))
         (extension (file-name-extension file t))
         (file-type (denote--filetype-heuristics file))
         (new-name (denote--format-file
                    dir id keywords (denote--sluggify title) extension))
         (max-mini-window-height 0.33)) ; allow minibuffer to be resized
    (when (denote--rename-file-prompt file new-name)
      (denote--rename-file file new-name)
      (denote-update-dired-buffers)
      (if (denote--edit-front-matter-p new-name file-type)
          (denote--rewrite-front-matter new-name title keywords file-type)
        (denote--add-front-matter new-name title keywords id file-type)))))

(define-obsolete-function-alias
  'denote-dired-rename-file-and-add-front-matter
  'denote-rename-file
  "0.5.0")

(define-obsolete-function-alias
  'denote-dired-rename-file
  'denote-rename-file
  "0.5.0")

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

- the file's extension is retained;

- a prompt is asked once for the KEYWORDS field and the input is
  applied to all file names;

- if the file is recognized as a Denote note, add a front matter
  or rewrite it to include the new keywords.  A confirmation to
  carry out this step is performed once at the outset.  Note that
  the affected buffers are not saved.  The user can thus check
  them to confirm that the new front matter does not cause any
  problems (e.g. with the command `diff-buffer-with-file').
  Multiple buffers can be saved with `save-some-buffers' (read
  its doc string).  The addition of front matter takes place only
  if the given file has the appropriate file type extension (per
  the user option `denote-file-type')."
  (interactive nil dired-mode)
  (if-let ((marks (dired-get-marked-files))
           (keywords (denote--keywords-prompt))
           ((yes-or-no-p "Add front matter or rewrite front matter of keywords (buffers are not saved)?")))
      (progn
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (denote--file-name-id file))
                 (file-type (denote--filetype-heuristics file))
                 (title (or (denote--retrieve-title-value file file-type)
                            (file-name-base file)))
                 (extension (file-name-extension file t))
                 (new-name (denote--format-file
                            dir id keywords (denote--sluggify title) extension)))
            (denote--rename-file file new-name)
            (if (denote--edit-front-matter-p new-name file-type)
                (denote--rewrite-keywords new-name keywords file-type)
              (denote--add-front-matter new-name title keywords id file-type))))
        (revert-buffer))
    (user-error "No marked files; aborting")))

(define-obsolete-function-alias
  'denote-dired-rename-marked-files-and-add-front-matter
  'denote-dired-rename-marked-files
  "0.5.0")

;;;###autoload
(defun denote-rename-file-using-front-matter (file)
  "Rename FILE using its front matter as input.
When called interactively, FILE is the return value of the
function `buffer-file-name' which is subsequently inspected for
the requisite front matter.  It is thus implied that the FILE has
a file type that is supported by Denote, per `denote-file-type'.

Ask for confirmation, showing the difference between the old and
the new file names.  Refrain from performing the operation if the
buffer has unsaved changes.

Never modify the identifier of the FILE, if any, even if it is
edited in the front matter.  Denote considers the file name to be
the source of truth in this case to avoid potential breakage with
typos and the like."
  (interactive (list (buffer-file-name)))
  (when (buffer-modified-p)
    (user-error "Save buffer before proceeding"))
  (if-let* ((file-type (denote--filetype-heuristics file))
            (title (denote--retrieve-title-value file file-type))
            (keywords (denote--retrieve-keywords-value file file-type))
            (extension (file-name-extension file t))
            (id (denote--file-name-id file))
            (dir (file-name-directory file))
            (new-name (denote--format-file
                       dir id keywords (denote--sluggify title) extension)))
      (when (denote--rename-file-prompt file new-name)
        (denote--rename-file file new-name)
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
                   (lambda (file)
                     (denote--writable-and-supported-p file))
                   (dired-get-marked-files))))
      (progn
        (dolist (file marks)
          (let* ((dir (file-name-directory file))
                 (id (denote--file-name-id file))
                 (file-type (denote--filetype-heuristics file))
                 (title (denote--retrieve-title-value file file-type))
                 (keywords (denote--retrieve-keywords-value file file-type))
                 (extension (file-name-extension file t))
                 (new-name (denote--format-file
                            dir id keywords (denote--sluggify title) extension)))
            (denote--rename-file file new-name)))
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
    (denote--title-prompt)
    (denote--keywords-prompt)))
  (denote--add-front-matter file title keywords (denote--file-name-id file)
                            (denote--filetype-heuristics file)))

;;;; The Denote faces

(defgroup denote-faces ()
  "Faces for Denote."
  :group 'denote)

(defface denote-faces-link '((t :inherit link))
  "Face used to style Denote links in the buffer."
  :group 'denote-faces
  :package-version '(denote . "0.5.0"))

(defface denote-faces-broken-link '((t :inherit (error link)))
  "Face used to style Denote broken links in the buffer.
This only works in Org files, as Emacs' generic buttons do not
provide a facility that uses a face based on certain conditions."
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

;; TODO 2022-08-10: I believe a nil value has the same effect, though
;; there is no pressing need to test this.
(defface denote-faces-title '((t ))
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
          "\\(?:\\(?3:--\\)\\(?4:[[:alnum:][:nonascii:]-]*\\)\\)?"
          "\\(?:\\(?5:__\\)\\(?6:[[:alnum:][:nonascii:]_-]*\\)\\)?"
          "\\(?7:\\..*\\)?$")
  "Regexp of file names for fontification.")

(defconst denote-faces-file-name-keywords
  `((,(concat " " denote-faces--file-name-regexp)
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-title nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-keywords nil t)
     (7 'denote-faces-extension nil t )))
  "Keywords for fontification of file names.")

(defconst denote-faces-file-name-keywords-for-backlinks
  `((,(concat "^\\(?8:.*/\\)?" denote-faces--file-name-regexp)
     (8 'denote-faces-subdirectory nil t)
     (1 'denote-faces-date)
     (2 'denote-faces-time)
     (3 'denote-faces-delimiter nil t)
     (4 'denote-faces-title nil t)
     (5 'denote-faces-delimiter nil t)
     (6 'denote-faces-keywords nil t)
     (7 'denote-faces-extension nil t )))
  "Keywords for fontification of file names in the backlinks buffer.")

;;;; Fontification in Dired

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
  :package-version '(denote . "0.1.0")
  :link '(info-link "(denote) Fontification in Dired")
  :group 'denote-dired)

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

(defcustom denote-link-fontify-backlinks t
  "When non-nil, apply faces to files in the backlinks' buffer."
  :type 'boolean
  :package-version '(denote . "0.1.0")
  :link '(info-link "(denote) The backlinks' buffer")
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
  :package-version '(denote . "0.1.0")
  :group 'denote-link)

;;;;; Link to note

;; Arguments are: FILE-ID FILE-TITLE
(defconst denote-link--format-org "[[denote:%s][%s]]"
  "Format of Org link to note.")

;; The %N$s notation is for `format'.
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

;; FIXME 2022-08-13: Write this cleanly
(defun denote-link--format-link (file pattern)
  "Prepare link to FILE using PATTERN."
  (if (denote--writable-and-supported-p file)
      (let* ((file-id (denote--retrieve-filename-identifier file))
             (file-type (denote--filetype-heuristics file))
             (file-title (unless (string= pattern denote-link--format-id-only)
                           (denote--retrieve-title-value file file-type))))
        (format pattern file-id file-title))
    (format denote-link--format-id-only
            (denote--retrieve-filename-identifier file))))

;;;###autoload
(defun denote-link (target &optional id-only)
  "Create link to TARGET note in variable `denote-directory'.
With optional ID-ONLY, such as a universal prefix
argument (\\[universal-argument]), insert links with just the
identifier and no further description.  In this case, the link
format is always [[denote:IDENTIFIER]]."
  (interactive (list (denote--retrieve-read-file-prompt) current-prefix-arg))
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
      (while (or (re-search-forward regexp nil t)
                 (re-search-forward denote-link--regexp-plain nil t))
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
  (let ((file-names (mapcar (lambda (f)
                              (denote--file-name-relative-to-denote-directory f))
                            files)))
    (completing-read
     "Find linked file "
     (denote--completion-table 'file file-names)
     nil t nil 'denote-link--find-file-history)))

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
(defvar denote-link-button-action #'find-file-other-window
  "Action for Denote buttons.")

(make-obsolete-variable 'denote-link-buton-action 'denote-link-button-action "0.5.0")

(defun denote-link--find-file-at-button (button)
  "Visit file referenced by BUTTON."
  (let* ((id (denote-link--id-from-string
              (buffer-substring-no-properties
               (button-start button)
               (button-end button))))
         (file (denote--get-note-path-by-id id)))
    (funcall denote-link-button-action file)))

;;;###autoload
(defun denote-link-buttonize-buffer (&optional beg end)
  "Make denote: links actionable buttons in the current buffer.

Add this to `find-file-hook'.  It will only work with Denote
notes and will not do anything in `org-mode' buffers, as buttons
already work there.  If you do not use Markdown or plain text,
then you do not need this.

When called from Lisp, with optional BEG and END as buffer
positions, limit the process to the region in-between."
  (interactive)
  (when (and (not (derived-mode-p 'org-mode)) (denote--current-file-is-note-p))
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward denote--id-regexp end t)
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

(defvar denote-backlink-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "n" #'forward-button)
    (define-key m "p" #'backward-button)
    m)
  "Keymap for `denote-backlink-mode'.")

;; TODO 2022-08-10: In some places we have "backlink" and in others
;; "backlinks".  We need to address this inconsistency.
(define-derived-mode denote-backlink-mode special-mode "Backlinks"
  "Major mode for backlinks buffers.")

(defun denote-link--prepare-backlinks (id files &optional title)
  "Create backlinks' buffer for ID including FILES.
Use optional TITLE for a prettier heading."
  (let ((inhibit-read-only t)
        (buf (format "*denote-backlinks to %s*" id)))
    (with-current-buffer (get-buffer-create buf)
      (setq-local default-directory (denote-directory))
      (erase-buffer)
      (denote-backlink-mode)
      (goto-char (point-min))
      (when-let* ((title)
                  (heading (format "Backlinks to %S (%s)" title id))
                  (l (length heading)))
        (insert (format "%s\n%s\n\n" heading (make-string l ?-))))
      (mapc (lambda (f)
              (insert (denote--file-name-relative-to-denote-directory f))
              (make-button (point-at-bol) (point-at-eol) :type 'denote-link-backlink-button)
              (newline))
            files)
      (goto-char (point-min))
      (when denote-link-fontify-backlinks
        (font-lock-add-keywords nil denote-faces-file-name-keywords-for-backlinks t)))
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
  (let* ((file (buffer-file-name))
         (id (denote--retrieve-filename-identifier file))
         (file-type (denote--filetype-heuristics file))
         (title (denote--retrieve-title-value file file-type)))
    (if-let ((files (denote--retrieve-process-grep id)))
        (denote-link--prepare-backlinks id files title)
      (user-error "No links to the current note"))))

(defalias 'denote-link-show-backlinks-buffer (symbol-function 'denote-link-backlinks))

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
  (let ((current-file (buffer-file-name)))
    (if-let ((files (delete current-file (denote--directory-files-matching-regexp regexp))))
        (let ((beg (point)))
          (insert (denote-link--prepare-links files current-file id-only))
          (unless (derived-mode-p 'org-mode)
            (denote-link-buttonize-buffer beg (point))))
      (user-error "No links matching `%s'" regexp))))

(defalias 'denote-link-insert-links-matching-regexp (symbol-function 'denote-link-add-links))

;;;;; Links from Dired marks

;; NOTE 2022-07-21: I don't think we need a history for this one.
(defun denote-link--buffer-prompt (buffers)
  "Select buffer from BUFFERS visiting Denote notes."
  (let ((buffer-file-names (mapcar
                            (lambda (name)
                              (file-name-nondirectory name))
                            buffers)))
    (completing-read
     "Select note buffer: "
     (denote--completion-table 'buffer buffer-file-names)
     nil t)))

(defun denote-link--map-over-notes ()
  "Return list of `denote--only-note-p' from Dired marked items."
  (seq-filter
   (lambda (f)
     (and (denote--only-note-p f)
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
        (insert (denote-link--prepare-links files (buffer-file-name) id-only))
        (denote-link-buttonize-buffer)))))

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

(defun denote-link-ol-face (link)
  "Return appropriate face for LINK.
If the LINK resolves to a note, use `denote-faces-link', else
return `denote-faces-broken-link'."
  (if (denote-link--ol-resolve-link-to-target link)
      'denote-faces-link
    'denote-faces-broken-link))

(defun denote-link-ol-complete ()
  "Like `denote-link' but for Org integration.
This lets the user complete a link through the `org-insert-link'
interface by first selecting the `denote:' hyperlink type."
  (concat
   "denote:"
   (denote--retrieve-filename-identifier (denote--retrieve-read-file-prompt))))

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
           :face #'denote-link-ol-face
           :complete #'denote-link-ol-complete
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
(make-obsolete-variable 'denote-last-title nil "0.5.0")
(make-obsolete-variable 'denote-last-keywords nil "0.5.0")
(make-obsolete-variable 'denote-last-buffer nil "0.5.0")
(make-obsolete-variable 'denote-last-front-matter nil "0.5.0")

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
  (let* ((title (denote--title-prompt))
         (keywords (denote--keywords-prompt))
         (front-matter (denote--format-front-matter
                        title (denote--date nil 'org) keywords
                        (format-time-string denote--id-format nil) 'org)))
    (setq denote-last-path
          (denote--path title keywords
                        (file-name-as-directory (denote-directory))
                        (format-time-string denote--id-format) 'org))
    (denote--keywords-add-to-history keywords)
    (concat front-matter denote-org-capture-specifiers)))

(defun denote-org-capture-delete-empty-file ()
  "Delete file if capture with `denote-org-capture' is aborted."
  (when-let* ((file denote-last-path)
              ((denote--file-empty-p file)))
    (delete-file denote-last-path)))

(add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)

;;;; For the migration of old Org filetags/Markdown+YAML tags

(defun denote--migrate-type-files (type file-type)
  "Return list of TYPE files in variable `denote-directory'.
TYPE is a string which matches the `file-name-extension'.
FILE-TYPE is the symbol file-type."
  (delq nil
        (mapcar
         (lambda (file)
           (when-let* ((value (denote--retrieve-keywords-line
                               file file-type))
                       ((cond
                         ((eq file-type 'markdown-yaml) (not (string-match-p "," value)))
                         ((eq file-type 'org) (not (string-match-p " :" value)))
                         (t nil))))
             file))
         (seq-remove
          (lambda (file)
            (not (string= (file-name-extension file) type)))
          (denote--directory-files)))))

;;;###autoload
(defun denote-migrate-old-org-filetags ()
  "Rewrite Org filetags' value as colon-separated.

Change the filetags from:

    #+filetags:   one  two

To the standard format of:

    #+filetags:  :one:two:

A single tags chnages from TAG to :TAG:.

Denote used to format filetags with two spaces between them, but
this is not fully supported by Org.  The colon-separated entries
are the rule.

The rewrite DOES NOT SAVE BUFFERS.  The user is expected to
review the changes, such as by using `diff-buffer-with-file'.
Multiple buffers can be saved with `save-some-buffers' (check its
doc string).

This command is provided for the convenience of the user.  It
shall be deprecated and eventually removed from future versions
of Denote.  Written on 2022-08-10 for version 0.5.0."
  (interactive)
  (when-let (((yes-or-no-p "Rewrite filetags in Org files to use colons (buffers are NOT saved)?"))
             (files (denote--migrate-type-files "org" 'org)))
    (dolist (file files)
      (when-let* ((kw (denote--retrieve-keywords-value file 'org))
                  ((denote--edit-front-matter-p file 'org)))
        (denote--rewrite-keywords file kw 'org)))))

;;;###autoload
(defun denote-migrate-old-markdown-yaml-tags ()
  "Rewrite Markdown YAML tags value as comma-separated strings.

Change the tags from:

    tags:   one  two

To the standard format of:

    tags:  [\"one\", \"two\"]

Denote used to format filetags with two spaces between them, but
this is not supported by YAML.

The rewrite DOES NOT SAVE BUFFERS.  The user is expected to
review the changes, such as by using `diff-buffer-with-file'.
Multiple buffers can be saved with `save-some-buffers' (check its
doc string).

This command is provided for the convenience of the user.  It
shall be deprecated and eventually removed from future versions
of Denote.  Written on 2022-08-10 for version 0.5.0."
  (interactive)
  (when-let (((yes-or-no-p "Rewrite tags in Markdown files with YAML header to use lists (buffers are NOT saved)?"))
             (files (denote--migrate-type-files "md" 'markdown-yaml)))
    (dolist (file files)
      (when-let* ((kw (denote--retrieve-keywords-value file 'markdown-yaml))
                  ((denote--edit-front-matter-p file 'markdown-yaml)))
        (denote--rewrite-keywords file kw 'markdown-yaml)))))

(provide 'denote)
;;; denote.el ends here
