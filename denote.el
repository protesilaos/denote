;;; denote.el --- Simple notes with a strict file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2022  Protesilaos Stavrou

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
;; "Denote" is the familiar word, though it also is a play on to the
;; "note" concept.  Plus, we can come up with acronyms like:
;;
;; * Don't Ever Note Only The Ephemeral
;; * Denote Everything Neatly; Omit The Excesses
;;
;; But I'll let you get back to work.  Don't Escape or Neglect your
;; Obligations, Tasks, Engagements...

;;; Code:

(defgroup denote ()
  "Simple notes with a strict file-naming scheme."
  :group 'files)

;;;; User options

(defcustom denote-directory (expand-file-name "~/Documents/notes/")
  "Directory for storing personal notes."
  :group 'denote
  :type 'directory)

(defcustom denote-known-keywords
  '("emacs" "philosophy" "politics" "economics")
  "List of strings with predefined keywords for `denote-new-note'.

The implicit assumption is that a keyword is a single word.  If
you need a keyword to be multiple words long, use underscores to
separate them.  Do not use hyphens or other characters, as those
are assumed to demarcate distinct keywords."
  :group 'denote
  :type '(repeat string))

(defcustom denote-infer-keywords t
  "Whether to infer keywords.

When non-nil, search the file names of existing notes in
`denote-directory' for their keyword field and extract the
entries as \"inferred keywords\".  These are combined with
`denote-known-keywords' and are presented as completion
candidated while using `denote-new-note' interactively.

If nil, refrain from inferring keywords.  The aforementioned
completion prompt only shows the `denote-known-keywords'."
  :group 'denote
  :type 'boolean)

(defcustom denote-sort-keywords t
  "Whether to sort keywords in new files.

When non-nil, the keywords of `denote-new-note' are sorted with
`string-lessp' regardless of the order they were inserted at the
minibuffer prompt.

If nil, show the keywords in their given order."
  :group 'denote
  :type 'boolean)

(defcustom denote-front-matter-date-format nil
  "Date format in the front matter (file header) of new notes.

If the value is nil, use a plain date in YEAR-MONTH-DAY notation,
like 2022-06-08.

If the value is the `org-timestamp' symbol, format the date as an
inactive Org timestamp such as: [2022-06-08 Wed 06:19].

If a string, use it as the argument of `format-time-string'.
Read the documentation of that function for valid format
specifiers."
  :type '(choice
          (const :tag "Just the date like 2022-06-08" nil)
          (const :tag "An inactive Org timestamp like [2022-06-08 Wed 06:19]" org-timestamp)
          (string :tag "Custom format for `format-time-string'"))
  :group 'denote)

;;;; Main variables

(defconst denote--id "%Y%m%d_%H%M%S"
  "Format of ID prefix of a note's filename.")

(defconst denote--id-regexp "\\([0-9_]+\\{15\\}\\)"
  "Regular expression to match `denote--id'.")

(defconst denote--keyword-regexp "\\(--\\)\\([0-9A-Za-z_+]*\\)\\(--\\)"
  "Regular expression to match `denote-keywords'.")

(defconst denote--file-regexp
  (concat denote--id-regexp denote--keyword-regexp "\\(.*\\)\\.org")
  "Regular expression to match file names from `denote-new-note'.")

(defconst denote--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

(defvar denote-last-path nil "Store last path.")
(defvar denote-last-title nil "Store last title.")
(defvar denote-last-keywords nil "Store last keywords.")
(defvar denote-last-buffer nil "Store last buffer.")
(defvar denote-last-front-matter nil "Store last front-matter.")

;;;; File helper functions

(defun denote--directory ()
  "Valid name format for `denote-directory'."
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
  "Make STR an appropriate file name slug."
  (downcase (denote--slug-hyphenate (denote--slug-no-punct str))))

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

(defun denote--directory-files ()
  "List `denote-directory' files, assuming flat directory."
  (let* ((dir (denote--directory))
         (default-directory dir))
    (seq-remove
     (lambda (file)
       (file-directory-p file))
     (directory-files dir nil directory-files-no-dot-files-regexp t))))

(defun denote--keywords-in-files ()
  "Produce list of keywords in `denote--directory-files'."
  (delq nil (mapcar
             (lambda (x)
               (denote--extract
                (concat denote--id-regexp denote--keyword-regexp) x 3))
             (denote--directory-files))))

(defun denote--inferred-keywords ()
  "Extract keywords from `denote--directory-files'."
  (let ((sequence (denote--keywords-in-files)))
    (mapcan (lambda (s)
              (split-string s "+" t))
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
      (mapconcat #'downcase keywords "+")
    keywords))

(defun denote--keywords-capitalize (keywords)
  "`capitalize' KEYWORDS output of `denote--keywords-prompt'."
  (if (and (> (length keywords) 1)
           (not (stringp keywords)))
      (mapconcat #'capitalize keywords ", ")
    (capitalize keywords)))

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

(defun denote--format-file (path id keywords slug)
  "Format file name.
PATH, ID, KEYWORDS, SLUG are expected to be supplied by `denote'
or equivalent: they will all be converted into a single string."
  (let ((kws (if denote-infer-keywords
                 (denote--keywords-combine keywords)
               keywords)))
    (format "%s%s--%s--%s.org" path id kws slug)))

(defun denote--file-meta-header (title date keywords filename id)
  "Front matter for new notes.

TITLE, DATE, KEYWORDS, FILENAME, ID are all strings which are
 provided by `denote-new-note'."
  (let ((kw (denote--keywords-capitalize keywords)))
    (concat "#+title:      " title     "\n"
            "#+date:       " date      "\n"
            "#+keywords:   " kw        "\n"
            "#+identifier: " id        "\n"
            "#+filename:   " (string-remove-prefix denote-directory filename)  "\n"
            "#+path:       " filename  "\n"
            "#+link:       " "denote /home/prot/Documents/notes/%s"
            "\n\n")))

(defun denote--path (title keywords)
  "Return path to new file with TITLE and KEYWORDS.
Format current time, else use optional ID."
  (setq denote-last-path
        (denote--format-file
         (file-name-as-directory denote-directory)
         (format-time-string denote--id)
         keywords
         (denote--sluggify title))))

(defun denote--date ()
  "Expand the date for a new note's front matter."
  (let ((format denote-front-matter-date-format))
    (cond
     ((eq format 'org-timestamp)
      (format-time-string "[%F %a %R]"))
     ((stringp format)
      (format-time-string format))
     (t (format-time-string "%F")))))

(defun denote--prepare-note (title keywords &optional path)
  "Use TITLE and KEYWORDS to prepare new note file.
Use optional PATH, else create it with `denote--path'."
  (let* ((p (or path (denote--path title keywords)))
         (default-directory denote-directory)
         (buffer (unless path (find-file p)))
         (header (denote--file-meta-header
                  title (denote--date) keywords p
                  (format-time-string denote--id))))
    (unless path
      (with-current-buffer buffer (insert header))
      (setq denote-last-buffer buffer))
    (setq denote-last-front-matter header)))

(defvar denote--title-history nil
  "Minibuffer history of `denote--title-prompt'.")

(defun denote--title-prompt ()
  "Read file title for `denote-new-note'."
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

(provide 'denote)
;;; denote.el ends here
