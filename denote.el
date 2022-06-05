;;; denote.el --- Do Easy NOTE -*- lexical-binding: t -*-

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
  "Simple tool for plain text notes."
  :group 'files)

;;; User options

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

(defcustom denote-org-capture-specifiers "%l\n%i\n%?"
  "String with format specifieirs for `org-capture-templates'.
Check that variable's documentation for the details.

This string is append to new notes in the `denote-org-capture'
function.  Every new note has the standard front matter we
define."
  :type 'string
  :group 'denote)

(defcustom denote-link-insert-functions
  (list #'denote-write-backlink)
  "Functions that run after `denote-link'.
Each function accepts a TARGET-FILE and an ORIGIN-LINK argument.
Both are supplied by `denote-link'."
  :type 'hook
  :group 'denote)

;;; Main variables

;; TODO 2022-06-04: Can we make the entire file name format a defcustom?

(defconst denote-id "%Y%m%d_%H%M%S"
  "Format of ID prefix of a note's filename.")

(defconst denote-id-regexp "\\([0-9_]+\\{15\\}\\)"
  "Regular expression to match `denote-id'.")

(defconst denote-keyword-regexp "\\(--\\)\\([0-9A-Za-z_+]*\\)\\(--\\)"
  "Regular expression to match `denote-keywords'.")

(defconst denote--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

(defvar denote-last-path nil "Store last path.")
(defvar denote-last-title nil "Store last title.")
(defvar denote-last-keywords nil "Store last keywords.")
(defvar denote-last-buffer nil "Store last buffer.")
(defvar denote-last-front-matter nil "Store last front-matter.")

;;;; File name helpers

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
                (concat denote-id-regexp denote-keyword-regexp) x 3))
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
         (format-time-string denote-id)
         keywords
         (denote--sluggify title))))

(defun denote--prepare-note (title keywords &optional path)
  "Use TITLE and KEYWORDS to prepare new note file.
Use optional PATH, else create it with `denote--path'."
  (let* ((path (or path (denote--path title keywords)))
         (default-directory denote-directory)
         (buffer (unless path (find-file path)))
         (header (denote--file-meta-header
                  title (format-time-string "%F") keywords path
                  (format-time-string denote-id))))
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
alphabetically."
  (declare (interactive-only t))
  (interactive
   (list
    (denote--title-prompt)
    (denote--keywords-prompt)))
  (denote--prepare-note title keywords)
  (denote--keywords-add-to-history keywords))

;;;###autoload
(defun denote-org-capture ()
  "Create new note through `org-capture-templates'.
Use this as a function that returns the path to the new file.
The file is populated with Denote's front matter.  It can then be
expanded with the usual specifiers or strings that
`org-capture-templates' supports.

Search the source code of this function for a comment with a
sample template.  We will eventually have a manual."
  (let ((title (denote--title-prompt))
        (keywords (denote--keywords-prompt)))
    (denote--path title keywords)
    (denote--prepare-note denote-last-title denote-last-keywords denote-last-path)
    (denote--keywords-add-to-history denote-last-keywords)
    ;; TODO 2022-06-05: Is there a better way to set up this hook?
    ;; Alternatively, can we prevent the creation of a file when the
    ;; capture is aborted?
    (add-hook 'org-capture-after-finalize-hook #'denote-org-capture-delete-empty-file)
    (concat denote-last-front-matter denote-org-capture-specifiers)))

(defun denote-org-capture-delete-empty-file ()
  "Delete file if capture with `denote-org-capture' is aborted."
  (when-let* ((file denote-last-path)
              ((zerop (or (file-attribute-size (file-attributes file)) 0))))
    (delete-file denote-last-path)))

;; Samples of an `org-capture-templates' entry:
;;
;; (setq org-capture-templates
;;       '(("n" "New note (with denote.el)" plain
;;          (file denote-last-path)
;;          #'denote-org-capture
;;          :no-save t
;;          :immediate-finish nil
;;          :kill-buffer t
;;          :jump-to-captured t)))
;;
;; (with-eval-after-load 'org-capture
;;   (add-to-list 'org-capture-templates
;;                '("n" "New note (with denote.el)" plain
;;                  (file denote-last-path)
;;                  #'denote-org-capture
;;                  :no-save t
;;                  :immediate-finish nil
;;                  :kill-buffer t
;;                  :jump-to-captured t)))

;; TODO 2022-06-04: `denote-rename-file'

;;;; Link to note

(defun denote--find-key-value-pair (regexp)
  "Produce a cons cell from REGEXP by searching the file."
  (goto-char (point-min))
  (re-search-forward regexp)
  (cons (match-string-no-properties 1)
        (match-string-no-properties 2)))

(defvar denote--title-regexp "^\\(#\\+title:\\)[\s\t]+\\(.*\\)"
  "Regular expression for title key and value.")

(defvar denote--filename-regexp "^\\(#\\+filename:\\)[\s\t]+\\(.*\\)"
  "Regular expression for filename key and value.")

(defvar denote--identifier-regexp "^\\(#\\+identifier:\\)[\s\t]+\\(.*\\)"
  "Regular expression for filename key and value.")

;; TODO 2022-06-05: Maybe this should be a defcustom?
(defvar denote--link-format "[[denote:%s][%s (%s)]]"
  "Format of Org link to note.")

(defvar denote--backlink-format "[[denote:%s][backlink: %s (%s)]]"
  "Format of Org link to note.")

(defun denote--retrieve-value (note regexp)
  "Return REGEXP value from NOTE."
  (let ((default-directory (denote--directory)))
    (with-temp-buffer
      (insert-file-contents-literally note)
      (denote--find-key-value-pair regexp))))

(defun denote--read-file-prompt ()
  "Prompt for regular file in `denote-directory'."
  (read-file-name "Select note: " (denote--directory)
                  nil t nil #'file-regular-p))

;;;###autoload
(defun denote-link (target)
  "Create Org link to TARGET note in `denote-directory'.
Run `denote-link-insert-functions' afterwards."
  (interactive (list (denote--read-file-prompt)))
  (let* ((target-id (cdr (denote--retrieve-value target denote--identifier-regexp)))
         (target-name (string-remove-prefix
                    (denote--directory)
                    (cdr (denote--retrieve-value target denote--filename-regexp))))
         (target-title (cdr (denote--retrieve-value target denote--title-regexp)))
         (target-link (format denote--link-format target-name target-title target-id))
         (origin-note (buffer-file-name))
         (origin-id (cdr (denote--retrieve-value origin-note denote--identifier-regexp)))
         (origin-name (string-remove-prefix
                    (denote--directory)
                    (cdr (denote--retrieve-value origin-note denote--filename-regexp))))
         (origin-title (cdr (denote--retrieve-value origin-note denote--title-regexp)))
         (origin-link (format denote--backlink-format origin-name origin-title origin-id)))
    (insert target-link)
    (run-hook-with-args 'denote-link-insert-functions target origin-link)))

;; NOTE 2022-06-05: A proof-of-concept.  We need to: (i) have a
;; Backlinks headeding, (ii) delete duplicates, (iii) ensure one
;; backlink per line, (iv) have a `denote-unlink' command or a
;; `denote-clean-backlinks' for invalid links.
(defun denote-write-backlink (target-file origin-link)
  "Insert ORIGIN-LINK to TARGET-FILE."
  (let ((default-directory (denote--directory)))
    (with-current-buffer (find-file-noselect target-file)
      (goto-char (point-max))
      (insert origin-link))))

(provide 'denote)
;;; denote.el ends here
