;;; denote-sequence.el --- Sequence notes extension for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote

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

;; Sequence notes extension for Denote.  It uses the SIGNATURE file
;; name component of Denote to establish a hierarchy between notes.
;; As such, note 1=1 is the child of the note 1.  The rest of the
;; Denote file naming scheme continues to apply as described in the
;; manual, as do all the other features of Denote.
;;
;; A new sequence note can be of the type `parent', `child', and
;; `sibling'.  For the convenience of the user, we provide commands to
;; create such "sequence notes", link only between them (as opposed to
;; a link to any other file with the Denote file-naminng scheme), and
;; re-parent them on demand.
;;
;; All the relevant functions we provide take care to automatically
;; use the right number for a given sequence.  If, for example, we
;; create a new child for parent 1=1, we make sure that it is the new
;; largest number among any existing children, so if 1=1=1 already
;; exists we use 1=1=2, and so on.
;;
;; This optional extension is not necessary for such a workflow.
;; Users can always define whatever SIGNATURE they want manually.  The
;; purpose of this extension is to streamline that work.

;;; Code:

;; NOTE 2024-12-25: Right now I am hardcoding the = as a field
;; separator inside of the Denote signature.  This is the default
;; behaviour, though we provide the `denote-file-name-slug-functions'
;; which, in principle, make the separator anything the user wants.
;; If we can accommodate such open-endedness, then I am happy to make
;; the relevant changes, but I prefer to keep it restricted at this
;; early stage.
;;
;; Similarly, I am not giving the option for Luhmann-style sequences
;; that include numbers and letters.  Ours consist only of numbers,
;; since (i) it is simpler and (ii) we already have the field
;; separator to give a sufficient sense of place.

(require 'denote)

(defgroup denote-sequence ()
  "Sequence notes extension for Denote."
  :group 'denote
  :link '(info-link "(denote) top")
  :link '(url-link :tag "homepage" "https://protesilaos.com/emacs/denote"))

(defconst denote-sequence-regexp "=?[0-9]+"
  "Pattern of a sequence.")

(defconst denote-sequence-types '(parent child sibling)
  "Types of sequence.")

(defun denote-sequence-p (sequence)
  "Return SEQUENCE string if it matches `denote-sequence-regexp'."
  (when (and (string-match-p denote-sequence-regexp sequence)
             (not (string-match-p "[a-zA-Z]" sequence))
             (not (string-suffix-p "=" sequence)))
    sequence))

(defun denote-sequence-file-p (file)
  "Return non-nil if Denote signature of FILE is a sequence.
A sequence is string that matches `denote-sequence-regexp'."
  (when-let* ((signature (denote-retrieve-filename-signature file)))
    (denote-sequence-p signature)))

(defun denote-sequence-split (sequence)
  "Split the SEQUENCE string into a list.
SEQUENCE conforms with `denote-sequence-p'."
  (if (denote-sequence-p sequence)
      (split-string sequence "=" t)
    (error "The sequence `%s' does not pass `denote-sequence-p'" sequence)))

(defun denote-sequence-depth (sequence)
  "Get the depth of SEQUENCE.
For example, 1=2=1 is three levels of depth."
  (length (denote-sequence-split sequence)))

(defun denote-sequence-get-all-files ()
  "Return all files in variable `denote-directory' with a sequence.
A sequence is a Denote signature that conforms with `denote-sequence-p'."
  (seq-filter #'denote-sequence-file-p (denote-directory-files)))

(defun denote-sequence-get-all-files-with-prefix (sequence &optional files)
  "Return all files in variable `denote-directory' with prefix SEQUENCE.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional FILES, operate on them, else use the return value of
`denote-directory-files'."
  (delq nil
        (mapcar
         (lambda (file)
           (when-let* ((file-sequence (denote-sequence-file-p file))
                       ((string-prefix-p sequence file-sequence)))
             file))
         (or files (denote-directory-files)))))

(defun denote-sequence-get-all-files-with-max-depth (depth &optional files)
  "Return all files with sequence depth up to DEPTH (inclusive).
With optional FILES, operate on them, else use the return value of
`denote-sequence-get-all-files'."
  (delq nil
        (mapcar
         (lambda (file)
           (when-let* ((sequence (denote-retrieve-filename-signature file))
                       (components (denote-sequence-split sequence))
                       ((>= depth (length components))))
             file))
         (or files (denote-sequence-get-all-files)))))

(defun denote-sequence-get-all-sequences (&optional files)
  "Return all sequences in `denote-directory-files'.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional FILES return all sequences among them instead."
  (delq nil (mapcar #'denote-sequence-file-p (or files (denote-directory-files)))))

(defun denote-sequence-get-all-sequences-with-prefix (sequence &optional sequences)
  "Get all sequences which extend SEQUENCE.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional SEQUENCES operate on those, else use the return value of
`denote-sequence-get-all-sequences'."
  (seq-filter
   (lambda (string)
     (string-prefix-p sequence string))
   (or sequences (denote-sequence-get-all-sequences))))

(defun denote-sequence-get-sequences-with-max-depth (depth &optional sequences)
  "Get sequences up to DEPTH (inclusive).
With optional SEQUENCES operate on those, else use the return value of
`denote-sequence-get-all-sequences'."
  (let* ((strings (or sequences (denote-sequence-get-all-sequences)))
         (lists-all (mapcar #'denote-sequence-split strings))
         (lists (seq-filter (lambda (element) (>= (length element) depth)) lists-all)))
    (delete-dups
     (mapcar
      (lambda (sequence)
        (mapconcat #'identity (seq-take sequence depth) "="))
      lists))))

(defun denote-sequence--pad (sequence type)
  "Create a new SEQUENCE with padded spaces for TYPE.
TYPE is a symbol among `denote-sequence-types'.  The special TYPE `all'
means to pad the full length of the sequence."
  (let* ((sequence-separator-p (string-match-p "=" sequence))
         (split (denote-sequence-split sequence))
         (s (cond
             ((eq type 'all)
              split)
             (sequence-separator-p
              (pcase type
                ('parent (car split))
                ('sibling split)
                ('child (car (nreverse split)))
                (_ (error "The type `%s' is not among `denote-sequence-types'" type))))
             (t
              sequence))))
    (if (listp s)
        (combine-and-quote-strings
         (mapcar
          (lambda (part)
            (string-pad part 5 32 :pad-from-start))
          s)
         "=")
      (string-pad s 32 32 :pad-from-start))))

(defun denote-sequence--get-largest (sequences type)
  "Return largest sequence in SEQUENCES given TYPE.
TYPE is a symbol among `denote-sequence-types'."
  (car (sort sequences
             :lessp (lambda (s1 s2)
                      (string<
                       (denote-sequence--pad s1 type)
                       (denote-sequence--pad s2 type)))
             :reverse t)))

(defun denote-sequence--get-new-parent (&optional sequences)
  "Return a new to increment largest among sequences.
With optional SEQUENCES consider only those, otherwise operate on the
return value of `denote-sequence-get-all-sequences'."
  (if-let* ((all (or sequences (denote-sequence-get-all-sequences))))
      (let* ((largest (denote-sequence--get-largest all 'parent))
             (first-component (car (denote-sequence-split largest)))
             (current-number (string-to-number first-component)))
        (number-to-string (+ current-number 1)))
    "1"))

(defun denote-sequence--get-new-child (sequence &optional sequences)
  "Return a new child of SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (if-let* ((depth (+ (denote-sequence-depth sequence) 1))
            (all-unfiltered (denote-sequence-get-all-sequences-with-prefix sequence sequences)))
      (if (= (length all-unfiltered) 1)
          (format "%s=1" (car all-unfiltered))
        (let* ((all (cond
                     ((= (length all-unfiltered) 1)
                      all-unfiltered)
                     ((denote-sequence-get-sequences-with-max-depth depth all-unfiltered))
                     (t all-unfiltered)))
               (largest (denote-sequence--get-largest all 'child)))
          (if (string-match-p "=" largest)
              (let* ((components (denote-sequence-split largest))
                     (butlast (butlast components))
                     (last-component (car (nreverse components)))
                     (current-number (string-to-number last-component))
                     (new-number (number-to-string (+ current-number 1))))
                (if butlast
                    (mapconcat #'identity (append butlast (list new-number)) "=")
                  (mapconcat #'identity (list largest new-number) "=")))
            (format "%s=1" largest))))
    (error "Cannot find sequences given sequence `%s'" sequence)))

(defun denote-sequence--get-prefix-for-siblings (sequence)
  "Get the prefix of SEQUENCE such that it is possible to find its siblings."
  (when (string-match-p "=" sequence)
    (mapconcat #'identity (butlast (denote-sequence-split sequence)) "=")))

(defun denote-sequence--get-new-sibling (sequence &optional sequences)
  "Return a new sibling SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (let* ((children-p (string-match-p "=" sequence)))
    (if-let* ((depth (denote-sequence-depth sequence))
              (all-unfiltered (if children-p
                                  (denote-sequence-get-all-sequences-with-prefix
                                   (denote-sequence--get-prefix-for-siblings sequence)
                                   sequences)
                                (denote-sequence-get-all-sequences)))
              (all (denote-sequence-get-sequences-with-max-depth depth all-unfiltered))
              ((member sequence all))
              (largest (if children-p
                           (denote-sequence--get-largest all 'sibling)
                         (denote-sequence--get-largest all 'parent))))
        (if children-p
            (let* ((components (denote-sequence-split largest))
                   (butlast (butlast components))
                   (last-component (car (nreverse components)))
                   (current-number (string-to-number last-component))
                   (new-number (number-to-string (+ current-number 1))))
              (mapconcat #'identity (append butlast (list new-number)) "="))
          (number-to-string (+ (string-to-number largest) 1)))
      (error "Cannot find sequences given sequence `%s'" sequence))))

(defun denote-sequence-get (type &optional sequence)
  "Return a sequence given TYPE among `denote-sequence-types'.
If TYPE is either `child' or `sibling', then optional SEQUENCE must be
non-nil and conform with `denote-sequence-p'."
  (pcase type
    ('parent (denote-sequence--get-new-parent))
    ('child (denote-sequence--get-new-child sequence))
    ('sibling (denote-sequence--get-new-sibling sequence))
    (_ (error "The type `%s' is not among `denote-sequence-types'" type))))

(defvar denote-sequence-type-history nil
  "Minibuffer history of `denote-sequence-type-prompt'.")

(defun denote-sequence-type-prompt ()
  "Prompt for sequence type among `denote-sequence-types'.
Return selected type as a symbol."
  (let ((default (car denote-sequence-type-history)))
    (intern
     (completing-read
      (format-prompt "Select sequence type" default)
      denote-sequence-types nil :require-match nil
      'denote-sequence-type-history default))))

(defvar denote-sequence-file-history nil
  "Minibuffer history for `denote-sequence-file-prompt'.")

(defun denote-sequence-file-prompt (&optional prompt-text files-with-sequences)
  "Prompt for file with sequence in variable `denote-directory'.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional PROMPT-TEXT use it instead of a generic prompt.

With optional FILES-WITH-SEQUENCES as a list of strings, use them as
completion candidates.  Else use `denote-sequence-get-all-files'."
  (if-let* ((relative-files (mapcar #'denote-get-file-name-relative-to-denote-directory
                                    (or files-with-sequences (denote-sequence-get-all-files))))
            (prompt (format-prompt (or prompt-text "Select FILE with sequence") nil))
            (input (completing-read
                    prompt
                    (denote--completion-table 'file relative-files)
                    nil :require-match
                    nil 'denote-sequence-file-history)))
      (concat (denote-directory) input)
    (error "There are no sequence notes in the `denote-directory'")))

;;;###autoload
(defun denote-sequence (type &optional file-with-sequence)
  "Create a new sequence note of TYPE among `denote-sequence-types'.
If TYPE is either `child' or `sibling', then it is an extension of SEQUENCE.

When called interactively, prompt for TYPE and, when necessary, for
FILE-WITH-SEQUENCE whose sequence will be used to derive a new sequence.
Files available at the minibuffer prompt are those returned by
`denote-sequence-get-all-files'."
  (interactive
   (let ((selected-type (denote-sequence-type-prompt)))
     (list
      selected-type
      (when (memq selected-type (delq 'parent denote-sequence-types))
        (denote-sequence-file-prompt (format "Make a new %s of SEQUENCE" selected-type))))))
  ;; TODO 2024-12-30: Do we need to wrap this in the following?
  ;;
  ;; (cl-letf (((alist-get 'signature denote-file-name-slug-functions) #'denote-sluggify-signature))
  (let* ((sequence (when file-with-sequence (denote-retrieve-filename-signature file-with-sequence)))
         (new-sequence (denote-sequence-get type sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-parent ()
  "Like `denote-sequence' to directly create new parent."
  (interactive)
  (let* ((new-sequence (denote-sequence-get 'parent))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-sibling (sequence)
  "Like `denote-sequence' to directly create new sibling of SEQUENCE.
When called from Lisp, SEQUENCE is a string that conforms with
`denote-sequence-p'."
  (interactive (list (denote-retrieve-filename-signature (denote-sequence-file-prompt))))
  (let* ((new-sequence (denote-sequence-get 'sibling sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-child (sequence)
  "Like `denote-sequence' to directly create new child of SEQUENCE.
When called from Lisp, SEQUENCE is a string that conforms with
`denote-sequence-p'."
  (interactive (list (denote-retrieve-filename-signature (denote-sequence-file-prompt))))
  (let* ((new-sequence (denote-sequence-get 'child sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-link (file &optional id-only)
  "Link to FILE with sequence.
This is like the `denote-link' command but only accepts to link to a
file that conforms with `denote-sequence-file-p'.  When called
interactively, only relevant files are shown for minibuffer completion
from the variable `denote-directory'.

Optional ID-ONLY has the same meaning as the `denote-link' command."
  (interactive (list (denote-sequence-file-prompt "Link to file with sequence")))
  (unless (denote-sequence-file-p file)
    (error "Can only link to file with a sequence; else use `denote-link' and related"))
  (let* ((type (denote-filetype-heuristics buffer-file-name))
         (description (denote-get-link-description file)))
    (denote-link file type description id-only)))

(defun denote-sequence-sort (file-with-sequence-1 file-with-sequence-2)
  "Sort FILE-WITH-SEQUENCE-1 and FILE-WITH-SEQUENCE-2."
  (let ((s1 (denote-retrieve-filename-signature file-with-sequence-1))
        (s2 (denote-retrieve-filename-signature file-with-sequence-2)))
    (string<
     (denote-sequence--pad s1 'all)
     (denote-sequence--pad s2 'all))))

(defvar denote-sequence-history nil
  "Minibuffer history of `denote-sequence-prompt'.")

(defun denote-sequence-prompt (&optional prompt-text sequences)
  "Prompt for a sequence.
With optional PROMPT-TEXT use it instead of a generic prompt.

With optional SEQUENCES as a list of strings, use them as completion
candidates.  Else use the return value of `denote-sequence-get-all-sequences'.
A sequence is a string conforming with `denote-sequence-p'.  Any other string
is ignored."
  (completing-read
   (format-prompt (or prompt-text "Select an existing sequence (empty for all)") nil)
   (or sequences (denote-sequence-get-all-sequences))
   #'denote-sequence-p :require-match nil 'denote-sequence-history))

(defun denote-sequence-depth-prompt (&optional prompt-text)
  "Prompt for the depth of a sequence.
With optional PROMPT-TEXT use it instead of the generic one."
  (read-number
   (or prompt-text
       "Get sequences up to this depth (e.g. `1=1=2' is `3' levels of depth): ")))

;;;###autoload
(defun denote-sequence-dired (&optional prefix depth)
  "Produce a Dired listing of all sequence notes.
Sort sequences from smallest to largest.

With optional PREFIX string, show only files whose sequence matches it.

With optional DEPTH as a number, limit the list to files whose sequence
is that many levels deep.  For example, 1=1=2 is three levels deep."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (cond
      ((= arg 16)
       (list
        (denote-sequence-prompt "Limit to files that extend SEQUENCE (empty for all)")
        (denote-sequence-depth-prompt)))
      ((= arg 4)
       (list
        (denote-sequence-prompt "Limit to files that extend SEQUENCE (empty for all)")))
      (t
       nil))))
  (if-let* ((default-directory (denote-directory))
            (all (if prefix
                     (denote-sequence-get-all-files-with-prefix prefix)
                   (denote-sequence-get-all-files)))
            (files-with-depth (if depth
                                  (denote-sequence-get-all-files-with-max-depth depth all)
                                all))
            (files-sorted (sort files-with-depth :lessp #'denote-sequence-sort))
            (buffer-name (format "Denote sequences at %s" (format-time-string "%T"))))
      (let ((dired-buffer (dired (cons buffer-name (mapcar #'file-relative-name files-sorted)))))
        (with-current-buffer dired-buffer
          (setq-local revert-buffer-function
                      (lambda (&rest _)
                        (kill-buffer dired-buffer)
                        (denote-sequence-dired)))))
    (user-error "There are no files whose Denote signature conforms with `denote-sequence-p'")))

;;;###autoload
(defun denote-sequence-reparent (current-file file-with-sequence)
  "Re-parent the CURRENT-FILE to be a child of FILE-WITH-SEQUENCE.
If CURRENT-FILE has a sequence (the Denote file name signature), change
it.  Else create a new one.

When called interactively, CURRENT-FILE is either the current file, or a
special Org buffer (like those of `org-capture'), or the file at point in
Dired.

When called interactively, prompt for FILE-WITH-SEQUENCE showing only
the files in the variable `denote-directory' which have a sequence.  If
no such files exist, throw an error.

When called from Lisp, CURRENT-FILE is a string pointing to a file.

When called from Lisp, FILE-WITH-SEQUENCE is either a file with a
sequence (per `denote-sequence-file-p') or the sequence string as
such (per `denote-sequence-p').  In both cases, what matters is to know
the target sequence."
  (interactive
   (list
    (if (denote--file-type-org-extra-p)
        denote-last-path-after-rename
      (denote--rename-dired-file-or-current-file-or-prompt))
    (denote-sequence-file-prompt
     (format "Reparent `%s' to be a child of"
             (propertize
              (denote--rename-dired-file-or-current-file-or-prompt)
              'face 'denote-faces-prompt-current-name)))))
  (let* ((target-sequence (or (denote-sequence-file-p file-with-sequence)
                              (denote-sequence-p file-with-sequence)
                              (user-error "No sequence of `denote-sequence-p' found in `%s'" file-with-sequence)))
         (new-sequence (denote-sequence--get-new-child target-sequence)))
    (denote-rename-file current-file 'keep-current 'keep-current new-sequence 'keep-current)))

(provide 'denote-sequence)
;;; denote-sequence.el ends here
