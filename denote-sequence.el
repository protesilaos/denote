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

;; TODO 2025-01-08: Test whether the built-in hierarchy.el can be used
;; to present the sequences in a nice way.  What do we need and how
;; exactly do we use that library.
(require 'denote)

(defgroup denote-sequence ()
  "Sequence notes extension for Denote."
  :group 'denote
  :link '(info-link "(denote) top")
  :link '(info-link "(denote) Sequence notes")
  :link '(url-link :tag "homepage" "https://protesilaos.com/emacs/denote"))

(defconst denote-sequence-schemes '(numeric alphanumeric)
  "Symbols representing sequence schemes.")

(defcustom denote-sequence-scheme 'numeric
  "Sequencing scheme to establish file hierarchies.
The value is the symbol `numeric' or `alphanumeric'.

Numeric sequences (the default) are the easier to understand but also
are the longest.  Each level of depth in the hierarchy is delimited by
an equals sign: the 1=1=2 thus refers to the second child of the first
child of parent 1.  Each level of depth can be a number of any length,
like 1=40=2=20.

Alphanumeric sequences are more compact than numeric ones.  Their depth
is derived via the alternation from numbers to latin characters, such
that 1a2 refers to the second child of the first child of parent 1.
Because they alternate between numbers and letters, they do not use the
equals sign.  When a number cannot be represented by a single letter,
two or more are used instead, such as the number 51 corresponding to
zx (z is 26 and x is 25)."
  :group 'denote-sequence
  :type '(choice (const :tag "Numeric like 1=1=2" numeric)
                 (const :tag "Alphanumeric like 1a2" alphanumeric)))

(defconst denote-sequence-numeric-regexp "=?[0-9]+"
  "Pattern of a numeric sequence.")

(defconst denote-sequence-alphanumeric-regexp "\\([0-9]+\\)\\([[:alpha:]]+\\)?"
  "Pattern of an alphanumeric sequence.")

(defconst denote-sequence-types '(parent child sibling)
  "Types of sequence.")

(defun denote-sequence-numeric-p (sequence)
  "Return SEQUENCE if it is numeric per `denote-sequence-scheme'."
  (when (and (string-match-p denote-sequence-numeric-regexp sequence)
             (not (string-match-p "[a-zA-Z]" sequence))
             (not (string-suffix-p "=" sequence)))
    sequence))

(defun denote-sequence-alphanumeric-p (sequence)
  "Return SEQUENCE if it is alphanumeric per `denote-sequence-scheme'."
  (when (and (string-match-p denote-sequence-alphanumeric-regexp sequence)
             (not (string-match-p "=" sequence)))
    sequence))

(defun denote-sequence-p (sequence)
  "Return SEQUENCE string is of a supported scheme.
Also see `denote-sequence-numeric-p' and `denote-sequence-alphanumeric-p'."
  (when (or (denote-sequence-numeric-p sequence)
            (denote-sequence-alphanumeric-p sequence))
    sequence))

(defun denote-sequence-with-error-p (sequence)
  "Return SEQUENCE string if it matches `denote-sequence-numeric-regexp'."
  (or (denote-sequence-p sequence)
      (error "The sequence `%s' does not pass `denote-sequence-p'" sequence)))

(defun denote-sequence--numeric-partial-p (string)
  "Return non-nil if STRING likely is part of a numeric sequence."
  (and (string-match-p "[0-9]+" string)
       (not (string-match-p "[[:alpha:][:punct:]]" string))))

(defun denote-sequence--alphanumeric-partial-p (string)
  "Return non-nil if STRING likely is part of an alphanumeric sequence."
  (and (string-match-p "[a-z]+" string)
       (not (string-match-p "[0-9[:punct:]]+" string))))

(defun denote-sequence-and-scheme-p (sequence &optional partial)
  "Return the sequencing scheme of SEQUENCE, per `denote-sequence-scheme'.
Return a cons cell of the form (sequence . scheme), where the `car' is
SEQUENCE and the `cdr' is its sequencing scheme as a symbol among
`denote-sequence-schemes'.

With optional PARTIAL as a non-nil value, assume SEQUENCE to be a string
that only represents part of a sequence, which itself consists entirely
of numbers or letters.

Produce an error if the sequencing scheme cannot be established."
  (cond
   ((or (and partial (denote-sequence--alphanumeric-partial-p sequence))
        (denote-sequence-alphanumeric-p sequence))
    (cons sequence 'alphanumeric))
   ((or (and partial (denote-sequence--numeric-partial-p sequence))
        (denote-sequence-numeric-p sequence))
    (cons sequence 'numeric))
   (t (error "The sequence `%s' does not pass `denote-sequence-p'" sequence))))

(defun denote-sequence--scheme-of-strings (strings)
  "Return the sequencing scheme of STRINGS, per `denote-sequence-scheme'."
  (if (seq-find (lambda (string) (string-match-p "[[:alpha:]]" string)) strings)
      'alphanumeric
    'numeric))

(defun denote-sequence-file-p (file)
  "Return non-nil if Denote signature of FILE is a sequence.
A sequence is string that matches `denote-sequence-numeric-regexp'."
  (when-let* ((signature (denote-retrieve-filename-signature file)))
    (denote-sequence-p signature)))

(defun denote-sequence-join (strings scheme)
  "Join STRINGS to form a sequence according to SCHEME.
SCHEME is a symbol among `denote-sequence-schemes'.  Return resulting
sequence if it conforms with `denote-sequence-p'."
  (pcase scheme
    ('numeric (mapconcat #'identity strings "="))
    ('alphanumeric (apply #'concat strings))))

(defun denote-sequence-split (sequence &optional partial)
  "Split the SEQUENCE string into a list.
SEQUENCE conforms with `denote-sequence-p'.  If PARTIAL is non-nil, it
has the same meaning as in `denote-sequence-and-scheme-p'."
  (pcase-let* ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence partial)))
    (pcase scheme
      ('numeric
       (split-string sequence "=" t))
      ('alphanumeric
       (let ((strings nil)
             (start 0))
         (while (string-match denote-sequence-alphanumeric-regexp sequence start)
           (push (match-string 1 sequence) strings)
           (when-let* ((two (match-string 2 sequence)))
             (push two strings)
             (setq start (match-end 2)))
           (setq start (match-end 1)))
         (if strings
             (nreverse strings)
           (split-string sequence "" :omit-nulls)))))))

(defun denote-sequence--alpha-to-number (string)
  "Convert STRING of alphabetic characters to its numeric equivalent."
  (let* ((strings (denote-sequence-split string :partial))
         (numbers (mapcar
                   (lambda (string)
                     (let ((num (- (string-to-char string) 96)))
                       (cond
                        ((and (> num 0) (<= num 26))
                         num)
                        (t
                         (let ((times (/ num 26)))
                           (if-let* ((mod (% num 26))
                                     ((> mod 0))
                                     (suffix (+ mod 96)))
                               (list (* times 26) suffix)
                             (list (* times 26))))))))
                   strings)))
    (format "%s" (apply #'+ numbers))))

(defun denote-sequence--number-to-alpha (string)
  "Convert STRING of numbers to its alphabetic equivalent."
  (let ((num (string-to-number string)))
    (cond
     ((= num 0)
      (char-to-string (+ num 97)))
     ((and (> num 0) (<= num 26))
      (char-to-string (+ num 96)))
     (t
      (let ((times (/ num 26)))
        (if-let* ((mod (% num 26))
                  ((> mod 0))
                  (prefix (make-string times ?z))
                  (suffix (char-to-string (+ mod 96))))
            (concat prefix suffix)
          (make-string times ?z)))))))

(defun denote-sequence-convert (string)
  "Convert STRING to its counterpart sequencing scheme.
Also see `denote-sequence-scheme'."
  (cond
   ((denote-sequence--alphanumeric-partial-p string)
    (denote-sequence--alpha-to-number string))
   ((denote-sequence--numeric-partial-p string)
    (denote-sequence--number-to-alpha string))
   (t
    (error "The `%s' must not contain both numbers and letters" string))))

(defun denote-sequence-increment (string)
  "Increment number represented by STRING and return it as a string.
STRING is part of a sequence, not the entirety of it."
  (cond
   ((denote-sequence--numeric-partial-p string)
    (number-to-string (+ (string-to-number string) 1)))
   ((denote-sequence--alphanumeric-partial-p string)
    (let* ((letters (split-string string "" :omit-nulls))
           (length-1 (= (length letters) 1))
           (first (car letters))
           (reverse (nreverse (copy-sequence letters)))
           (last (car reverse)))
      (cond
       ((and length-1 (string= "z" first))
        "za")
       (length-1
        (char-to-string (+ (string-to-char first) 1)))
       ((string= "z" last)
        (apply #'concat (append letters (list "a"))))
       (t
        (let ((last last))
          (apply #'concat
                 (append (butlast letters)
                         (list (char-to-string (+ (string-to-char last) 1))))))))))
   (t
    (error "The string `%s' must contain only numbers or letters" string))))

(defun denote-sequence-depth (sequence)
  "Get the depth of SEQUENCE.
For example, 1=2=1 and 1b1 are three levels of depth."
  (length (denote-sequence-split sequence)))

(defun denote-sequence--children-implied-p (sequence)
  "Return non-nil if SEQUENCE implies children.
This does not actually check if there are children in the variable
`denote-directory', but only that SEQUENCE is greater than 1."
  (> (denote-sequence-depth sequence) 1))

(defun denote-sequence--get-parent (sequence)
  "Return implied parent of SEQUENCE, else nil.
Produce an error if SEQUENCE does not conform with `denote-sequence-p'.
The implied check here has the same meaning as described in
`denote-sequence--children-implied-p'."
  (pcase-let* ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence)))
    (when (and (denote-sequence-with-error-p sequence)
               (denote-sequence--children-implied-p sequence))
      (let ((strings (thread-last
                       (denote-sequence-split sequence)
                       (butlast))))
        (denote-sequence-join strings scheme)))))

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

(defun denote-sequence-get-all-sequences-with-max-depth (depth &optional sequences)
  "Get sequences up to DEPTH (inclusive).
With optional SEQUENCES operate on those, else use the return value of
`denote-sequence-get-all-sequences'."
  (let* ((strings (or sequences (denote-sequence-get-all-sequences)))
         (lists-all (mapcar #'denote-sequence-split strings))
         (lists (seq-filter (lambda (element) (>= (length element) depth)) lists-all)))
    (delete-dups
     (mapcar
      (lambda (strings)
        (denote-sequence-join (seq-take strings depth) (denote-sequence--scheme-of-strings strings)))
      lists))))

(defun denote-sequence--pad (sequence type)
  "Create a new SEQUENCE with padded spaces for TYPE.
TYPE is a symbol among `denote-sequence-types'.  The special TYPE `all'
means to pad the full length of the sequence."
  (let* ((sequence-separator-p (denote-sequence--children-implied-p sequence))
         (split (denote-sequence-split sequence))
         (s (cond
             ((eq type 'all) split)
             (sequence-separator-p
              (pcase type
                ('parent (car split))
                ('sibling split)
                ('child (car (nreverse split)))
                (_ (error "The type `%s' is not among `denote-sequence-types'" type))))
             (t sequence))))
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
  (car
   (reverse
    (sort sequences
          (lambda (s1 s2)
            (string<
             (denote-sequence--pad s1 type)
             (denote-sequence--pad s2 type)))))))

(defun denote-sequence--tail-alphanumeric-p (sequence)
  "Return non-nil if the last character of SEQUENCE is alphanumeric.
This is for use in `denote-sequence--get-start'."
  (denote-sequence--alphanumeric-partial-p (substring sequence -1)))

(defun denote-sequence--get-start (&optional sequence prepend-delimiter)
  "Return the start of a new sequence.
With optional SEQUENCE, do so based on the final level of depth therein.
This is usefule only for the alphanumeric `denote-sequence-scheme'.  If
optional PREPEND-DELIMITER is non-nil, prepend the equals sign to the
number if `denote-sequence-scheme' is numeric."
  (pcase denote-sequence-scheme
    ('numeric (if prepend-delimiter "=1" "1"))
    ('alphanumeric (if (denote-sequence--tail-alphanumeric-p sequence)
                       "1"
                     "a"))))

(defun denote-sequence--get-new-parent (&optional sequences)
  "Return a new to increment largest among sequences.
With optional SEQUENCES consider only those, otherwise operate on the
return value of `denote-sequence-get-all-sequences'."
  (if-let* ((all (or sequences (denote-sequence-get-all-sequences))))
      (let* ((largest (denote-sequence--get-largest all 'parent))
             (first-component (car (denote-sequence-split largest)))
             (current-number (string-to-number first-component)))
        (number-to-string (+ current-number 1)))
    (denote-sequence--get-start)))

(defun denote-sequence-filter-scheme (sequences &optional scheme)
  "Return list of SEQUENCES that are `denote-sequence-scheme' or SCHEME."
  (let ((predicate (pcase (or scheme denote-sequence-scheme)
                     ('alphanumeric #'denote-sequence-alphanumeric-p)
                     ('numeric #'denote-sequence-numeric-p))))
    (seq-filter predicate sequences)))

(defun denote-sequence--get-new-child (sequence &optional sequences)
  "Return a new child of SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (if-let* ((depth (+ (denote-sequence-depth sequence) 1))
            (all-unfiltered (denote-sequence-get-all-sequences-with-prefix sequence sequences))
            (start-child (denote-sequence--get-start sequence :prepend-delimiter)))
      (if (= (length all-unfiltered) 1)
          (format "%s%s" (car all-unfiltered) start-child)
        (if-let* ((all-schemeless (cond
                                   ((= (length all-unfiltered) 1) all-unfiltered)
                                   ((denote-sequence-get-all-sequences-with-max-depth depth all-unfiltered))
                                   (t all-unfiltered)))
                  (all (denote-sequence-filter-scheme all-schemeless))
                  (largest (denote-sequence--get-largest all 'child)))
            (if (denote-sequence--children-implied-p largest)
                (pcase-let* ((`(,largest . ,scheme) (denote-sequence-and-scheme-p largest))
                             (components (denote-sequence-split largest))
                             (butlast (butlast components))
                             (last-component (car (nreverse components)))
                             (new-number (denote-sequence-increment last-component)))
                  (denote-sequence-join
                   (if butlast
                       (append butlast (list new-number))
                     (list largest new-number))
                   scheme))
              (format "%s%s" largest start-child))
          (format "%s%s" sequence start-child)))
    (error "Cannot find sequences given sequence `%s' using scheme `%s'" sequence denote-sequence-scheme)))

(defun denote-sequence--get-prefix-for-siblings (sequence)
  "Get the prefix of SEQUENCE such that it is possible to find its siblings."
  (pcase-let ((`(,sequence . ,scheme) (denote-sequence-and-scheme-p sequence)))
    (when (denote-sequence--children-implied-p sequence)
      (denote-sequence-join (butlast (denote-sequence-split sequence)) scheme))))

(defun denote-sequence--get-new-sibling (sequence &optional sequences)
  "Return a new sibling SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (let* ((children-p (denote-sequence--children-implied-p sequence)))
    (if-let* ((depth (denote-sequence-depth sequence))
              (all-unfiltered (if children-p
                                  (denote-sequence-get-all-sequences-with-prefix
                                   (denote-sequence--get-prefix-for-siblings sequence)
                                   sequences)
                                (denote-sequence-get-all-sequences)))
              (all-schemeless (denote-sequence-get-all-sequences-with-max-depth depth all-unfiltered))
              (all (denote-sequence-filter-scheme all-schemeless))
              ((member sequence all))
              (largest (if children-p
                           (denote-sequence--get-largest all 'sibling)
                         (denote-sequence--get-largest all 'parent))))
        (if children-p
            (pcase-let* ((`(,largest . ,scheme) (denote-sequence-and-scheme-p largest))
                         (components (denote-sequence-split largest))
                         (butlast (butlast components))
                         (last-component (car (nreverse components)))
                         (new-number (denote-sequence-increment last-component)))
              (denote-sequence-join (append butlast (list new-number)) scheme))
          (number-to-string (+ (string-to-number largest) 1)))
      (error "Cannot find sequences given sequence `%s' using scheme `%s'" sequence denote-sequence-scheme))))

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
When called interactively, SEQUENCE is a file among files in the variable
`denote-directory' that have a sequence (per `denote-sequence-file-p').

When called from Lisp, SEQUENCE is a string that conforms with
`denote-sequence-p'."
  (interactive
   (list
    (denote-retrieve-filename-signature
     (denote-sequence-file-prompt "Make a new sibling of SEQUENCE"))))
  (let* ((new-sequence (denote-sequence-get 'sibling sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-sibling-of-current (sequence)
  "Create a new sibling sequence of the current file with SEQUENCE.
If the current file does not have a sequence, then behave exactly like
`denote-sequence-new-sibling'."
  (interactive
   (list
    (or (denote-sequence-file-p buffer-file-name)
        (denote-retrieve-filename-signature
         (denote-sequence-file-prompt "Make a new sibling of SEQUENCE")))))
  (let* ((new-sequence (denote-sequence-get 'sibling sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-child (sequence)
  "Like `denote-sequence' to directly create new child of SEQUENCE.
When called interactively, SEQUENCE is a file among files in the variable
`denote-directory' that have a sequence (per `denote-sequence-file-p').

When called from Lisp, SEQUENCE is a string that conforms with
`denote-sequence-p'."
  (interactive
   (list
    (denote-retrieve-filename-signature
     (denote-sequence-file-prompt "Make a new child of SEQUENCE"))))
  (let* ((new-sequence (denote-sequence-get 'child sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-child-of-current (sequence)
  "Create a new child sequence of the current file with SEQUENCE.
If the current file does not have a sequence, then behave exactly like
`denote-sequence-new-child'."
  (interactive
   (list
    (or (denote-sequence-file-p buffer-file-name)
        (denote-retrieve-filename-signature
         (denote-sequence-file-prompt "Make a new child of SEQUENCE")))))
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

(defun denote-sequence-sort-files (files-with-sequence)
  "Sort FILES-WITH-SEQUENCE according to their sequence."
  (sort
   files-with-sequence
   (lambda (file-with-sequence-1 file-with-sequence-2)
     (let ((s1 (denote-retrieve-filename-signature file-with-sequence-1))
           (s2 (denote-retrieve-filename-signature file-with-sequence-2)))
       (string<
        (denote-sequence--pad s1 'all)
        (denote-sequence--pad s2 'all))))))

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
       (format "Get sequences up to this depth %s: "
               (if (eq denote-sequence-scheme 'alphanumeric)
                   "(e.g. `1a2' is `3' levels of depth)"
                 "(e.g. `1=1=2' is `3' levels of depth)")))))

(defun denote-sequence--get-dired-buffer-name (&optional prefix depth)
  "Return a string for `denote-sequence-dired' buffer.
Use optional PREFIX and DEPTH to format the string accordingly."
  (let ((time (format-time-string "%F %T")))
    (cond
     ((and prefix depth)
      (format-message "*Denote sequences of prefix `%s' and depth `%s', %s*" prefix depth time))
     ((and prefix (not (string-empty-p prefix)))
      (format-message "*Denote sequences of prefix `%s', %s*" prefix time))
     (t
      (format "*Denote sequences, %s*" time)))))

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
            (files-sorted (denote-sequence-sort-files files-with-depth))
            (buffer-name (denote-sequence--get-dired-buffer-name prefix depth)))
      (let ((dired-buffer (dired (cons buffer-name (mapcar #'file-relative-name files-sorted)))))
        (with-current-buffer dired-buffer
          (setq-local revert-buffer-function
                      (lambda (&rest _)
                        ;; FIXME 2025-01-04: Killing the buffer has
                        ;; the unintended side effect of affecting the
                        ;; window configuration when we call
                        ;; `denote-update-dired-buffers'.
                        (kill-buffer dired-buffer)
                        (denote-sequence-dired prefix depth)))))
    (user-error "No Denote sequences matching those terms")))

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
