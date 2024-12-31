;;; denote-sequence.el --- Sequence notes extension for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

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

;; WORK-IN-PROGRESS.  Sequence notes extension for Denote.

;;; Code:

;; FIXME 2024-12-25: Right now I am hardcoding the = as a field
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
  "Sequence notes extension for denote."
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

(defun denote-sequence-split (sequence)
  "Split the SEQUENCE string into a list.
SEQUENCE conforms with `denote-sequence-p'."
  (if (denote-sequence-p sequence)
      (split-string sequence "=" t)
    (error "The sequence `%s' does not pass `denote-sequence-p'" sequence)))

(defun denote-sequence-depth (sequence)
  "Get the depth of SEQUENCE.
For example, 1=2=1 is three levels deep."
  (length (denote-sequence-split sequence)))

(defun denote-sequence-get-all-files ()
  "Return all files in variable `denote-directory' with a sequence.
A sequence is a Denote signature that conforms with `denote-sequence-p'."
  (seq-filter
   (lambda (file)
     (when-let* ((signature (denote-retrieve-filename-signature file)))
       (denote-sequence-p signature)))
   (denote-directory-files)))

(defun denote-sequence-get-all-sequences (&optional files)
  "Return all sequences in `denote-directory-files'.
A sequence is a Denote signature that conforms with `denote-sequence-p'.

With optional FILES return all sequences among them instead."
  (delq nil
        (mapcar
         (lambda (file)
           (when-let* ((signature (denote-retrieve-filename-signature file)))
             (denote-sequence-p signature)))
         (or files (denote-directory-files)))))

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
TYPE is a symbol among `denote-sequence-types'."
  (let* ((sequence-separator-p (string-match-p "=" sequence))
         (split (denote-sequence-split sequence))
         (s (if sequence-separator-p
                (pcase type
                  ('parent (car split))
                  ('sibling split)
                  ('child (car (nreverse split)))
                  ;; FIXME 2024-12-30: This is the last descendant. Do
                  ;; we define a new `descendant' type or do we error
                  ;; here?
                  (_ (butlast split)))
              sequence)))
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
             (first-component (car (split-string largest "=")))
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

(defun denote-sequence--get-new-sibling (sequence &optional sequences)
  "Return a new sibling SEQUENCE.
Optional SEQUENCES has the same meaning as that specified in the
function `denote-sequence-get-all-sequences-with-prefix'."
  (let* ((children-p (string-match-p "=" sequence)))
    (if-let* ((all (if children-p
                       (denote-sequence-get-all-sequences-with-prefix sequence sequences)
                     (denote-sequence-get-all-sequences)))
              (largest (if children-p
                           (denote-sequence--get-largest all 'sibling)
                         (denote-sequence--get-largest all 'parent))))
        (if children-p
            (let* ((components (split-string largest "="))
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

(defun denote-sequence-file-prompt ()
  "Prompt for file with sequence in variable `denote-directory'.
A sequence is a Denote signature that conforms with `denote-sequence-p'."
  (if-let* ((relative-files (mapcar #'denote-get-file-name-relative-to-denote-directory
                                      (denote-sequence-get-all-files)))
              (prompt "Select FILE with sequence: ")
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
        (denote-sequence-file-prompt)))))
  ;; TODO 2024-12-30: Do we need to wrap this in the following?
  ;;
  ;; (cl-letf (((alist-get 'signature denote-file-name-slug-functions) #'denote-sluggify-signature))
  (let* ((sequence (denote-retrieve-filename-signature file-with-sequence))
         (new-sequence (denote-sequence-get type sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-parent ()
  "Convenience wrapper of `denote-sequence' to create new parent."
  (declare (interactive-only t))
  (interactive)
  (let* ((new-sequence (denote-sequence-get 'parent))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-sibling (sequence)
  "Convenience wrapper of `denote-sequence' to create new sibling of SEQUENCE."
  (interactive (list (denote-retrieve-filename-signature (denote-sequence-file-prompt))))
  (let* ((new-sequence (denote-sequence-get 'sibling sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

;;;###autoload
(defun denote-sequence-new-child (sequence)
  "Convenience wrapper of `denote-sequence' to create new child of SEQUENCE."
  (interactive (list (denote-retrieve-filename-signature (denote-sequence-file-prompt))))
  (let* ((new-sequence (denote-sequence-get 'child sequence))
         (denote-use-signature new-sequence))
    (call-interactively 'denote)))

(provide 'denote-sequence)
;;; denote-sequence.el ends here
