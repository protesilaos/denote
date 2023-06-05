;;; denote-test.el --- Unit tests for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Denote Development <~protesilaos/denote@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/denote
;; Mailing-List: https://lists.sr.ht/~protesilaos/denote

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

;; WORK-IN-PROGRESS

;;; Code:

(require 'ert)
(require 'denote)

;; TODO 2023-05-22: Incorporate an actual silo in this test directory
;; and modify the test accordingly.
(ert-deftest denote-test-denote--default-directory-is-silo-p ()
  "Test that `denote--default-directory-is-silo-p' returns a path."
  (let ((path (denote--default-directory-is-silo-p)))
    (should (or (null path)
                (and (stringp path)
                     (file-exists-p path)
                     (file-directory-p path))))))

(ert-deftest denote-test--denote--make-denote-directory ()
  "Test that `denote--make-denote-directory' creates the directory."
  (should (null (denote--make-denote-directory))))

(ert-deftest denote-test--denote-directory ()
  "Test that variable `denote-directory' returns an absolute directory name."
  (let ((path (denote-directory)))
    (should (and (file-directory-p path)
                 (file-name-absolute-p path)))))

(ert-deftest denote-test--denote--slug-no-punct ()
  "Test that `denote--slug-no-punct' removes punctuation from the string.
Concretely, replace with spaces anything that matches the
`denote-excluded-punctuation-regexp' and
`denote-excluded-punctuation-extra-regexp'."
  (should (equal (denote--slug-no-punct "This is !@# test")
                 "This is  test")))

(ert-deftest denote-test--denote--slug-hyphenate ()
  "Test that `denote--slug-hyphenate' hyphenates the string.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (should (equal (denote--slug-hyphenate "__  This is   a    test  __  ")
                 "This-is-a-test")))

(ert-deftest denote-test--denote-sluggify ()
  "Test that `denote-sluggify' sluggifies the string.
To sluggify is to (i) downcase, (ii) hyphenate, (iii) de-punctuate, and (iv) remove spaces from the string."
  (should (equal (denote-sluggify " ___ !~!!$%^ This iS a tEsT ++ ?? ")
                 "this-is-a-test")))

(ert-deftest denote-test--denote--slug-put-equals ()
  "Test that `denote--slug-put-equals' replaces spaces/underscores with =.
Otherwise do the same as what is described in
`denote-test--denote--slug-hyphenate'.

The use of the equals sign is for the SIGNATURE field of the
Denote file name."
  (should (equal (denote--slug-put-equals "__  This is   a    test  __  ")
                 "This=is=a=test")))

(ert-deftest denote-test--denote-sluggify-signature ()
  "Test that `denote-sluggify-signature' sluggifies the string for file signatures.
This is like `denote-test--denote-sluggify', except that it also
accounts for what we describe in `denote-test--denote--slug-put-equals'."
  (should (equal (denote-sluggify-signature " ___ !~!!$%^ This iS a tEsT ++ ?? ")
                 "this=is=a=test")))

(ert-deftest denote-test--denote-sluggify-and-join ()
  "Test that `denote-sluggify-and-join' sluggifies the string while joining words.
In this context, to join words is to elimitate any space or
delimiter between them.

Otherwise, this is like `denote-test--denote-sluggify'."
  (should (equal (denote-sluggify-and-join " ___ !~!!$%^ This iS a tEsT ++ ?? ")
                 "thisisatest")))

(ert-deftest denote-test--denote-sluggify-keywords ()
  "Test that `denote-sluggify-keywords' sluggifies a list of strings.
The function also account for the value of the user option
`denote-allow-multi-word-keywords'."
  (should
   (let ((denote-allow-multi-word-keywords nil))
     (equal (denote-sluggify-keywords '("one !@# one" "   two" "__  three  __"))
            '("oneone" "two" "three"))))
  (should
   (let ((denote-allow-multi-word-keywords t))
     (equal (denote-sluggify-keywords '("one !@# one" "   two" "__  three  __"))
            '("one-one" "two" "three")))))

(ert-deftest denote-test--denote-desluggify ()
  "Test that `denote-desluggify' upcases first character and de-hyphenates string."
  (should (equal (denote-desluggify "this-is-a-test") "This is a test"))
  (should (null (equal (denote-desluggify "this=is=a=test") "This is a test"))))

(ert-deftest denote-test--denote--file-empty-p ()
  "Test that `denote--file-empty-p' returns non-nil on empty file."
  ;; (should (null (denote--file-empty-p user-init-file))
  (should (let ((file (make-temp-file "denote-test")))
            (prog1
                (denote--file-empty-p file)
              (delete-file file)))))

(ert-deftest denote-test--denote-file-is-note-p ()
  "Test that `denote-file-is-note-p' checks that files is a Denote note.
For our purposes, a note must note be a directory, must satisfy
`file-regular-p', its path must be part of the variable
`denote-directory', it must have a Denote identifier in its name,
and use one of the extensions implied by `denote-file-type'."
  (should (let* ((tmp (temporary-file-directory))
                 (denote-directory tmp)
                 (file (concat tmp "20230522T154900--test__keyword.txt")))
            (with-current-buffer (find-file-noselect file)
              (write-file file))
            (prog1
                (denote-file-is-note-p file)
              (delete-file file)))))

(ert-deftest denote-test--denote-file-has-identifier-p ()
  "Test that `denote-file-has-identifier-p' checks for a Denote identifier."
  (should (denote-file-has-identifier-p "20230522T154900--test__keyword.txt"))
  (should (null (denote-file-has-identifier-p "T154900--test__keyword.txt"))))

(ert-deftest denote-test--denote-file-has-signature-p ()
  "Test that `denote-file-has-signature-p' checks for a Denote signature."
  (should (denote-file-has-signature-p "20230522T154900==sig--test__keyword.txt"))
  (should (null (denote-file-has-signature-p "20230522T154900--test__keyword.txt"))))

(ert-deftest denote-test--denote-file-has-supported-extension-p ()
  "Test that `denote-file-has-supported-extension-p' matches a supported extension."
  (should
   (member
    (file-name-extension "20230522T154900==sig--test__keyword.txt" :period)
    (denote-file-type-extensions-with-encryption)))
  (should
   (null
    (member
     (file-name-extension "20230522T154900==sig--test__keyword" :period)
     (denote-file-type-extensions-with-encryption)))))

(ert-deftest denote-test--denote-file-type-extensions ()
  "Test that `denote-file-type-extensions' returns file extensions.
We check for the common file type extensions, though the user can
theoretically set `denote-file-types' to nil and handle things on
their own.  We do not have to test for that scenario, because
such a user will be redefining large parts of Denote's behaviour
with regard to file types."
  (let ((extensions (denote-file-type-extensions)))
    (should (or (member ".md" extensions)
                (member ".org" extensions)
                (member ".txt" extensions)))))

(ert-deftest denote-test--denote-file-type-extensions-with-encryption ()
  "Test that `denote-file-type-extensions-with-encryption' covers encryption.
Extend what we do in `denote-test--denote-file-type-extensions'."
  (let ((extensions (denote-file-type-extensions-with-encryption)))
    (should (or (member ".md" extensions)
                (member ".org" extensions)
                (member ".txt" extensions)
                (member ".md.gpg" extensions)
                (member ".org.gpg" extensions)
                (member ".txt.gpg" extensions)
                (member ".md.age" extensions)
                (member ".org.age" extensions)
                (member ".txt.age" extensions)))))

(provide 'denote-test)
;;; denote-test.el ends here
