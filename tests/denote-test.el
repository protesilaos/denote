;;; denote-test.el --- Unit tests for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

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

;; Tests for Denote.  Note that we are using Shorthands in this file,
;; so the "dt-" prefix really is "denote-test-".  Evaluate the
;; following to learn more:
;;
;;    (info "(elisp) Shorthands")

;;; Code:

(require 'ert)

;;;; Tests for denote.el

(require 'denote)

(ert-deftest dt-denote-directories--get-paths ()
  "Test that `denote-directories--get-paths' returns a list of paths."
  (let ((path "~/Documents/notes"))
    (should (equal
             (denote-directories--get-paths path)
             (list (file-name-as-directory (expand-file-name path))))))
  (let ((paths (list "~/Documents/notes" "~/Documents/books")))
    (should (equal
             (denote-directories--get-paths paths)
             (mapcar
              (lambda (path)
                (file-name-as-directory (expand-file-name path)))
              paths)))
    (should (seq-every-p
             (lambda (path)
               (string-suffix-p "/" path))
             (denote-directories--get-paths paths)))
    (should-not (null (denote-directories--get-paths paths))))
  (should-error (denote-directories--get-paths 'hello))
  (should-error (denote-directories--get-paths '(hello)))
  (should-error (denote-directories--get-paths '(1 2 3))))

(ert-deftest dt-denote--make-denote-directory ()
  "Test that `denote--make-denote-directory' creates the directory."
  (let* ((names (list "denote-test-made-directory-1" "denote-test-made-directory-2"))
         (directories (mapcar
                       (lambda (directory)
                         (file-name-as-directory (expand-file-name directory temporary-file-directory)))
                       names)))
    (should-not (denote-directories--make-paths directories))
    (should (seq-every-p #'file-name-directory directories))
    (dolist (directory directories)
      (delete-directory directory))))

(ert-deftest dt-denote-directories ()
  "Test that `denote-directories' returns list of directories.
It does so by expanding the value of the user option `denote-directory'.

Also see `denote-test--denote--make-denote-directory',
`denote-test---denote-directories--get-paths'."
  (let ((denote-directory "/tmp/denote-test-notes"))
    (should
     (equal
      (denote-directories)
      (list (file-name-as-directory (expand-file-name denote-directory)))))
    (should (and (file-directory-p denote-directory)
                 (file-name-absolute-p denote-directory))))
  (let ((denote-directory (list "/tmp/denote-test-notes-1" "/tmp/denote-test-notes-2")))
    (should
     (equal
      (denote-directories)
      (mapcar
       (lambda (directory)
         (file-name-as-directory (expand-file-name directory)))
       denote-directory)))
    (should (and (seq-every-p #'file-directory-p denote-directory)
                 (seq-every-p #'file-name-absolute-p denote-directory)))))

(ert-deftest dt-denote-sluggify-title ()
  "Test that `denote-sluggify-title' removes punctuation from the string.
Concretely, remove anything specified in `denote-sluggify-title'."
  (should (equal (denote-sluggify-title "this-is-!@#test") "this-is-test")))

(ert-deftest dt-denote-slug-keep-only-ascii ()
  "Test that `denote-slug-keep-only-ascii' removes non-ASCII characters."
  (should (equal
           (denote-slug-keep-only-ascii "There are no-ASCII ： characters ｜ here 😀")
           "There are no-ASCII   characters   here  ")))

(ert-deftest dt-denote-slug-hyphenate ()
  "Test that `denote-slug-hyphenate' hyphenates the string.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (should (equal (denote-slug-hyphenate "__  This is   a    test  __  ") "This-is-a-test")))

(ert-deftest dt-denote-sluggify ()
  "Test that `denote-sluggify' sluggifies the string.
To sluggify is to (i) downcase, (ii) hyphenate, (iii) de-punctuate, and (iv) remove spaces from the string."
  (should (equal (denote-sluggify 'title " ___ !~!!$%^ This iS a tEsT ++ ?? ") "this-is-a-test")))

(ert-deftest Ddenote--slug-put-equals ()
  "Test that `denote-slug-put-equals' replaces spaces/underscores with =.
Otherwise do the same as what is described in
`denote-test--denote-slug-hyphenate'.

The use of the equals sign is for the SIGNATURE field of the
Denote file name."
  (should (equal (denote-slug-put-equals "__  This is   a    test  __  ") "This=is=a=test")))

(ert-deftest dt-denote-sluggify-signature ()
  "Test that `denote-sluggify-signature' sluggifies the string for file signatures.
This is like `denote-test--denote-sluggify', except that it also
accounts for what we describe in `denote-test--denote-slug-put-equals'."
  (should (equal (denote-sluggify-signature "--- ___ !~!!$%^ This -iS- a tEsT ++ ?? ") "this=is=a=test")))

(ert-deftest dt-denote-sluggify-keyword ()
  "Test that `denote-sluggify-keyword' sluggifies the string while joining words.
In this context, to join words is to elimitate any space or
delimiter between them.

Otherwise, this is like `denote-test--denote-sluggify'."
  (should (equal (denote-sluggify-keyword "--- ___ !~!!$%^ This iS a - tEsT ++ ?? ") "thisisatest")))

(ert-deftest dt-denote-sluggify-keywords ()
  "Test that `denote-sluggify-keywords' sluggifies a list of strings.
The function also account for the value of the user option
`denote-allow-multi-word-keywords'."
  (should (equal (denote-sluggify-keywords '("one !@# --- one" "   two" "__  three  __")) '("oneone" "two" "three"))))

(ert-deftest dt-denote--file-empty-p ()
  "Test that `denote--file-empty-p' returns non-nil on empty file."
  (let ((file (make-temp-file "denote-test")))
    (should (denote--file-empty-p file))
    (with-current-buffer (find-file-noselect file)
      (insert "Hello world, this is not empty anymore!")
      (save-buffer))
    (should-not (denote--file-empty-p file))
    (delete-file file)))

(ert-deftest dt-denote-file-has-denoted-filename-p ()
  "Test that `denote-file-has-denoted-filename-p' validates a Denote file name."
  (should (denote-file-has-denoted-filename-p "20230522T154900--test__keyword.txt"))
  (should-not (denote-file-has-denoted-filename-p "hello")))

(ert-deftest dt-denote-file-has-identifier-p ()
  "Test that `denote-file-has-identifier-p' checks for a Denote identifier."
  (should (denote-file-has-identifier-p "20230522T154900--test__keyword.txt"))
  (should-not (denote-file-has-identifier-p "T154900--test__keyword.txt")))

(ert-deftest dt-denote-file-has-signature-p ()
  "Test that `denote-file-has-signature-p' checks for a Denote signature."
  (should (denote-file-has-signature-p "20230522T154900==sig--test__keyword.txt"))
  (should-not (denote-file-has-signature-p "20230522T154900--test__keyword.txt")))

(ert-deftest dt-denote-file-has-supported-extension-p ()
  "Test that `denote-file-has-supported-extension-p' matches a supported extension."
  (let ((extensions (denote-file-type-extensions-with-encryption)))
    (should
     (member
      (file-name-extension "20230522T154900==sig--test__keyword.txt" :period)
      extensions))
    (should-not
     (member
      (file-name-extension "20230522T154900==sig--test__keyword" :period)
      extensions))))

(ert-deftest dt-denote-file-type-extensions ()
  "Test that `denote-file-type-extensions' returns file extensions.
We check for the common file type extensions, though the user can set
`denote-file-types' to nil and handle things on their own or, anyhow,
modify that variable."
  (let ((extensions (denote-file-type-extensions)))
    (should (or (member ".md" extensions)
                (member ".org" extensions)
                (member ".txt" extensions)))))

(ert-deftest dt-denote-file-type-extensions-with-encryption ()
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

(ert-deftest dt-denote--format-front-matter ()
  "Test that `denote--format-front-matter' formats front matter correctly.
To make the test reproducible, set `denote-date-format' to a value that
does not involve the time zone."
  (let ((denote-date-format "%Y-%m-%d")
        (denote-front-matter-components-present-even-if-empty-value '(title keywords signature date identifier)))
    (should (equal (denote--format-front-matter "" (date-to-time "20240101T120000") '("") "" "" 'text)
                   (string-join
                    '("title:      "
                      "date:       2024-01-01"
                      "tags:       "
                      "identifier: "
                      "signature:  "
                      "---------------------------\n\n")
                    "\n")))
    (should (equal (denote--format-front-matter "Some test" (date-to-time "2023-06-05") '("one" "two") "20230605T102234" "sig" 'text)
                   (string-join
                    '("title:      Some test"
                      "date:       2023-06-05"
                      "tags:       one  two"
                      "identifier: 20230605T102234"
                      "signature:  sig"
                      "---------------------------\n\n")
                    "\n")))
    (should (equal (denote--format-front-matter "" (date-to-time "20240101T120000") nil "" "" 'org)
                   (string-join
                    '("#+title:      "
                      "#+date:       2024-01-01"
                      "#+filetags:   "
                      "#+identifier: "
                      "#+signature:  "
                      "\n")
                    "\n")))
    (should (equal
             (denote--format-front-matter
              "Some test" (date-to-time "2023-06-05") '("one" "two")
              "20230605T102234" "sig" 'org)
             (string-join
              '("#+title:      Some test"
                "#+date:       2023-06-05"
                "#+filetags:   :one:two:"
                "#+identifier: 20230605T102234"
                "#+signature:  sig"
                "\n")
              "\n")))
    (should (equal (denote--format-front-matter "" (date-to-time "20240101T120000") nil "" "" 'markdown-yaml)
                   (string-join
                    '("---"
                      "title:      \"\""
                      "date:       2024-01-01"
                      "tags:       []"
                      "identifier: \"\""
                      "signature:  \"\""
                      "---"
                      "\n")
                    "\n")))
    (should (equal (denote--format-front-matter "Some test" (date-to-time "2023-06-05") '("one" "two") "20230605T102234" "sig" 'markdown-yaml)
             (string-join
              '("---"
                "title:      \"Some test\""
                "date:       2023-06-05"
                "tags:       [\"one\", \"two\"]"
                "identifier: \"20230605T102234\""
                "signature:  \"sig\""
                "---"
                "\n")
              "\n")))
    (should (equal (denote--format-front-matter "" (date-to-time "20240101T120000") nil "" "" 'markdown-toml)
                   (string-join
                    '("+++"
                      "title      = \"\""
                      "date       = 2024-01-01"
                      "tags       = []"
                      "identifier = \"\""
                      "signature  = \"\""
                      "+++"
                      "\n")
                    "\n")))
    (should (equal (denote--format-front-matter "Some test" (date-to-time "2023-06-05") '("one" "two") "20230605T102234" "sig" 'markdown-toml)
                   (string-join
                    '("+++"
                      "title      = \"Some test\""
                      "date       = 2023-06-05"
                      "tags       = [\"one\", \"two\"]"
                      "identifier = \"20230605T102234\""
                      "signature  = \"sig\""
                      "+++"
                      "\n")
                    "\n")))))

(ert-deftest dt-denote-format-file-name ()
  "Test that `denote-format-file-name' returns all expected paths."
  (let* ((title "Some test")
         (id (format-time-string denote-date-identifier-format (denote-valid-date-p "2023-11-28 05:53:11")))
         (denote-directory "/tmp/test-denote")
         (dir (car (denote-directories)))
         (ext (denote--file-extension 'org))
         (kws '("one" "two")))
    (should-error (denote-format-file-name nil id kws title ext ""))
    (should-error (denote-format-file-name "" id kws title ext ""))
    ;; NOTE that `denote-directory' is the `let' bound value without the suffix
    (should-error (denote-format-file-name denote-directory id kws title ext ""))
    (should-error (denote-format-file-name dir "" nil "" ext ""))
    (should (equal (denote-format-file-name dir nil kws title ext "") "/tmp/test-denote/--some-test__one_two.org"))
    (should (equal (denote-format-file-name dir "" kws title ext "") "/tmp/test-denote/--some-test__one_two.org"))
    (should (equal (denote-format-file-name dir "0123456" kws title ext "") "/tmp/test-denote/@@0123456--some-test__one_two.org"))
    (should (equal (denote-format-file-name dir id kws title ext "") "/tmp/test-denote/20231128T055311--some-test__one_two.org"))
    (should (equal (denote-format-file-name dir id nil "" ext "") "/tmp/test-denote/20231128T055311.org"))
    (should (equal (denote-format-file-name dir id nil nil ext nil) "/tmp/test-denote/20231128T055311.org"))
    (should (equal (denote-format-file-name dir id kws title ext "sig") "/tmp/test-denote/20231128T055311==sig--some-test__one_two.org"))))

(ert-deftest dt-denote-get-file-extension ()
  "Test that `denote-get-file-extension' gets the correct file extension."
  (should (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing") ""))
  (should (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing.org") ".org"))
  (should (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing.org.gpg") ".org.gpg"))
  (should (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing.org.age") ".org.age")))

(ert-deftest dt-denote-get-file-extension-sans-encryption ()
  "Test that `denote-get-file-extension-sans-encryption' gets the file extension without encryption."
  (should (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing") ""))
  (should (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing.org") ".org"))
  (should (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing.org.gpg") ".org"))
  (should (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing.org.age") ".org")))

;; TODO 2026-04-06: For the `denote-file-type'.

(ert-deftest dt-denote-filetype-heuristics ()
  "Test that `denote-filetype-heuristics' gets the correct file type."
  (should-not (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing"))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.org") 'org))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.org.gpg") 'org))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.org.age") 'org))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.txt") 'text))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.txt.gpg") 'text))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.txt.age") 'text))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.md")
              (caar (denote--file-types-with-extension ".md"))))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.md.gpg")
              (caar (denote--file-types-with-extension ".md"))))
  (should (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.md.age")
              (caar (denote--file-types-with-extension ".md")))))

(ert-deftest dt-denote-retrieve-filename-identifier ()
  "Test that `denote-retrieve-filename-identifier' returns only the identifier."
  (should (null (denote-retrieve-filename-identifier "/path/to/testing/--this-is-a-test-reordered__denote_testing.org")))
  (should (equal (denote-retrieve-filename-identifier "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org") "20240610T194654"))
  (should (equal (denote-retrieve-filename-identifier "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org") "20240610T194654"))
  (should (equal (denote-retrieve-filename-identifier "/path/to/testing/--this-is-a-test-reordered__denote_testing@@20240610T194654.org") "20240610T194654"))
  (should (equal (denote-retrieve-filename-identifier "/path/to/testing/__denote_testing--this-is-a-test-reordered@@20240610T194654.org") "20240610T194654"))
  (should (equal (denote-retrieve-filename-identifier "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "20240610T194654"))
  (should (equal (denote-retrieve-filename-identifier "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "20240610T194654")))

(ert-deftest dt-denote-retrieve-filename-title ()
  "Test that `denote-retrieve-filename-title' returns only the title."
  (should (null (denote-retrieve-filename-title "/path/to/testing/20240610T194654__denote_testing.org")))
  (should (equal (denote-retrieve-filename-title "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org") "this-is-a-test-reordered"))
  (should (equal (denote-retrieve-filename-title "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org") "this-is-a-test-reordered"))
  (should (equal (denote-retrieve-filename-title "/path/to/testing/--this-is-a-test-reordered__denote_testing@@20240610T194654.org") "this-is-a-test-reordered"))
  (should (equal (denote-retrieve-filename-title "/path/to/testing/__denote_testing--this-is-a-test-reordered@@20240610T194654.org") "this-is-a-test-reordered"))
  (should (equal (denote-retrieve-filename-title "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "this-is-a-test-reordered"))
  (should (equal (denote-retrieve-filename-title "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "this-is-a-test-reordered")))

(ert-deftest dt-denote-retrieve-filename-keywords ()
  "Test that `denote-retrieve-filename-keywords' returns only the keywords."
  (should (null (denote-retrieve-filename-keywords "/path/to/testing/20240610T194654--this-is-a-test-reordered.org")))
  (should (equal (denote-retrieve-filename-keywords "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org") "denote_testing"))
  (should (equal (denote-retrieve-filename-keywords "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org") "denote_testing"))
  (should (equal (denote-retrieve-filename-keywords "/path/to/testing/--this-is-a-test-reordered__denote_testing@@20240610T194654.org") "denote_testing"))
  (should (equal (denote-retrieve-filename-keywords "/path/to/testing/__denote_testing--this-is-a-test-reordered@@20240610T194654.org") "denote_testing"))
  (should (equal (denote-retrieve-filename-keywords "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "denote_testing"))
  (should (equal (denote-retrieve-filename-keywords "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "denote_testing")))

(ert-deftest dt-denote-retrieve-filename-signature ()
  "Test that `denote-retrieve-filename-signature' returns only the signature."
  (should (null (denote-retrieve-filename-signature "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org")))
  (should (equal (denote-retrieve-filename-signature "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org") "signature"))
  (should (equal (denote-retrieve-filename-signature "/path/to/testing/--this-is-a-test-reordered==signature__denote_testing@@20240610T194654.org") "signature"))
  (should (equal (denote-retrieve-filename-signature "/path/to/testing/__denote_testing--this-is-a-test-reordered==signature@@20240610T194654.org") "signature"))
  (should (equal (denote-retrieve-filename-signature "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered==signature.org") "signature"))
  (should (equal (denote-retrieve-filename-signature "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org") "signature")))

(ert-deftest dt-denote-date-identifier-p ()
  "Test that `denote-date-identifier-p' works for Denote identifiers."
  (should (denote-date-identifier-p "20240901T090910"))
  (should-not (denote-date-identifier-p "20240901T090910-not-identifier-format")))

(ert-deftest dt-denote-id-to-date ()
  "Test that `denote-id-to-date' returns the date from an identifier."
  (should (equal (denote-id-to-date "20240901T090910") "2024-09-01"))
  (should-error (denote-id-to-date "20240901T090910-not-identifier-format")))

(ert-deftest dt-denote--date-convert ()
  "Test that `denote--date-convert' works with dates."
  (should (equal (denote--date-convert '(26454 45206 461174 657000) :list) '(26454 45206 461174 657000)))
  (should (equal (denote--date-convert '(26454 45206 461174 657000) :string) "2024-12-09 10:55:50"))
  (should (equal (denote--date-convert nil :string) ""))
  (should (equal (denote--date-convert nil :list) nil))
  (should-error (denote--date-convert '(26454 45206 461174 657000) :not-valid-type))
  (should-error (denote--date-convert nil :not-valid-type)))

;; TODO 2026-02-23: Add support for Windows paths.  But I need somebody who has a Windows machine...
(ert-deftest dt-denote--get-common-root-directory ()
  "Test that `denote--get-common-root-directory' returns the right path."
  (should (string=
           (denote--get-common-root-directory
            '("/home/prot/Documents/notes/"
              "/home/prot/Documents/notes/attachments"))
           "/home/prot/Documents/notes/"))
  (should (string=
           (denote--get-common-root-directory
            '("/home/prot/Documents/"
              "/home/prot/Documents/notes/attachments"))
           "/home/prot/Documents/"))
  (should (string=
           (denote--get-common-root-directory
            '("/home/prot/Books/"
              "/home/prot/Documents/notes/"))
           "/home/prot/"))
  (should (string=
           (denote--get-common-root-directory
            '("/tmp/notes/"
              "/home/prot/Documents/notes/"))
           "/"))
  (should (string=
           (denote--get-common-root-directory
            '("/home/prot/Documents/notes/"))
           "/home/prot/Documents/notes/"))
  (should-error (denote--get-common-root-directory '("~/Documents/notes/"))))

(ert-deftest dt-denote-query--keywords-as-regexp ()
  "Test that `denote-query--keywords-as-regexp' works as intended."
  (should (string= (denote-query--keywords-as-regexp '("one" "two")) "_\\(?:one\\|two\\)"))
  (should-error (denote-query--keywords-as-regexp "one"))
  (should-error (denote-query--keywords-as-regexp '("one" 1))))

(ert-deftest dt-denote-file-type ()
  "Test that function `denote-file-type' does what it is meant to."
  (let* ((create-file-fn (lambda (name extension)
                           (expand-file-name
                            (format "denote-test-file-type-%s.%s" name extension)
                            (temporary-file-directory))))
         (modify-file-fn (lambda (file contents)
                           (with-current-buffer (find-file-noselect file)
                             (erase-buffer)
                             (insert contents)
                             (save-buffer))))
         (yaml-file (funcall create-file-fn "yaml" "md"))
         (toml-file (funcall create-file-fn "toml" "md"))
         (org-file (funcall create-file-fn "org" "org")))
    (progn
      (funcall modify-file-fn yaml-file denote-yaml-front-matter)
      (should (eq (denote-file-type yaml-file) 'markdown-yaml)))
    (progn
      (funcall modify-file-fn toml-file denote-toml-front-matter)
      (should (eq (denote-file-type toml-file) 'markdown-toml)))
    (progn
      (funcall modify-file-fn org-file denote-org-front-matter)
      (should (eq (denote-file-type org-file) 'org)))
    (should-not (denote-file-type "test"))
    (let ((denote-file-types nil))
      (should-not (denote-file-type "test.md")))))

(provide 'denote-test)
;;; denote-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dt" . "denote-test-"))
;; End:
