;;; denote-test.el --- Unit tests for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Free Software Foundation, Inc.

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

(ert-deftest dt-denote--make-denote-directory ()
  "Test that `denote--make-denote-directory' creates the directory."
  (should (null (denote--make-denote-directory))))

(ert-deftest dt-denote-directory ()
  "Test that variable `denote-directory' returns an absolute directory name."
  (let ((path (denote-directory)))
    (should (and (file-directory-p path)
                 (file-name-absolute-p path)))))

(ert-deftest dt-denote-sluggify-title ()
  "Test that `denote-sluggify-title' removes punctuation from the string.
Concretely, remove anything specified in `denote-sluggify-title'."
  (should (equal (denote-sluggify-title "this-is-!@#test")
                 "this-is-test")))

(ert-deftest dt-denote-slug-keep-only-ascii ()
  "Test that `denote-slug-keep-only-ascii' removes non-ASCII characters."
  (should (equal
           (denote-slug-keep-only-ascii "There are no-ASCII ： characters ｜ here 😀")
           "There are no-ASCII   characters   here  ")))

(ert-deftest dt-denote-slug-hyphenate ()
  "Test that `denote-slug-hyphenate' hyphenates the string.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (should (equal (denote-slug-hyphenate "__  This is   a    test  __  ")
                 "This-is-a-test")))

(ert-deftest dt-denote-sluggify ()
  "Test that `denote-sluggify' sluggifies the string.
To sluggify is to (i) downcase, (ii) hyphenate, (iii) de-punctuate, and (iv) remove spaces from the string."
  (should (equal (denote-sluggify 'title " ___ !~!!$%^ This iS a tEsT ++ ?? ")
                 "this-is-a-test")))

(ert-deftest Ddenote--slug-put-equals ()
  "Test that `denote-slug-put-equals' replaces spaces/underscores with =.
Otherwise do the same as what is described in
`dt-denote-slug-hyphenate'.

The use of the equals sign is for the SIGNATURE field of the
Denote file name."
  (should (equal (denote-slug-put-equals "__  This is   a    test  __  ")
                 "This=is=a=test")))

(ert-deftest dt-denote-sluggify-signature ()
  "Test that `denote-sluggify-signature' sluggifies the string for file signatures.
This is like `dt-denote-sluggify', except that it also
accounts for what we describe in `dt-denote-slug-put-equals'."
  (should (equal (denote-sluggify-signature "--- ___ !~!!$%^ This -iS- a tEsT ++ ?? ")
                 "this=is=a=test")))

(ert-deftest dt-denote-sluggify-keyword ()
  "Test that `denote-sluggify-keyword' sluggifies the string while joining words.
In this context, to join words is to elimitate any space or
delimiter between them.

Otherwise, this is like `dt-denote-sluggify'."
  (should (equal (denote-sluggify-keyword "--- ___ !~!!$%^ This iS a - tEsT ++ ?? ")
                 "thisisatest")))

(ert-deftest dt-denote-sluggify-keywords ()
  "Test that `denote-sluggify-keywords' sluggifies a list of strings.
The function also account for the value of the user option
`denote-allow-multi-word-keywords'."
  (should
   (equal (denote-sluggify-keywords '("one !@# --- one" "   two" "__  three  __"))
          '("oneone" "two" "three"))))

(ert-deftest dt-denote--file-empty-p ()
  "Test that `denote--file-empty-p' returns non-nil on empty file."
  ;; (should (null (denote--file-empty-p user-init-file))
  (should (let ((file (make-temp-file "denote-test")))
            (prog1
                (denote--file-empty-p file)
              (delete-file file)))))

(ert-deftest dt-denote-file-is-note-p ()
  "Test that `denote-file-is-note-p' checks that files is a Denote note.
For our purposes, a note must note be a directory, must satisfy
`file-regular-p', its path must be part of the variable
`denote-directory', it must have a Denote identifier in its name,
and use one of the extensions implied by the variable `denote-file-type'."
  (should (let* ((tmp (temporary-file-directory))
                 (denote-directory tmp)
                 (file (concat tmp "20230522T154900--test__keyword.txt")))
            (with-current-buffer (find-file-noselect file)
              (write-file file))
            (prog1
                (denote-file-is-note-p file)
              (delete-file file)))))

(ert-deftest dt-denote-file-has-identifier-p ()
  "Test that `denote-file-has-identifier-p' checks for a Denote identifier."
  (should (denote-file-has-identifier-p "20230522T154900--test__keyword.txt"))
  (should (null (denote-file-has-identifier-p "T154900--test__keyword.txt"))))

(ert-deftest dt-denote-file-has-signature-p ()
  "Test that `denote-file-has-signature-p' checks for a Denote signature."
  (should (denote-file-has-signature-p "20230522T154900==sig--test__keyword.txt"))
  (should (null (denote-file-has-signature-p "20230522T154900--test__keyword.txt"))))

(ert-deftest dt-denote-file-has-supported-extension-p ()
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

(ert-deftest dt-denote-file-type-extensions ()
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

(ert-deftest dt-denote-file-type-extensions-with-encryption ()
  "Test that `denote-file-type-extensions-with-encryption' covers encryption.
Extend what we do in `dt-denote-file-type-extensions'."
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
    (should (and (equal (denote--format-front-matter "" (date-to-time "20240101T120000") '("") "" "" 'text)
                        (mapconcat #'identity
                                   '("title:      "
                                     "date:       2024-01-01"
                                     "tags:       "
                                     "identifier: "
                                     "signature:  "
                                     "---------------------------\n\n")
                                   "\n"))

                 (equal
                  (denote--format-front-matter
                   "Some test" (date-to-time "2023-06-05") '("one" "two")
                   "20230605T102234" "sig" 'text)
                  (mapconcat #'identity
                             '("title:      Some test"
                               "date:       2023-06-05"
                               "tags:       one  two"
                               "identifier: 20230605T102234"
                               "signature:  sig"
                               "---------------------------\n\n")
                             "\n"))))

    (should (and (equal (denote--format-front-matter "" (date-to-time "20240101T120000") nil "" "" 'org)
                        (mapconcat #'identity
                                   '("#+title:      "
                                     "#+date:       2024-01-01"
                                     "#+filetags:   "
                                     "#+identifier: "
                                     "#+signature:  "
                                     "\n")
                                   "\n"))

                 (equal
                  (denote--format-front-matter
                   "Some test" (date-to-time "2023-06-05") '("one" "two")
                   "20230605T102234" "sig" 'org)
                  (mapconcat #'identity
                             '("#+title:      Some test"
                               "#+date:       2023-06-05"
                               "#+filetags:   :one:two:"
                               "#+identifier: 20230605T102234"
                               "#+signature:  sig"
                               "\n")
                             "\n"))))

    (should (and (equal (denote--format-front-matter "" (date-to-time "20240101T120000") nil "" "" 'markdown-yaml)
                        (mapconcat #'identity
                                   '("---"
                                     "title:      \"\""
                                     "date:       2024-01-01"
                                     "tags:       []"
                                     "identifier: \"\""
                                     "signature:  \"\""
                                     "---"
                                     "\n")
                                   "\n"))

                 (equal
                  (denote--format-front-matter
                   "Some test" (date-to-time "2023-06-05") '("one" "two")
                   "20230605T102234" "sig" 'markdown-yaml)
                  (mapconcat #'identity
                             '("---"
                               "title:      \"Some test\""
                               "date:       2023-06-05"
                               "tags:       [\"one\", \"two\"]"
                               "identifier: \"20230605T102234\""
                               "signature:  \"sig\""
                               "---"
                               "\n")
                             "\n"))))

    (should (and (equal (denote--format-front-matter "" (date-to-time "20240101T120000") nil "" "" 'markdown-toml)
                        (mapconcat #'identity
                                   '("+++"
                                     "title      = \"\""
                                     "date       = 2024-01-01"
                                     "tags       = []"
                                     "identifier = \"\""
                                     "signature  = \"\""
                                     "+++"
                                     "\n")
                                   "\n"))

                 (equal
                  (denote--format-front-matter
                   "Some test" (date-to-time "2023-06-05") '("one" "two")
                   "20230605T102234" "sig" 'markdown-toml)
                  (mapconcat #'identity
                             '("+++"
                               "title      = \"Some test\""
                               "date       = 2023-06-05"
                               "tags       = [\"one\", \"two\"]"
                               "identifier = \"20230605T102234\""
                               "signature  = \"sig\""
                               "+++"
                               "\n")
                             "\n"))))))

(ert-deftest dt-denote-format-file-name ()
  "Test that `denote-format-file-name' returns all expected paths."
  (let* ((title "Some test")
         (id (format-time-string denote-id-format (denote-valid-date-p "2023-11-28 05:53:11")))
         (denote-directory "/tmp/test-denote")
         (kws '("one" "two")))
    (should-error (denote-format-file-name
                    nil
                    id
                    kws
                    title
                    (denote--file-extension 'org)
                    ""))

    (should-error (denote-format-file-name
                    ""
                    id
                    kws
                    title
                    (denote--file-extension 'org)
                    ""))

    (should-error (denote-format-file-name
                   denote-directory ; notice this is the `let' bound value without the suffix
                   id
                   kws
                   title
                   (denote--file-extension 'org)
                   ""))

    (should-error (denote-format-file-name
                   (denote-directory)
                   ""
                   nil
                   ""
                   (denote--file-extension 'org)
                   ""))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    nil
                    kws
                    title
                    (denote--file-extension 'org)
                    "")
                   "/tmp/test-denote/--some-test__one_two.org"))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    ""
                    kws
                    title
                    (denote--file-extension 'org)
                    "")
                   "/tmp/test-denote/--some-test__one_two.org"))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    "0123456"
                    kws
                    title
                    (denote--file-extension 'org)
                    "")
                   "/tmp/test-denote/@@0123456--some-test__one_two.org"))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    id
                    kws
                    title
                    (denote--file-extension 'org)
                    "")
                   "/tmp/test-denote/20231128T055311--some-test__one_two.org"))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    id
                    nil
                    ""
                    (denote--file-extension 'org)
                    "")
                   "/tmp/test-denote/20231128T055311.org"))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    id
                    nil
                    nil
                    (denote--file-extension 'org)
                    nil)
                   "/tmp/test-denote/20231128T055311.org"))

    (should (equal (denote-format-file-name
                    (denote-directory)
                    id
                    kws
                    title
                    (denote--file-extension 'org)
                    "sig")
                   "/tmp/test-denote/20231128T055311==sig--some-test__one_two.org"))))

(ert-deftest dt-denote-get-file-extension ()
  "Test that `denote-get-file-extension' gets the correct file extension."
  (should (and (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing") "")
               (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing.org") ".org")
               (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing.org.gpg") ".org.gpg")
               (equal (denote-get-file-extension "20231010T105034--some-test-file__denote_testing.org.age") ".org.age"))))

(ert-deftest dt-denote-get-file-extension-sans-encryption ()
  "Test that `denote-get-file-extension-sans-encryption' gets the file extension without encryption."
  (should (and (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing") "")
               (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing.org") ".org")
               (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing.org.gpg") ".org")
               (equal (denote-get-file-extension-sans-encryption "20231010T105034--some-test-file__denote_testing.org.age") ".org"))))

(ert-deftest dt-denote-filetype-heuristics ()
  "Test that `denote-filetype-heuristics' gets the correct file type."
  (should (and (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing") nil)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.org") 'org)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.org.gpg") 'org)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.org.age") 'org)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.txt") 'text)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.txt.gpg") 'text)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.txt.age") 'text)
               ;; NOTE 2023-10-11: It returns `markdown-yaml' as a fallback.  In
               ;; an actual file, it reads the file contents to determine what
               ;; it is and can return `markdown-toml'.  In principle, we should
               ;; be testing this here, though I prefer to keep things simple.
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.md") 'markdown-yaml)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.md.gpg") 'markdown-yaml)
               (eq (denote-filetype-heuristics "20231010T105034--some-test-file__denote_testing.md.age") 'markdown-yaml))))

(ert-deftest dt-denote-get-identifier ()
  "Test that `denote-get-identifier' returns an identifier."
  (should (and (equal (denote-get-identifier nil) "")
               (equal (denote-get-identifier 1705644188) "20240119T080308")
               (equal (denote-get-identifier '(26026 4251)) "20240119T080307"))))

(ert-deftest dt-denote-retrieve-filename-identifier ()
  "Test that `denote-retrieve-filename-identifier' returns only the identifier."
  (should (and (null
                (denote-retrieve-filename-identifier "/path/to/testing/--this-is-a-test-reordered__denote_testing.org"))
               (equal
                (denote-retrieve-filename-identifier "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org")
                "20240610T194654")
               (equal
                (denote-retrieve-filename-identifier "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org")
                "20240610T194654")
               (equal
                (denote-retrieve-filename-identifier "/path/to/testing/--this-is-a-test-reordered__denote_testing@@20240610T194654.org")
                "20240610T194654")
               (equal
                (denote-retrieve-filename-identifier "/path/to/testing/__denote_testing--this-is-a-test-reordered@@20240610T194654.org")
                "20240610T194654")
               (equal
                (denote-retrieve-filename-identifier "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "20240610T194654")
               (equal
                (denote-retrieve-filename-identifier "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "20240610T194654"))))

(ert-deftest dt-denote-retrieve-filename-title ()
  "Test that `denote-retrieve-filename-title' returns only the title."
  (should (and (null
                (denote-retrieve-filename-title "/path/to/testing/20240610T194654__denote_testing.org"))
               (equal
                (denote-retrieve-filename-title "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org")
                "this-is-a-test-reordered")
               (equal
                (denote-retrieve-filename-title "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org")
                "this-is-a-test-reordered")
               (equal
                (denote-retrieve-filename-title "/path/to/testing/--this-is-a-test-reordered__denote_testing@@20240610T194654.org")
                "this-is-a-test-reordered")
               (equal
                (denote-retrieve-filename-title "/path/to/testing/__denote_testing--this-is-a-test-reordered@@20240610T194654.org")
                "this-is-a-test-reordered")
               (equal
                (denote-retrieve-filename-title "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "this-is-a-test-reordered")
               (equal
                (denote-retrieve-filename-title "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "this-is-a-test-reordered"))))

(ert-deftest dt-denote-retrieve-filename-keywords ()
  "Test that `denote-retrieve-filename-keywords' returns only the keywords."
  (should (and (null
                (denote-retrieve-filename-keywords "/path/to/testing/20240610T194654--this-is-a-test-reordered.org"))
               (equal
                (denote-retrieve-filename-keywords "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org")
                "denote_testing")
               (equal
                (denote-retrieve-filename-keywords "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org")
                "denote_testing")
               (equal
                (denote-retrieve-filename-keywords "/path/to/testing/--this-is-a-test-reordered__denote_testing@@20240610T194654.org")
                "denote_testing")
               (equal
                (denote-retrieve-filename-keywords "/path/to/testing/__denote_testing--this-is-a-test-reordered@@20240610T194654.org")
                "denote_testing")
               (equal
                (denote-retrieve-filename-keywords "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "denote_testing")
               (equal
                (denote-retrieve-filename-keywords "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "denote_testing"))))

(ert-deftest dt-denote-retrieve-filename-signature ()
  "Test that `denote-retrieve-filename-signature' returns only the signature."
  (should (and (null
                (denote-retrieve-filename-signature "/path/to/testing/20240610T194654--this-is-a-test-reordered__denote_testing.org"))
               (equal
                (denote-retrieve-filename-signature "/path/to/testing/20240610T194654==signature--this-is-a-test-reordered__denote_testing.org")
                "signature")
               (equal
                (denote-retrieve-filename-signature "/path/to/testing/--this-is-a-test-reordered==signature__denote_testing@@20240610T194654.org")
                "signature")
               (equal
                (denote-retrieve-filename-signature "/path/to/testing/__denote_testing--this-is-a-test-reordered==signature@@20240610T194654.org")
                "signature")
               (equal
                (denote-retrieve-filename-signature "/path/to/testing/__denote_testing@@20240610T194654--this-is-a-test-reordered==signature.org")
                "signature")
               (equal
                (denote-retrieve-filename-signature "/path/to/testing/==signature__denote_testing@@20240610T194654--this-is-a-test-reordered.org")
                "signature"))))

(ert-deftest dt-denote-identifier-p ()
  "Test that `denote-identifier-p' works for Denote identifiers."
  (should (and (denote-identifier-p "20240901T090910")
               (null (denote-identifier-p "20240901T090910-not-identifier-format")))))

(ert-deftest dt-denote--id-to-date ()
  "Test that `denote--id-to-date' returns the date from an identifier."
  (should (equal (denote--id-to-date "20240901T090910") "2024-09-01"))
  (should-error (denote--id-to-date "20240901T090910-not-identifier-format")))

(ert-deftest dt-denote--date-convert ()
  "Test that `denote--date-convert' works with dates."
  (should (and
           (equal (denote--date-convert '(26454 45206 461174 657000) :list)
                  '(26454 45206 461174 657000))

           (equal (denote--date-convert '(26454 45206 461174 657000) :string)
                  "2024-12-09 10:55:50")

           (equal (denote--date-convert nil :string)
                  "")

           (equal (denote--date-convert nil :list)
                  nil)))
  (should-error (denote--date-convert '(26454 45206 461174 657000) :not-valid-type))
  (should-error (denote--date-convert nil :not-valid-type)))

(provide 'denote-test)
;;; denote-test.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dt" . "denote-test-"))
;; End:
