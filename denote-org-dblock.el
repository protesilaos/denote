;;; denote-org-dblock.el --- Compatibility alieases for Denote Org Dynamic blocks -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Authors: Elias Storms <elias.storms@gmail.com>,
;;          Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; Deprecated-since: 3.0.0
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
;;
;; This file defines compatibility aliases for Org dynamic blocks set
;; up by Denote.  The new source of these is the denote-org-extras.el.
;;
;; Below is the old commentary.
;;
;; * * *
;;
;; This file defines Org dynamic blocks using the facility described
;; in the Org manual.  Evaluate this:
;;
;;    (info "(org) Dynamic Blocks")
;;
;; The dynamic blocks defined herein are documented at length in the
;; Denote manual.  See the following node and its subsections:
;;
;;    (info "(denote) Use Org dynamic blocks")

;;; Code:

(require 'denote-org-extras)

(define-obsolete-function-alias
  'denote-org-dblock-insert-links
  'denote-org-extras-dblock-insert-links
  "3.0.0")

(define-obsolete-function-alias
  'denote-org-dblock-insert-backlinks
  'denote-org-extras-dblock-insert-backlinks
  "3.0.0")

(define-obsolete-function-alias
  'denote-org-dblock-insert-files
  'denote-org-extras-dblock-insert-files
  "3.0.0")

(display-warning
 'denote
 "`denote-org-dblock.el' is obsolete; use `denote-org-extras.el'"
 :warning)

(provide 'denote-org-dblock)
;;; denote-org-dblock.el ends here
