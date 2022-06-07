# Denote (WORK-IN-PROGRESS)

Take notes using a strict file-naming convention.  Denote does not do
anything else:

- Want to search your notes?  Use `M-x grep`, `M-x find-name-dired`,
  `M-x consult-find`, `M-x consult-grep`, and so on.

- Want to quickly jump to the directory of your notes?  Visit it with
  `M-x find-file RET path/to/notes` and then make a bookmark with `M-x
  bookmark-set`.  Access bookmarks with `M-x bookmark-jump`, `M-x
  consult-buffer`, and the like.  And/or treat your notes as a project
  with the built-in project.el.

- Narrow the list of notes?  Do it from the completion UI and export the
  results with `embark` (or equivalent).  To achieve the same in Dired,
  do `M-x dired-mark-files-regexp RET type-regexp-here RET t k`.  The `t
  k` will toggle the match so that it marks all files that do not match
  the regexp and `k` will remove them from the buffer (restore them by
  reverting the buffer).

* * *

This is successor to usls: <https://gitlab.com/protesilaos/usls>, which
I had been using for more than a year.  Denote is intended for private
use, until I eventually turn it into a package that others can use.  As
such, there is no manual for the time being and the code may change
considerably.


