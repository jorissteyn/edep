;;; semantic-php-db.el --- PHP integration for semantic

;; Copyright (C) 2014 Joris Steyn

;; Author: Joris Steyn <jorissteyn@gmail.com>
;; Created: 1 Nov 2014
;; Keywords: languages
;; Homepage: https://github.com/jorissteyn/edep

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(require 'edep/phptags-db)
(require 'semantic/db)
(require 'semantic/db-typecache)

(defvar-mode-local php-mode semanticdb-project-system-databases
  (list (semanticdb-project-database-phptags "PHPTAGS"))
  "List of omniscience semanticdb database objects.

If there are any alternatives to PHPTAGS, those backends should
be added here.")

(defvar-mode-local php-mode semanticdb-find-default-throttle
  '(project system omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.

Note: system is only going to work if the EDE component is setup.")

(define-mode-local-override semanticdb-find-translate-path
  php-mode (path brutish)
  "Return a list of semanticdb tables asociated with PATH.
If brutish, do the default action.
If not brutish, do the default action, and append the system
database (if available.)"
  (let ((default
	  ;; When we recurse, disable searching of system databases
          ;; so that our PHP database only shows up once when
	  ;; we append it in this iteration.
          (let ((semanticdb-search-system-databases nil))
	    (semanticdb-find-translate-path-default path brutish))))
    ;; Don't add anything if BRUTISH is on (it will be added in that fcn)
    ;; or if we aren't supposed to search the system.
    (if (or brutish (not semanticdb-search-system-databases))
        default
      (let ((tables (apply #'append
			   (mapcar
			    (lambda (db) (semanticdb-get-database-tables db))
			    semanticdb-project-system-databases))))
	(append default tables)))))

(define-mode-local-override semanticdb-typecache-find
  php-mode (type &optional path find-file-match)
  "Search the typecache for TYPE in PATH.
If type is a string, split the string, and search for the parts.
If type is a list, treat the type as a pre-split string.
PATH can be nil for the current buffer, or a semanticdb table.
FIND-FILE-MATCH is non-nil to force all found tags to be loaded into a buffer."
  ;; If type is a string, strip the leading separator.
  (unless (listp type)
    (setq type (list (replace-regexp-in-string "\\(^[\\]\\)" "" type))))

  ;; TODO: searching with phptags if no results are found by the
  ;; default search routine should not be needed because we add the
  ;; table to the search path in find-translate-path. But that doesn't
  ;; work so we make sure tags are found here - temporarily.
  (let ((result
         ;; First try finding the type using the default routine.
         (or (semanticdb-typecache-find-default type path find-file-match)

             ;; Or try phptags, note both the database and the table (always
             ;; one table) are singletons, so we're not actually creating a
             ;; new instance here.
             (car (semanticdb-find-tags-by-name-method
                   (semanticdb-table-phptags "PHP")

                   ;; TODO: use analyze-unsplit-name, and solve the
                   ;; problem that in PHP, there's two ways to join
                   ;; tags instead of a single separator like:
                   ;;   A::B::C in c++ could be A\B\C or A\B::C in PHP.
                   (mapconcat 'identity type "\\"))))))

    (when (and result find-file-match)
      (find-file-noselect (semantic-tag-file-name result)))

    result))

(provide 'edep/semantic-php-db)
