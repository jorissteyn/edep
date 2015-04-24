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

(require 'edep/phptags)
(require 'semantic/db)
(require 'semantic/db-typecache)

(eval-and-compile
  (when (and (<= emacs-major-version 24) (<= emacs-minor-version 5))
      (defalias 'cl-defmethod 'defmethod)
      (defalias 'cl-call-next-method 'call-next-method)))

(defclass semanticdb-table-php (semanticdb-search-results-table eieio-singleton)
  ((major-mode :initform php-mode))
  "Database table for PHP.")

(defclass semanticdb-project-database-php (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-php
		    :type class
		    :documentation
                    "New tables created for this database are of this class."))
  "Database representing PHP.")

(defvar-mode-local php-mode semanticdb-project-system-databases
  (list (semanticdb-project-database-php "PHP"))
  "Search for built-in and project symbols.")

(defvar-mode-local php-mode semanticdb-find-default-throttle
  '(project system omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to do system or recursive searching because of
the omniscience database.")

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

  (let ((result
         ;; First try finding the type using the default routine.
         (or (semanticdb-typecache-find-default type path find-file-match)

             ;; Or try phptags, note both the database and the table (always
             ;; one table) are singletons, so we're not actually creating a
             ;; new instance here.
             (car (semanticdb-find-tags-by-name-method
                   (semanticdb-table-php "PHP")
                   (mapconcat 'identity type "\\"))))))

    (when (and result find-file-match)
      (find-file-noselect (semantic-tag-file-name result)))

    result))

(cl-defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-php))
  "Get the list of tables in the built-in symbol database."
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-php "PHP")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
  (cl-call-next-method))))

(cl-defmethod semanticdb-file-table ((obj semanticdb-project-database-php) filename)
  "Return the table of this single-table database."
  (car (semanticdb-get-database-tables obj)))

(cl-defmethod semanticdb-equivalent-mode ((table semanticdb-table-php) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by `semantic-equivalent-major-modes'
local variable."
  (with-current-buffer
      buffer
    (eq (or mode-local-active-mode major-mode) 'php-mode)))

;;; Search Overrides
(cl-defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-php) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (cl-call-next-method)
    ;; Call out to PHPTAGS for some results.
    (edep-phptags-find-semantic-tags (concat "^" name "$"))))

(cl-defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-php) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (cl-call-next-method)
    (edep-phptags-find-semantic-tags regex)))

(cl-defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-php) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags
      (cl-call-next-method)
    (edep-phptags-find-semantic-tags prefix)))

;;; Deep Searches
;;
;; If your language does not have a `deep' concept, these can be left
;; alone, otherwise replace with implementations similar to those
;; above.
;;
(cl-defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-php) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-method table name tags))

(cl-defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-php) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(cl-defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-php) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for global."
  (semanticdb-find-tags-for-completion-method table prefix tags))

(provide 'edep/semantic-php-db)
