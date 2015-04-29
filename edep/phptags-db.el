;;; edep/phptags-db.el --- PHPTAGS semanticdb

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

(eval-and-compile
  (when (and (<= emacs-major-version 24) (<= emacs-minor-version 5))
      (defalias 'cl-defmethod 'defmethod)
      (defalias 'cl-call-next-method 'call-next-method)))

(defclass semanticdb-table-phptags (semanticdb-search-results-table eieio-singleton)
  ((major-mode :initform 'php-mode))
  "Database table for PHPTAGS.")

(defclass semanticdb-project-database-phptags (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-phptags
		    :type class
		    :documentation
                    "New tables created for this database are of this class."))
  "Database representing PHP.")

(cl-defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-phptags))
              "Get the list of tables in the built-in symbol database."
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-phptags "PHPTAGS")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
  (cl-call-next-method))))

(cl-defmethod semanticdb-file-table ((obj semanticdb-project-database-phptags) filename)
  "Return the table of this single-table database."
  (car (semanticdb-get-database-tables obj)))

(cl-defmethod semanticdb-equivalent-mode ((table semanticdb-table-phptags) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by `semantic-equivalent-major-modes'
local variable."
  (with-current-buffer
      buffer
    (eq (or mode-local-active-mode major-mode) 'php-mode)))

;;; Search Overrides
(cl-defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-phptags) name &optional tags)
  "Find all tags named NAME in TABLE.
Return a list of tags."
  (if tags
      ;; If TAGS are passed in, then we don't need to do work here.
      (cl-call-next-method)
    ;; Call out to PHPTAGS for some results.
    (edep-phptags-find-semantic-tags (concat "^" name "$"))))

(cl-defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-phptags) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (cl-call-next-method)
    (edep-phptags-find-semantic-tags regex)))

(cl-defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-phptags) prefix &optional tags)
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
  ((table semanticdb-table-phptags) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-method table name tags))

(cl-defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-phptags) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(cl-defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-phptags) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for global."
  (semanticdb-find-tags-for-completion-method table prefix tags))

(provide 'edep/phptags-db)
