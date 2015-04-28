;;; edep/wisent-php-macro.el --- EDEP / Wisent grammar macros

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

(require 'semantic/wisent/grammar)

;; TODO: create cleverer expand* macro's to accomodate cases like
;; inline_use_statements, parenthesized expressions, etc.

(defun edep/wisent-php-macro-EXPANDFULL (symb nonterm &optional depth)
  "Expand call to EXPANDFULL grammar macro.
Return the form to recursively parse an area.
SYMB is a $I placeholder symbol that gives the bounds of the area.
NONTERM is the nonterminal symbol to start with.
DEPTH is the maximum parsing depth (defaults to 1)."
  (unless (member nonterm (semantic-grammar-start))
    (error "EXPANDFULL macro called with %s, but not used with %%start"
           nonterm))
  (let (($ri (wisent-grammar-region-placeholder symb)))
    (if $ri
        `(semantic-parse-region
          (car ,$ri) (cdr ,$ri) ',nonterm (or ,depth 1))
      (error "Invalid form (EXPANDFULL %s %s)" symb nonterm))))

(defun edep/wisent-php-macro-EXPANDLIST (tags)
  "Expand a list of raw tags TAGS."
  `(dolist (tag ,tags ,tags)
     (wisent-cook-tag tag)))

(defun edep/wisent-php-macro-USEGROUP (name decls)
  "Resolve the names for use declarations inside a group statement.

NAME is the group name, DECLS is the compound use declarations."
  `(let (result aliasmember)
     (dolist (decl ,decls (nreverse result))
       ;; The tag structure for use statements is not straightforward:
       ;; ("AliasName" 'use :kind 'alias :members ("FQName" 'use))
       (setq aliasmember (car (semantic-tag-type-members decl)))

       (semantic-tag-set-name aliasmember
                              (concat ,name "\\"
                                      (semantic-tag-name aliasmember)))
       (push decl result))))

(defun edep/wisent-php-macro-USEDECL (name &optional alias)
  "Create a temporary tag for single use declaration.

NAME is the fully- or unqualified name, ALIAS is the optional alias."
  `(wisent-raw-tag
    (semantic-tag
     (or ,alias
         ;; The last part of the name acts as an alias in PHP.
         (car (last (split-string ,name "\\\\"))))
     'use
     :kind 'alias
     :members (list (semantic-tag-new-type ,name "unknown" nil nil)))))

(defun edep/wisent-php-macro-USETYPE (decl type)
  "Set the type of a use declaration.

DECL is the result of the USEDECL macro, TYPE is the declaration
type (class, function, const)."
  `(dolist (tag (if (semantic-tag-p ,decl)
                    (list ,decl)
                  ,decl) ,decl)
     (semantic-tag-put-attribute tag :type ,type)))

(provide 'edep/wisent-php-macro)
