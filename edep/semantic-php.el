;;; edep/semantic-php.el --- EDEP / Semantic parser setup

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

(require 'edep/semantic-php-db)
(require 'edep/semantic-php-symref)
(require 'edep/wisent-php)
(require 'semantic)
(require 'semantic/analyze)
(require 'semantic/ctxt)
(require 'semantic/sort)

;;;
;;; Setup function for the semantic-php
;;;
(defun semantic-php-default-setup ()
  "Set the current buffer up for parsing with semantic"
  (wisent-php--install-parser)

  (setq
   ;; Lexical analysis
   semantic-lex-analyzer 'wisent-php-lexer

   ;; Syntax table modifications
   semantic-lex-syntax-modifications
   '(
     (?= ".")
     (?& ".")
     (?+ ".")
     (?- ".")
     (?| ".")
     (?< ".")
     (?> ".")
     (?% ".")
     (?' "\"")
     (?\" "\"")
     (?` "\"")
     (?_ "w")
     (?$ "_")
     (?/ ". 124b")
     (?* ". 23")
     (?\n "> b")
     (?# "< b"))

   ;; Semantic requires this expression for line-comments,
   ;; if lexing without major mode
   semantic-lex-comment-regex "\\s<\\|\\(/\\*\\|//\\)"

   ;; Not related to semantic, this variable needs to be turned on in
   ;; order for comments to not get non-comment syntax properties in
   ;; their contents. This is also done by cc-mode (and thus
   ;; php-mode), and is only here to ensure correct parsing without a
   ;; major mode (test cases, batch project scan)
   parse-sexp-ignore-comments t

   ;; Use the phptags symref implementation
   semantic-symref-tool 'phptags

   ;; Separators to use when finding context prefix
   semantic-type-relation-separator-character '("::" "->")
   semantic-command-separation-character ";"

   ;; Tag expansion
   semantic-tag-expand-function 'semantic-php-expand-tag

   ;; imenu setup
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index

   ;; Specify the labels of different tag types
   semantic-bucketize-tag-class 'semantic-php-bucketize-tag-class

   semantic-symbol->name-assoc-list '((namespace . "Namespaces")
                                      (class     . "Classes")
                                      (interface . "Interfaces")
                                      (trait     . "Traits")
                                      (variable  . "Variables")
                                      (constant  . "Constants")
                                      (function  . "Functions"))

   semantic-symbol->name-assoc-list-for-type-parts '((constant . "Constants")
                                                     (variable . "Properties")
                                                     (function . "Methods"))))

;; Enable the mode-local overrides in semantic/bovine/c.el.  Disabled
;; because the tags we emit are not identical to the ones the c parser
;; emits. Not sure to what extent the c overrides are suitable for
;; semantic-php, we'll write our own.
;; (define-child-mode php-mode c++-mode)

;; Define all modes applicable for semantic-php.
(define-child-mode web-mode php-mode)

;;;
;;; Dealing with compound tags / tag expansion
;;;
(defun semantic-php-expand-tag (tag)
  "Expand compound declarations found in TAG into separate tags.

If the name of the tag is a cons cell, assume (name . (start . end))
and set bounds accordingly."
  ;; TODO: restore expansion of use_declarations, compount class
  ;; properties and constant definitions, trait usages and global and
  ;; static variables.
  nil)

(defun semantic-php-bucketize-tag-class (tag)
  "Get the type of given TAG.

This function augments the standard bucketize behaviour by
emitting separate symbols for classes, interfaces and traits."
  (let ((tag-class (semantic-tag-class tag))
        (tag-attribs (semantic-tag-attributes tag)))
    ;; For type tags, return the type attribute as set by the parser
    ;; (class, interface, trait). For all other tags, return the
    ;; regular tag class.
    (if (eq tag-class 'type)
        ;; Convert string to symbol accepted by semantic-symbol->*
        (intern (plist-get tag-attribs :type))
      tag-class)))

;;;
;;; Semantic mode-local overrides
;;;
(define-mode-local-override semantic-tag-components
  php-mode (tag)
  "Return a list of components for TAG."
  (cond ((semantic-tag-of-class-p tag 'type)
	 (semantic-tag-type-members tag))
        ((semantic-tag-of-class-p tag 'function)
         (append (semantic-tag-function-arguments tag)
                 (semantic-tag-type-members tag)))
        ((semantic-tag-of-class-p tag 'code)
         (semantic-tag-function-arguments tag))
        (t nil)))

;; This implementation is very different from
;; `semantic-get-local-variables-default'. We use the current parser
;; result to extract variable tags based on the current context, or
;; the top-level context when not in a function context. It will not
;; re-parse part of the buffer.
(define-mode-local-override semantic-get-local-variables
  php-mode (&optional point)
  "Get local values from the context of point."
  (let ((ctx (semantic-fetch-tags))
        (fctx (semantic-current-tag-of-class 'function))
        resnames
        restags)
    (if fctx
        (setq ctx (semantic-tag-type-members fctx)))
    (dolist (tag ctx restags)
      ;; Find local variables and remove duplicates
      (if (and (semantic-tag-of-class-p tag 'variable)
               (not (member (semantic-tag-name tag) resnames)))
          (setq restags (append restags (list tag))
                resnames (append resnames (list (semantic-tag-name tag))))))))

(provide 'edep/semantic-php)
