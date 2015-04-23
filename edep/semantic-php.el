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

;;;
;;; Namespace resolving functions
;;;
(defun semantic-php-fully-qualified-name-p (name)
  "Returns t if NAME is a fully qualified name."
  (equal "\\" (substring name 0 1)))

(defun semantic-php-resolve-fully-qualified-name (name)
  "Resolves the fully qualified name for unqualified NAME."
  (concat "\\"
          (semantic-php-resolve-qualified-name name)))

(defun semantic-php-resolve-qualified-name (name)
  "Resolves the qualified name for unqualified NAME.

If NAME is fully qualified, strip the leading namespace separator."
  ;; Don't resolve the namespace if the name already is fully qualified.
  (if (semantic-php-fully-qualified-name-p name)
      ;; Strip the leading namespace separator.
      (replace-regexp-in-string "\\(^[\\]\\)" "" name)
    ;; Find namespace context and apply resolution rules.
    (let* ((tags (semantic-fetch-tags))
           (tagparts (split-string name "\\\\"))
           (taglead (car tagparts))
           (tagtail (mapconcat 'identity (cdr tagparts) "\\"))
           (ns-context (semantic-php-analyze-current-namespace))
           (ns (car ns-context))
           (usetags (cdr ns-context))
           usedname)

      ;; Resolve use statements.
      (setq usedname (semantic-php-find-use-declaration name usetags))

      (cond (usedname)
            ;; Resolved by use-statement.
            ((or (eq nil ns)
                 (equal "\\" (semantic-tag-name ns)))
             ;; We're in the global namespace, or unqualified name in
             ;; global space 'namespace {}'.
             (identity name))
            (t
             ;; Prepend namespace to unqualified NAME.
             (mapconcat 'identity (list (semantic-tag-name ns) name) "\\"))))))

(defun semantic-php-find-use-declaration (name usetags)
  "Determine the fully qualified for NAME given use tags USETAG.

If the name is not imported in the given use tag, return nil."
  (let (result)
    (dolist (decl usetags result)
      (let* ((usename (semantic-tag-name decl))
             (usealias (semantic-tag-get-attribute decl :alias))
             (tagparts (split-string name "\\\\"))
             (taglead (car tagparts))
             (tagtail (mapconcat 'identity (cdr tagparts) "\\")))
        (if (equal usealias taglead)
            (setq result usename))))))

(defun semantic-php-analyze-current-namespace ()
  "Analyze the current namespace- and use tags in effect at point.

A list in the from (NSTAG USETAGS)"
  ;; Look for tags around point by overlay.
  (let ((tags (nreverse (semantic-find-tag-by-overlay)))
        nstag
        usetags)
    (while (and tags
                (not (and (equal "namespace" (semantic-tag-type (car tags)))
                          (eq 'type (semantic-tag-class (car tags))))))
      (setq tags (cdr tags)))

    (if (car tags)
        ;; If a tag by overlay was found, we're in a brace-block namespace.
        (progn (setq nstag (car tags))
               (dolist (nsmember (semantic-tag-components nstag))
                 (if (semantic-tag-of-class-p nsmember 'use)
                     (setq usetags (append usetags (list nsmember))))))

      ;; If not found, scan the current buffer to see if a namespace was
      ;; defined above point.
      (dolist (tag (semantic-fetch-tags))
        (if (and (equal "namespace" (semantic-tag-type tag))
                 (< (car (semantic-tag-bounds tag)) (point)))
            (setq nstag tag
                  usetags nil)
          (if (semantic-tag-of-class-p tag 'use)
              (setq usetags (append usetags (list tag)))))))

    (cons nstag usetags)))

;;;
;;; Type calculation
;;;
(defun semantic-php-variablep (name)
  "Return non-nil if NAME is a variable."
  (equal "$" (substring name 0 1)))

(defun semantic-php-resolve-variable-type (name scope &optional curfunc)
  "Determine the type of variable NAME.

SCOPE is the scope object returned by `semantic-calculate-scope`."
  (let ((tag (semantic-analyze-find-tag name 'variable scope)))
    (if (semantic-tag-type tag)
        (semantic-php-resolve-qualified-name (semantic-tag-type tag)))))

(define-mode-local-override semantic-analyze-split-name
  php-mode (name)
  "Split up NAME by namespace parts."
  (let ((parts (delete "" (split-string name "\\\\"))))
    (if (= (length parts) 1)
        name
      ;; Given NS1\NS2\Name, return a list (NS1\NS2 Name)
      (list (car parts)
            (mapconcat 'identity (cdr parts) "\\")))))

(define-mode-local-override semantic-analyze-unsplit-name
  php-mode (namelist)
  "Concatenate the names in NAMELIST with a \\ between."
  (mapconcat 'identity namelist "\\"))

;;;
;;; Make tag names fully qualified
;;;
;;; TODO: this is a big hack, tag names should be resolved
;;; on-demand. For now, this is usefull to allow semantic to lookup
;;; parent classes and typehints.
(defun semantic-php-fetch-tags (tags &optional curns)
  "Resolve fully qualified type definitions.

CURNS indicates the parent namespace when called recursively.

Modifies TAGS and returns it.

This function does not resolve use-statements like the
analyze-routines do, because it only deals with definitions."
  (if (not (derived-mode-p 'php-mode))
      ;; This advice must be mode-local, of course.
      tags
    (dolist (tag tags tags)
      ;; Make sure we process the tag only once
      (unless (semantic-tag-get-attribute tag :isfqn)
        (semantic-tag-put-attribute tag :isfqn t)
        (cond ((and (semantic-tag-of-type-p tag "namespace")
                    (not (equal 'alias (semantic-tag-get-attribute tag :kind))))
               ;; Make all namespace members fully qualified
               (semantic-php-fetch-tags
                (semantic-tag-components tag) tag))
              ((semantic-tag-of-class-p tag 'type)
               ;; Make all type names fully qualified
               (semantic-tag-set-name
                tag (if curns
                        (concat (semantic-tag-name curns)
                                "\\"
                                (semantic-tag-name tag))
                      (semantic-tag-name tag)))

               ;; Resolve the superclasses
               (semantic-tag-put-attribute
                tag
                :superclasses (mapcar
                               'semantic-php-resolve-qualified-name
                               (semantic-tag-type-superclasses tag)))

               ;; Resolve the interfaces
               (semantic-tag-put-attribute
                tag
                :interfaces (mapcar
                             'semantic-php-resolve-qualified-name
                             (semantic-tag-type-interfaces tag)))))))))

;; semantic-fetch-tags is not an mode-local overloadable function,
;; so we simulate that behaviour using a filter-return advice
;; where we only change the tags if the major mode is as expected
(advice-add 'semantic-fetch-tags :filter-return #'semantic-php-fetch-tags)

(provide 'edep/semantic-php)
