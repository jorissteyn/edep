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
;;; Dealing with compound tags / tag expansion.
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
  (let ((functiontag (semantic-current-tag-of-class 'function))
        (functionparent (car-safe (semantic-find-tags-by-type
                           "class" (semantic-find-tag-by-overlay))))
        classparent   ;; the parent class of the function parent
        alltags       ;; collect all tags in scope (TODO: use scope object)
        variabletags  ;; list of variable tags
        namelist)     ;; the names of tags in variabletags (used for dedupping)

    (setq alltags
          (if functiontag
              ;; In a function.
              (semantic-tag-type-members functiontag)
            ;; In the toplevel space.
            (semantic-fetch-tags)))

    ;; Find local variables and remove duplicates.
    (dolist (tag alltags variabletags)
      (when (and (semantic-tag-of-class-p tag 'variable)
                 (not (member (semantic-tag-name tag) namelist)))
        (push (semantic-tag-name tag) namelist)
        (push tag variabletags)))

    ;; Handle special function variables/keywords.
    (when functionparent
      ;; Add self as variable (while not actually a variable).
      (push (semantic-tag-new-variable "self" functionparent nil) variabletags)

      ;; If in non-static context, add $this.
      (if (not (member "static"
                       (semantic-tag-get-attribute functiontag :typemodifiers)))
          (push (semantic-tag-new-variable "$this" functionparent nil) variabletags))

      ;; Find the parent class of the parent, this is the type of the
      ;; parent keyword.
      (setq classparent (car-safe (semantic-tag-type-superclasses functionparent)))
      (if classparent
          (push (semantic-tag-new-variable "parent" classparent nil) variabletags)))

    variabletags))

(define-mode-local-override semantic-ctxt-scoped-types
  php-mode (&optional point)
  "Return the types in scope at POINT.

Do not return the parent class or namespace at point, only types
imported by use statements. This would be an easy task was it not
for the fact that namespaces can not always be determined by
overlay.

If the namespace at point is braceless, it is also included as
scoped type. Other namespaces are considered parents and are
added to the scope object by `semantic-analyze-scope-nested-tags'."
  (when point (goto-char point))

  ;; Find the current namespace by overlay.
  (let ((curns (car (semantic-find-tags-by-type
                     "namespace" (semantic-find-tag-by-overlay))))
        nsmembers)

    (if curns
        ;; Found it, this was easy. Include all use statements, the
        ;; namespace can be found by overlay, so don't include it.
        (setq nsmembers
              (semantic-find-tags-by-class
               'use (semantic-tag-components curns)))

      ;; No namespace context found by overlay, try to find a
      ;; braceless namespace. This is more complex because we have to
      ;; look back finding a braceless namespace declaration, and then
      ;; find the use statements below it.
      (catch 'no-preceding-ns
        (let (prev)
          (while (not curns)
            ;; Find previous tag.
            (setq prev (semantic-find-tag-by-overlay-prev
                        (if prev
                            (semantic-tag-start prev)
                          point)))

            (if (not prev)
                (throw 'no-preceding-ns "Not in a namespace context")
              (when (semantic-tag-of-type-p prev "namespace")
                ;; We found a namespace, it's either braceless (so
                ;; we're in a namespace) or outside of the braces of a
                ;; namespace block, which is invalid but handle it
                ;; like we're in the global namespace.
                (if (semantic-tag-get-attribute prev :braceless)
                    (setq curns prev)
                  (throw 'no-preceding-ns "Invalid namespace context")))))))

      ;; If we have a namespace tag now, then find the nsmembers by
      ;; overlay. Those members are not parents of the tag since it's
      ;; not a brace-block namespace.
      (if curns
          (setq nsmembers
                (append
                 ;; Add braceless namespaces as scoped type.
                 (list curns)

                 ;; Add all use statements between namespace
                 ;; declaration and point.
                 (semantic-find-tags-by-class
                  'use (semantic-find-tag-by-overlay-in-region
                        (semantic-tag-start curns) (point)))))

        ;; Else, extract the use tags from the global namespace in the
        ;; file.
        (setq nsmembers
              (semantic-find-tags-by-class 'use
                                           (current-buffer)))))))

(define-mode-local-override semantic-analyze-scope-nested-tags
  php-mode (position scopedtypes)
  "Add a scoped braceless namespace declaration to the list of parents."
  (let ((result (semantic-analyze-scope-nested-tags-default position scopedtypes)))
    (dolist (scopedtype scopedtypes result)
      (when (and (semantic-tag-of-type-p scopedtype "namespace")
                 (semantic-tag-get-attribute scopedtype :braceless))
        (push scopedtype result)))))

(define-mode-local-override semantic-analyze-split-name
  php-mode (name)
  "Split up NAME by namespace parts."
  ;; Note: namespaces in PHP are not hierarchical so we need to figure
  ;; out a way to never walk the split name all the way down. We
  ;; actually only want to split the name in two pieces:
  ;;   A\B\C -> A\B, C
  ;; but semantic calls this function repeatedly untill there's
  ;; nothing left to split, this is not easy to change.
  (let ((parts (delete "" (split-string name "\\\\"))))
    (if (= (length parts) 1)
        (car parts)
      parts)))

(define-mode-local-override semantic-analyze-unsplit-name
  php-mode (type)
  "Get the name of TYPE considering its parents."
  ;; TODO: Don't know how to handle this. This function is not the
  ;; reverse of s-a-split-name. Also, can we determine the parent of
  ;; :parent is not always set?
  (error "Unimplemented semantic-analyze-unsplit-name in semantic-php, for type %S" type))

(defun semantic-php-fetch-tags (tags)
  "Advice `semantic-fetch-tags` to handle braceless namespaces.

Semantic can determine the fully qualified name of a class if it
knows what namespace a tag belongs to. Normally this can be done
two ways:

 * by tag nesting in the :members attribute
 * by :parent attribute, but for php it is not possible to do in parser
 * by overlay, which is not applicable for braceless namespaces

This advice sets the :members attribute of all namespaces without
brace blocks."
  (if (not (derived-mode-p 'php-mode))
      ;; This advice must be mode-local, of course.
      tags
    (let (result curns)
      (dolist (tag tags)
        ;; If we encounter a namespace, add curns to the result (if
        ;; not empty) and set curns to collect the upcoming tags in
        ;; the list
        (if (and (semantic-tag-of-type-p tag "namespace")
                 (not (semantic-tag-of-type-p tag 'use)))

            (progn
              ;; We have a new namespace, so process the previous
              ;; namespace
              (when (and (semantic-tag-p curns)
                         (semantic-tag-type-members curns))
                (push curns result))

              ;; If tag is braceless, set it to curns to collect
              ;; upcoming tags
              (if (semantic-tag-get-attribute tag :braceless)
                  (progn
                    (semantic-tag-put-attribute tag :members nil)
                    (setq curns tag))
                (push tag result)))

          ;; Not a namespace, nest the tag under the current
          ;; namespace, or add to result
          (if curns
              (semantic-tag-put-attribute curns
                                          :members (append (semantic-tag-type-members curns)
                                                           (list tag)))
            (push tag result))))

      ;; Don't forget last namespace not added to result in dolist
      (when curns
        (push curns result))

      ;; Return the modified tag list
      (nreverse result))))

;; semantic-fetch-tags is not an mode-local overloadable function,
;; so we simulate that behaviour using a filter-return advice
;; where we only change the tags if the major mode is as expected
(advice-add 'semantic-fetch-tags :filter-return #'semantic-php-fetch-tags)

(provide 'edep/semantic-php)
