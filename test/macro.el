;;; edep/test/macro.el --- Macro definitions for edep tests

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

(require 'edep)
(require 'ert)
(require 'semantic/db-mode)
(require 'test/php-mode)

;;;
;;; Test macros
;;;
(defmacro with-test-buffer (source &rest body)
  "Set up new buffer for isolated test.

SOURCE must contain the contents of the test buffer, the opening
tag '<?php' must be omitted. BODY is evaluated in the temporary
buffer."
  `(with-test-buffer-plain
    ,(concat "<?php " source)
    ,@body))

(defmacro with-saved-test-buffer (source &rest body)
  "Set up new buffer for isolated test and save it to a temporary file.

SOURCE must contain the contents of the test buffer, the opening
tag '<?php' must be omitted. BODY is evaluated in the temporary
buffer."
  `(let ((filename (concat temporary-file-directory
                           "/edep-test/"
                           ,(md5 source)
                           ".php")))

     ;; Create fake project root in /tmp
     (or (file-exists-p "/tmp/edep-test")
         (mkdir "/tmp/edep-test"))

     (with-test-buffer-plain
      ,(concat "<?php " source)

      ;; Write source file to disk
      (write-file filename)

      (unwind-protect
          (progn
            ;; Would expect (semantic-force-refresh) gets everything in
            ;; order, but calling this hook seems the only way to initialize
            ;; a database for the test buffer. If we don't do this,
            ;; semanticdb-current-database will be nil, and much of the
            ;; context analysis won't work.
            (semanticdb-semantic-init-hook-fcn)
            (semantic-force-refresh)

            ,@body)

        ;; Clean up temporary source file
        (delete-file filename)))))

(defmacro with-test-buffer-plain (source &rest body)
  "Set up new buffer for isolated test

SOURCE must contain the contents of the test buffer, the opening
tag '<?php' must be included. BODY is evaluated in the temporary
buffer."
  `(with-temp-buffer
     (insert ,source)
     (goto-char 0)
     (php-mode) ;; Not the real php-mode, this is a "dummy"

     ;; Enable EDEP
     (edep-mode)

     ,@body))

(defmacro with-lex-tokens (&rest body)
  "Load the wisent token stream for current buffer and execute BODY"
  ;; Skip the first T_OPEN_TAG token
  `(let ((tokens (cdr (semantic-lex-buffer))))
     ,@body))

(defmacro with-semantic-tags (&rest body)
  "Analyze tags in current buffer and execute BODY"
  `(let ((tags (semantic-parse-region (point-min) (point-max))))
     ,@body))

(defmacro with-semantic-first-tag (&rest body)
  "Analyze tags in current buffer, select the first one and execute BODY"
  `(with-semantic-tags
    (with-semantic-tag (car tags)
                       ,@body)))

(defmacro with-semantic-tag (tag &rest body)
  "Analyze given TAG and execute BODY"
  `(let ((tag ,tag)
         tag-name tag-class tag-type tag-attribs tag-props tag-overlay)
     (setq tag-name (semantic-tag-name tag)
           tag-class (semantic-tag-class tag)
           tag-type (semantic-tag-type tag)
           tag-attribs (semantic-tag-attributes tag)
           tag-props (semantic-tag-properties tag)
           tag-overlay (semantic-tag-overlay tag))
     ,@body))

(defmacro with-phptags-stub (stub &rest forms)
  "Stub the external phptags query.

STUB is the stubbed form, it can contain should macro's and
determine the return value of edep-phptags-find-tags.
FORMS is the macro body."
  `(let ((origfun (symbol-function 'edep-phptags-find-tags)))
     (fset 'edep-phptags-find-tags
           (lambda (pattern &optional type usage-type)
             ,stub))
     ,@forms
     (fset 'edep-phptags-find-tags origfun)))

(provide 'test/macro)
