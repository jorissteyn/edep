;;; semantic-php-symref.el --- Symref implementation for phptags

;; Copyright (C) 2014 Joris Steyn

;; Author: Joris Steyn <jorissteyn@gmail.com>
;; Created: 1 Nov 2014
;; Keywords: languages
;; Homepage: https://github.com/jorissteyn/semantic-php

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
(require 'semantic/symref)

(eval-and-compile
  (when (and (<= emacs-major-version 24) (< emacs-minor-version 5))
      (defalias 'cl-defmethod 'defmethod)))

(defclass semantic-symref-tool-phptags (semantic-symref-tool-baseclass)
  ()
  "A symref tool implementation using PHPTAGS.")

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-phptags))
  "Perform a search with PHPTAGS."
  (let ((tags (edep-phptags-find-tags (oref tool :searchfor)))
        result)
    (dolist (tag tags result)
      (push (cons (edep-phptags-tag-linestart tag)
                  (edep-phptags-tag-filename tag))
            result))))

(provide 'edep/semantic-php-symref)
