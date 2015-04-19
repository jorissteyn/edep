;;; edep/test/parser/overlay.el --- Test cases for tag overlays

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
(require 'test/macro)
(require 'test/php-mode)

(ert-deftest edep-test-parser-overlay-variable()
  "Test overlay for variable tags"
  (with-test-buffer
   "$tag;"
   (with-semantic-first-tag
    (should (equal 'simple_variable (plist-get tag-props 'reparse-symbol)))
    (should (equal [7 11] tag-overlay)))))

(ert-deftest edep-test-parser-overlay-variable-in-expression()
  "Test overlay for variables in expressions"
  (with-test-buffer
   "$testA * $testB;"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal [7 13] tag-overlay))
                       (should (equal 'simple_variable (plist-get tag-props 'reparse-symbol))))
    (with-semantic-tag (nth 1 tags)
                       (should (equal [16 22] tag-overlay))
                       (should (equal 'simple_variable (plist-get tag-props 'reparse-symbol)))))))

(ert-deftest edep-test-parser-overlay-namespace()
  "Test overlay for namespace tags"
  (with-test-buffer
   "namespace Test;"
   (with-semantic-first-tag
    (should (equal [7 22] tag-overlay))
    (should (equal 'top_statement (plist-get tag-props 'reparse-symbol))))))

(provide 'test/parser/overlay)
