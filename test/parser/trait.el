;;; edep/test/parser/trait.el --- Test cases for trait tags

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

(ert-deftest edep-test-parser-trait-declarations()
  "Test tags for trait declarations"
  (with-test-buffer
   "trait Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "trait" (plist-get tag-attribs :type))))))

(ert-deftest edep-test-parser-trait-inheritance()
  "Test trait declarations inheritance (extends)"
  (with-test-buffer
   "trait Test extends TestAbstract {}"
   (with-semantic-first-tag (should (equal "TestAbstract" (plist-get tag-attribs :superclasses))))))

(ert-deftest edep-test-parser-trait-interface-implementation()
  "Test traits implementing interfaces (implements)"
  (with-test-buffer
   "trait Test implements TestA, TestB {}"
   (with-semantic-first-tag (should (equal '("TestA" "TestB") (plist-get tag-attribs :interfaces))))))

(provide 'test/parser/trait)
