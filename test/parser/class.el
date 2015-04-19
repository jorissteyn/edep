;;; edep/test/parser/class.el --- Test cases for class tags

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

(ert-deftest edep-test-parser-class-declarations()
  "Test tags for regular class declarations"
  (with-test-buffer
   "class Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "class" (plist-get tag-attribs :type))))))

(ert-deftest edep-test-parser-class-inheritance()
  "Test class declarations inheritance (extends)"
  (with-test-buffer
   "class Test extends TestAbstract {}"
   (with-semantic-first-tag (should (equal "TestAbstract" (plist-get tag-attribs :superclasses))))))

(ert-deftest edep-test-parser-class-interface-implementation()
  "Test classes implementing interfaces (implements)"
  (with-test-buffer
   "class Test implements TestA, TestB {}"
   (with-semantic-first-tag (should (equal '("TestA" "TestB") (plist-get tag-attribs :interfaces))))))

(ert-deftest edep-test-parser-class-type-modifiers()
  "Test class declaration with type modifiers (abstract, final)"
  (with-test-buffer
   "abstract class Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "class" (plist-get tag-attribs :type)))
                            (should (equal '("abstract") (plist-get tag-attribs :typemodifiers)))))
  (with-test-buffer
   "final class Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "class" (plist-get tag-attribs :type)))
                            (should (equal '("final") (plist-get tag-attribs :typemodifiers))))))

(provide 'test/parser/class)
