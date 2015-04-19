;;; edep/test/parser/interface.el --- Test cases for interface tags

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

(ert-deftest edep-test-parser-interface-declarations()
  "Test tags for interface declarations"
  (with-test-buffer
   "interface Test {}"
   (with-semantic-first-tag (should (equal "Test" tag-name))
                            (should (equal 'type tag-class))
                            (should (equal "interface" (plist-get tag-attribs :type))))))

(ert-deftest edep-test-parser-interface-extends-interfaces()
  "Test interface extending other interfaces (extends)"
  (with-test-buffer
   "interface Test extends TestA, TestB {}"
   (with-semantic-first-tag (should (equal '("TestA" "TestB") (plist-get tag-attribs :superclasses))))))

(provide 'test/parser/interface)
