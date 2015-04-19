;;; edep/test/parser/constant.el --- Test cases for constant tags

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

(ert-deftest edep-test-parser-constant-tags()
  "Test non-class constants"
  (with-test-buffer
   "const TESTA = 123, TESTB = 'abc';"
   (with-semantic-tags
    (with-semantic-tag (nth 0 tags)
                       (should (equal "TESTA" tag-name))
                       (should (equal 'constant tag-class)))
    (with-semantic-tag (nth 1 tags)
                       (should (equal "TESTB" tag-name))
                       (should (equal 'constant tag-class))))))

(provide 'test/parser/constant)
