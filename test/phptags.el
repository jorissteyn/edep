;;; edep/test/phptags.el --- Test the phptags backend

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

(ert-deftest edep-test-phptags ()
  "Test tag generation on results from phptags backend."
  (with-phptags-stub
   (prog1
       ;; The fixture.
       (list '("TestA" "file.php" "class" 1 2 3 4 "comment"))

      ;; Query expectation.
      (should (equal "test" pattern)))

   ;; Test body.
   (let* ((result (car (edep-phptags-find-semantic-tags "test"))))
     (should (equal "TestA" (semantic-tag-name result)))
     (should (equal 'type (semantic-tag-class result)))
     (should (equal "class" (semantic-tag-type result)))
     (should (equal (list 3 4) (semantic-tag-bounds result)))
     (should (equal "file.php" (semantic--tag-get-property result :filename))))))

(provide 'test/phptags)
