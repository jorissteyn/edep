;;; edep/test/context.el --- Test context functions

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

;; TODO: context analysis quickly becomes a mess, need a proper way of
;; testing this.

(require 'edep)
(require 'ert)
(require 'test/macro)
(require 'test/php-mode)

(ert-deftest edep-test-context-simple ()
  "Test context analysis as used by semantic-ia-*."
  (with-saved-test-buffer
   "
namespace TestNS;

use TestA as AliasA;

class Test extends TestA {
    public function test() {
        AliasA
        TestB
    }
}
"
   (search-forward "test()")
   (search-forward "AliasA")

   ;; Stub phptags.
   (with-phptags-stub
    (prog1
        ;; The phptags fixture.
        (list '("TestA" "/tmp/a/file.php" "class" 1 2 300 400 "comment"))

      ;; Query expectation.
      (should (equal "^TestA$" pattern)))

    ;; Test body.
    (let* ((ctxt (semantic-analyze-current-context))
           (prefix (car (oref ctxt prefix))))
      (should (equal "TestA" (semantic-tag-name prefix)))))

   (search-forward "TestB")
   (with-phptags-stub
    (prog1
        ;; The phptags fixture.
        (list '("TestNS\\TestB" "/tmp/a/file.php" "class" 1 2 300 400 "comment"))

      ;; Query expectation.
      (should (equal "^TestNS\\TestB$" pattern)))

    ;; Test body.
    (let* ((ctxt (semantic-analyze-current-context))
           (prefix (car (oref ctxt prefix))))
      (should (equal "TestNS\\TestB" (semantic-tag-name prefix)))))))

(ert-deftest edep-test-context-method-prefix()
  "Test context analysis for prefixes of two parts."
  (with-saved-test-buffer
   "
function(A $test) {
    $test->test();
}
"
   (search-forward "test()")

   ;; Stub phptags.
   (with-phptags-stub
    (prog1
        nil ;; return nothing, just assert the phptags query.
      (should (equal "^A::test$" pattern)))

    ;; Test body.
    (semantic-analyze-current-context))))

(ert-deftest edep-test-context-scope ()
  "Test scope calculation"
  (with-saved-test-buffer
   "
class Test extends TestA {
    public function test() {
        inMethod;
    }

    public function test2() {}
}

class TestA {
    public function test2() {}
}
"
   (search-forward "inMethod")

   (let* ((scope (semantic-calculate-scope))
          (tag (oref scope tag))
          (parents (oref scope parents))
          (parentinheritance (oref scope parentinheritance)))
     (should (equal "test" (semantic-tag-name tag)))
     (should (equal "Test" (semantic-tag-name (car parents))))

     ;; TODO: Yes, it finds the parent. But it wont query the phptags
     ;; database if it's not found in the buffers table.
     (should (equal "TestA" (semantic-tag-name (caar parentinheritance)))))))

(provide 'test/context)
