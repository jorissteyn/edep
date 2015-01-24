;;; edep-test.el --- Tests for EDEP

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

;; Tests for wisent parser/lexer
(require 'test/lexer)
(require 'test/parser/cast)
(require 'test/parser/class)
(require 'test/parser/class-member)
(require 'test/parser/condition)
(require 'test/parser/constant)
(require 'test/parser/loop)
(require 'test/parser/function)
(require 'test/parser/interface)
(require 'test/parser/misc)
(require 'test/parser/namespace)
(require 'test/parser/overlay)
(require 'test/parser/trait)
(require 'test/parser/try-catch)
(require 'test/parser/use)
(require 'test/parser/variable)

;; Tests demonstrating to what extent EDEP works with semantic's
;; context analysis.
(require 'test/context)

;; Misc testing utils
(require 'test/utils)

(provide 'edep-test)
