;;; edep/keymap.el --- An Emacs Development Environment for PHP

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

(defcustom edep-keymap-prefix (kbd "C-c C-.")
  "The keymap prefix for EDEP commands."
  :group 'edep
  :type 'string)

(defvar edep-mode-map (make-sparse-keymap)
  "Keymap for EDEP.")

(defvar edep-command-map (make-sparse-keymap)
  "Map of all EDEP commands and their default keys.")

;; Required for command map to be displayed in mode docstring
(fset 'edep-command-map edep-command-map)

;; Make all commands available under the prefix key
(define-key edep-mode-map edep-keymap-prefix 'edep-command-map)

;; Define a menu item
(define-key global-map [menu-bar edep]
  (cons "EDEP" (make-sparse-keymap "EDEP")))

(provide 'edep/keymap)
