;;; edep/modeline.el --- An Emacs Development Environment for PHP

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

(require 'edep/phptags)

(defcustom edep-mode-line
  '(:eval (format " EDEP%s"
                  (edep-mode-line-project-status)))
  "Mode line lighter for EDEP.

Set this variable to nil to disable the lighter."
  :group 'edep
  :type 'sexp
  :risky t)

(defun edep-mode-line-project-status ()
  "Report status of current project index."
  (if (numberp edep-phptags-index-progress)
      (format "[%d%%%%]" edep-phptags-index-progress)
    ""))

(provide 'edep/mode-line)
