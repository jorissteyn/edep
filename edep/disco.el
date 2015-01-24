;;; edep/disco.el --- Bells 'n whistles to visualize the parser

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

(require 'semantic/decorate/mode)
(require 'semantic/edit)

;;;###autoload
(define-minor-mode edep-disco-mode
  "EDEP // An Emacs Development Environment for PHP

Visualize parser results using semantic decoration modes"
  :lighter " DISCO"
  :group 'edep
  :require 'edep
  (cond
   ;; Enable the minor mode
   (edep-disco-mode
    ;; Show underline for all tags in the buffer
    (setq semantic-decoration-styles
          '(("edep-disco-tag-boundary" . t)))

    (semantic-decoration-mode 1)

    ;; Red underline for unmatched syntax
    (semantic-show-unmatched-syntax-mode 1)

    ;; Show active tag in buffer header
    (semantic-stickyfunc-mode 1)

    ;; Highlight edits
    (setq semantic-edits-verbose-flag t)
    (semantic-highlight-edits-mode 1))
   ;; Disable the minor mode
   (t (semantic-decoration-mode -1)
      (semantic-show-unmatched-syntax-mode -1)
      (semantic-stickyfunc-mode -1)
      (semantic-highlight-edits-mode -1))))

;;;
;;; Custom decoration style for disco-mode
;;;
(define-semantic-decoration-style edep-disco-tag-boundary
  "Place an overline above every generated tag.")

(defun edep-disco-tag-boundary-p-default (tag)
  "Return non-nil for any TAG."
  t)

(defun edep-disco-tag-boundary-highlight-default (tag)
  "Highlight the first line of TAG as a boundary."
  (semantic-tag-boundary-highlight-default tag))

(provide 'edep/disco)
