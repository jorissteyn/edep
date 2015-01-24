;;; edep/jump.el --- Jump to context functions

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
(require 'pulse)
(require 'semantic/find)

(define-key edep-command-map (kbd "p") 'edep-jump-parent)
(define-key edep-command-map (kbd "i") 'edep-jump-interface)
(define-key edep-command-map (kbd "t") 'edep-jump-types)
(define-key edep-command-map (kbd "f") 'edep-jump-functions)

;; Note: these are just some hacked extras, actual cool jumping using
;; control+click is not done in this file, but directly with
;; semantic-ia!

(defun edep-jump-parent ()
  (interactive)
  (let ((cur (semantic-current-tag-of-class 'type))
        super tag)
    (unless cur
      (error "Not in a class context"))

    (setq super (car (semantic-tag-type-superclasses cur)))

    (unless super
      (error "Class has no parent"))

    (setq tag (semanticdb-typecache-find super))

    (unless tag
      (error "No tag found for %s" super))

    (edep-jump-tag tag)))

(defun edep-jump-interface ()
  (interactive)
  (let ((cur (semantic-current-tag-of-class 'type))
        ifaces choice tag)
    (unless cur
      (error "Not in a class context"))

    (setq ifaces (semantic-tag-type-interfaces cur))

    (unless ifaces
      (error "Class has no interfaces"))

    (setq choice
          (if (eq 1 (length ifaces))
              (car ifaces)
            (completing-read "Interface: " ifaces)))
    (setq tag (semanticdb-typecache-find choice))

    (unless tag
      (error "No tag found for %s" choice))

    (edep-jump-tag tag)))

(defun edep-jump-types ()
  "List all types and jump to selection."
  (interactive)

  ;; TODO: group by namespace part!
  (let (names choice)
    (dolist (res (edep-phptags-find-tags "^" "class"))
      (push (edep-phptags-tag-name res) names))

    (setq choice (completing-read "Type: " names))
    (when choice
      (edep-jump-tag (semanticdb-typecache-find choice)))))

(defun edep-jump-functions ()
  "List all functions and jump to selection."
  (interactive)

  (let (names choice)
    ;; Warning: 100K+ results, be careful!
    (dolist (res (edep-phptags-find-tags "^[^:]+$" "function"))
      (push (edep-phptags-tag-name res) names))

    (setq choice (completing-read "Function: " names))
    (when choice
      (edep-jump-tag (semanticdb-typecache-find choice)))))

(defun edep-jump-tag (tag)
  "Open the file of TAG in a new buffer and jump to it."
  (let ((target (semantic-tag-buffer tag)))
    ;; TODO: use fast-jump-helper (but our fake tags are bogus)
    ;; (semantic-ia--fast-jump-helper target)
    (if target
        (progn (switch-to-buffer target)
               (goto-char (car (semantic-tag-bounds tag)))
               (pulse-momentary-highlight-one-line (point)))
      (error "Could not find file for tag %s" (semantic-tag-name tag)))))

(provide 'edep/jump)
