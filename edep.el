;;; edep.el --- An Emacs Development Environment for PHP

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

(require 'edep/jump)
(require 'edep/keymap)
(require 'edep/mode-line)
(require 'edep/phptags)
(require 'edep/semantic-php)
(require 'semantic)

;;;###autoload
(defgroup edep nil
  "EDEP // An Emacs Development Environment for PHP"
  :group 'tools)

;;;###autoload
(define-minor-mode edep-mode
  "EDEP // An Emacs Development Environment for PHP

\\{edep-mode-map}"
  :lighter edep-mode-line
  :global t
  :group 'edep
  (cond (edep-mode
         ;; Enable edep-mode
         (add-to-list 'semantic-new-buffer-setup-functions
                      '(php-mode . semantic-php-default-setup))

         ;; Enable semantic
         (semantic-mode)

         ;; Enable jump on mouse button press
         (global-unset-key [(C-down-mouse-1)])
         (global-set-key [(C-mouse-1)] 'semantic-ia-fast-mouse-jump))
        (t
         ;; Disable edep-mode
         (setq semantic-new-buffer-setup-functions
               (delete '(php-mode . semantic-php-default-setup)
                       semantic-new-buffer-setup-functions))


         ;; This does not restore the original binding.
         (global-unset-key [(C-mouse-1)])

         ;; Disable semantic
         (semantic-mode -1))))

(provide 'edep)
