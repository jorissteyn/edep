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
(require 'edep/semantic-php-db)
(require 'semantic)

;;;###autoload
(defgroup edep nil
  "EDEP // An Emacs Development Environment for PHP"
  :group 'tools)

(defcustom edep-use-phptags t
  "Enable phptags semanticdb backend."
  :group 'edep
  :type 'boolean)

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

         ;; Add phptags database
         (if edep-use-phptags
             (add-to-list 'semanticdb-project-system-databases
                          (semanticdb-project-database-php "PHP")))

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

         ;; Delete the phptags database
         (let (new-databases '())
           (dolist (database semanticdb-project-system-databases)
             (unless (semanticdb-project-database-php-p database)
               (push database new-databases)))
           (setq semanticdb-project-system-databases new-databases))

         ;; This does not restore the original binding.
         (global-unset-key [(C-mouse-1)])

         ;; Disable semantic
         (semantic-mode -1))))

(provide 'edep)
