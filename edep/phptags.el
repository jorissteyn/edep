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

(require 'edep/keymap)
(require 'semantic/tag)

;; Optional dependency.
(require 'projectile nil t)
(declare-function projectile-project-root "projectile")

(define-key edep-command-map (kbd "x") 'edep-phptags-index)

(defcustom edep-phptags-executable "phptags"
  "Location of the phptags executable"
  :group 'edep
  :type 'string)

(defcustom edep-phptags-database-name "PHPTAGS.sqlite"
  "Name of the tags database file.

This value is passed to the phptags executable, and is used to
determine the project root."
  :group 'edep
  :type 'string)

(defcustom edep-phptags-debug-limit 1
  "Maximum size in MB of *PHPTAGS* debug buffer."
  :group 'edep
  :type 'string)

(defcustom edep-use-projectile t
  "Use projectile to determine the project root."
  :group 'edep
  :type 'boolean)

;; TODO: allow selecting a preset.
;; Default: .php[s457]?
;; Drupal:  .(inc|module|install|profile|php)
(defcustom edep-phptags-default-pattern "/\\.php[s457]?$/i"
  "Default pattern to match source file names for indexing"
  :group 'edep
  :type 'string)

(defvar edep-phptags-index-progress nil
  "Variable indicating progress of pending indexing process.

The value is a number between 0 and 100, or nil when no process is running.")

(defun edep-phptags-find-project-root ()
  "Find project root directory.

This root directory is passed to the phptags executable which
will use it as a starting point for the recursive indexing
action.

If the root can not be determined using projectile, find an
existing database file or prompt the user for input."
  (let (dir)
    ;; First try projectile.
    (when (and (featurep 'projectile)
               edep-use-projectile)
      (condition-case nil
          (setq dir (projectile-project-root))
        (error nil)))

    ;; Look for dominating tags database file.
    (unless dir
      (setq dir (locate-dominating-file default-directory edep-phptags-database-name)))

    ;; Just ask the user.
    (unless dir
      (setq dir (read-directory-name "Project root: ")))

    dir))

(defun edep-phptags-index ()
  "Prompt directory name and index it using PHPTAGS."
  (interactive)
  (when edep-phptags-index-progress
    (error "Indexing progress already started"))

  (let* ((root (edep-phptags-find-project-root))
         (process (start-process
                  "phptags"
                  "*PHPTAGS*"
                  edep-phptags-executable
                  "index"
                  "--root" root
                  "--database" (expand-file-name edep-phptags-database-name root)
                  "--pattern" edep-phptags-default-pattern)))

    (set-process-sentinel process 'edep-phptags-index-process-sentinel)
    (set-process-filter process 'edep-phptags-index-process-filter)))

(defun edep-phptags-index-reset-progress ()
  "Reset the progress counter to indicate no indexing in progress."
  (setq edep-phptags-index-progress nil)
  (force-mode-line-update))

(defun edep-phptags-index-process-sentinel (process event)
  "Monitor process events for the indexing process"
  (cond ((string-match "finished" event)
         (setq edep-phptags-index-progress 100)
         (run-with-timer 10 nil 'edep-phptags-index-reset-progress))
        ((string-match "exited abnormally" event)
         (edep-phptags-index-reset-progress)
         ;; TODO: Solve this prettier than throwing an error inside
         ;; the sentinel, maybe we could let process-filter handle
         ;; this and actually show the error message.
         (error "Command failed: '%S' - run this command outside emacs to debug."
                (mapconcat 'identity (process-command process) " ")))))

(defun edep-phptags-index-process-filter (process string &optional silent)
  "Process status updates for the indexing process.

Non-nil SILENT will supress extra status info in the minibuffer."
  (when (string-match "\\([0-9]+\\)%" string)
    (let ((progress (string-to-number (match-string 1 string))))

      ;; Report in minibuffer after 10%.
      (when (and (not silent)
                 (equal 0 (% progress 10))
                 (not (equal progress edep-phptags-index-progress)))
        (if (string-match "\\[\\([0-9]+\\) files.* \\([0-9]+\\) tags.*\\]" string)
            (message "%s tags in %s files (%d%%)"
                     (match-string 2 string)
                     (match-string 1 string)
                     progress)
          (message "%S" string)))

      (setq edep-phptags-index-progress progress)

      (force-mode-line-update))))

(defun edep-phptags-execute (&rest args)
  "Execute PHP TAGS with given arguments.

ARGS is a list of arguments.

The output is returned as a string."
  (unless edep-phptags-executable
    (error "PHP TAGS not configured, please customize `edep-phptags-executable'."))

  (let ((buffer (get-buffer-create "*PHPTAGS*"))
        (cd default-directory)
        exitcode
        result)

    (with-current-buffer buffer
      (erase-buffer)
      (setq default-directory cd)
      (setq exitcode (apply 'call-process
                            edep-phptags-executable
                            nil buffer nil
                            args)

            result (buffer-string))

      (insert (format "command `%s %s` finished with code %s\n"
                      edep-phptags-executable
                      (mapconcat 'identity args " ")
                      exitcode)))

    (if (< 0 exitcode)
        (error "Execution of PHPTAGS failed: %s %s" exitcode result)
      (read result))))

(defun edep-phptags-find-tags (pattern &optional type usage-type)
  "Find all tags matching a pattern.

PATTERN is the PCRE expression matching on the tag name,
TYPE is the tag type like class, constant or variable,
USAGE-TYPE defaults to definition, but reference is also possible."
  (edep-phptags-execute
   "query"
   ;; TODO: seems hard to get the quoting right, we want
   ;; a literal backslash but have to account for
   ;; escaping it as a shell argument as well as for the
   ;; PCRE pattern. On the other hand we want $, ^ to be
   ;; taken literally in the PCRE.
   ;;
   ;; Below regexp quotes every backslash ([\\]) twice,
   ;; leading to four backslashes - that's 8 backslashes
   ;; in an elisp string.
   (replace-regexp-in-string "[\\]" "\\\\\\\\"
                             pattern)
   "--format" "lisp"
   ;; The tag type, defaults to all types
   "--type" (or type "*")))

(defun edep-phptags-find-semantic-tags (pattern &optional type usage-type)
  "Find all tags matching a pattern, and return them as semantic tags.

TODO: shouldn't be needed.

PATTERN is the PCRE expression matching on the tag name,
TYPE is the tag type like class, constant or variable,
USAGE-TYPE defaults to definition, but reference is also possible."
  (let ((result (edep-phptags-find-tags pattern type usage-type))
        res tags tag tagtype tagname shortname)
    (dolist (res result tags)
      (setq tagtype (edep-phptags-tag-type res)
            tagname (edep-phptags-tag-name res))
      (setq tag
            (cond ((equal "class" tagtype)
                   (semantic-tag tagname 'type :type "class"))
                  ((equal "interface" tagtype)
                   (semantic-tag tagname 'type :type "interface"))
                  ((equal "function" tagtype)
                   (semantic-tag tagname 'function))
                  ((equal "variable" tagtype)
                   (semantic-tag tagname 'variable))
                  ((equal "constant" tagtype)
                   (semantic-tag tagname 'constant))
                  (t (error "Unknown type %s" (edep-phptags-tag-type res)))))

      (semantic--tag-put-property tag :filename (edep-phptags-tag-filename res))

      ;; If filename is not an internal symbol, set the buffer position.
      (unless (string-match "<\\(\\sw+\\)>" (edep-phptags-tag-filename res))
        (semantic-tag-set-bounds tag
                                 (edep-phptags-tag-filestart res)
                                 (edep-phptags-tag-fileend res))
        (semantic-tag-put-attribute tag :line (edep-phptags-tag-linestart res)))

      (push tag tags))))

(defun edep-phptags-tag-name (result)
  "The name column of a PHPTAGS result line."
  (nth 0 result))

(defun edep-phptags-tag-filename (result)
  "The filename column of a PHPTAGS result line."
  (nth 1 result))

(defun edep-phptags-tag-type (result)
  "The type column of a PHPTAGS result line."
  (nth 2 result))

(defun edep-phptags-tag-linestart (result)
  "The linestart column of a PHPTAGS result line."
  (nth 3 result))

(defun edep-phptags-tag-lineend (result)
  "The lineend column of a PHPTAGS result line."
  (nth 4 result))

(defun edep-phptags-tag-filestart (result)
  "The filestart column of a PHPTAGS result line."
  (nth 5 result))

(defun edep-phptags-tag-fileend (result)
  "The fileend column of a PHPTAGS result line."
  (nth 6 result))

(defun edep-phptags-tag-comment (result)
  "The comment column of a PHPTAGS result line."
  (nth 7 result))

(provide 'edep/phptags)
