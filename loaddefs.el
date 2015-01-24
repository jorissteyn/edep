;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "edep" "edep.el" (21811 49682 328842 295000))
;;; Generated autoloads from edep.el

(let ((loads (get 'edep 'custom-loads))) (if (member '"edep" loads) nil (put 'edep 'custom-loads (cons '"edep" loads))))

(defvar edep-mode nil "\
Non-nil if Edep mode is enabled.
See the command `edep-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `edep-mode'.")

(custom-autoload 'edep-mode "edep" nil)

(autoload 'edep-mode "edep" "\
EDEP // An Emacs Development Environment for PHP

\\{edep-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "edep/disco" "edep/disco.el" (21811 44668 330101
;;;;;;  300000))
;;; Generated autoloads from edep/disco.el

(autoload 'edep-disco-mode "edep/disco" "\
EDEP // An Emacs Development Environment for PHP

Visualize parser results using semantic decoration modes

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("edep-test.el" "edep/jump.el" "edep/keymap.el"
;;;;;;  "edep/mode-line.el" "edep/phptags.el" "edep/semantic-php-db.el"
;;;;;;  "edep/semantic-php-symref.el" "edep/semantic-php.el" "edep/wisent-php-macro.el"
;;;;;;  "edep/wisent-php.el") (21811 57230 856221 105000))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
