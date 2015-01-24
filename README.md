      __  ___   _____   ____    _____   ____
     / / |__ \ | ____| |  _ \  | ____| |  _ \
    / /    / / |  _|   | | | | |  _|   | |_) |
    \ \   |_|  | |___  | |_| | | |___  |  __/
     \_\  (_)  |_____| |____/  |_____| |_|
      An Emacs Development Environment for PHP

This is a very experimental semantic integration layer.

# Requirements
* emacs 24.4 or higher
* php-mode (any version) or web-mode
* a `phptags` installation (see separate repository)

# Installation
* clone this repository
* make sure the directory is in your load path, then load the autoload definitions

```
(add-to-list 'load-path "~/.emacs.d/custom/edep")
(load "~/.emacs.d/custom/edep/loaddefs.el")
```

* clone phptags and follow its installation instructions
* customize `edep-phptags-executable` (see `M-x customize-group RET edep`)

# Usage examples
## Activating EDEP
Enable the global minor-mode `edep-mode`. This activates the semantic-php integration and enables semantic-mode, CEDET tools like imenu and senator should work out-of-the-box.

## Indexing a project
Make sure phptags is installed and the executable path is configured. Run `M-x edep-phptags-index`. If the root directory of the project can not be determined using projectile, a prompt will show up in the minibuffer. Once the root directory is set, a process is started in the background and progress is reported in the mode line.

## Navigating
In addition to semantics jumping facilities, there are a few extra commands available to navigate non-local tags. For example, the commands `edep-jump-parent` can be used to jump to the parent class. The commands `edep-jump-types` and `edep-jump-functions` can be used to select and jump to any type or function.

Especially nice is the `semantic-ia-fast-jump` command (read: jump to definition) which can be used on any reference to a class or function name. You can navigate a project by control-clicking on those symbols. Note that this implementation is very basic and only works for type names (classes, interfaces, traits) and global functions. Jumping to the definition of a method name is also possible with the limitation that the variable should be a typed function parameter and the method should be directly defined on the type, not inherited.

# Tests and utilities
* Run `make test` in the edep source directory to invoke the test suite.
* Run `make autoload` to generate the autoload definitions.
* Run `make parser-create` to regenerate the parser from the wisent grammar.
* Run `make parser-test PARSE_PATH=/path/to/lots/of/source/files` to feed arbitrary source files to the wisent parser.
* Run `make compile` byte-compile the lisp source files.

# License
    Copyright 2014 Joris Steyn

    EDEP is not part of GNU Emacs.

    This file is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This file is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this file; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301, USA.
