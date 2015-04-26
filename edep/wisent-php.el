;;; wisent-php.el --- Generated parser support file

;; Copyright (C) 2014 Joris Steyn

;; Author:  <joris@falcon>
;; Created: 2015-04-25 23:10:30+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file wisent-php.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;
;; We need a slightly bigger stack than allowed in a standard emacs
  ;; configuration. A future improvement might be to skip some rules
  ;; not required by EDEP so max-specpdl-size can be left untouched.
  (setq max-specpdl-size 1400)

;;; Declarations
;;
(defconst wisent-php--keyword-table
  (semantic-lex-make-keyword-table
   '(("include" . T_INCLUDE)
     ("include_once" . T_INCLUDE_ONCE)
     ("eval" . T_EVAL)
     ("require" . T_REQUIRE)
     ("require_once" . T_REQUIRE_ONCE)
     ("or" . T_LOGICAL_OR)
     ("xor" . T_LOGICAL_XOR)
     ("and" . T_LOGICAL_AND)
     ("print" . T_PRINT)
     ("yield" . T_YIELD)
     ("instanceof" . T_INSTANCEOF)
     ("(int)" . T_INT_CAST)
     ("(double)" . T_DOUBLE_CAST)
     ("(string)" . T_STRING_CAST)
     ("(array)" . T_ARRAY_CAST)
     ("(object)" . T_OBJECT_CAST)
     ("(bool)" . T_BOOL_CAST)
     ("(unset)" . T_UNSET_CAST)
     ("new" . T_NEW)
     ("clone" . T_CLONE)
     ("exit" . T_EXIT)
     ("if" . T_IF)
     ("elseif" . T_ELSEIF)
     ("else" . T_ELSE)
     ("endif" . T_ENDIF)
     ("echo" . T_ECHO)
     ("do" . T_DO)
     ("while" . T_WHILE)
     ("endwhile" . T_ENDWHILE)
     ("for" . T_FOR)
     ("endfor" . T_ENDFOR)
     ("foreach" . T_FOREACH)
     ("endforeach" . T_ENDFOREACH)
     ("declare" . T_DECLARE)
     ("enddeclare" . T_ENDDECLARE)
     ("as" . T_AS)
     ("switch" . T_SWITCH)
     ("endswitch" . T_ENDSWITCH)
     ("case" . T_CASE)
     ("default" . T_DEFAULT)
     ("break" . T_BREAK)
     ("continue" . T_CONTINUE)
     ("goto" . T_GOTO)
     ("function" . T_FUNCTION)
     ("const" . T_CONST)
     ("return" . T_RETURN)
     ("try" . T_TRY)
     ("catch" . T_CATCH)
     ("finally" . T_FINALLY)
     ("throw" . T_THROW)
     ("use" . T_USE)
     ("insteadof" . T_INSTEADOF)
     ("global" . T_GLOBAL)
     ("static" . T_STATIC)
     ("abstract" . T_ABSTRACT)
     ("final" . T_FINAL)
     ("private" . T_PRIVATE)
     ("protected" . T_PROTECTED)
     ("public" . T_PUBLIC)
     ("var" . T_VAR)
     ("unset" . T_UNSET)
     ("isset" . T_ISSET)
     ("empty" . T_EMPTY)
     ("__halt_compiler" . T_HALT_COMPILER)
     ("class" . T_CLASS)
     ("trait" . T_TRAIT)
     ("interface" . T_INTERFACE)
     ("extends" . T_EXTENDS)
     ("implements" . T_IMPLEMENTS)
     ("list" . T_LIST)
     ("array" . T_ARRAY)
     ("callable" . T_CALLABLE)
     ("__LINE__" . T_LINE)
     ("__FILE__" . T_FILE)
     ("__DIR__" . T_DIR)
     ("__CLASS__" . T_CLASS_C)
     ("__TRAIT__" . T_TRAIT_C)
     ("__METHOD__" . T_METHOD_C)
     ("__FUNCTION__" . T_FUNC_C)
     ("namespace" . T_NAMESPACE)
     ("__NAMESPACE__" . T_NS_C))
   'nil)
  "Table of language keywords.")

(defconst wisent-php--token-table
  (semantic-lex-make-type-table
   '(("heredoc-end"
      (T_END_HEREDOC))
     ("heredoc-start"
      (T_START_HEREDOC))
     ("inline-html"
      (T_INLINE_HTML))
     ("close-tag"
      (T_CLOSE_TAG))
     ("open-tag"
      (T_OPEN_TAG))
     ("open-tag-echo"
      (T_OPEN_TAG_WITH_ECHO))
     ("punctuation"
      (T_POW_EQUAL . "**=")
      (T_POW . "**")
      (T_COALESCE . "??")
      (T_ELLIPSIS . "...")
      (T_NS_SEPARATOR . "\\")
      (T_PAAMAYIM_NEKUDOTAYIM . "::")
      (T_CURLY_OPEN . "{$")
      (T_DOLLAR_OPEN_CURLY_BRACES . "${")
      (T_DOUBLE_ARROW . "=>")
      (T_OBJECT_OPERATOR . "->")
      (T_DEC . "--")
      (T_INC . "++")
      (T_SR . ">>")
      (T_SL . "<<")
      (T_SPACESHIP . "<=>")
      (T_IS_GREATER_OR_EQUAL . ">=")
      (T_IS_SMALLER_OR_EQUAL . "<=")
      (T_IS_NOT_IDENTICAL . "!==")
      (T_IS_IDENTICAL . "===")
      (T_IS_NOT_EQUAL . "!=")
      (T_IS_EQUAL . "==")
      (T_BOOLEAN_AND . "&&")
      (T_BOOLEAN_OR . "||")
      (T_SR_EQUAL . ">>=")
      (T_SL_EQUAL . "<<=")
      (T_XOR_EQUAL . "^=")
      (T_OR_EQUAL . "|=")
      (T_AND_EQUAL . "&=")
      (T_MOD_EQUAL . "%=")
      (T_CONCAT_EQUAL . ".=")
      (T_DIV_EQUAL . "/=")
      (T_MUL_EQUAL . "*=")
      (T_MINUS_EQUAL . "-=")
      (T_PLUS_EQUAL . "+=")
      (XOR . "^")
      (OR . "|")
      (BITNOT . "~")
      (GT . ">")
      (LT . "<")
      (DIVIDE . "/")
      (MULTIPLY . "*")
      (MODULO . "%")
      (MINUS . "-")
      (PLUS . "+")
      (NEGATE . "!")
      (QUESTION . "?")
      (DOUBLEQUOTE . "\"")
      (BACKQUOTE . "`")
      (AMPERSAND . "&")
      (ASSIGN . "=")
      (AT . "@")
      (DOT . ".")
      (COMMA . ",")
      (COLON . ":")
      (SEMICOLON . ";")
      (DOLLAR . "$"))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)"))
     ("mb"
      (mbstring))
     ("variable"
      (T_VARIABLE))
     ("encapsed-and-whitespace"
      (T_ENCAPSED_AND_WHITESPACE))
     ("quoted-string"
      (T_CONSTANT_ENCAPSED_STRING))
     ("string"
      (T_STRING))
     ("float"
      (T_DNUMBER))
     ("integer"
      (T_LNUMBER)))
   '(("heredoc-end" matchdatatype sexp)
     ("heredoc-end" :declared t)
     ("heredoc-start" matchdatatype sexp)
     ("heredoc-start" :declared t)
     ("inline-html" matchdatatype sexp)
     ("inline-html" :declared t)
     ("close-tag" matchdatatype sexp)
     ("close-tag" :declared t)
     ("open-tag" matchdatatype sexp)
     ("open-tag" :declared t)
     ("open-tag-echo" matchdatatype sexp)
     ("open-tag-echo" :declared t)
     ("punctuation" syntax "\\(\\s.\\|\\s$\\|\\s'\\|[$]\\|[\\]\\)+")
     ("punctuation" matchdatatype string)
     ("punctuation" :declared t)
     ("keyword" :declared t)
     ("block" :declared t)
     ("mb" syntax "[[:nonascii:]]+")
     ("mb" :declared t)
     ("variable" syntax "\\([$][a-zA-Z_]+[a-zA-Z0-9_]*\\)")
     ("variable" :declared t)
     ("encapsed-and-whitespace" :declared t)
     ("quoted-string" syntax "\\s\"")
     ("quoted-string" matchdatatype sexp)
     ("quoted-string" :declared t)
     ("string" syntax "\\<\\([a-zA-Z0-9_]\\)+\\>")
     ("string" matchdatatype regexp)
     ("string" :declared t)
     ("float" :declared t)
     ("integer" :declared t)))
  "Table of lexical tokens.")

(defconst wisent-php--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((T_LNUMBER T_DNUMBER T_STRING T_CONSTANT_ENCAPSED_STRING T_ENCAPSED_AND_WHITESPACE T_VARIABLE mbstring PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND T_PRINT T_YIELD T_INSTANCEOF T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST T_NEW T_CLONE T_EXIT T_IF T_ELSEIF T_ELSE T_ENDIF T_ECHO T_DO T_WHILE T_ENDWHILE T_FOR T_ENDFOR T_FOREACH T_ENDFOREACH T_DECLARE T_ENDDECLARE T_AS T_SWITCH T_ENDSWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE T_GOTO T_FUNCTION T_CONST T_RETURN T_TRY T_CATCH T_FINALLY T_THROW T_USE T_INSTEADOF T_GLOBAL T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC T_VAR T_UNSET T_ISSET T_EMPTY T_HALT_COMPILER T_CLASS T_TRAIT T_INTERFACE T_EXTENDS T_IMPLEMENTS T_LIST T_ARRAY T_CALLABLE T_LINE T_FILE T_DIR T_CLASS_C T_TRAIT_C T_METHOD_C T_FUNC_C T_NAMESPACE T_NS_C DOLLAR SEMICOLON COLON COMMA DOT AT ASSIGN AMPERSAND BACKQUOTE DOUBLEQUOTE QUESTION NEGATE PLUS MINUS MODULO MULTIPLY DIVIDE LT GT BITNOT OR XOR T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_SMALLER_OR_EQUAL T_IS_GREATER_OR_EQUAL T_SPACESHIP T_SL T_SR T_INC T_DEC T_OBJECT_OPERATOR T_DOUBLE_ARROW T_DOLLAR_OPEN_CURLY_BRACES T_CURLY_OPEN T_PAAMAYIM_NEKUDOTAYIM T_NS_SEPARATOR T_ELLIPSIS T_COALESCE T_POW T_POW_EQUAL T_OPEN_TAG_WITH_ECHO T_OPEN_TAG T_CLOSE_TAG T_INLINE_HTML T_START_HEREDOC T_END_HEREDOC)
       ((left T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE)
        (left COMMA)
        (left T_LOGICAL_OR)
        (left T_LOGICAL_XOR)
        (left T_LOGICAL_AND)
        (right T_PRINT)
        (right T_YIELD)
        (right T_DOUBLE_ARROW)
        (left ASSIGN T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL)
        (left QUESTION COLON)
        (right T_COALESCE)
        (left T_BOOLEAN_OR)
        (left T_BOOLEAN_AND)
        (left OR)
        (left XOR)
        (left AMPERSAND)
        (nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL)
        (nonassoc LT T_IS_SMALLER_OR_EQUAL GT T_IS_GREATER_OR_EQUAL T_SPACESHIP)
        (left T_SL T_SR)
        (left PLUS MINUS DOT)
        (left MULTIPLY DIVIDE MODULO)
        (right NEGATE)
        (nonassoc T_INSTANCEOF)
        (right BITNOT T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST AT)
        (right T_POW)
        (right LBRACK)
        (nonassoc T_NEW T_CLONE)
        (left T_ELSEIF)
        (left T_ELSE)
        (left T_ENDIF)
        (right T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC))
       (top_statement_start
        ((T_OPEN_TAG top_statement)
         (identity $2))
        ((T_OPEN_TAG_WITH_ECHO top_statement)
         (identity $2))
        ((top_statement))
        ((T_CLOSE_TAG)
         nil))
       (top_statement_list
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((top_statement)))
       (namespace_name
        ((T_STRING))
        ((namespace_name T_NS_SEPARATOR T_STRING)
         (concat $1 $2 $3)))
       (name
        ((namespace_name))
        ((T_NAMESPACE T_NS_SEPARATOR namespace_name)
         nil)
        ((T_NS_SEPARATOR namespace_name)
         (concat $1 $2)))
       (top_statement
        ((statement))
        ((function_declaration_statement))
        ((class_declaration_statement))
        ((trait_declaration_statement))
        ((interface_declaration_statement))
        ((T_HALT_COMPILER PAREN_BLOCK SEMICOLON)
         nil)
        ((T_NAMESPACE namespace_name SEMICOLON)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-type
            (wisent-php-normalize-qualified-name $2)
            $1 nil nil :braceless t))))
        ((T_NAMESPACE namespace_name BRACE_BLOCK)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-type
            (wisent-php-normalize-qualified-name $2)
            $1
            (semantic-parse-region
             (car $region3)
             (cdr $region3)
             'top_statement_list
             (or nil 1))
            nil))))
        ((T_NAMESPACE BRACE_BLOCK)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-type "\\" $1
                                  (semantic-parse-region
                                   (car $region2)
                                   (cdr $region2)
                                   'top_statement_list
                                   (or nil 1))
                                  nil))))
        ((T_USE mixed_group_use_declaration SEMICOLON)
         (identity $2))
        ((T_USE use_type group_use_declaration SEMICOLON)
         (dolist
             (tag
              (if
                  (semantic-tag-p $3)
                  (list $3)
                $3)
              $3)
           (semantic-tag-put-attribute tag :type $2)))
        ((T_USE use_declarations SEMICOLON)
         (dolist
             (tag $2 $2)
           (wisent-cook-tag tag)))
        ((T_USE use_type use_declarations SEMICOLON)
         (dolist
             (tag
              (dolist
                  (tag
                   (if
                       (semantic-tag-p $3)
                       (list $3)
                     $3)
                   $3)
                (semantic-tag-put-attribute tag :type $2))
              (dolist
                  (tag
                   (if
                       (semantic-tag-p $3)
                       (list $3)
                     $3)
                   $3)
                (semantic-tag-put-attribute tag :type $2)))
           (wisent-cook-tag tag)))
        ((T_CONST const_list SEMICOLON)
         (identity $2)))
       (use_type
        ((T_FUNCTION))
        ((T_CONST)))
       (group_use_declaration
        ((namespace_name T_NS_SEPARATOR BRACE_BLOCK)
         (let
             (result)
           (dolist
               (decl
                (semantic-parse-region
                 (car $region3)
                 (cdr $region3)
                 'use_declarations_brace_block
                 (or nil 1))
                (nreverse result))
             (semantic-tag-set-name decl
                                    (concat $1 "\\"
                                            (semantic-tag-name decl)))
             (push decl result)))))
       (mixed_group_use_declaration
        ((namespace_name T_NS_SEPARATOR BRACE_BLOCK)
         (let
             (result)
           (dolist
               (decl
                (semantic-parse-region
                 (car $region3)
                 (cdr $region3)
                 'inline_use_declarations_brace_block
                 (or nil 1))
                (nreverse result))
             (semantic-tag-set-name decl
                                    (concat $1 "\\"
                                            (semantic-tag-name decl)))
             (push decl result)))))
       (inline_use_declarations
        ((inline_use_declarations COMMA inline_use_declaration)
         (append $1
                 (list $3)))
        ((inline_use_declaration)
         (list $1)))
       (inline_use_declarations_brace_block
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((inline_use_declaration)))
       (inline_use_declaration
        ((use_declaration)
         (dolist
             (tag
              (if
                  (semantic-tag-p $1)
                  (list $1)
                $1)
              $1)
           (semantic-tag-put-attribute tag :type "class")))
        ((use_type use_declaration)
         (dolist
             (tag
              (if
                  (semantic-tag-p $2)
                  (list $2)
                $2)
              $2)
           (semantic-tag-put-attribute tag :type $1))))
       (use_declarations
        ((use_declarations COMMA use_declaration)
         (append $1
                 (list $3)))
        ((use_declaration)
         (list $1)))
       (use_declarations_brace_block
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((use_declaration)))
       (use_declaration
        ((namespace_name)
         (wisent-raw-tag
          (semantic-tag $1 'use :alias
                        (or nil
                            (car
                             (last
                              (split-string $1 "\\\\")))))))
        ((namespace_name T_AS T_STRING)
         (wisent-raw-tag
          (semantic-tag $1 'use :alias
                        (or $3
                            (car
                             (last
                              (split-string $1 "\\\\")))))))
        ((T_NS_SEPARATOR namespace_name)
         (wisent-raw-tag
          (semantic-tag $2 'use :alias
                        (or nil
                            (car
                             (last
                              (split-string $2 "\\\\")))))))
        ((T_NS_SEPARATOR namespace_name T_AS T_STRING)
         (wisent-raw-tag
          (semantic-tag $2 'use :alias
                        (or $4
                            (car
                             (last
                              (split-string $2 "\\\\"))))))))
       (const_list_paren_block
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((const_list)))
       (const_list
        ((const_list COMMA const_decl)
         (append $1
                 (wisent-cook-tag $3)))
        ((const_decl)
         (wisent-cook-tag $1)))
       (inner_statement_list
        (nil)
        ((inner_statement_list inner_statement)
         (append $1 $2)))
       (inner_statement_list_brace_block
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((inner_statement)))
       (inner_statement
        ((statement))
        ((function_declaration_statement))
        ((class_declaration_statement))
        ((trait_declaration_statement))
        ((interface_declaration_statement))
        ((T_HALT_COMPILER PAREN_BLOCK SEMICOLON)
         nil))
       (statement
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'inner_statement_list_brace_block
          (or nil 1)))
        ((if_stmt))
        ((alt_if_stmt))
        ((T_WHILE PAREN_BLOCK while_statement)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'expr_paren_block
           (or nil 1))
          $3))
        ((T_DO statement T_WHILE PAREN_BLOCK SEMICOLON)
         (append $2
                 (semantic-parse-region
                  (car $region4)
                  (cdr $region4)
                  'expr_paren_block
                  (or nil 1))))
        ((T_FOR PAREN_BLOCK for_statement)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'for_exprs_paren_block
           (or nil 1))
          $3))
        ((T_SWITCH PAREN_BLOCK switch_case_list)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'expr_paren_block
           (or nil 1))
          $3))
        ((T_BREAK optional_expr SEMICOLON)
         (identity $2))
        ((T_CONTINUE optional_expr SEMICOLON)
         (identity $2))
        ((T_RETURN optional_expr SEMICOLON)
         (identity $2))
        ((T_GLOBAL global_var_list SEMICOLON)
         (identity $2))
        ((T_STATIC static_var_list SEMICOLON)
         (identity $2))
        ((T_ECHO echo_expr_list SEMICOLON)
         (identity $2))
        ((T_CLOSE_TAG T_INLINE_HTML T_OPEN_TAG)
         nil)
        ((T_CLOSE_TAG T_INLINE_HTML T_OPEN_TAG_WITH_ECHO)
         nil)
        ((expr SEMICOLON))
        ((T_UNSET PAREN_BLOCK SEMICOLON)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'unset_variables
          (or nil 1)))
        ((T_FOREACH PAREN_BLOCK foreach_statement)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'foreach_paren_block
           (or nil 1))
          $3))
        ((T_DECLARE PAREN_BLOCK declare_statement)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'const_list_paren_block
          (or nil 1)))
        ((SEMICOLON)
         nil)
        ((T_TRY BRACE_BLOCK catch_list finally_statement)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'inner_statement_list_brace_block
           (or nil 1))
          $3 $4))
        ((T_THROW expr SEMICOLON)
         (identity $2))
        ((T_GOTO T_STRING SEMICOLON)
         nil)
        ((T_STRING COLON)
         nil))
       (foreach_paren_block
        ((LPAREN expr T_AS foreach_variable RPAREN)
         (append $2 $4))
        ((LPAREN expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable RPAREN)
         (append $2 $4 $6)))
       (catch_list
        (nil)
        ((catch_list T_CATCH PAREN_BLOCK BRACE_BLOCK)
         (append $1
                 (semantic-parse-region
                  (car $region3)
                  (cdr $region3)
                  'catch_list_name_paren_block
                  (or nil 1))
                 (semantic-parse-region
                  (car $region4)
                  (cdr $region4)
                  'inner_statement_list_brace_block
                  (or nil 1)))))
       (catch_list_name_paren_block
        ((LPAREN name T_VARIABLE RPAREN)
         (wisent-raw-tag
          (semantic-tag-new-variable $3 $2 nil))))
       (finally_statement
        (nil)
        ((T_FINALLY BRACE_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'inner_statement_list_brace_block
          (or nil 1))))
       (unset_variables
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((COMMA)
         nil)
        ((unset_variable)))
       (unset_variable
        ((variable)))
       (function_declaration_statement
        ((function returns_ref T_STRING PAREN_BLOCK return_type BRACE_BLOCK)
         (wisent-raw-tag
          (semantic-tag-new-function $3 $5
                                     (semantic-parse-region
                                      (car $region4)
                                      (cdr $region4)
                                      'parameter_list
                                      (or nil 1))
                                     :members
                                     (semantic-parse-region
                                      (car $region6)
                                      (cdr $region6)
                                      'inner_statement_list_brace_block
                                      (or nil 1))))))
       (is_reference
        (nil)
        ((AMPERSAND)))
       (is_variadic
        (nil)
        ((T_ELLIPSIS)))
       (class_declaration_statement
        ((class_modifiers T_CLASS T_STRING extends_from implements_list BRACE_BLOCK)
         (wisent-raw-tag
          (semantic-tag-new-type $3 $2
                                 (semantic-parse-region
                                  (car $region6)
                                  (cdr $region6)
                                  'class_statement_list
                                  (or nil 1))
                                 (cons $4 $5)
                                 :typemodifiers $1)))
        ((T_CLASS T_STRING extends_from implements_list BRACE_BLOCK)
         (wisent-raw-tag
          (semantic-tag-new-type $2 $1
                                 (semantic-parse-region
                                  (car $region5)
                                  (cdr $region5)
                                  'class_statement_list
                                  (or nil 1))
                                 (cons $3 $4)))))
       (trait_declaration_statement
        ((T_TRAIT T_STRING extends_from implements_list BRACE_BLOCK)
         (wisent-raw-tag
          (semantic-tag-new-type $2 $1
                                 (semantic-parse-region
                                  (car $region5)
                                  (cdr $region5)
                                  'class_statement_list
                                  (or nil 1))
                                 (cons $3 $4)))))
       (interface_declaration_statement
        ((T_INTERFACE T_STRING interface_extends_list BRACE_BLOCK)
         (wisent-raw-tag
          (semantic-tag-new-type $2 $1
                                 (semantic-parse-region
                                  (car $region4)
                                  (cdr $region4)
                                  'class_statement_list
                                  (or nil 1))
                                 (cons $3 nil)))))
       (class_modifiers
        ((class_modifier)
         (list $1))
        ((class_modifiers class_modifier)
         (append $1
                 (list $2))))
       (class_modifier
        ((T_ABSTRACT))
        ((T_FINAL)))
       (extends_from
        (nil)
        ((T_EXTENDS name)
         (identity $2)))
       (interface_extends_list
        (nil)
        ((T_EXTENDS name_list)
         (identity $2)))
       (implements_list
        (nil)
        ((T_IMPLEMENTS name_list)
         (identity $2)))
       (foreach_variable
        ((variable))
        ((AMPERSAND variable)
         (identity $2))
        ((T_LIST PAREN_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'assignment_list_elements
          (or nil 1))))
       (for_statement
        ((statement))
        ((COLON inner_statement_list T_ENDFOR SEMICOLON)
         (identity $2)))
       (foreach_statement
        ((statement))
        ((COLON inner_statement_list T_ENDFOREACH SEMICOLON)
         (identity $2)))
       (declare_statement
        ((statement))
        ((COLON inner_statement_list T_ENDDECLARE SEMICOLON)
         (identity $2)))
       (switch_case_list
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'case_list_brace_block
          (or nil 1)))
        ((COLON case_list T_ENDSWITCH SEMICOLON)
         (identity $2))
        ((COLON SEMICOLON case_list T_ENDSWITCH SEMICOLON)
         (identity $3)))
       (case_list_brace_block
        ((LBRACE)
         nil)
        ((LBRACE SEMICOLON)
         nil)
        ((RBRACE)
         nil)
        ((case_list)))
       (case_list
        (nil)
        ((case_list T_CASE expr case_separator inner_statement_list)
         (append $1 $3 $5))
        ((case_list T_DEFAULT case_separator inner_statement_list)
         (append $1 $4)))
       (case_separator
        ((COLON))
        ((SEMICOLON)))
       (while_statement
        ((statement))
        ((COLON inner_statement_list T_ENDWHILE SEMICOLON)
         (identity $2)))
       (if_stmt_without_else
        ((T_IF PAREN_BLOCK statement)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'expr_paren_block
           (or nil 1))
          $3))
        ((if_stmt_without_else T_ELSEIF PAREN_BLOCK statement)
         (append $1
                 (semantic-parse-region
                  (car $region3)
                  (cdr $region3)
                  'expr_paren_block
                  (or nil 1))
                 $4)))
       (if_stmt
        ((if_stmt_without_else))
        ((if_stmt_without_else T_ELSE statement)
         (append $1 $3)))
       (alt_if_stmt_without_else
        ((T_IF PAREN_BLOCK COLON inner_statement_list)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'expr_paren_block
           (or nil 1))
          $4))
        ((alt_if_stmt_without_else T_ELSEIF PAREN_BLOCK COLON inner_statement_list)
         (append $1
                 (semantic-parse-region
                  (car $region3)
                  (cdr $region3)
                  'expr_paren_block
                  (or nil 1))
                 $5)))
       (alt_if_stmt
        ((alt_if_stmt_without_else T_ENDIF SEMICOLON))
        ((alt_if_stmt_without_else T_ELSE COLON inner_statement_list T_ENDIF SEMICOLON)
         (append $1 $4)))
       (parameter_list
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((parameter COMMA))
        ((parameter RPAREN)))
       (parameter
        ((optional_type is_reference is_variadic T_VARIABLE)
         (wisent-raw-tag
          (semantic-tag-new-variable $4 $1 nil :typemodifiers
                                     (remq nil
                                           (list $2 $3)))))
        ((optional_type is_reference is_variadic T_VARIABLE ASSIGN expr)
         (wisent-raw-tag
          (semantic-tag-new-variable $4 $1
                                     (cond
                                      ((and
                                        (semantic-tag-p $6)
                                        (memq
                                         (semantic-tag-class $6)
                                         '(code constant)))
                                       (semantic-tag-name $6))
                                      ((stringp $6)
                                       (identity $6)))
                                     :typemodifiers
                                     (remq nil
                                           (list $2 $3))))))
       (optional_type
        (nil)
        ((type)))
       (type
        ((T_ARRAY))
        ((T_CALLABLE))
        ((name)))
       (return_type
        (nil)
        ((COLON type)
         (identity $2)))
       (argument_list
        ((PAREN_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'non_empty_argument_list
          (or nil 1))))
       (non_empty_argument_list
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((COMMA)
         nil)
        ((argument)))
       (argument
        ((expr))
        ((T_ELLIPSIS expr)
         (identity $2)))
       (global_var_list
        ((global_var_list COMMA global_var)
         (append $1 $3))
        ((global_var)
         (identity $1)))
       (global_var
        ((simple_variable)))
       (static_var_list
        ((static_var_list COMMA static_var)
         (append $1 $3))
        ((static_var)))
       (static_var
        ((T_VARIABLE)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $1 nil nil))))
        ((T_VARIABLE ASSIGN expr)
         (append
          (wisent-cook-tag
           (wisent-raw-tag
            (semantic-tag-new-variable $1 nil nil)))
          $3)))
       (class_statement_list
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((class_statement)))
       (class_statement
        ((variable_modifiers property_list SEMICOLON)
         (let
             ((result nil))
           (dolist
               (property
                (nreverse $2)
                result)
             (semantic-tag-put-attribute property :typemodifiers $1)
             (setq result
                   (cons property result)))))
        ((T_CONST class_const_list SEMICOLON)
         (identity $2))
        ((T_USE name_list trait_adaptations)
         (let
             (result tmp)
           (dolist
               (name $2 result)
             (setq tmp
                   (wisent-raw-tag
                    (semantic-tag name 'trait-usage :adaptations $3)))
             (setq result
                   (append result
                           (wisent-cook-tag tmp))))))
        ((method_modifiers function returns_ref T_STRING PAREN_BLOCK return_type method_body)
         (wisent-raw-tag
          (semantic-tag-new-function $4 $6
                                     (semantic-parse-region
                                      (car $region5)
                                      (cdr $region5)
                                      'parameter_list
                                      (or nil 1))
                                     :typemodifiers $1 :constructor-flag
                                     (equal $4 "__construct")
                                     :destructor-flag
                                     (equal $4 "__destruct")
                                     :members $7))))
       (name_list
        ((name)
         (list $1))
        ((name_list COMMA name)
         (append $1
                 (list $3))))
       (trait_adaptations
        ((SEMICOLON)
         nil)
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'trait_adaptations_brace_block
          (or nil 1))))
       (trait_adaptations_brace_block
        (nil)
        ((LBRACE trait_adaptation_list RBRACE)
         (identity $2)))
       (trait_adaptation_list
        ((trait_adaptation)
         (list $1))
        ((trait_adaptation_list trait_adaptation)
         (append $1
                 (list $2))))
       (trait_adaptation
        ((trait_precedence SEMICOLON))
        ((trait_alias SEMICOLON)))
       (trait_precedence
        ((absolute_trait_method_reference T_INSTEADOF name_list)
         (wisent-raw-tag
          (semantic-tag $1 'trait-precedence :insteadof $3))))
       (trait_alias
        ((trait_method_reference T_AS trait_modifiers T_STRING)
         (wisent-raw-tag
          (semantic-tag $1 'trait-alias :alias $3 :modifiers $4)))
        ((trait_method_reference T_AS member_modifier)
         (wisent-raw-tag
          (semantic-tag $1 'trait-alias :alias $3))))
       (trait_method_reference
        ((T_STRING))
        ((absolute_trait_method_reference)))
       (absolute_trait_method_reference
        ((name T_PAAMAYIM_NEKUDOTAYIM T_STRING)
         (concat $1 $2 $3)))
       (trait_modifiers
        (nil)
        ((member_modifier)))
       (method_body
        ((SEMICOLON)
         nil)
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'inner_statement_list_brace_block
          (or nil 1))))
       (variable_modifiers
        ((non_empty_member_modifiers))
        ((T_VAR)
         (identity "public")))
       (method_modifiers
        (nil)
        ((non_empty_member_modifiers)))
       (non_empty_member_modifiers
        ((member_modifier)
         (list $1))
        ((non_empty_member_modifiers member_modifier)
         (append $1
                 (list $2))))
       (member_modifier
        ((T_PUBLIC))
        ((T_PROTECTED))
        ((T_PRIVATE))
        ((T_STATIC))
        ((T_ABSTRACT))
        ((T_FINAL)))
       (property_list
        ((property_list COMMA property)
         (append $1 $3))
        ((property)))
       (property
        ((T_VARIABLE)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $1 nil nil))))
        ((T_VARIABLE ASSIGN expr)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $1 nil nil)))))
       (class_const_list
        ((class_const_list COMMA const_decl)
         (append $1
                 (wisent-cook-tag $3)))
        ((const_decl)
         (wisent-cook-tag $1)))
       (const_decl
        ((T_STRING ASSIGN expr)
         (wisent-raw-tag
          (semantic-tag $1 'constant))))
       (echo_expr_list
        ((echo_expr_list COMMA echo_expr)
         (append $1 $3))
        ((echo_expr)))
       (echo_expr
        ((expr)))
       (for_exprs_paren_block
        ((LPAREN for_exprs SEMICOLON for_exprs SEMICOLON for_exprs RPAREN)
         (append $2 $4 $6)))
       (for_exprs
        (nil)
        ((non_empty_for_exprs)))
       (non_empty_for_exprs
        ((non_empty_for_exprs COMMA expr)
         (append $1 $3))
        ((expr)))
       (new_expr
        ((T_NEW class_name_reference ctor_arguments)
         nil))
       (expr_without_variable
        ((T_LIST PAREN_BLOCK ASSIGN expr)
         (append
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'assignment_list_elements
           (or nil 1))
          $4))
        ((variable ASSIGN expr)
         (progn
           (if
               (and
                (semantic-tag-p
                 (car $3))
                (equal "new"
                       (semantic-tag-code-detail
                        (car $3)))
                (stringp
                 (semantic-tag-name
                  (car $3))))
               (semantic-tag-put-attribute
                (car $1)
                :type
                (semantic-tag-name
                 (car $3))))
           (append $1 $3)))
        ((variable ASSIGN AMPERSAND variable)
         (append $1 $4))
        ((T_CLONE expr)
         (identity $2))
        ((variable T_PLUS_EQUAL expr)
         (append $1 $3))
        ((variable T_MINUS_EQUAL expr)
         (append $1 $3))
        ((variable T_MUL_EQUAL expr)
         (append $1 $3))
        ((variable T_POW_EQUAL expr)
         (append $1 $3))
        ((variable T_DIV_EQUAL expr)
         (append $1 $3))
        ((variable T_CONCAT_EQUAL expr)
         (append $1 $3))
        ((variable T_MOD_EQUAL expr)
         (append $1 $3))
        ((variable T_AND_EQUAL expr)
         (append $1 $3))
        ((variable T_OR_EQUAL expr)
         (append $1 $3))
        ((variable T_XOR_EQUAL expr)
         (append $1 $3))
        ((variable T_SL_EQUAL expr)
         (append $1 $3))
        ((variable T_SR_EQUAL expr)
         (append $1 $3))
        ((variable T_INC))
        ((T_INC variable)
         (identity $2))
        ((variable T_DEC))
        ((T_DEC variable)
         (identity $2))
        ((expr T_BOOLEAN_OR expr)
         (append $1 $3))
        ((expr T_BOOLEAN_AND expr)
         (append $1 $3))
        ((expr T_LOGICAL_OR expr)
         (append $1 $3))
        ((expr T_LOGICAL_AND expr)
         (append $1 $3))
        ((expr T_LOGICAL_XOR expr)
         (append $1 $3))
        ((expr OR expr)
         (append $1 $3))
        ((expr AMPERSAND expr)
         (append $1 $3))
        ((expr XOR expr)
         (append $1 $3))
        ((expr DOT expr)
         (append $1 $3))
        ((expr PLUS expr)
         (append $1 $3))
        ((expr MINUS expr)
         (append $1 $3))
        ((expr MULTIPLY expr)
         (append $1 $3))
        ((expr T_POW expr)
         (append $1 $3))
        ((expr DIVIDE expr)
         (append $1 $3))
        ((expr MODULO expr)
         (append $1 $3))
        ((expr T_SL expr)
         (append $1 $3))
        ((expr T_SR expr)
         (append $1 $3))
        ((PLUS expr)
         [T_INC]
         (identity $2))
        ((MINUS expr)
         [T_INC]
         (identity $2))
        ((NEGATE expr)
         (identity $2))
        ((BITNOT expr)
         (identity $2))
        ((expr T_IS_IDENTICAL expr)
         (append $1 $3))
        ((expr T_IS_NOT_IDENTICAL expr)
         (append $1 $3))
        ((expr T_IS_EQUAL expr)
         (append $1 $3))
        ((expr T_IS_NOT_EQUAL expr)
         (append $1 $3))
        ((expr LT expr)
         (append $1 $3))
        ((expr T_IS_SMALLER_OR_EQUAL expr)
         (append $1 $3))
        ((expr GT expr)
         (append $1 $3))
        ((expr T_IS_GREATER_OR_EQUAL expr)
         (append $1 $3))
        ((expr T_SPACESHIP expr)
         (append $1 $3))
        ((expr T_INSTANCEOF class_name_reference))
        ((PAREN_BLOCK)
         nil)
        ((new_expr))
        ((expr QUESTION expr COLON expr)
         (append $1 $3 $5))
        ((expr QUESTION COLON expr)
         (append $1 $4))
        ((expr T_COALESCE expr)
         (append $1 $3))
        ((internal_functions_in_yacc))
        ((T_INT_CAST expr)
         (identity $2))
        ((T_DOUBLE_CAST expr)
         (identity $2))
        ((T_STRING_CAST expr)
         (identity $2))
        ((T_ARRAY_CAST expr)
         (identity $2))
        ((T_OBJECT_CAST expr)
         (identity $2))
        ((T_BOOL_CAST expr)
         (identity $2))
        ((T_UNSET_CAST expr)
         (identity $2))
        ((T_EXIT exit_expr)
         (identity $2))
        ((AT expr)
         (identity $2))
        ((scalar))
        ((BACKQUOTE backticks_expr BACKQUOTE)
         (identity $2))
        ((T_PRINT expr)
         (identity $2))
        ((T_YIELD))
        ((T_YIELD expr)
         (identity $2))
        ((T_YIELD expr T_DOUBLE_ARROW expr)
         (append $2 $4))
        ((function returns_ref PAREN_BLOCK lexical_vars return_type BRACE_BLOCK)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function "closure" $5
                                      (semantic-parse-region
                                       (car $region3)
                                       (cdr $region3)
                                       'parameter_list
                                       (or nil 1))
                                      :lexical-scope $4 :members
                                      (semantic-parse-region
                                       (car $region6)
                                       (cdr $region6)
                                       'inner_statement_list
                                       (or nil 1))))))
        ((T_STATIC function returns_ref PAREN_BLOCK lexical_vars return_type BRACE_BLOCK)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-function "closure" $6
                                      (semantic-parse-region
                                       (car $region4)
                                       (cdr $region4)
                                       'parameter_list
                                       (or nil 1))
                                      :lexical-scope $5 :members
                                      (semantic-parse-region
                                       (car $region7)
                                       (cdr $region7)
                                       'inner_statement_list
                                       (or nil 1)))))))
       #'((T_FUNCTION))
       (returns_ref
        (nil)
        ((AMPERSAND)))
       (lexical_vars
        (nil)
        ((T_USE PAREN_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'lexical_var_list
          (or nil 1))))
       (lexical_var_list
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((COMMA)
         nil)
        ((lexical_var)))
       (lexical_var
        ((T_VARIABLE)
         (wisent-raw-tag
          (semantic-tag-new-variable $1 nil nil)))
        ((AMPERSAND T_VARIABLE)
         (wisent-raw-tag
          (semantic-tag-new-variable $2 nil nil :typemodifiers
                                     (list $1)))))
       (function_call
        ((name argument_list)
         nil)
        ((class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list)
         nil)
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list)
         nil)
        ((callable_expr argument_list)
         nil))
       (class_name
        ((T_STATIC))
        ((name)))
       (class_name_reference
        ((class_name))
        ((new_variable)))
       (exit_expr
        (nil)
        ((PAREN_BLOCK)
         nil))
       (backticks_expr
        (nil)
        ((T_ENCAPSED_AND_WHITESPACE)
         nil)
        ((encaps_list)
         nil))
       (ctor_arguments
        (nil)
        ((argument_list)))
       (dereferencable_scalar
        ((T_ARRAY PAREN_BLOCK)
         nil)
        ((BRACK_BLOCK)
         nil)
        ((T_CONSTANT_ENCAPSED_STRING)
         nil))
       (scalar
        ((T_LNUMBER)
         nil)
        ((T_DNUMBER)
         nil)
        ((T_LINE)
         nil)
        ((T_FILE)
         nil)
        ((T_DIR)
         nil)
        ((T_TRAIT_C)
         nil)
        ((T_METHOD_C)
         nil)
        ((T_FUNC_C)
         nil)
        ((T_NS_C)
         nil)
        ((T_CLASS_C)
         nil)
        ((T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC)
         nil)
        ((T_START_HEREDOC T_END_HEREDOC)
         nil)
        ((DOUBLEQUOTE encaps_list DOUBLEQUOTE)
         (identity $2))
        ((T_START_HEREDOC encaps_list T_END_HEREDOC)
         (identity $2))
        ((dereferencable_scalar)
         nil)
        ((class_name_scalar)
         nil)
        ((constant)))
       (constant
        ((name)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag $1 'constant))))
        ((class_name T_PAAMAYIM_NEKUDOTAYIM T_STRING)
         nil)
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM T_STRING)
         (identity $1)))
       (possible_comma
        (nil)
        ((COMMA)))
       (expr
        ((variable))
        ((expr_without_variable)))
       (expr_paren_block
        ((LPAREN expr RPAREN)
         (identity $2)))
       (expr_brace_block
        ((LBRACE expr RBRACE)
         (identity $2)))
       (optional_expr
        (nil)
        ((expr)))
       (optional_expr_paren_block
        ((LPAREN optional_expr RPAREN)
         (identity $2)))
       (optional_expr_brack_block
        ((LBRACK optional_expr RBRACK)
         (identity $2)))
       (variable_class_name
        ((dereferencable)
         nil))
       (dereferencable
        ((variable))
        ((PAREN_BLOCK)
         nil)
        ((dereferencable_scalar)
         nil))
       (callable_expr
        ((callable_variable))
        ((PAREN_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'expr
          (or nil 1)))
        ((PAREN_BLOCK)
         nil)
        ((dereferencable_scalar)
         nil))
       (callable_variable
        ((simple_variable))
        ((dereferencable BRACK_BLOCK)
         (append $1
                 (semantic-parse-region
                  (car $region2)
                  (cdr $region2)
                  'optional_expr
                  (or nil 1))))
        ((constant BRACK_BLOCK)
         (append $1
                 (semantic-parse-region
                  (car $region2)
                  (cdr $region2)
                  'optional_expr
                  (or nil 1))))
        ((dereferencable BRACE_BLOCK)
         (append $1
                 (semantic-parse-region
                  (car $region2)
                  (cdr $region2)
                  'expr
                  (or nil 1))))
        ((dereferencable T_OBJECT_OPERATOR member_name argument_list)
         (append $1 $4))
        ((function_call)
         nil))
       (variable
        ((callable_variable))
        ((static_member)
         nil)
        ((dereferencable T_OBJECT_OPERATOR member_name)
         nil))
       (variable_brace_block
        ((LBRACE variable RBRACE)
         (identity $2)))
       (simple_variable
        ((T_VARIABLE)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $1 nil nil))))
        ((DOLLAR BRACE_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'expr_brace_block
          (or nil 1)))
        ((DOLLAR simple_variable)
         (identity $2)))
       (static_member
        ((class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         nil)
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         nil))
       (new_variable
        ((simple_variable))
        ((new_variable BRACK_BLOCK))
        ((new_variable BRACE_BLOCK))
        ((new_variable T_OBJECT_OPERATOR member_name))
        ((class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         nil)
        ((new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable)))
       (member_name
        ((T_STRING))
        ((BRACE_BLOCK)
         nil)
        ((simple_variable)))
       (member_name_brace_block
        ((LBRACE member_name RBRACE)
         (identity $2)))
       (assignment_list_elements
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((assignment_list_element COMMA))
        ((assignment_list_element RPAREN)))
       (assignment_list_element
        ((variable))
        ((T_LIST PAREN_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'assignment_list_elements
          (or nil 1))))
       (array_pair_list_paren_block
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((COMMA)
         nil)
        ((array_pair)))
       (array_pair_list_brack_block
        ((LBRACK)
         nil)
        ((RBRACK)
         nil)
        ((COMMA)
         nil)
        ((array_pair)))
       (array_pair
        ((expr T_DOUBLE_ARROW expr)
         (append $1 $3))
        ((expr))
        ((expr T_DOUBLE_ARROW AMPERSAND variable)
         (append $1 $4))
        ((AMPERSAND variable)
         (identity $2)))
       (encaps_list
        ((encaps_list encaps_var)
         (append $1 $2))
        ((encaps_list T_ENCAPSED_AND_WHITESPACE))
        ((encaps_var))
        ((T_ENCAPSED_AND_WHITESPACE encaps_var)
         (identity $2)))
       (encaps_var
        ((T_VARIABLE)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $1 nil nil))))
        ((T_VARIABLE BRACK_BLOCK)
         (append
          (wisent-cook-tag
           (wisent-raw-tag
            (semantic-tag-new-variable $1 nil nil)))
          (semantic-parse-region
           (car $region2)
           (cdr $region2)
           'encaps_var_offset_brack_block
           (or nil 1))))
        ((T_VARIABLE T_OBJECT_OPERATOR T_STRING)
         (wisent-cook-tag
          (wisent-raw-tag
           (semantic-tag-new-variable $1 nil nil))))
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'variable_brace_block
          (or nil 1))))
       (encaps_var_offset_brack_block
        ((LBRACK encaps_var_offset RBRACK)
         (identity $2)))
       (encaps_var_offset
        ((T_STRING)
         nil)
        ((T_LNUMBER)
         nil)
        ((T_VARIABLE)
         (wisent-raw-tag
          (semantic-tag-new-variable $1 nil nil))))
       (internal_functions_in_yacc
        ((T_ISSET PAREN_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'isset_variables
          (or nil 1)))
        ((T_EMPTY PAREN_BLOCK)
         (semantic-parse-region
          (car $region2)
          (cdr $region2)
          'expr
          (or nil 1)))
        ((T_INCLUDE expr)
         nil)
        ((T_INCLUDE_ONCE expr)
         nil)
        ((T_EVAL PAREN_BLOCK)
         nil)
        ((T_REQUIRE expr)
         nil)
        ((T_REQUIRE_ONCE expr)
         nil))
       (isset_variables
        ((RPAREN)
         nil)
        ((LPAREN isset_variable)
         (identity $2))
        ((COMMA isset_variable)
         (identity $2)))
       (isset_variable
        ((expr)))
       (class_name_scalar
        ((class_name T_PAAMAYIM_NEKUDOTAYIM T_CLASS)
         (concat $1 $2 $3))))
     '(top_statement_start assignment_list_elements case_list_brace_block catch_list_name_paren_block class_statement_list const_list_paren_block encaps_var_offset_brack_block expr expr_brace_block expr_paren_block expr_without_variable for_exprs_paren_block foreach_paren_block inline_use_declarations_brace_block inner_statement_list_brace_block isset_variables lexical_var_list non_empty_argument_list optional_expr parameter_list top_statement_list trait_adaptations_brace_block unset_variables use_declarations_brace_block variable_brace_block class_const_list const_list constant encaps_var inner_statement_list property simple_variable static_var top_statement)))
  "Parser table.")

(defun wisent-php--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table wisent-php--parse-table
        semantic-debug-parser-source "wisent-php.wy"
        semantic-flex-keywords-obarray wisent-php--keyword-table
        semantic-lex-types-obarray wisent-php--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-sexp-type-analyzer wisent-php--<quoted-string>-sexp-analyzer
  "sexp analyzer for <quoted-string> tokens."
  "\\s\""
  'T_CONSTANT_ENCAPSED_STRING)

(define-lex-regex-type-analyzer wisent-php--<variable>-regexp-analyzer
  "regexp analyzer for <variable> tokens."
  "\\([$][a-zA-Z_]+[a-zA-Z0-9_]*\\)"
  nil
  'T_VARIABLE)

(define-lex-block-type-analyzer wisent-php--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACK))
  )

(define-lex-string-type-analyzer wisent-php--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\|[$]\\|[\\]\\)+"
  '((T_POW_EQUAL . "**=")
    (T_POW . "**")
    (T_COALESCE . "??")
    (T_ELLIPSIS . "...")
    (T_NS_SEPARATOR . "\\")
    (T_PAAMAYIM_NEKUDOTAYIM . "::")
    (T_CURLY_OPEN . "{$")
    (T_DOLLAR_OPEN_CURLY_BRACES . "${")
    (T_DOUBLE_ARROW . "=>")
    (T_OBJECT_OPERATOR . "->")
    (T_DEC . "--")
    (T_INC . "++")
    (T_SR . ">>")
    (T_SL . "<<")
    (T_SPACESHIP . "<=>")
    (T_IS_GREATER_OR_EQUAL . ">=")
    (T_IS_SMALLER_OR_EQUAL . "<=")
    (T_IS_NOT_IDENTICAL . "!==")
    (T_IS_IDENTICAL . "===")
    (T_IS_NOT_EQUAL . "!=")
    (T_IS_EQUAL . "==")
    (T_BOOLEAN_AND . "&&")
    (T_BOOLEAN_OR . "||")
    (T_SR_EQUAL . ">>=")
    (T_SL_EQUAL . "<<=")
    (T_XOR_EQUAL . "^=")
    (T_OR_EQUAL . "|=")
    (T_AND_EQUAL . "&=")
    (T_MOD_EQUAL . "%=")
    (T_CONCAT_EQUAL . ".=")
    (T_DIV_EQUAL . "/=")
    (T_MUL_EQUAL . "*=")
    (T_MINUS_EQUAL . "-=")
    (T_PLUS_EQUAL . "+=")
    (XOR . "^")
    (OR . "|")
    (BITNOT . "~")
    (GT . ">")
    (LT . "<")
    (DIVIDE . "/")
    (MULTIPLY . "*")
    (MODULO . "%")
    (MINUS . "-")
    (PLUS . "+")
    (NEGATE . "!")
    (QUESTION . "?")
    (DOUBLEQUOTE . "\"")
    (BACKQUOTE . "`")
    (AMPERSAND . "&")
    (ASSIGN . "=")
    (AT . "@")
    (DOT . ".")
    (COMMA . ",")
    (COLON . ":")
    (SEMICOLON . ";")
    (DOLLAR . "$"))
  'punctuation)

(define-lex-regex-type-analyzer wisent-php--<mb>-regexp-analyzer
  "regexp analyzer for <mb> tokens."
  "[[:nonascii:]]+"
  nil
  'mbstring)

(define-lex-regex-type-analyzer wisent-php--<string>-regexp-analyzer
  "regexp analyzer for <string> tokens."
  "\\<\\([a-zA-Z0-9_]\\)+\\>"
  nil
  'T_STRING)

(define-lex-keyword-type-analyzer wisent-php--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
;;;
;;; Action helpers
;;;
(defun wisent-php-normalize-qualified-name (name)
  "Return NAME without leading backslash."
  (if (equal "\\" (substring name 0 1))
      (substring name 1)
    name))

;;;
;;; Define lexical analyzers
;;;
(define-lex-simple-regex-analyzer wisent-php-integer-analyzer
  "Regular expression matching integer literals in PHP.

Known limitation: PHP recognizes an integer literals as floating-point
when the represented value exceeds the capacity of the system's
integer type.

The expression tries to accurately match T_LNUMBER so this looks
different from semantic-lex-number-expression. See the '>' sections
in below grammar to see how the expression is contrived.

  integer-literal::
    decimal-literal
    octal-literal
    hexadecimal-literal
    binary-literal

    decimal-literal::
      nonzero-digit
      decimal-literal   digit
    > [1-9][0-9]*

    octal-literal::
      0
      octal-literal   octal-digit
    > 0[0-7]+

    hexadecimal-literal::
      hexadecimal-prefix   hexadecimal-digit
      hexadecimal-literal   hexadecimal-digit

    hexadecimal-prefix:: one of
      0x  0X
    > \\(0x\\|0X\\)[0-9a-fA-F]+

    binary-literal::
      binary-prefix   binary-digit
      binary-literal   binary-digit

    binary-prefix:: one of
      0b  0B
    > \\(0b\\|0B\\)[01]+

    digit:: one of
      0  1  2  3  4  5  6  7  8  9
    > [0-9]+

    nonzero-digit:: one of
      1  2  3  4  5  6  7  8  9
    > [1-9]+

    octal-digit:: one of
      0  1  2  3  4  5  6  7
    > [1-7]+

    hexadecimal-digit:: one of
      0  1  2  3  4  5  6  7  8  9
      a  b  c  d  e  f
      A  B  C  D  E  F
    > [0-9a-zA-Z]+

    binary-digit:: one of
        0  1
    > [01]+"
    (concat
     "\\<\\("
              "[1-9][0-9]*"
     "\\|"    "0[0-7]+"
     "\\|"    "\\(0x\\|0X\\)[0-9a-fA-F]+"
     "\\|"    "\\(0b\\|0B\\)[01]+"
     "\\|"    "0+"
     "\\)\\>")
  'T_LNUMBER)

(define-lex-simple-regex-analyzer wisent-php-float-analyzer
  "Regular expression matching floating-point literals in PHP.

The expression tries to accurately match T_DNUMBER so this looks
different from semantic-lex-number-expression. See the '>' sections
in below grammar to see how the expression is contrived.

  floating-literal::
    fractional-literal   exponent-partopt
    digit-sequence   exponent-part
  > \\([0-9]*[.][0-9]+\\|[0-9]+[.]\\)\\(?:[eE][-+]*[0-9]+\\)
  > [0-9]+[eE][-+]*[0-9]+

  fractional-literal::
    digit-sequenceopt . digit-sequence
    digit-sequence .
  > [0-9]*[.][0-9]+
  > [0-9]+[.]

  exponent-part::
    e  signopt   digit-sequence
    E  signopt   digit-sequence
  > [eE][-+]*[0-9]+

  sign:: one of
    +  -
  > [-+]

  digit-sequence::
    digit
    digit-sequence   digit
  > [0-9]+"
  (concat
   "\\(" "\\([0-9]*[.][0-9]+\\|[0-9]+[.]\\)\\(?:[eE][-+]*[0-9]+\\)"
     "\\|" "[0-9]+[eE][-+]*[0-9]+"
     "\\|" "[0-9]*[.][0-9]+"
     "\\|" "[0-9]+[.]"
     "\\)")
  'T_DNUMBER)

(define-lex-analyzer wisent-php-cast-analyzer
  "Analyzer matching T_*_CAST tokens."
  (looking-at "(\\(int\\|double\\|string\\|array\\|object\\|bool\\|unset\\))")
  (semantic-lex-push-token
   (semantic-lex-token (intern (concat "T_" (upcase (match-string 1)) "_CAST"))
                       (match-beginning 0) (match-end 0))))

(define-lex-regex-analyzer wisent-php-encapsed-and-whitespace-analyzer
  "Analyzer matching T_ENCAPSED_AND_WHITESPACE between variable tokens inside strings.

This matches everything except interpolated variables. Adjacent tokens are merged."
  "\\(.*\\)[$][a-zA-Z_]"
  (if (eq (semantic-lex-token-class (car semantic-lex-token-stream))
          'T_ENCAPSED_AND_WHITESPACE)
      ;; Merge the token with previous token if it's the same class,
      ;; this analyzer does not scan multiple lines.
      (let ((bounds-end (match-end 1)))
        (setq semantic-lex-end-point bounds-end)
        (setcdr (semantic-lex-token-bounds (car semantic-lex-token-stream)) bounds-end))
    (semantic-lex-push-token
     (semantic-lex-token
      'T_ENCAPSED_AND_WHITESPACE (match-beginning 1) (match-end 1)))))

(define-lex-regex-analyzer wisent-php-encapsed-and-whitespace-analyzer-2
  "Regular expression matching T_ENCAPSED_AND_WHITESPACE to the end of the line.

This analyzer must always run after
`wisent-php-encapsed-and-whitespace-analyzer' and exists to match the
encapsed token to the end of string to prevent unmatched
syntax. Adjacent tokens are merged."
  ".+"
  ;; Language wants whitespaces.  Create a token for it.
  (if (eq (semantic-lex-token-class (car semantic-lex-token-stream))
          'T_ENCAPSED_AND_WHITESPACE)
      ;; Merge the token with previous token if it's the same class,
      ;; this analyzer does not scan multiple lines.
      (let ((bounds-end (match-end 0)))
        (setq semantic-lex-end-point bounds-end)
        (setcdr (semantic-lex-token-bounds (car semantic-lex-token-stream)) bounds-end))
    (semantic-lex-push-token
     (semantic-lex-token
      'T_ENCAPSED_AND_WHITESPACE (match-beginning 0) (match-end 0)))))

(define-lex-analyzer wisent-php-open-close-tag-analyzer
  "Detect and create tokens for open/close tags"
  (looking-at (concat
               "\\(" "<[?%]"    ;; T_OPEN_TAG
               "\\|" "<[?%]php" ;; T_OPEN_TAG (non-short)
               "\\|" "<[?%]="   ;; T_OPEN_TAG_WITH_ECHO
               "\\|" "[?%]>"    ;; T_CLOSE_TAG
               "\\)"))
  (cond ((looking-at "\\(<[?%]=\\)")
         (semantic-lex-push-token
          (semantic-lex-token 'T_OPEN_TAG_WITH_ECHO
                              (point) (match-end 0))))
        ((looking-at "\\(<[?%]php\\)")
         (semantic-lex-push-token
          (semantic-lex-token 'T_OPEN_TAG
                              (point) (match-end 0))))
        ((looking-at "\\(<[?%]\\)")
         (semantic-lex-push-token
          (semantic-lex-token 'T_OPEN_TAG
                              (point) (match-end 0))))
        ((looking-at "\\([?%]>\\)")
         (semantic-lex-push-token
          (semantic-lex-token 'T_CLOSE_TAG
                              (point) (match-end 0))))))

(define-lex-analyzer wisent-php-inline-html-analyzer
  "Detect and create tokens for inline HTML"
  ;; If at the beginning of buffer or,
  ;; the last token in the stream is a close tag.
  (or (= 1 semantic-lex-end-point)
      (equal 'T_CLOSE_TAG (semantic-lex-token-class (car semantic-lex-token-stream))))
  (semantic-lex-push-token
   (semantic-lex-token 'T_INLINE_HTML
                       (point)
                       ;; The inline HML ends before the next close tag
                       ;; Or else, the end of the buffer.
                       (save-excursion
                         (if (search-forward-regexp "\\(<[?%]\\)" nil t)
                             (match-beginning 0)
                           (point-max))))))

(define-lex-analyzer wisent-php-nowdoc-analyzer
  "Detect and create tokens for nowdocs"
  (looking-at "<<<'\\(\\(\\sw\\|\\s_\\)+\\)'")
  (let ((startend (match-end 0))
        (docname (match-string 1)))
    ;; Match start of nowdoc
    (semantic-lex-push-token
     (semantic-lex-token 'T_START_HEREDOC
                         (point)
                         startend))
    ;; Match body of nowdoc
    (if (search-forward-regexp (concat "^\\(" docname "\\)\\($\\|;\\)") nil t)
        (progn
          (semantic-lex-push-token
           (semantic-lex-token 'T_ENCAPSED_AND_WHITESPACE
                               (1+ startend)
                               (1- (match-beginning 1))))
          ;; Emit end of nowdoc
          (semantic-lex-push-token
           (semantic-lex-token 'T_END_HEREDOC
                               (match-beginning 1)
                               (match-end 1))))
      ;; Could not find end of nowdoc, emit a phony end tag
      (semantic-lex-push-token
       (semantic-lex-token 'T_END_HEREDOC startend startend)))))

(define-lex-analyzer wisent-php-heredoc-analyzer
  "Detect and create tokens for heredocs"
  (looking-at "<<<\\(\\(\\sw\\|\\s_\\)+\\)")
  (let ((docname (match-string 1))      ;; Name of the heredoc (NAME in <<<NAME)
        (start-beginning (point))       ;; Beginning of the start token
        (start-end (match-end 0))       ;; End of the start token
        end-beginning                   ;; Beginning of the end tokencond
        end-end)                        ;; End of the end token

    ;; Emit start of heredoc
    (semantic-lex-push-token
     (semantic-lex-token 'T_START_HEREDOC start-beginning start-end))

    ;; Find end of heredoc
    (if (search-forward-regexp (concat "^\\(" docname "\\)\\($\\|;\\)") nil t)
        ;; End of heredoc found, save the position and emit the end
        ;; token alter
        (progn
          (setq end-beginning (match-beginning 1)
                end-end (match-end 1))

          (dolist (token (funcall 'wisent-php-string-lexer (1+ start-end) (1- end-beginning)))
            (semantic-lex-push-token token)))

      ;; No heredoc end found, emit a phony end token
      (setq end-beginning start-end
            end-end start-end))

    ;; Emit end of heredoc
    (semantic-lex-push-token
     (semantic-lex-token 'T_END_HEREDOC end-beginning end-end))))


;;;
;;; Define the lexer
;;;
(define-lex wisent-php-lexer
  "Lexical analyzer that handles PHP buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments

  wisent-php-open-close-tag-analyzer
  wisent-php-inline-html-analyzer
  wisent-php-float-analyzer
  wisent-php-integer-analyzer
  wisent-php-cast-analyzer
  wisent-php-nowdoc-analyzer
  wisent-php-heredoc-analyzer

  wisent-php--<variable>-regexp-analyzer
  wisent-php--<punctuation>-string-analyzer
  wisent-php--<keyword>-keyword-analyzer
  wisent-php--<block>-block-analyzer
  wisent-php--<string>-regexp-analyzer
  wisent-php--<quoted-string>-sexp-analyzer
  wisent-php--<mb>-regexp-analyzer

  semantic-lex-default-action)

(define-lex wisent-php-string-lexer
  "Lexical analyzer that handles PHP heredoc contents."

  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline

  wisent-php--<variable>-regexp-analyzer
  wisent-php-encapsed-and-whitespace-analyzer
  wisent-php-encapsed-and-whitespace-analyzer-2

  semantic-lex-default-action)

(provide 'edep/wisent-php)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; wisent-php.el ends here
