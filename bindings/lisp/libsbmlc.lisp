;; -*- model:common-lisp package: LIBSBMLC -*-
#|
\file    libsbmlc.lisp
\brief
\author  Martin Ginkel <mginkel@mpi-mageburg.mpg.de>

$Id$
$Source$

Copyright 2004 Max-Planck-Institute Magdeburg

This is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or
any later version.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
|#


(cl::in-package :cl-user)

;(cl:load "uffi:;uffi-config.lisp")
;(cl:load "uffi:uffi.asd")
(asdf:operate 'asdf:load-op :uffi)

(unless (cl:find-package "LIBSBMLC")
  (cl::defpackage "LIBSBMLC" (:use )))
(cl::in-package "LIBSBMLC")

(uffi:def-function ("free" uffi::c-free)
    ((o :pointer-void))
  :returning :void)

(cl:defmacro uffi::lboolm (x)
  `(cl:if (cl:> (cl:the cl:fixnum ,x) 0) cl:t cl:nil))
(cl:defun uffi::lbool (x)
  (uffi::lboolm x))

(cl:defmacro uffi::cboolm (x)
  `(cl:if ,x 1 0))
(cl:defun uffi::cbool (x)
  (uffi::cboolm x))

(uffi:load-foreign-library (cl:translate-logical-pathname "libsbml:;libsbml.so"))

;;struct{int dummy} FILE
;; enum{sbml_compartment, sbml_document, sbml_event, sbml_event_assignment, sbml_function_definition, sbml_kinetic_law, sbml_list_of, sbml_model, sbml_parameter, sbml_reaction, sbml_species, sbml_species_reference, sbml_modifier_species_reference, sbml_unit_definition, sbml_unit, sbml_algebraic_rule, sbml_assignment_rule, sbml_rate_rule, sbml_species_concentration_rule, sbml_compartment_volume_rule, sbml_parameter_rule} SBMLTypeCode_t

(uffi:def-foreign-type sbmltype-code-t :int) 
(common-lisp:defconstant +sbml-compartment+ 0) 
(common-lisp:defconstant +sbml-document+ 1) 
(common-lisp:defconstant +sbml-event+ 2) 
(common-lisp:defconstant +sbml-event-assignment+ 3) 
(common-lisp:defconstant +sbml-function-definition+ 4) 
(common-lisp:defconstant +sbml-kinetic-law+ 5) 
(common-lisp:defconstant +sbml-list-of+ 6) 
(common-lisp:defconstant +sbml-model+ 7) 
(common-lisp:defconstant +sbml-parameter+ 8) 
(common-lisp:defconstant +sbml-reaction+ 9) 
(common-lisp:defconstant +sbml-species+ 10) 
(common-lisp:defconstant +sbml-species-reference+ 11) 
(common-lisp:defconstant +sbml-modifier-species-reference+ 12) 
(common-lisp:defconstant +sbml-unit-definition+ 13) 
(common-lisp:defconstant +sbml-unit+ 14) 
(common-lisp:defconstant +sbml-algebraic-rule+ 15) 
(common-lisp:defconstant +sbml-assignment-rule+ 16) 
(common-lisp:defconstant +sbml-rate-rule+ 17) 
(common-lisp:defconstant +sbml-species-concentration-rule+ 18) 
(common-lisp:defconstant +sbml-compartment-volume-rule+ 19) 
(common-lisp:defconstant +sbml-parameter-rule+ 20) 
;;void SBase_t
;; Function void SBase_init(void* sb, SBMLTypeCode_t tc)
(uffi:def-function ("SBase_init" sbase-init)
                   ((sb :pointer-void) (tc :int))
                   :returning
                   :void) 

;; Function void SBase_clear(void* sb)
(uffi:def-function ("SBase_clear" sbase-clear)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function SBMLTypeCode_t SBase_getTypeCode(void* sb)
(uffi:def-function ("SBase_getTypeCode" sbase-get-type-code)
                   ((sb :pointer-void))
                   :returning
                   :int) 

;; Function unsigned int SBase_getColumn(void* sb)
(uffi:def-function ("SBase_getColumn" sbase-get-column)
                   ((sb :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int SBase_getLine(void* sb)
(uffi:def-function ("SBase_getLine" sbase-get-line)
                   ((sb :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function char* SBase_getMetaId(void* sb)
(uffi:def-function ("SBase_getMetaId" sbase-get-meta-id1952)
                   ((sb :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun sbase-get-meta-id (sb)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (sbase-get-meta-id1952 sb))
      common-lisp:nil))) 

;; Function char* SBase_getNotes(void* sb)
(uffi:def-function ("SBase_getNotes" sbase-get-notes1953)
                   ((sb :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun sbase-get-notes (sb)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (sbase-get-notes1953 sb))
      common-lisp:nil))) 

;; Function char* SBase_getAnnotation(void* sb)
(uffi:def-function ("SBase_getAnnotation" sbase-get-annotation1954)
                   ((sb :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun sbase-get-annotation (sb)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (sbase-get-annotation1954 sb))
      common-lisp:nil))) 

;; Function int SBase_isSetMetaId(void* sb)
(uffi:def-function ("SBase_isSetMetaId" sbase-is-set-meta-id1955)
                   ((sb :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun sbase-is-set-meta-id (sb)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (sbase-is-set-meta-id1955 sb))
      common-lisp:nil))) 

;; Function int SBase_isSetNotes(void* sb)
(uffi:def-function ("SBase_isSetNotes" sbase-is-set-notes)
                   ((sb :pointer-void))
                   :returning
                   :int) 

;; Function int SBase_isSetAnnotation(void* sb)
(uffi:def-function ("SBase_isSetAnnotation" sbase-is-set-annotation)
                   ((sb :pointer-void))
                   :returning
                   :int) 

;; Function void SBase_setMetaId(void* sb, char* metaid)
(uffi:def-function ("SBase_setMetaId" sbase-set-meta-id1956)
                   ((sb :pointer-void) (metaid :cstring))
                   :returning
                   :void) 
(common-lisp:defun sbase-set-meta-id (sb metaid)
  (common-lisp:setq metaid (uffi:convert-to-cstring metaid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbase-set-meta-id1956 sb metaid)
      common-lisp:nil
      (uffi:free-cstring metaid)))) 

;; Function void SBase_setNotes(void* sb, char* notes)
(uffi:def-function ("SBase_setNotes" sbase-set-notes1957)
                   ((sb :pointer-void) (notes :cstring))
                   :returning
                   :void) 
(common-lisp:defun sbase-set-notes (sb notes)
  (common-lisp:setq notes (uffi:convert-to-cstring notes))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbase-set-notes1957 sb notes)
      common-lisp:nil
      (uffi:free-cstring notes)))) 

;; Function void SBase_setAnnotation(void* sb, char* annotation)
(uffi:def-function ("SBase_setAnnotation" sbase-set-annotation1958)
                   ((sb :pointer-void) (annotation :cstring))
                   :returning
                   :void) 
(common-lisp:defun sbase-set-annotation (sb annotation)
  (common-lisp:setq annotation (uffi:convert-to-cstring annotation))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbase-set-annotation1958 sb annotation)
      common-lisp:nil
      (uffi:free-cstring annotation)))) 

;; Function void SBase_unsetMetaId(void* sb)
(uffi:def-function ("SBase_unsetMetaId" sbase-unset-meta-id)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function void SBase_unsetNotes(void* sb)
(uffi:def-function ("SBase_unsetNotes" sbase-unset-notes)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function void SBase_unsetAnnotation(void* sb)
(uffi:def-function ("SBase_unsetAnnotation" sbase-unset-annotation)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;;void ListNode_t
;;void List_t
;;function* ListItemComparator
;;function* ListItemPredicate
;; Function void* List_create()
(uffi:def-function ("List_create" list-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* ListNode_create(void* item)
(uffi:def-function ("ListNode_create" list-node-create)
                   ((item :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void List_free(void* lst)
(uffi:def-function ("List_free" list-free)
                   ((lst :pointer-void))
                   :returning
                   :void) 

;; Function void ListNode_free(void* node)
(uffi:def-function ("ListNode_free" list-node-free)
                   ((node :pointer-void))
                   :returning
                   :void) 

;; Function void List_add(void* lst, void* item)
(uffi:def-function ("List_add" list-add)
                   ((lst :pointer-void) (item :pointer-void))
                   :returning
                   :void) 

;; Function unsigned int List_countIf(void* lst, ListItemPredicate predicate)
(uffi:def-function ("List_countIf" list-count-if)
                   ((lst :pointer-void) (predicate :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void* List_find(void* lst, void* item1, ListItemComparator comparator)
(uffi:def-function ("List_find" list-find)
                   ((lst :pointer-void) (item1 :pointer-void)
                    (comparator :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* List_findIf(void* lst, ListItemPredicate predicate)
(uffi:def-function ("List_findIf" list-find-if)
                   ((lst :pointer-void) (predicate :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* List_get(void* lst, unsigned int n)
(uffi:def-function ("List_get" list-get)
                   ((lst :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void List_prepend(void* lst, void* item)
(uffi:def-function ("List_prepend" list-prepend)
                   ((lst :pointer-void) (item :pointer-void))
                   :returning
                   :void) 

;; Function void* List_remove(void* lst, unsigned int n)
(uffi:def-function ("List_remove" list-remove)
                   ((lst :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function unsigned int List_size(void* lst)
(uffi:def-function ("List_size" list-size)
                   ((lst :pointer-void))
                   :returning
                   :unsigned-int) 

;;void ListOf_t
;; Function void* ListOf_create()
(uffi:def-function ("ListOf_create" list-of-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void ListOf_free(void* lo)
(uffi:def-function ("ListOf_free" list-of-free)
                   ((lo :pointer-void))
                   :returning
                   :void) 

;; Function void ListOf_append(void* lo, void* item)
(uffi:def-function ("ListOf_append" list-of-append)
                   ((lo :pointer-void) (item :pointer-void))
                   :returning
                   :void) 

;; Function unsigned int ListOf_countIf(void* lo, ListItemPredicate predicate)
(uffi:def-function ("ListOf_countIf" list-of-count-if)
                   ((lo :pointer-void) (predicate :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void* ListOf_find(void* lo, void* item1, ListItemComparator comparator)
(uffi:def-function ("ListOf_find" list-of-find)
                   ((lo :pointer-void) (item1 :pointer-void)
                    (comparator :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* ListOf_get(void* lo, unsigned int n)
(uffi:def-function ("ListOf_get" list-of-get)
                   ((lo :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function unsigned int ListOf_getNumItems(void* lo)
(uffi:def-function ("ListOf_getNumItems" list-of-get-num-items)
                   ((lo :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void ListOf_prepend(void* lo, void* item)
(uffi:def-function ("ListOf_prepend" list-of-prepend)
                   ((lo :pointer-void) (item :pointer-void))
                   :returning
                   :void) 

;; Function void* ListOf_remove(void* lo, unsigned int n)
(uffi:def-function ("ListOf_remove" list-of-remove)
                   ((lo :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;;struct{char* formula, unsigned int pos} FormulaTokenizer_t
;; enum{tt_plus, tt_minus, tt_times, tt_divide, tt_power, tt_lparen, tt_rparen, tt_comma, tt_end, tt_name, tt_integer, tt_real, tt_real_e, tt_unknown} TokenType_t

(uffi:def-foreign-type token-type-t :int) 
(common-lisp:defconstant +tt-plus+ 43) 
(common-lisp:defconstant +tt-minus+ 45) 
(common-lisp:defconstant +tt-times+ 42) 
(common-lisp:defconstant +tt-divide+ 47) 
(common-lisp:defconstant +tt-power+ 94) 
(common-lisp:defconstant +tt-lparen+ 40) 
(common-lisp:defconstant +tt-rparen+ 41) 
(common-lisp:defconstant +tt-comma+ 44) 
(common-lisp:defconstant +tt-end+ 0) 
(common-lisp:defconstant +tt-name+ 256) 
(common-lisp:defconstant +tt-integer+ 257) 
(common-lisp:defconstant +tt-real+ 258) 
(common-lisp:defconstant +tt-real-e+ 259) 
(common-lisp:defconstant +tt-unknown+ 260) 
;;struct{TokenType_t type, union{char ch,char* name,long integer,double real} value, long exponent} Token_t
;; Function FormulaTokenizer_t* FormulaTokenizer_create(char* formula)
(uffi:def-function ("FormulaTokenizer_create" formula-tokenizer-create1959)
                   ((formula :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun formula-tokenizer-create (formula)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:let ()
    (common-lisp:unwind-protect (formula-tokenizer-create1959 formula)
      common-lisp:nil
      (uffi:free-cstring formula)))) 

;; Function void FormulaTokenizer_free(FormulaTokenizer_t* ft)
(uffi:def-function ("FormulaTokenizer_free" formula-tokenizer-free)
                   ((ft :pointer-void))
                   :returning
                   :void) 

;; Function Token_t* FormulaTokenizer_nextToken(FormulaTokenizer_t* ft)
(uffi:def-function ("FormulaTokenizer_nextToken" formula-tokenizer-next-token)
                   ((ft :pointer-void))
                   :returning
                   :pointer-void) 

;; Function Token_t* Token_create()
(uffi:def-function ("Token_create" token-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void Token_free(Token_t* t)
(uffi:def-function ("Token_free" token-free)
                   ((t :pointer-void))
                   :returning
                   :void) 

;; Function long Token_getInteger(struct{TokenType_t type, union{char ch,char* name,long integer,double real} value, long exponent}* t)
(uffi:def-function ("Token_getInteger" token-get-integer)
                   ((t :pointer-void))
                   :returning
                   :long) 

;; Function double Token_getReal(struct{TokenType_t type, union{char ch,char* name,long integer,double real} value, long exponent}* t)
(uffi:def-function ("Token_getReal" token-get-real)
                   ((t :pointer-void))
                   :returning
                   :double) 

;; Function void Token_negateValue(Token_t* t)
(uffi:def-function ("Token_negateValue" token-negate-value)
                   ((t :pointer-void))
                   :returning
                   :void) 

;; enum{ast_plus, ast_minus, ast_times, ast_divide, ast_power, ast_integer, ast_real, ast_real_e, ast_rational, ast_name, ast_name_delay, ast_name_time, ast_constant_e, ast_constant_false, ast_constant_pi, ast_constant_true, ast_lambda, ast_function, ast_function_abs, ast_function_arccos, ast_function_arccosh, ast_function_arccot, ast_function_arccoth, ast_function_arccsc, ast_function_arccsch, ast_function_arcsec, ast_function_arcsech, ast_function_arcsin, ast_function_arcsinh, ast_function_arctan, ast_function_arctanh, ast_function_ceiling, ast_function_cos, ast_function_cosh, ast_function_cot, ast_function_coth, ast_function_csc, ast_function_csch, ast_function_exp, ast_function_factorial, ast_function_floor, ast_function_ln, ast_function_log, ast_function_piecewise, ast_function_power, ast_function_root, ast_function_sec, ast_function_sech, ast_function_sin, ast_function_sinh, ast_function_tan, ast_function_tanh, ast_logical_and, ast_logical_not, ast_logical_or, ast_logical_xor, ast_relational_eq, ast_relational_geq, ast_relational_gt, ast_relational_leq, ast_relational_lt, ast_relational_neq, ast_unknown} ASTNodeType_t

(uffi:def-foreign-type astnode-type-t :int) 
(common-lisp:defconstant +ast-plus+ 43) 
(common-lisp:defconstant +ast-minus+ 45) 
(common-lisp:defconstant +ast-times+ 42) 
(common-lisp:defconstant +ast-divide+ 47) 
(common-lisp:defconstant +ast-power+ 94) 
(common-lisp:defconstant +ast-integer+ 256) 
(common-lisp:defconstant +ast-real+ 257) 
(common-lisp:defconstant +ast-real-e+ 258) 
(common-lisp:defconstant +ast-rational+ 259) 
(common-lisp:defconstant +ast-name+ 260) 
(common-lisp:defconstant +ast-name-delay+ 261) 
(common-lisp:defconstant +ast-name-time+ 262) 
(common-lisp:defconstant +ast-constant-e+ 263) 
(common-lisp:defconstant +ast-constant-false+ 264) 
(common-lisp:defconstant +ast-constant-pi+ 265) 
(common-lisp:defconstant +ast-constant-true+ 266) 
(common-lisp:defconstant +ast-lambda+ 267) 
(common-lisp:defconstant +ast-function+ 268) 
(common-lisp:defconstant +ast-function-abs+ 269) 
(common-lisp:defconstant +ast-function-arccos+ 270) 
(common-lisp:defconstant +ast-function-arccosh+ 271) 
(common-lisp:defconstant +ast-function-arccot+ 272) 
(common-lisp:defconstant +ast-function-arccoth+ 273) 
(common-lisp:defconstant +ast-function-arccsc+ 274) 
(common-lisp:defconstant +ast-function-arccsch+ 275) 
(common-lisp:defconstant +ast-function-arcsec+ 276) 
(common-lisp:defconstant +ast-function-arcsech+ 277) 
(common-lisp:defconstant +ast-function-arcsin+ 278) 
(common-lisp:defconstant +ast-function-arcsinh+ 279) 
(common-lisp:defconstant +ast-function-arctan+ 280) 
(common-lisp:defconstant +ast-function-arctanh+ 281) 
(common-lisp:defconstant +ast-function-ceiling+ 282) 
(common-lisp:defconstant +ast-function-cos+ 283) 
(common-lisp:defconstant +ast-function-cosh+ 284) 
(common-lisp:defconstant +ast-function-cot+ 285) 
(common-lisp:defconstant +ast-function-coth+ 286) 
(common-lisp:defconstant +ast-function-csc+ 287) 
(common-lisp:defconstant +ast-function-csch+ 288) 
(common-lisp:defconstant +ast-function-exp+ 289) 
(common-lisp:defconstant +ast-function-factorial+ 290) 
(common-lisp:defconstant +ast-function-floor+ 291) 
(common-lisp:defconstant +ast-function-ln+ 292) 
(common-lisp:defconstant +ast-function-log+ 293) 
(common-lisp:defconstant +ast-function-piecewise+ 294) 
(common-lisp:defconstant +ast-function-power+ 295) 
(common-lisp:defconstant +ast-function-root+ 296) 
(common-lisp:defconstant +ast-function-sec+ 297) 
(common-lisp:defconstant +ast-function-sech+ 298) 
(common-lisp:defconstant +ast-function-sin+ 299) 
(common-lisp:defconstant +ast-function-sinh+ 300) 
(common-lisp:defconstant +ast-function-tan+ 301) 
(common-lisp:defconstant +ast-function-tanh+ 302) 
(common-lisp:defconstant +ast-logical-and+ 303) 
(common-lisp:defconstant +ast-logical-not+ 304) 
(common-lisp:defconstant +ast-logical-or+ 305) 
(common-lisp:defconstant +ast-logical-xor+ 306) 
(common-lisp:defconstant +ast-relational-eq+ 307) 
(common-lisp:defconstant +ast-relational-geq+ 308) 
(common-lisp:defconstant +ast-relational-gt+ 309) 
(common-lisp:defconstant +ast-relational-leq+ 310) 
(common-lisp:defconstant +ast-relational-lt+ 311) 
(common-lisp:defconstant +ast-relational-neq+ 312) 
(common-lisp:defconstant +ast-unknown+ 313) 
;;void ASTNode_t
;;function* ASTNodePredicate
;; Function void* ASTNode_create()
(uffi:def-function ("ASTNode_create" astnode-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* ASTNode_createWithType(ASTNodeType_t type)
(uffi:def-function ("ASTNode_createWithType" astnode-create-with-type)
                   ((type :int))
                   :returning
                   :pointer-void) 

;; Function void* ASTNode_createFromToken(Token_t* token)
(uffi:def-function ("ASTNode_createFromToken" astnode-create-from-token)
                   ((token :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void ASTNode_free(void* node)
(uffi:def-function ("ASTNode_free" astnode-free)
                   ((node :pointer-void))
                   :returning
                   :void) 

;; Function void ASTNode_freeName(void* node)
(uffi:def-function ("ASTNode_freeName" astnode-free-name)
                   ((node :pointer-void))
                   :returning
                   :void) 

;; Function int ASTNode_canonicalize(void* node)
(uffi:def-function ("ASTNode_canonicalize" astnode-canonicalize)
                   ((node :pointer-void))
                   :returning
                   :int) 

;; Function void ASTNode_addChild(void* node, void* child)
(uffi:def-function ("ASTNode_addChild" astnode-add-child)
                   ((node :pointer-void) (child :pointer-void))
                   :returning
                   :void) 

;; Function void ASTNode_prependChild(void* node, void* child)
(uffi:def-function ("ASTNode_prependChild" astnode-prepend-child)
                   ((node :pointer-void) (child :pointer-void))
                   :returning
                   :void) 

;; Function void* ASTNode_getChild(void* node, unsigned int n)
(uffi:def-function ("ASTNode_getChild" astnode-get-child)
                   ((node :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* ASTNode_getLeftChild(void* node)
(uffi:def-function ("ASTNode_getLeftChild" astnode-get-left-child)
                   ((node :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* ASTNode_getRightChild(void* node)
(uffi:def-function ("ASTNode_getRightChild" astnode-get-right-child)
                   ((node :pointer-void))
                   :returning
                   :pointer-void) 

;; Function unsigned int ASTNode_getNumChildren(void* node)
(uffi:def-function ("ASTNode_getNumChildren" astnode-get-num-children)
                   ((node :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void* ASTNode_getListOfNodes(void* node, ASTNodePredicate predicate)
(uffi:def-function ("ASTNode_getListOfNodes" astnode-get-list-of-nodes)
                   ((node :pointer-void) (predicate :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void ASTNode_fillListOfNodes(void* node, ASTNodePredicate predicate, void* list)
(uffi:def-function ("ASTNode_fillListOfNodes" astnode-fill-list-of-nodes)
                   ((node :pointer-void) (predicate :pointer-void)
                    (list :pointer-void))
                   :returning
                   :void) 

;; Function char ASTNode_getCharacter(void* node)
(uffi:def-function ("ASTNode_getCharacter" astnode-get-character)
                   ((node :pointer-void))
                   :returning
                   :char) 

;; Function long ASTNode_getInteger(void* node)
(uffi:def-function ("ASTNode_getInteger" astnode-get-integer)
                   ((node :pointer-void))
                   :returning
                   :long) 

;; Function char* ASTNode_getName(void* node)
(uffi:def-function ("ASTNode_getName" astnode-get-name1960)
                   ((node :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun astnode-get-name (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (astnode-get-name1960 node))
      common-lisp:nil))) 

;; Function long ASTNode_getNumerator(void* node)
(uffi:def-function ("ASTNode_getNumerator" astnode-get-numerator)
                   ((node :pointer-void))
                   :returning
                   :long) 

;; Function long ASTNode_getDenominator(void* node)
(uffi:def-function ("ASTNode_getDenominator" astnode-get-denominator)
                   ((node :pointer-void))
                   :returning
                   :long) 

;; Function double ASTNode_getReal(void* node)
(uffi:def-function ("ASTNode_getReal" astnode-get-real)
                   ((node :pointer-void))
                   :returning
                   :double) 

;; Function double ASTNode_getMantissa(void* node)
(uffi:def-function ("ASTNode_getMantissa" astnode-get-mantissa)
                   ((node :pointer-void))
                   :returning
                   :double) 

;; Function long ASTNode_getExponent(void* node)
(uffi:def-function ("ASTNode_getExponent" astnode-get-exponent)
                   ((node :pointer-void))
                   :returning
                   :long) 

;; Function int ASTNode_getPrecedence(void* node)
(uffi:def-function ("ASTNode_getPrecedence" astnode-get-precedence)
                   ((node :pointer-void))
                   :returning
                   :int) 

;; Function ASTNodeType_t ASTNode_getType(void* node)
(uffi:def-function ("ASTNode_getType" astnode-get-type)
                   ((node :pointer-void))
                   :returning
                   :int) 

;; Function int ASTNode_isConstant(void* node)
(uffi:def-function ("ASTNode_isConstant" astnode-is-constant1961)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-constant (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-constant1961 node))
      common-lisp:nil))) 

;; Function int ASTNode_isFunction(void* node)
(uffi:def-function ("ASTNode_isFunction" astnode-is-function1962)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-function (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-function1962 node))
      common-lisp:nil))) 

;; Function int ASTNode_isInteger(void* node)
(uffi:def-function ("ASTNode_isInteger" astnode-is-integer1963)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-integer (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-integer1963 node))
      common-lisp:nil))) 

;; Function int ASTNode_isLambda(void* node)
(uffi:def-function ("ASTNode_isLambda" astnode-is-lambda1964)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-lambda (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-lambda1964 node))
      common-lisp:nil))) 

;; Function int ASTNode_isLog10(void* node)
(uffi:def-function ("ASTNode_isLog10" astnode-is-log101965)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-log10 (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-log101965 node))
      common-lisp:nil))) 

;; Function int ASTNode_isLogical(void* node)
(uffi:def-function ("ASTNode_isLogical" astnode-is-logical1966)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-logical (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-logical1966 node))
      common-lisp:nil))) 

;; Function int ASTNode_isName(void* node)
(uffi:def-function ("ASTNode_isName" astnode-is-name1967)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-name (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-name1967 node))
      common-lisp:nil))) 

;; Function int ASTNode_isNumber(void* node)
(uffi:def-function ("ASTNode_isNumber" astnode-is-number1968)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-number (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-number1968 node))
      common-lisp:nil))) 

;; Function int ASTNode_isOperator(void* node)
(uffi:def-function ("ASTNode_isOperator" astnode-is-operator1969)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-operator (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-operator1969 node))
      common-lisp:nil))) 

;; Function int ASTNode_isRational(void* node)
(uffi:def-function ("ASTNode_isRational" astnode-is-rational1970)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-rational (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-rational1970 node))
      common-lisp:nil))) 

;; Function int ASTNode_isReal(void* node)
(uffi:def-function ("ASTNode_isReal" astnode-is-real1971)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-real (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-real1971 node))
      common-lisp:nil))) 

;; Function int ASTNode_isRelational(void* node)
(uffi:def-function ("ASTNode_isRelational" astnode-is-relational1972)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-relational (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-relational1972 node))
      common-lisp:nil))) 

;; Function int ASTNode_isSqrt(void* node)
(uffi:def-function ("ASTNode_isSqrt" astnode-is-sqrt1973)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-sqrt (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-sqrt1973 node))
      common-lisp:nil))) 

;; Function int ASTNode_isUMinus(void* node)
(uffi:def-function ("ASTNode_isUMinus" astnode-is-uminus1974)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-uminus (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-uminus1974 node))
      common-lisp:nil))) 

;; Function int ASTNode_isUnknown(void* node)
(uffi:def-function ("ASTNode_isUnknown" astnode-is-unknown1975)
                   ((node :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun astnode-is-unknown (node)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (astnode-is-unknown1975 node))
      common-lisp:nil))) 

;; Function void ASTNode_setCharacter(void* node, char value)
(uffi:def-function ("ASTNode_setCharacter" astnode-set-character)
                   ((node :pointer-void) (value :char))
                   :returning
                   :void) 

;; Function void ASTNode_setName(void* node, char* name)
(uffi:def-function ("ASTNode_setName" astnode-set-name1976)
                   ((node :pointer-void) (name :cstring))
                   :returning
                   :void) 
(common-lisp:defun astnode-set-name (node name)
  (common-lisp:setq name (uffi:convert-to-cstring name))
  (common-lisp:let ()
    (common-lisp:unwind-protect (astnode-set-name1976 node name)
      common-lisp:nil
      (uffi:free-cstring name)))) 

;; Function void ASTNode_setInteger(void* node, long value)
(uffi:def-function ("ASTNode_setInteger" astnode-set-integer)
                   ((node :pointer-void) (value :long))
                   :returning
                   :void) 

;; Function void ASTNode_setRational(void* node, long numerator, long denominator)
(uffi:def-function ("ASTNode_setRational" astnode-set-rational)
                   ((node :pointer-void) (numerator :long) (denominator :long))
                   :returning
                   :void) 

;; Function void ASTNode_setReal(void* node, double value)
(uffi:def-function ("ASTNode_setReal" astnode-set-real)
                   ((node :pointer-void) (value :double))
                   :returning
                   :void) 

;; Function void ASTNode_setRealWithExponent(void* node, double mantissa, long exponent)
(uffi:def-function
 ("ASTNode_setRealWithExponent" astnode-set-real-with-exponent)
 ((node :pointer-void) (mantissa :double) (exponent :long))
 :returning
 :void) 

;; Function void ASTNode_setType(void* node, ASTNodeType_t type)
(uffi:def-function ("ASTNode_setType" astnode-set-type)
                   ((node :pointer-void) (type :int))
                   :returning
                   :void) 

;; Function void ASTNode_swapChildren(void* node, void* that)
(uffi:def-function ("ASTNode_swapChildren" astnode-swap-children)
                   ((node :pointer-void) (that :pointer-void))
                   :returning
                   :void) 

;;void FunctionDefinition_t
;; Function void* FunctionDefinition_create()
(uffi:def-function ("FunctionDefinition_create" function-definition-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* FunctionDefinition_createWith(char* sid, void* math)
(uffi:def-function
 ("FunctionDefinition_createWith" function-definition-create-with1977)
 ((sid :cstring) (math :pointer-void))
 :returning
 :pointer-void) 
(common-lisp:defun function-definition-create-with (sid math)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (function-definition-create-with1977 sid math)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void FunctionDefinition_free(void* fd)
(uffi:def-function ("FunctionDefinition_free" function-definition-free)
                   ((fd :pointer-void))
                   :returning
                   :void) 

;; Function char* FunctionDefinition_getId(void* fd)
(uffi:def-function ("FunctionDefinition_getId" function-definition-get-id1978)
                   ((fd :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun function-definition-get-id (fd)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (function-definition-get-id1978 fd))
      common-lisp:nil))) 

;; Function char* FunctionDefinition_getName(void* fd)
(uffi:def-function
 ("FunctionDefinition_getName" function-definition-get-name1979)
 ((fd :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun function-definition-get-name (fd)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (function-definition-get-name1979 fd))
      common-lisp:nil))) 

;; Function void* FunctionDefinition_getMath(void* fd)
(uffi:def-function ("FunctionDefinition_getMath" function-definition-get-math)
                   ((fd :pointer-void))
                   :returning
                   :pointer-void) 

;; Function int FunctionDefinition_isSetId(void* fd)
(uffi:def-function
 ("FunctionDefinition_isSetId" function-definition-is-set-id1980)
 ((fd :pointer-void))
 :returning
 :int) 
(common-lisp:defun function-definition-is-set-id (fd)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (function-definition-is-set-id1980 fd))
      common-lisp:nil))) 

;; Function int FunctionDefinition_isSetName(void* fd)
(uffi:def-function
 ("FunctionDefinition_isSetName" function-definition-is-set-name1981)
 ((fd :pointer-void))
 :returning
 :int) 
(common-lisp:defun function-definition-is-set-name (fd)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (function-definition-is-set-name1981 fd))
      common-lisp:nil))) 

;; Function int FunctionDefinition_isSetMath(void* fd)
(uffi:def-function
 ("FunctionDefinition_isSetMath" function-definition-is-set-math1982)
 ((fd :pointer-void))
 :returning
 :int) 
(common-lisp:defun function-definition-is-set-math (fd)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (function-definition-is-set-math1982 fd))
      common-lisp:nil))) 

;; Function void FunctionDefinition_setId(void* fd, char* sid)
(uffi:def-function ("FunctionDefinition_setId" function-definition-set-id1983)
                   ((fd :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun function-definition-set-id (fd sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (function-definition-set-id1983 fd sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void FunctionDefinition_setName(void* fd, char* string)
(uffi:def-function
 ("FunctionDefinition_setName" function-definition-set-name1984)
 ((fd :pointer-void) (string :cstring))
 :returning
 :void) 
(common-lisp:defun function-definition-set-name (fd string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (function-definition-set-name1984 fd string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void FunctionDefinition_setMath(void* fd, void* math)
(uffi:def-function ("FunctionDefinition_setMath" function-definition-set-math)
                   ((fd :pointer-void) (math :pointer-void))
                   :returning
                   :void) 

;; Function void FunctionDefinition_unsetName(void* fd)
(uffi:def-function
 ("FunctionDefinition_unsetName" function-definition-unset-name)
 ((fd :pointer-void))
 :returning
 :void) 

;; Function int FunctionDefinitionIdCmp(char* sid, void* fd)
(uffi:def-function ("FunctionDefinitionIdCmp" function-definition-id-cmp1985)
                   ((sid :cstring) (fd :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun function-definition-id-cmp (sid fd)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (function-definition-id-cmp1985 sid fd)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; enum{unit_kind_ampere, unit_kind_becquerel, unit_kind_candela, unit_kind_celsius, unit_kind_coulomb, unit_kind_dimensionless, unit_kind_farad, unit_kind_gram, unit_kind_gray, unit_kind_henry, unit_kind_hertz, unit_kind_item, unit_kind_joule, unit_kind_katal, unit_kind_kelvin, unit_kind_kilogram, unit_kind_liter, unit_kind_litre, unit_kind_lumen, unit_kind_lux, unit_kind_meter, unit_kind_metre, unit_kind_mole, unit_kind_newton, unit_kind_ohm, unit_kind_pascal, unit_kind_radian, unit_kind_second, unit_kind_siemens, unit_kind_sievert, unit_kind_steradian, unit_kind_tesla, unit_kind_volt, unit_kind_watt, unit_kind_weber, unit_kind_invalid} UnitKind_t

(uffi:def-foreign-type unit-kind-t :int) 
(common-lisp:defconstant +unit-kind-ampere+ 0) 
(common-lisp:defconstant +unit-kind-becquerel+ 1) 
(common-lisp:defconstant +unit-kind-candela+ 2) 
(common-lisp:defconstant +unit-kind-celsius+ 3) 
(common-lisp:defconstant +unit-kind-coulomb+ 4) 
(common-lisp:defconstant +unit-kind-dimensionless+ 5) 
(common-lisp:defconstant +unit-kind-farad+ 6) 
(common-lisp:defconstant +unit-kind-gram+ 7) 
(common-lisp:defconstant +unit-kind-gray+ 8) 
(common-lisp:defconstant +unit-kind-henry+ 9) 
(common-lisp:defconstant +unit-kind-hertz+ 10) 
(common-lisp:defconstant +unit-kind-item+ 11) 
(common-lisp:defconstant +unit-kind-joule+ 12) 
(common-lisp:defconstant +unit-kind-katal+ 13) 
(common-lisp:defconstant +unit-kind-kelvin+ 14) 
(common-lisp:defconstant +unit-kind-kilogram+ 15) 
(common-lisp:defconstant +unit-kind-liter+ 16) 
(common-lisp:defconstant +unit-kind-litre+ 17) 
(common-lisp:defconstant +unit-kind-lumen+ 18) 
(common-lisp:defconstant +unit-kind-lux+ 19) 
(common-lisp:defconstant +unit-kind-meter+ 20) 
(common-lisp:defconstant +unit-kind-metre+ 21) 
(common-lisp:defconstant +unit-kind-mole+ 22) 
(common-lisp:defconstant +unit-kind-newton+ 23) 
(common-lisp:defconstant +unit-kind-ohm+ 24) 
(common-lisp:defconstant +unit-kind-pascal+ 25) 
(common-lisp:defconstant +unit-kind-radian+ 26) 
(common-lisp:defconstant +unit-kind-second+ 27) 
(common-lisp:defconstant +unit-kind-siemens+ 28) 
(common-lisp:defconstant +unit-kind-sievert+ 29) 
(common-lisp:defconstant +unit-kind-steradian+ 30) 
(common-lisp:defconstant +unit-kind-tesla+ 31) 
(common-lisp:defconstant +unit-kind-volt+ 32) 
(common-lisp:defconstant +unit-kind-watt+ 33) 
(common-lisp:defconstant +unit-kind-weber+ 34) 
(common-lisp:defconstant +unit-kind-invalid+ 35) 
;; Function int UnitKind_equals(UnitKind_t uk1, UnitKind_t uk2)
(uffi:def-function ("UnitKind_equals" unit-kind-equals)
                   ((uk1 :int) (uk2 :int))
                   :returning
                   :int) 

;; Function UnitKind_t UnitKind_forName(char* name)
(uffi:def-function ("UnitKind_forName" unit-kind-for-name1986)
                   ((name :cstring))
                   :returning
                   :int) 
(common-lisp:defun unit-kind-for-name (name)
  (common-lisp:setq name (uffi:convert-to-cstring name))
  (common-lisp:let ()
    (common-lisp:unwind-protect (unit-kind-for-name1986 name)
      common-lisp:nil
      (uffi:free-cstring name)))) 

;; Function char* UnitKind_toString(UnitKind_t uk)
(uffi:def-function ("UnitKind_toString" unit-kind-to-string1987)
                   ((uk :int))
                   :returning
                   :cstring) 
(common-lisp:defun unit-kind-to-string (uk)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (unit-kind-to-string1987 uk))
      common-lisp:nil))) 

;; Function int UnitKind_isValidUnitKindString(char* string)
(uffi:def-function
 ("UnitKind_isValidUnitKindString" unit-kind-is-valid-unit-kind-string1988)
 ((string :cstring))
 :returning
 :int) 
(common-lisp:defun unit-kind-is-valid-unit-kind-string (string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (unit-kind-is-valid-unit-kind-string1988 string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;;void Unit_t
;; Function void* Unit_create()
(uffi:def-function ("Unit_create" unit-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Unit_createWith(UnitKind_t kind, int exponent, int scale)
(uffi:def-function ("Unit_createWith" unit-create-with)
                   ((kind :int) (exponent :int) (scale :int))
                   :returning
                   :pointer-void) 

;; Function void Unit_free(void* u)
(uffi:def-function ("Unit_free" unit-free) ((u :pointer-void)) :returning :void) 

;; Function void Unit_initDefaults(void* u)
(uffi:def-function ("Unit_initDefaults" unit-init-defaults)
                   ((u :pointer-void))
                   :returning
                   :void) 

;; Function UnitKind_t Unit_getKind(void* u)
(uffi:def-function ("Unit_getKind" unit-get-kind)
                   ((u :pointer-void))
                   :returning
                   :int) 

;; Function int Unit_getExponent(void* u)
(uffi:def-function ("Unit_getExponent" unit-get-exponent)
                   ((u :pointer-void))
                   :returning
                   :int) 

;; Function int Unit_getScale(void* u)
(uffi:def-function ("Unit_getScale" unit-get-scale)
                   ((u :pointer-void))
                   :returning
                   :int) 

;; Function double Unit_getMultiplier(void* u)
(uffi:def-function ("Unit_getMultiplier" unit-get-multiplier)
                   ((u :pointer-void))
                   :returning
                   :double) 

;; Function double Unit_getOffset(void* u)
(uffi:def-function ("Unit_getOffset" unit-get-offset)
                   ((u :pointer-void))
                   :returning
                   :double) 

;; Function int Unit_isSetKind(void* u)
(uffi:def-function ("Unit_isSetKind" unit-is-set-kind1989)
                   ((u :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun unit-is-set-kind (u)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (unit-is-set-kind1989 u))
      common-lisp:nil))) 

;; Function void Unit_setKind(void* u, UnitKind_t kind)
(uffi:def-function ("Unit_setKind" unit-set-kind)
                   ((u :pointer-void) (kind :int))
                   :returning
                   :void) 

;; Function void Unit_setExponent(void* u, int value)
(uffi:def-function ("Unit_setExponent" unit-set-exponent)
                   ((u :pointer-void) (value :int))
                   :returning
                   :void) 

;; Function void Unit_setScale(void* u, int value)
(uffi:def-function ("Unit_setScale" unit-set-scale)
                   ((u :pointer-void) (value :int))
                   :returning
                   :void) 

;; Function void Unit_setMultiplier(void* u, double value)
(uffi:def-function ("Unit_setMultiplier" unit-set-multiplier)
                   ((u :pointer-void) (value :double))
                   :returning
                   :void) 

;; Function void Unit_setOffset(void* u, double value)
(uffi:def-function ("Unit_setOffset" unit-set-offset)
                   ((u :pointer-void) (value :double))
                   :returning
                   :void) 

;;void UnitDefinition_t
;; Function void* UnitDefinition_create()
(uffi:def-function ("UnitDefinition_create" unit-definition-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* UnitDefinition_createWith(char* sid)
(uffi:def-function
 ("UnitDefinition_createWith" unit-definition-create-with1990)
 ((sid :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun unit-definition-create-with (sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (unit-definition-create-with1990 sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* UnitDefinition_createWithName(char* string)
(uffi:def-function
 ("UnitDefinition_createWithName" unit-definition-create-with-name1991)
 ((string :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun unit-definition-create-with-name (string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (unit-definition-create-with-name1991 string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void UnitDefinition_free(void* ud)
(uffi:def-function ("UnitDefinition_free" unit-definition-free)
                   ((ud :pointer-void))
                   :returning
                   :void) 

;; Function char* UnitDefinition_getId(void* ud)
(uffi:def-function ("UnitDefinition_getId" unit-definition-get-id1992)
                   ((ud :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun unit-definition-get-id (ud)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (unit-definition-get-id1992 ud))
      common-lisp:nil))) 

;; Function char* UnitDefinition_getName(void* ud)
(uffi:def-function ("UnitDefinition_getName" unit-definition-get-name1993)
                   ((ud :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun unit-definition-get-name (ud)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (unit-definition-get-name1993 ud))
      common-lisp:nil))) 

;; Function int UnitDefinition_isSetId(void* ud)
(uffi:def-function ("UnitDefinition_isSetId" unit-definition-is-set-id1994)
                   ((ud :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun unit-definition-is-set-id (ud)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (unit-definition-is-set-id1994 ud))
      common-lisp:nil))) 

;; Function int UnitDefinition_isSetName(void* ud)
(uffi:def-function ("UnitDefinition_isSetName" unit-definition-is-set-name1995)
                   ((ud :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun unit-definition-is-set-name (ud)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (unit-definition-is-set-name1995 ud))
      common-lisp:nil))) 

;; Function void UnitDefinition_setId(void* ud, char* sid)
(uffi:def-function ("UnitDefinition_setId" unit-definition-set-id1996)
                   ((ud :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun unit-definition-set-id (ud sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (unit-definition-set-id1996 ud sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void UnitDefinition_setName(void* ud, char* string)
(uffi:def-function ("UnitDefinition_setName" unit-definition-set-name1997)
                   ((ud :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun unit-definition-set-name (ud string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (unit-definition-set-name1997 ud string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void UnitDefinition_unsetName(void* ud)
(uffi:def-function ("UnitDefinition_unsetName" unit-definition-unset-name)
                   ((ud :pointer-void))
                   :returning
                   :void) 

;; Function void UnitDefinition_addUnit(void* ud, void* u)
(uffi:def-function ("UnitDefinition_addUnit" unit-definition-add-unit)
                   ((ud :pointer-void) (u :pointer-void))
                   :returning
                   :void) 

;; Function void* UnitDefinition_getListOfUnits(void* ud)
(uffi:def-function
 ("UnitDefinition_getListOfUnits" unit-definition-get-list-of-units)
 ((ud :pointer-void))
 :returning
 :pointer-void) 

;; Function void* UnitDefinition_getUnit(void* ud, unsigned int n)
(uffi:def-function ("UnitDefinition_getUnit" unit-definition-get-unit)
                   ((ud :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function unsigned int UnitDefinition_getNumUnits(void* ud)
(uffi:def-function ("UnitDefinition_getNumUnits" unit-definition-get-num-units)
                   ((ud :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function int UnitDefinitionIdCmp(char* sid, void* ud)
(uffi:def-function ("UnitDefinitionIdCmp" unit-definition-id-cmp1998)
                   ((sid :cstring) (ud :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun unit-definition-id-cmp (sid ud)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (unit-definition-id-cmp1998 sid ud)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void Compartment_t
;; Function void* Compartment_create()
(uffi:def-function ("Compartment_create" compartment-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Compartment_createWith(char* sid, double size, char* units, char* outside)
(uffi:def-function ("Compartment_createWith" compartment-create-with1999)
                   ((sid :cstring) (size :double) (units :cstring)
                    (outside :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun compartment-create-with (sid size units outside)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:setq units (uffi:convert-to-cstring units))
  (common-lisp:setq outside (uffi:convert-to-cstring outside))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (compartment-create-with1999 sid size units outside)
      common-lisp:nil
      (uffi:free-cstring outside)
      (uffi:free-cstring units)
      (uffi:free-cstring sid)))) 

;; Function void Compartment_free(void* c)
(uffi:def-function ("Compartment_free" compartment-free)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function void Compartment_initDefaults(void* c)
(uffi:def-function ("Compartment_initDefaults" compartment-init-defaults)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function char* Compartment_getId(void* c)
(uffi:def-function ("Compartment_getId" compartment-get-id2000)
                   ((c :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun compartment-get-id (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (compartment-get-id2000 c))
      common-lisp:nil))) 

;; Function char* Compartment_getName(void* c)
(uffi:def-function ("Compartment_getName" compartment-get-name2001)
                   ((c :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun compartment-get-name (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (compartment-get-name2001 c))
      common-lisp:nil))) 

;; Function unsigned int Compartment_getSpatialDimensions(void* c)
(uffi:def-function
 ("Compartment_getSpatialDimensions" compartment-get-spatial-dimensions)
 ((c :pointer-void))
 :returning
 :unsigned-int) 

;; Function double Compartment_getSize(void* c)
(uffi:def-function ("Compartment_getSize" compartment-get-size)
                   ((c :pointer-void))
                   :returning
                   :double) 

;; Function double Compartment_getVolume(void* c)
(uffi:def-function ("Compartment_getVolume" compartment-get-volume)
                   ((c :pointer-void))
                   :returning
                   :double) 

;; Function char* Compartment_getUnits(void* c)
(uffi:def-function ("Compartment_getUnits" compartment-get-units2002)
                   ((c :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun compartment-get-units (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (compartment-get-units2002 c))
      common-lisp:nil))) 

;; Function char* Compartment_getOutside(void* c)
(uffi:def-function ("Compartment_getOutside" compartment-get-outside2003)
                   ((c :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun compartment-get-outside (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (compartment-get-outside2003 c))
      common-lisp:nil))) 

;; Function int Compartment_getConstant(void* c)
(uffi:def-function ("Compartment_getConstant" compartment-get-constant2004)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-get-constant (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (compartment-get-constant2004 c))
      common-lisp:nil))) 

;; Function int Compartment_isSetId(void* c)
(uffi:def-function ("Compartment_isSetId" compartment-is-set-id2005)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-is-set-id (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (compartment-is-set-id2005 c))
      common-lisp:nil))) 

;; Function int Compartment_isSetName(void* c)
(uffi:def-function ("Compartment_isSetName" compartment-is-set-name2006)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-is-set-name (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (compartment-is-set-name2006 c))
      common-lisp:nil))) 

;; Function int Compartment_isSetSize(void* c)
(uffi:def-function ("Compartment_isSetSize" compartment-is-set-size2007)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-is-set-size (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (compartment-is-set-size2007 c))
      common-lisp:nil))) 

;; Function int Compartment_isSetVolume(void* c)
(uffi:def-function ("Compartment_isSetVolume" compartment-is-set-volume2008)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-is-set-volume (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (compartment-is-set-volume2008 c))
      common-lisp:nil))) 

;; Function int Compartment_isSetUnits(void* c)
(uffi:def-function ("Compartment_isSetUnits" compartment-is-set-units2009)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-is-set-units (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (compartment-is-set-units2009 c))
      common-lisp:nil))) 

;; Function int Compartment_isSetOutside(void* c)
(uffi:def-function ("Compartment_isSetOutside" compartment-is-set-outside2010)
                   ((c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-is-set-outside (c)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (compartment-is-set-outside2010 c))
      common-lisp:nil))) 

;; Function void Compartment_setId(void* c, char* sid)
(uffi:def-function ("Compartment_setId" compartment-set-id2011)
                   ((c :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun compartment-set-id (c sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (compartment-set-id2011 c sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Compartment_setName(void* c, char* string)
(uffi:def-function ("Compartment_setName" compartment-set-name2012)
                   ((c :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun compartment-set-name (c string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (compartment-set-name2012 c string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Compartment_setSpatialDimensions(void* c, unsigned int value)
(uffi:def-function
 ("Compartment_setSpatialDimensions" compartment-set-spatial-dimensions)
 ((c :pointer-void) (value :unsigned-int))
 :returning
 :void) 

;; Function void Compartment_setSize(void* c, double value)
(uffi:def-function ("Compartment_setSize" compartment-set-size)
                   ((c :pointer-void) (value :double))
                   :returning
                   :void) 

;; Function void Compartment_setVolume(void* c, double value)
(uffi:def-function ("Compartment_setVolume" compartment-set-volume)
                   ((c :pointer-void) (value :double))
                   :returning
                   :void) 

;; Function void Compartment_setUnits(void* c, char* sid)
(uffi:def-function ("Compartment_setUnits" compartment-set-units2013)
                   ((c :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun compartment-set-units (c sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (compartment-set-units2013 c sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Compartment_setOutside(void* c, char* sid)
(uffi:def-function ("Compartment_setOutside" compartment-set-outside2014)
                   ((c :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun compartment-set-outside (c sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (compartment-set-outside2014 c sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Compartment_setConstant(void* c, int value)
(uffi:def-function ("Compartment_setConstant" compartment-set-constant)
                   ((c :pointer-void) (value :int))
                   :returning
                   :void) 

;; Function void Compartment_unsetName(void* c)
(uffi:def-function ("Compartment_unsetName" compartment-unset-name)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function void Compartment_unsetSize(void* c)
(uffi:def-function ("Compartment_unsetSize" compartment-unset-size)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function void Compartment_unsetVolume(void* c)
(uffi:def-function ("Compartment_unsetVolume" compartment-unset-volume)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function void Compartment_unsetUnits(void* c)
(uffi:def-function ("Compartment_unsetUnits" compartment-unset-units)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function void Compartment_unsetOutside(void* c)
(uffi:def-function ("Compartment_unsetOutside" compartment-unset-outside)
                   ((c :pointer-void))
                   :returning
                   :void) 

;; Function int CompartmentIdCmp(char* sid, void* c)
(uffi:def-function ("CompartmentIdCmp" compartment-id-cmp2015)
                   ((sid :cstring) (c :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun compartment-id-cmp (sid c)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (compartment-id-cmp2015 sid c)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void Species_t
;; Function void* Species_create()
(uffi:def-function ("Species_create" species-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Species_createWith(char* sid, char* compartment, double initialAmount, char* substanceUnits, int boundaryCondition, int charge)
(uffi:def-function ("Species_createWith" species-create-with2016)
                   ((sid :cstring) (compartment :cstring)
                    (initial-amount :double) (substance-units :cstring)
                    (boundary-condition :int) (charge :int))
                   :returning
                   :pointer-void) 
(common-lisp:defun species-create-with
                   (sid compartment initial-amount substance-units
                    boundary-condition charge)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:setq compartment (uffi:convert-to-cstring compartment))
  (common-lisp:setq substance-units (uffi:convert-to-cstring substance-units))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (species-create-with2016 sid compartment initial-amount substance-units
         boundary-condition charge)
      common-lisp:nil
      (uffi:free-cstring substance-units)
      (uffi:free-cstring compartment)
      (uffi:free-cstring sid)))) 

;; Function void Species_free(void* s)
(uffi:def-function ("Species_free" species-free)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function void Species_initDefaults(void* s)
(uffi:def-function ("Species_initDefaults" species-init-defaults)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function char* Species_getId(void* s)
(uffi:def-function ("Species_getId" species-get-id2017)
                   ((s :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun species-get-id (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-get-id2017 s))
      common-lisp:nil))) 

;; Function char* Species_getName(void* s)
(uffi:def-function ("Species_getName" species-get-name2018)
                   ((s :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun species-get-name (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-get-name2018 s))
      common-lisp:nil))) 

;; Function char* Species_getCompartment(void* s)
(uffi:def-function ("Species_getCompartment" species-get-compartment2019)
                   ((s :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun species-get-compartment (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-get-compartment2019 s))
      common-lisp:nil))) 

;; Function double Species_getInitialAmount(void* s)
(uffi:def-function ("Species_getInitialAmount" species-get-initial-amount)
                   ((s :pointer-void))
                   :returning
                   :double) 

;; Function double Species_getInitialConcentration(void* s)
(uffi:def-function
 ("Species_getInitialConcentration" species-get-initial-concentration)
 ((s :pointer-void))
 :returning
 :double) 

;; Function char* Species_getSubstanceUnits(void* s)
(uffi:def-function
 ("Species_getSubstanceUnits" species-get-substance-units2020)
 ((s :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun species-get-substance-units (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-get-substance-units2020 s))
      common-lisp:nil))) 

;; Function char* Species_getSpatialSizeUnits(void* s)
(uffi:def-function
 ("Species_getSpatialSizeUnits" species-get-spatial-size-units2021)
 ((s :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun species-get-spatial-size-units (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-get-spatial-size-units2021 s))
      common-lisp:nil))) 

;; Function char* Species_getUnits(void* s)
(uffi:def-function ("Species_getUnits" species-get-units2022)
                   ((s :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun species-get-units (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-get-units2022 s))
      common-lisp:nil))) 

;; Function int Species_getHasOnlySubstanceUnits(void* s)
(uffi:def-function
 ("Species_getHasOnlySubstanceUnits" species-get-has-only-substance-units2023)
 ((s :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-get-has-only-substance-units (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-get-has-only-substance-units2023 s))
      common-lisp:nil))) 

;; Function int Species_getBoundaryCondition(void* s)
(uffi:def-function
 ("Species_getBoundaryCondition" species-get-boundary-condition2024)
 ((s :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-get-boundary-condition (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-get-boundary-condition2024 s))
      common-lisp:nil))) 

;; Function int Species_getCharge(void* s)
(uffi:def-function ("Species_getCharge" species-get-charge)
                   ((s :pointer-void))
                   :returning
                   :int) 

;; Function int Species_getConstant(void* s)
(uffi:def-function ("Species_getConstant" species-get-constant2025)
                   ((s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-get-constant (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (species-get-constant2025 s))
      common-lisp:nil))) 

;; Function int Species_isSetId(void* s)
(uffi:def-function ("Species_isSetId" species-is-set-id2026)
                   ((s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-is-set-id (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (species-is-set-id2026 s))
      common-lisp:nil))) 

;; Function int Species_isSetName(void* s)
(uffi:def-function ("Species_isSetName" species-is-set-name2027)
                   ((s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-is-set-name (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (species-is-set-name2027 s))
      common-lisp:nil))) 

;; Function int Species_isSetCompartment(void* s)
(uffi:def-function ("Species_isSetCompartment" species-is-set-compartment2028)
                   ((s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-is-set-compartment (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-is-set-compartment2028 s))
      common-lisp:nil))) 

;; Function int Species_isSetInitialAmount(void* s)
(uffi:def-function
 ("Species_isSetInitialAmount" species-is-set-initial-amount2029)
 ((s :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-is-set-initial-amount (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-is-set-initial-amount2029 s))
      common-lisp:nil))) 

;; Function int Species_isSetInitialConcentration(void* s)
(uffi:def-function
 ("Species_isSetInitialConcentration" species-is-set-initial-concentration2030)
 ((s :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-is-set-initial-concentration (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-is-set-initial-concentration2030 s))
      common-lisp:nil))) 

;; Function int Species_isSetSubstanceUnits(void* s)
(uffi:def-function
 ("Species_isSetSubstanceUnits" species-is-set-substance-units)
 ((s :pointer-void))
 :returning
 :int) 

;; Function int Species_isSetSpatialSizeUnits(void* s)
(uffi:def-function
 ("Species_isSetSpatialSizeUnits" species-is-set-spatial-size-units2031)
 ((s :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-is-set-spatial-size-units (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-is-set-spatial-size-units2031 s))
      common-lisp:nil))) 

;; Function int Species_isSetUnits(void* s)
(uffi:def-function ("Species_isSetUnits" species-is-set-units2032)
                   ((s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-is-set-units (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (species-is-set-units2032 s))
      common-lisp:nil))) 

;; Function int Species_isSetCharge(void* s)
(uffi:def-function ("Species_isSetCharge" species-is-set-charge2033)
                   ((s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-is-set-charge (s)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (species-is-set-charge2033 s))
      common-lisp:nil))) 

;; Function void Species_setId(void* s, char* sid)
(uffi:def-function ("Species_setId" species-set-id2034)
                   ((s :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun species-set-id (s sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-id2034 s sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Species_setName(void* s, char* string)
(uffi:def-function ("Species_setName" species-set-name2035)
                   ((s :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun species-set-name (s string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-name2035 s string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Species_setCompartment(void* s, char* sid)
(uffi:def-function ("Species_setCompartment" species-set-compartment2036)
                   ((s :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun species-set-compartment (s sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-compartment2036 s sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Species_setInitialAmount(void* s, double value)
(uffi:def-function ("Species_setInitialAmount" species-set-initial-amount)
                   ((s :pointer-void) (value :double))
                   :returning
                   :void) 

;; Function void Species_setInitialConcentration(void* s, double value)
(uffi:def-function
 ("Species_setInitialConcentration" species-set-initial-concentration)
 ((s :pointer-void) (value :double))
 :returning
 :void) 

;; Function void Species_setSubstanceUnits(void* s, char* sid)
(uffi:def-function
 ("Species_setSubstanceUnits" species-set-substance-units2037)
 ((s :pointer-void) (sid :cstring))
 :returning
 :void) 
(common-lisp:defun species-set-substance-units (s sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-substance-units2037 s sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Species_setSpatialSizeUnits(void* s, char* sid)
(uffi:def-function
 ("Species_setSpatialSizeUnits" species-set-spatial-size-units2038)
 ((s :pointer-void) (sid :cstring))
 :returning
 :void) 
(common-lisp:defun species-set-spatial-size-units (s sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-spatial-size-units2038 s sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Species_setUnits(void* s, char* sname)
(uffi:def-function ("Species_setUnits" species-set-units2039)
                   ((s :pointer-void) (sname :cstring))
                   :returning
                   :void) 
(common-lisp:defun species-set-units (s sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-units2039 s sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;; Function void Species_setHasOnlySubstanceUnits(void* s, int value)
(uffi:def-function
 ("Species_setHasOnlySubstanceUnits" species-set-has-only-substance-units2040)
 ((s :pointer-void) (value :int))
 :returning
 :void) 
(common-lisp:defun species-set-has-only-substance-units (s value)
  (common-lisp:setq value (uffi::cboolm value))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (species-set-has-only-substance-units2040 s value)
      common-lisp:nil))) 

;; Function void Species_setBoundaryCondition(void* s, int value)
(uffi:def-function
 ("Species_setBoundaryCondition" species-set-boundary-condition2041)
 ((s :pointer-void) (value :int))
 :returning
 :void) 
(common-lisp:defun species-set-boundary-condition (s value)
  (common-lisp:setq value (uffi::cboolm value))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-boundary-condition2041 s value)
      common-lisp:nil))) 

;; Function void Species_setCharge(void* s, int value)
(uffi:def-function ("Species_setCharge" species-set-charge)
                   ((s :pointer-void) (value :int))
                   :returning
                   :void) 

;; Function void Species_setConstant(void* s, int value)
(uffi:def-function ("Species_setConstant" species-set-constant2042)
                   ((s :pointer-void) (value :int))
                   :returning
                   :void) 
(common-lisp:defun species-set-constant (s value)
  (common-lisp:setq value (uffi::cboolm value))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-set-constant2042 s value)
      common-lisp:nil))) 

;; Function void Species_unsetName(void* s)
(uffi:def-function ("Species_unsetName" species-unset-name)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function void Species_unsetInitialAmount(void* s)
(uffi:def-function ("Species_unsetInitialAmount" species-unset-initial-amount)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function void Species_unsetInitialConcentration(void* s)
(uffi:def-function
 ("Species_unsetInitialConcentration" species-unset-initial-concentration)
 ((s :pointer-void))
 :returning
 :void) 

;; Function void Species_unsetSubstanceUnits(void* s)
(uffi:def-function
 ("Species_unsetSubstanceUnits" species-unset-substance-units)
 ((s :pointer-void))
 :returning
 :void) 

;; Function void Species_unsetSpatialSizeUnits(void* s)
(uffi:def-function
 ("Species_unsetSpatialSizeUnits" species-unset-spatial-size-units)
 ((s :pointer-void))
 :returning
 :void) 

;; Function void Species_unsetUnits(void* s)
(uffi:def-function ("Species_unsetUnits" species-unset-units)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function void Species_unsetCharge(void* s)
(uffi:def-function ("Species_unsetCharge" species-unset-charge)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function int SpeciesIdCmp(char* sid, void* s)
(uffi:def-function ("SpeciesIdCmp" species-id-cmp2043)
                   ((sid :cstring) (s :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun species-id-cmp (sid s)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-id-cmp2043 sid s)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void Parameter_t
;; Function void* Parameter_create()
(uffi:def-function ("Parameter_create" parameter-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Parameter_createWith(char* sid, double value, char* units)
(uffi:def-function ("Parameter_createWith" parameter-create-with2044)
                   ((sid :cstring) (value :double) (units :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun parameter-create-with (sid value units)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:setq units (uffi:convert-to-cstring units))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-create-with2044 sid value units)
      common-lisp:nil
      (uffi:free-cstring units)
      (uffi:free-cstring sid)))) 

;; Function void Parameter_free(void* p)
(uffi:def-function ("Parameter_free" parameter-free)
                   ((p :pointer-void))
                   :returning
                   :void) 

;; Function void Parameter_initDefaults(void* p)
(uffi:def-function ("Parameter_initDefaults" parameter-init-defaults)
                   ((p :pointer-void))
                   :returning
                   :void) 

;; Function char* Parameter_getId(void* p)
(uffi:def-function ("Parameter_getId" parameter-get-id2045)
                   ((p :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun parameter-get-id (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (parameter-get-id2045 p))
      common-lisp:nil))) 

;; Function char* Parameter_getName(void* p)
(uffi:def-function ("Parameter_getName" parameter-get-name2046)
                   ((p :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun parameter-get-name (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (parameter-get-name2046 p))
      common-lisp:nil))) 

;; Function double Parameter_getValue(void* p)
(uffi:def-function ("Parameter_getValue" parameter-get-value)
                   ((p :pointer-void))
                   :returning
                   :double) 

;; Function char* Parameter_getUnits(void* p)
(uffi:def-function ("Parameter_getUnits" parameter-get-units2047)
                   ((p :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun parameter-get-units (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (parameter-get-units2047 p))
      common-lisp:nil))) 

;; Function int Parameter_getConstant(void* p)
(uffi:def-function ("Parameter_getConstant" parameter-get-constant2048)
                   ((p :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-get-constant (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (parameter-get-constant2048 p))
      common-lisp:nil))) 

;; Function int Parameter_isSetId(void* p)
(uffi:def-function ("Parameter_isSetId" parameter-is-set-id2049)
                   ((p :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-is-set-id (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (parameter-is-set-id2049 p))
      common-lisp:nil))) 

;; Function int Parameter_isSetName(void* p)
(uffi:def-function ("Parameter_isSetName" parameter-is-set-name2050)
                   ((p :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-is-set-name (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (parameter-is-set-name2050 p))
      common-lisp:nil))) 

;; Function int Parameter_isSetValue(void* p)
(uffi:def-function ("Parameter_isSetValue" parameter-is-set-value2051)
                   ((p :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-is-set-value (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (parameter-is-set-value2051 p))
      common-lisp:nil))) 

;; Function int Parameter_isSetUnits(void* p)
(uffi:def-function ("Parameter_isSetUnits" parameter-is-set-units2052)
                   ((p :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-is-set-units (p)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (parameter-is-set-units2052 p))
      common-lisp:nil))) 

;; Function void Parameter_setId(void* p, char* sid)
(uffi:def-function ("Parameter_setId" parameter-set-id2053)
                   ((p :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun parameter-set-id (p sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-set-id2053 p sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Parameter_setName(void* p, char* string)
(uffi:def-function ("Parameter_setName" parameter-set-name2054)
                   ((p :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun parameter-set-name (p string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-set-name2054 p string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Parameter_setValue(void* p, double value)
(uffi:def-function ("Parameter_setValue" parameter-set-value)
                   ((p :pointer-void) (value :double))
                   :returning
                   :void) 

;; Function void Parameter_setUnits(void* p, char* sid)
(uffi:def-function ("Parameter_setUnits" parameter-set-units2055)
                   ((p :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun parameter-set-units (p sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-set-units2055 p sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Parameter_setConstant(void* p, int value)
(uffi:def-function ("Parameter_setConstant" parameter-set-constant2056)
                   ((p :pointer-void) (value :int))
                   :returning
                   :void) 
(common-lisp:defun parameter-set-constant (p value)
  (common-lisp:setq value (uffi::cboolm value))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-set-constant2056 p value)
      common-lisp:nil))) 

;; Function void Parameter_unsetName(void* p)
(uffi:def-function ("Parameter_unsetName" parameter-unset-name)
                   ((p :pointer-void))
                   :returning
                   :void) 

;; Function void Parameter_unsetValue(void* p)
(uffi:def-function ("Parameter_unsetValue" parameter-unset-value)
                   ((p :pointer-void))
                   :returning
                   :void) 

;; Function void Parameter_unsetUnits(void* p)
(uffi:def-function ("Parameter_unsetUnits" parameter-unset-units)
                   ((p :pointer-void))
                   :returning
                   :void) 

;; Function int ParameterIdCmp(char* sid, void* p)
(uffi:def-function ("ParameterIdCmp" parameter-id-cmp2057)
                   ((sid :cstring) (p :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-id-cmp (sid p)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-id-cmp2057 sid p)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void Rule_t
;; Function char* Rule_getFormula(void* r)
(uffi:def-function ("Rule_getFormula" rule-get-formula2058)
                   ((r :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun rule-get-formula (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (rule-get-formula2058 r))
      common-lisp:nil))) 

;; Function void* Rule_getMath(void* r)
(uffi:def-function ("Rule_getMath" rule-get-math)
                   ((r :pointer-void))
                   :returning
                   :pointer-void) 

;; Function int Rule_isSetFormula(void* r)
(uffi:def-function ("Rule_isSetFormula" rule-is-set-formula2059)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun rule-is-set-formula (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (rule-is-set-formula2059 r))
      common-lisp:nil))) 

;; Function int Rule_isSetMath(void* r)
(uffi:def-function ("Rule_isSetMath" rule-is-set-math2060)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun rule-is-set-math (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (rule-is-set-math2060 r))
      common-lisp:nil))) 

;; Function void Rule_setFormula(void* r, char* string)
(uffi:def-function ("Rule_setFormula" rule-set-formula2061)
                   ((r :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun rule-set-formula (r string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (rule-set-formula2061 r string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Rule_setFormulaFromMath(void* r)
(uffi:def-function ("Rule_setFormulaFromMath" rule-set-formula-from-math)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; Function void Rule_setMath(void* r, void* math)
(uffi:def-function ("Rule_setMath" rule-set-math)
                   ((r :pointer-void) (math :pointer-void))
                   :returning
                   :void) 

;; Function void Rule_setMathFromFormula(void* r)
(uffi:def-function ("Rule_setMathFromFormula" rule-set-math-from-formula)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; enum{rule_type_rate, rule_type_scalar, rule_type_invalid} RuleType_t

(uffi:def-foreign-type rule-type-t :int) 
(common-lisp:defconstant +rule-type-rate+ 0) 
(common-lisp:defconstant +rule-type-scalar+ 1) 
(common-lisp:defconstant +rule-type-invalid+ 2) 
;; Function RuleType_t RuleType_forName(char* name)
(uffi:def-function ("RuleType_forName" rule-type-for-name2062)
                   ((name :cstring))
                   :returning
                   :int) 
(common-lisp:defun rule-type-for-name (name)
  (common-lisp:setq name (uffi:convert-to-cstring name))
  (common-lisp:let ()
    (common-lisp:unwind-protect (rule-type-for-name2062 name)
      common-lisp:nil
      (uffi:free-cstring name)))) 

;; Function char* RuleType_toString(RuleType_t rt)
(uffi:def-function ("RuleType_toString" rule-type-to-string2063)
                   ((rt :int))
                   :returning
                   :cstring) 
(common-lisp:defun rule-type-to-string (rt)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (rule-type-to-string2063 rt))
      common-lisp:nil))) 

;;void AssignmentRule_t
;; Function void* AssignmentRule_create()
(uffi:def-function ("AssignmentRule_create" assignment-rule-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* AssignmentRule_createWith(char* variable, void* math)
(uffi:def-function
 ("AssignmentRule_createWith" assignment-rule-create-with2064)
 ((variable :cstring) (math :pointer-void))
 :returning
 :pointer-void) 
(common-lisp:defun assignment-rule-create-with (variable math)
  (common-lisp:setq variable (uffi:convert-to-cstring variable))
  (common-lisp:let ()
    (common-lisp:unwind-protect (assignment-rule-create-with2064 variable math)
      common-lisp:nil
      (uffi:free-cstring variable)))) 

;; Function void AssignmentRule_free(void* ar)
(uffi:def-function ("AssignmentRule_free" assignment-rule-free)
                   ((ar :pointer-void))
                   :returning
                   :void) 

;; Function void AssignmentRule_initDefaults(void* ar)
(uffi:def-function
 ("AssignmentRule_initDefaults" assignment-rule-init-defaults)
 ((ar :pointer-void))
 :returning
 :void) 

;; Function RuleType_t AssignmentRule_getType(void* ar)
(uffi:def-function ("AssignmentRule_getType" assignment-rule-get-type)
                   ((ar :pointer-void))
                   :returning
                   :int) 

;; Function char* AssignmentRule_getVariable(void* ar)
(uffi:def-function
 ("AssignmentRule_getVariable" assignment-rule-get-variable2065)
 ((ar :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun assignment-rule-get-variable (ar)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (assignment-rule-get-variable2065 ar))
      common-lisp:nil))) 

;; Function int AssignmentRule_isSetVariable(void* ar)
(uffi:def-function
 ("AssignmentRule_isSetVariable" assignment-rule-is-set-variable2066)
 ((ar :pointer-void))
 :returning
 :int) 
(common-lisp:defun assignment-rule-is-set-variable (ar)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (assignment-rule-is-set-variable2066 ar))
      common-lisp:nil))) 

;; Function void AssignmentRule_setType(void* ar, RuleType_t rt)
(uffi:def-function ("AssignmentRule_setType" assignment-rule-set-type)
                   ((ar :pointer-void) (rt :int))
                   :returning
                   :void) 

;; Function void AssignmentRule_setVariable(void* ar, char* sid)
(uffi:def-function
 ("AssignmentRule_setVariable" assignment-rule-set-variable2067)
 ((ar :pointer-void) (sid :cstring))
 :returning
 :void) 
(common-lisp:defun assignment-rule-set-variable (ar sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (assignment-rule-set-variable2067 ar sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void RateRule_t
;; Function void* RateRule_create()
(uffi:def-function ("RateRule_create" rate-rule-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* RateRule_createWith(char* variable, void* math)
(uffi:def-function ("RateRule_createWith" rate-rule-create-with2068)
                   ((variable :cstring) (math :pointer-void))
                   :returning
                   :pointer-void) 
(common-lisp:defun rate-rule-create-with (variable math)
  (common-lisp:setq variable (uffi:convert-to-cstring variable))
  (common-lisp:let ()
    (common-lisp:unwind-protect (rate-rule-create-with2068 variable math)
      common-lisp:nil
      (uffi:free-cstring variable)))) 

;; Function void RateRule_free(void* rr)
(uffi:def-function ("RateRule_free" rate-rule-free)
                   ((rr :pointer-void))
                   :returning
                   :void) 

;; Function char* RateRule_getVariable(void* rr)
(uffi:def-function ("RateRule_getVariable" rate-rule-get-variable2069)
                   ((rr :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun rate-rule-get-variable (rr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (rate-rule-get-variable2069 rr))
      common-lisp:nil))) 

;; Function int RateRule_isSetVariable(void* rr)
(uffi:def-function ("RateRule_isSetVariable" rate-rule-is-set-variable2070)
                   ((rr :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun rate-rule-is-set-variable (rr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (rate-rule-is-set-variable2070 rr))
      common-lisp:nil))) 

;; Function void RateRule_setVariable(void* rr, char* sid)
(uffi:def-function ("RateRule_setVariable" rate-rule-set-variable2071)
                   ((rr :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun rate-rule-set-variable (rr sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (rate-rule-set-variable2071 rr sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void AlgebraicRule_t
;; Function void* AlgebraicRule_create()
(uffi:def-function ("AlgebraicRule_create" algebraic-rule-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* AlgebraicRule_createWith(char* formula)
(uffi:def-function ("AlgebraicRule_createWith" algebraic-rule-create-with2072)
                   ((formula :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun algebraic-rule-create-with (formula)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:let ()
    (common-lisp:unwind-protect (algebraic-rule-create-with2072 formula)
      common-lisp:nil
      (uffi:free-cstring formula)))) 

;; Function void* AlgebraicRule_createWithMath(void* math)
(uffi:def-function
 ("AlgebraicRule_createWithMath" algebraic-rule-create-with-math)
 ((math :pointer-void))
 :returning
 :pointer-void) 

;; Function void AlgebraicRule_free(void* ar)
(uffi:def-function ("AlgebraicRule_free" algebraic-rule-free)
                   ((ar :pointer-void))
                   :returning
                   :void) 

;;void CompartmentVolumeRule_t
;; Function void* CompartmentVolumeRule_create()
(uffi:def-function
 ("CompartmentVolumeRule_create" compartment-volume-rule-create)
 common-lisp:nil
 :returning
 :pointer-void) 

;; Function void* CompartmentVolumeRule_createWith(char* formula, RuleType_t type, char* compartment)
(uffi:def-function
 ("CompartmentVolumeRule_createWith" compartment-volume-rule-create-with2073)
 ((formula :cstring) (type :int) (compartment :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun compartment-volume-rule-create-with
                   (formula type compartment)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:setq compartment (uffi:convert-to-cstring compartment))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (compartment-volume-rule-create-with2073 formula type compartment)
      common-lisp:nil
      (uffi:free-cstring compartment)
      (uffi:free-cstring formula)))) 

;; Function void CompartmentVolumeRule_free(void* cvr)
(uffi:def-function ("CompartmentVolumeRule_free" compartment-volume-rule-free)
                   ((cvr :pointer-void))
                   :returning
                   :void) 

;; Function char* CompartmentVolumeRule_getCompartment(void* cvr)
(uffi:def-function
 ("CompartmentVolumeRule_getCompartment"
  compartment-volume-rule-get-compartment2074)
 ((cvr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun compartment-volume-rule-get-compartment (cvr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (compartment-volume-rule-get-compartment2074 cvr))
      common-lisp:nil))) 

;; Function int CompartmentVolumeRule_isSetCompartment(void* cvr)
(uffi:def-function
 ("CompartmentVolumeRule_isSetCompartment"
  compartment-volume-rule-is-set-compartment2075)
 ((cvr :pointer-void))
 :returning
 :int) 
(common-lisp:defun compartment-volume-rule-is-set-compartment (cvr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (compartment-volume-rule-is-set-compartment2075 cvr))
      common-lisp:nil))) 

;; Function void CompartmentVolumeRule_setCompartment(void* cvr, char* sname)
(uffi:def-function
 ("CompartmentVolumeRule_setCompartment"
  compartment-volume-rule-set-compartment2076)
 ((cvr :pointer-void) (sname :cstring))
 :returning
 :void) 
(common-lisp:defun compartment-volume-rule-set-compartment (cvr sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (compartment-volume-rule-set-compartment2076 cvr sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;;void ParameterRule_t
;; Function void* ParameterRule_create()
(uffi:def-function ("ParameterRule_create" parameter-rule-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* ParameterRule_createWith(char* formula, RuleType_t type, char* name)
(uffi:def-function ("ParameterRule_createWith" parameter-rule-create-with2077)
                   ((formula :cstring) (type :int) (name :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun parameter-rule-create-with (formula type name)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:setq name (uffi:convert-to-cstring name))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (parameter-rule-create-with2077 formula type name)
      common-lisp:nil
      (uffi:free-cstring name)
      (uffi:free-cstring formula)))) 

;; Function void ParameterRule_free(void* pr)
(uffi:def-function ("ParameterRule_free" parameter-rule-free)
                   ((pr :pointer-void))
                   :returning
                   :void) 

;; Function char* ParameterRule_getName(void* pr)
(uffi:def-function ("ParameterRule_getName" parameter-rule-get-name2078)
                   ((pr :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun parameter-rule-get-name (pr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (parameter-rule-get-name2078 pr))
      common-lisp:nil))) 

;; Function char* ParameterRule_getUnits(void* pr)
(uffi:def-function ("ParameterRule_getUnits" parameter-rule-get-units2079)
                   ((pr :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun parameter-rule-get-units (pr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (parameter-rule-get-units2079 pr))
      common-lisp:nil))) 

;; Function int ParameterRule_isSetName(void* pr)
(uffi:def-function ("ParameterRule_isSetName" parameter-rule-is-set-name2080)
                   ((pr :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-rule-is-set-name (pr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (parameter-rule-is-set-name2080 pr))
      common-lisp:nil))) 

;; Function int ParameterRule_isSetUnits(void* pr)
(uffi:def-function ("ParameterRule_isSetUnits" parameter-rule-is-set-units2081)
                   ((pr :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun parameter-rule-is-set-units (pr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (parameter-rule-is-set-units2081 pr))
      common-lisp:nil))) 

;; Function void ParameterRule_setName(void* pr, char* sname)
(uffi:def-function ("ParameterRule_setName" parameter-rule-set-name2082)
                   ((pr :pointer-void) (sname :cstring))
                   :returning
                   :void) 
(common-lisp:defun parameter-rule-set-name (pr sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-rule-set-name2082 pr sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;; Function void ParameterRule_setUnits(void* pr, char* sname)
(uffi:def-function ("ParameterRule_setUnits" parameter-rule-set-units2083)
                   ((pr :pointer-void) (sname :cstring))
                   :returning
                   :void) 
(common-lisp:defun parameter-rule-set-units (pr sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect (parameter-rule-set-units2083 pr sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;; Function void ParameterRule_unsetUnits(void* pr)
(uffi:def-function ("ParameterRule_unsetUnits" parameter-rule-unset-units)
                   ((pr :pointer-void))
                   :returning
                   :void) 

;;void SpeciesConcentrationRule_t
;; Function void* SpeciesConcentrationRule_create()
(uffi:def-function
 ("SpeciesConcentrationRule_create" species-concentration-rule-create)
 common-lisp:nil
 :returning
 :pointer-void) 

;; Function void* SpeciesConcentrationRule_createWith(char* formula, RuleType_t type, char* species)
(uffi:def-function
 ("SpeciesConcentrationRule_createWith"
  species-concentration-rule-create-with2084)
 ((formula :cstring) (type :int) (species :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun species-concentration-rule-create-with
                   (formula type species)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:setq species (uffi:convert-to-cstring species))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (species-concentration-rule-create-with2084 formula type species)
      common-lisp:nil
      (uffi:free-cstring species)
      (uffi:free-cstring formula)))) 

;; Function void SpeciesConcentrationRule_free(void* scr)
(uffi:def-function
 ("SpeciesConcentrationRule_free" species-concentration-rule-free)
 ((scr :pointer-void))
 :returning
 :void) 

;; Function char* SpeciesConcentrationRule_getSpecies(void* scr)
(uffi:def-function
 ("SpeciesConcentrationRule_getSpecies"
  species-concentration-rule-get-species2085)
 ((scr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun species-concentration-rule-get-species (scr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (species-concentration-rule-get-species2085 scr))
      common-lisp:nil))) 

;; Function int SpeciesConcentrationRule_isSetSpecies(void* scr)
(uffi:def-function
 ("SpeciesConcentrationRule_isSetSpecies"
  species-concentration-rule-is-set-species2086)
 ((scr :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-concentration-rule-is-set-species (scr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-concentration-rule-is-set-species2086 scr))
      common-lisp:nil))) 

;; Function void SpeciesConcentrationRule_setSpecies(void* scr, char* sname)
(uffi:def-function
 ("SpeciesConcentrationRule_setSpecies"
  species-concentration-rule-set-species2087)
 ((scr :pointer-void) (sname :cstring))
 :returning
 :void) 
(common-lisp:defun species-concentration-rule-set-species (scr sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (species-concentration-rule-set-species2087 scr sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;;void KineticLaw_t
;; Function void* KineticLaw_create()
(uffi:def-function ("KineticLaw_create" kinetic-law-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* KineticLaw_createWith(char* formula, char* timeUnits, char* substanceUnits)
(uffi:def-function ("KineticLaw_createWith" kinetic-law-create-with2088)
                   ((formula :cstring) (time-units :cstring)
                    (substance-units :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun kinetic-law-create-with (formula time-units substance-units)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:setq time-units (uffi:convert-to-cstring time-units))
  (common-lisp:setq substance-units (uffi:convert-to-cstring substance-units))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (kinetic-law-create-with2088 formula time-units substance-units)
      common-lisp:nil
      (uffi:free-cstring substance-units)
      (uffi:free-cstring time-units)
      (uffi:free-cstring formula)))) 

;; Function void KineticLaw_free(void* kl)
(uffi:def-function ("KineticLaw_free" kinetic-law-free)
                   ((kl :pointer-void))
                   :returning
                   :void) 

;; Function char* KineticLaw_getFormula(void* kl)
(uffi:def-function ("KineticLaw_getFormula" kinetic-law-get-formula2089)
                   ((kl :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun kinetic-law-get-formula (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (kinetic-law-get-formula2089 kl))
      common-lisp:nil))) 

;; Function void* KineticLaw_getMath(void* kl)
(uffi:def-function ("KineticLaw_getMath" kinetic-law-get-math)
                   ((kl :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* KineticLaw_getListOfParameters(void* kl)
(uffi:def-function
 ("KineticLaw_getListOfParameters" kinetic-law-get-list-of-parameters)
 ((kl :pointer-void))
 :returning
 :pointer-void) 

;; Function char* KineticLaw_getTimeUnits(void* kl)
(uffi:def-function ("KineticLaw_getTimeUnits" kinetic-law-get-time-units2090)
                   ((kl :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun kinetic-law-get-time-units (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (kinetic-law-get-time-units2090 kl))
      common-lisp:nil))) 

;; Function char* KineticLaw_getSubstanceUnits(void* kl)
(uffi:def-function
 ("KineticLaw_getSubstanceUnits" kinetic-law-get-substance-units2091)
 ((kl :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun kinetic-law-get-substance-units (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (kinetic-law-get-substance-units2091 kl))
      common-lisp:nil))) 

;; Function int KineticLaw_isSetFormula(void* kl)
(uffi:def-function ("KineticLaw_isSetFormula" kinetic-law-is-set-formula2092)
                   ((kl :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun kinetic-law-is-set-formula (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (kinetic-law-is-set-formula2092 kl))
      common-lisp:nil))) 

;; Function int KineticLaw_isSetMath(void* kl)
(uffi:def-function ("KineticLaw_isSetMath" kinetic-law-is-set-math2093)
                   ((kl :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun kinetic-law-is-set-math (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (kinetic-law-is-set-math2093 kl))
      common-lisp:nil))) 

;; Function int KineticLaw_isSetTimeUnits(void* kl)
(uffi:def-function
 ("KineticLaw_isSetTimeUnits" kinetic-law-is-set-time-units2094)
 ((kl :pointer-void))
 :returning
 :int) 
(common-lisp:defun kinetic-law-is-set-time-units (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (kinetic-law-is-set-time-units2094 kl))
      common-lisp:nil))) 

;; Function int KineticLaw_isSetSubstanceUnits(void* kl)
(uffi:def-function
 ("KineticLaw_isSetSubstanceUnits" kinetic-law-is-set-substance-units2095)
 ((kl :pointer-void))
 :returning
 :int) 
(common-lisp:defun kinetic-law-is-set-substance-units (kl)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (kinetic-law-is-set-substance-units2095 kl))
      common-lisp:nil))) 

;; Function void KineticLaw_setFormula(void* kl, char* string)
(uffi:def-function ("KineticLaw_setFormula" kinetic-law-set-formula2096)
                   ((kl :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun kinetic-law-set-formula (kl string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (kinetic-law-set-formula2096 kl string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void KineticLaw_setFormulaFromMath(void* kl)
(uffi:def-function
 ("KineticLaw_setFormulaFromMath" kinetic-law-set-formula-from-math)
 ((kl :pointer-void))
 :returning
 :void) 

;; Function void KineticLaw_setMath(void* kl, void* math)
(uffi:def-function ("KineticLaw_setMath" kinetic-law-set-math)
                   ((kl :pointer-void) (math :pointer-void))
                   :returning
                   :void) 

;; Function void KineticLaw_setMathFromFormula(void* kl)
(uffi:def-function
 ("KineticLaw_setMathFromFormula" kinetic-law-set-math-from-formula)
 ((kl :pointer-void))
 :returning
 :void) 

;; Function void KineticLaw_setTimeUnits(void* kl, char* sname)
(uffi:def-function ("KineticLaw_setTimeUnits" kinetic-law-set-time-units2097)
                   ((kl :pointer-void) (sname :cstring))
                   :returning
                   :void) 
(common-lisp:defun kinetic-law-set-time-units (kl sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect (kinetic-law-set-time-units2097 kl sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;; Function void KineticLaw_setSubstanceUnits(void* kl, char* sname)
(uffi:def-function
 ("KineticLaw_setSubstanceUnits" kinetic-law-set-substance-units2098)
 ((kl :pointer-void) (sname :cstring))
 :returning
 :void) 
(common-lisp:defun kinetic-law-set-substance-units (kl sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect (kinetic-law-set-substance-units2098 kl sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;; Function void KineticLaw_addParameter(void* kl, void* p)
(uffi:def-function ("KineticLaw_addParameter" kinetic-law-add-parameter)
                   ((kl :pointer-void) (p :pointer-void))
                   :returning
                   :void) 

;; Function void* KineticLaw_getParameter(void* kl, unsigned int n)
(uffi:def-function ("KineticLaw_getParameter" kinetic-law-get-parameter)
                   ((kl :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function unsigned int KineticLaw_getNumParameters(void* kl)
(uffi:def-function
 ("KineticLaw_getNumParameters" kinetic-law-get-num-parameters)
 ((kl :pointer-void))
 :returning
 :unsigned-int) 

;; Function void KineticLaw_unsetTimeUnits(void* kl)
(uffi:def-function ("KineticLaw_unsetTimeUnits" kinetic-law-unset-time-units)
                   ((kl :pointer-void))
                   :returning
                   :void) 

;; Function void KineticLaw_unsetSubstanceUnits(void* kl)
(uffi:def-function
 ("KineticLaw_unsetSubstanceUnits" kinetic-law-unset-substance-units)
 ((kl :pointer-void))
 :returning
 :void) 

;;void SimpleSpeciesReference_t
;; Function char* SimpleSpeciesReference_getSpecies(void* ssr)
(uffi:def-function
 ("SimpleSpeciesReference_getSpecies" simple-species-reference-get-species2099)
 ((ssr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun simple-species-reference-get-species (ssr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (simple-species-reference-get-species2099 ssr))
      common-lisp:nil))) 

;; Function int SimpleSpeciesReference_isSetSpecies(void* ssr)
(uffi:def-function
 ("SimpleSpeciesReference_isSetSpecies"
  simple-species-reference-is-set-species2100)
 ((ssr :pointer-void))
 :returning
 :int) 
(common-lisp:defun simple-species-reference-is-set-species (ssr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (simple-species-reference-is-set-species2100 ssr))
      common-lisp:nil))) 

;; Function void SimpleSpeciesReference_setSpecies(void* ssr, char* sid)
(uffi:def-function
 ("SimpleSpeciesReference_setSpecies" simple-species-reference-set-species2101)
 ((ssr :pointer-void) (sid :cstring))
 :returning
 :void) 
(common-lisp:defun simple-species-reference-set-species (ssr sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (simple-species-reference-set-species2101 ssr sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function int SimpleSpeciesReferenceCmp(char* sid, void* ssr)
(uffi:def-function
 ("SimpleSpeciesReferenceCmp" simple-species-reference-cmp2102)
 ((sid :cstring) (ssr :pointer-void))
 :returning
 :int) 
(common-lisp:defun simple-species-reference-cmp (sid ssr)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (simple-species-reference-cmp2102 sid ssr)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void ModifierSpeciesReference_t
;; Function void* ModifierSpeciesReference_create()
(uffi:def-function
 ("ModifierSpeciesReference_create" modifier-species-reference-create)
 common-lisp:nil
 :returning
 :pointer-void) 

;; Function void* ModifierSpeciesReference_createWith(char* species)
(uffi:def-function
 ("ModifierSpeciesReference_createWith"
  modifier-species-reference-create-with2103)
 ((species :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun modifier-species-reference-create-with (species)
  (common-lisp:setq species (uffi:convert-to-cstring species))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (modifier-species-reference-create-with2103 species)
      common-lisp:nil
      (uffi:free-cstring species)))) 

;; Function void ModifierSpeciesReference_free(void* msr)
(uffi:def-function
 ("ModifierSpeciesReference_free" modifier-species-reference-free)
 ((msr :pointer-void))
 :returning
 :void) 

;; Function char* ModifierSpeciesReference_getSpecies(void* msr)
(uffi:def-function
 ("ModifierSpeciesReference_getSpecies"
  modifier-species-reference-get-species2104)
 ((msr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun modifier-species-reference-get-species (msr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (modifier-species-reference-get-species2104 msr))
      common-lisp:nil))) 

;; Function int ModifierSpeciesReference_isSetSpecies(void* msr)
(uffi:def-function
 ("ModifierSpeciesReference_isSetSpecies"
  modifier-species-reference-is-set-species2105)
 ((msr :pointer-void))
 :returning
 :int) 
(common-lisp:defun modifier-species-reference-is-set-species (msr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (modifier-species-reference-is-set-species2105 msr))
      common-lisp:nil))) 

;; Function void ModifierSpeciesReference_setSpecies(void* msr, char* sid)
(uffi:def-function
 ("ModifierSpeciesReference_setSpecies"
  modifier-species-reference-set-species2106)
 ((msr :pointer-void) (sid :cstring))
 :returning
 :void) 
(common-lisp:defun modifier-species-reference-set-species (msr sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (modifier-species-reference-set-species2106 msr sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void SpeciesReference_t
;; Function void* SpeciesReference_create()
(uffi:def-function ("SpeciesReference_create" species-reference-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* SpeciesReference_createWith(char* species, double stoichiometry, int denominator)
(uffi:def-function
 ("SpeciesReference_createWith" species-reference-create-with2107)
 ((species :cstring) (stoichiometry :double) (denominator :int))
 :returning
 :pointer-void) 
(common-lisp:defun species-reference-create-with
                   (species stoichiometry denominator)
  (common-lisp:setq species (uffi:convert-to-cstring species))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (species-reference-create-with2107 species stoichiometry denominator)
      common-lisp:nil
      (uffi:free-cstring species)))) 

;; Function void SpeciesReference_free(void* sr)
(uffi:def-function ("SpeciesReference_free" species-reference-free)
                   ((sr :pointer-void))
                   :returning
                   :void) 

;; Function void SpeciesReference_initDefaults(void* sr)
(uffi:def-function
 ("SpeciesReference_initDefaults" species-reference-init-defaults)
 ((sr :pointer-void))
 :returning
 :void) 

;; Function char* SpeciesReference_getSpecies(void* sr)
(uffi:def-function
 ("SpeciesReference_getSpecies" species-reference-get-species2108)
 ((sr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun species-reference-get-species (sr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (species-reference-get-species2108 sr))
      common-lisp:nil))) 

;; Function double SpeciesReference_getStoichiometry(void* sr)
(uffi:def-function
 ("SpeciesReference_getStoichiometry" species-reference-get-stoichiometry)
 ((sr :pointer-void))
 :returning
 :double) 

;; Function void* SpeciesReference_getStoichiometryMath(void* sr)
(uffi:def-function
 ("SpeciesReference_getStoichiometryMath"
  species-reference-get-stoichiometry-math)
 ((sr :pointer-void))
 :returning
 :pointer-void) 

;; Function int SpeciesReference_getDenominator(void* sr)
(uffi:def-function
 ("SpeciesReference_getDenominator" species-reference-get-denominator)
 ((sr :pointer-void))
 :returning
 :int) 

;; Function int SpeciesReference_isSetSpecies(void* sr)
(uffi:def-function
 ("SpeciesReference_isSetSpecies" species-reference-is-set-species2109)
 ((sr :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-reference-is-set-species (sr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-reference-is-set-species2109 sr))
      common-lisp:nil))) 

;; Function int SpeciesReference_isSetStoichiometryMath(void* sr)
(uffi:def-function
 ("SpeciesReference_isSetStoichiometryMath"
  species-reference-is-set-stoichiometry-math2110)
 ((sr :pointer-void))
 :returning
 :int) 
(common-lisp:defun species-reference-is-set-stoichiometry-math (sr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (species-reference-is-set-stoichiometry-math2110 sr))
      common-lisp:nil))) 

;; Function void SpeciesReference_setSpecies(void* sr, char* sname)
(uffi:def-function
 ("SpeciesReference_setSpecies" species-reference-set-species2111)
 ((sr :pointer-void) (sname :cstring))
 :returning
 :void) 
(common-lisp:defun species-reference-set-species (sr sname)
  (common-lisp:setq sname (uffi:convert-to-cstring sname))
  (common-lisp:let ()
    (common-lisp:unwind-protect (species-reference-set-species2111 sr sname)
      common-lisp:nil
      (uffi:free-cstring sname)))) 

;; Function void SpeciesReference_setStoichiometry(void* sr, double value)
(uffi:def-function
 ("SpeciesReference_setStoichiometry" species-reference-set-stoichiometry)
 ((sr :pointer-void) (value :double))
 :returning
 :void) 

;; Function void SpeciesReference_setStoichiometryMath(void* sr, void* math)
(uffi:def-function
 ("SpeciesReference_setStoichiometryMath"
  species-reference-set-stoichiometry-math)
 ((sr :pointer-void) (math :pointer-void))
 :returning
 :void) 

;; Function void SpeciesReference_setDenominator(void* sr, int value)
(uffi:def-function
 ("SpeciesReference_setDenominator" species-reference-set-denominator)
 ((sr :pointer-void) (value :int))
 :returning
 :void) 

;;void Reaction_t
;; Function void* Reaction_create()
(uffi:def-function ("Reaction_create" reaction-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Reaction_createWith(char* sid, void* kl, int reversible, int fast)
(uffi:def-function ("Reaction_createWith" reaction-create-with2112)
                   ((sid :cstring) (kl :pointer-void) (reversible :int)
                    (fast :int))
                   :returning
                   :pointer-void) 
(common-lisp:defun reaction-create-with (sid kl reversible fast)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (reaction-create-with2112 sid kl reversible fast)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Reaction_free(void* r)
(uffi:def-function ("Reaction_free" reaction-free)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; Function void Reaction_initDefaults(void* r)
(uffi:def-function ("Reaction_initDefaults" reaction-init-defaults)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; Function char* Reaction_getId(void* r)
(uffi:def-function ("Reaction_getId" reaction-get-id2113)
                   ((r :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun reaction-get-id (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (reaction-get-id2113 r))
      common-lisp:nil))) 

;; Function char* Reaction_getName(void* r)
(uffi:def-function ("Reaction_getName" reaction-get-name2114)
                   ((r :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun reaction-get-name (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (reaction-get-name2114 r))
      common-lisp:nil))) 

;; Function void* Reaction_getKineticLaw(void* r)
(uffi:def-function ("Reaction_getKineticLaw" reaction-get-kinetic-law)
                   ((r :pointer-void))
                   :returning
                   :pointer-void) 

;; Function int Reaction_getReversible(void* r)
(uffi:def-function ("Reaction_getReversible" reaction-get-reversible2115)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-get-reversible (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (reaction-get-reversible2115 r))
      common-lisp:nil))) 

;; Function int Reaction_getFast(void* r)
(uffi:def-function ("Reaction_getFast" reaction-get-fast2116)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-get-fast (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (reaction-get-fast2116 r))
      common-lisp:nil))) 

;; Function int Reaction_isSetId(void* r)
(uffi:def-function ("Reaction_isSetId" reaction-is-set-id2117)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-is-set-id (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (reaction-is-set-id2117 r))
      common-lisp:nil))) 

;; Function int Reaction_isSetName(void* r)
(uffi:def-function ("Reaction_isSetName" reaction-is-set-name2118)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-is-set-name (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (reaction-is-set-name2118 r))
      common-lisp:nil))) 

;; Function int Reaction_isSetKineticLaw(void* r)
(uffi:def-function ("Reaction_isSetKineticLaw" reaction-is-set-kinetic-law2119)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-is-set-kinetic-law (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (reaction-is-set-kinetic-law2119 r))
      common-lisp:nil))) 

;; Function int Reaction_isSetFast(void* r)
(uffi:def-function ("Reaction_isSetFast" reaction-is-set-fast2120)
                   ((r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-is-set-fast (r)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (reaction-is-set-fast2120 r))
      common-lisp:nil))) 

;; Function void Reaction_setId(void* r, char* sid)
(uffi:def-function ("Reaction_setId" reaction-set-id2121)
                   ((r :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun reaction-set-id (r sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-set-id2121 r sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Reaction_setName(void* r, char* string)
(uffi:def-function ("Reaction_setName" reaction-set-name2122)
                   ((r :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun reaction-set-name (r string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-set-name2122 r string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Reaction_setKineticLaw(void* r, void* kl)
(uffi:def-function ("Reaction_setKineticLaw" reaction-set-kinetic-law)
                   ((r :pointer-void) (kl :pointer-void))
                   :returning
                   :void) 

;; Function void Reaction_setReversible(void* r, int value)
(uffi:def-function ("Reaction_setReversible" reaction-set-reversible2123)
                   ((r :pointer-void) (value :int))
                   :returning
                   :void) 
(common-lisp:defun reaction-set-reversible (r value)
  (common-lisp:setq value (uffi::cboolm value))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-set-reversible2123 r value)
      common-lisp:nil))) 

;; Function void Reaction_setFast(void* r, int value)
(uffi:def-function ("Reaction_setFast" reaction-set-fast2124)
                   ((r :pointer-void) (value :int))
                   :returning
                   :void) 
(common-lisp:defun reaction-set-fast (r value)
  (common-lisp:setq value (uffi::cboolm value))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-set-fast2124 r value)
      common-lisp:nil))) 

;; Function void* Reaction_getListOfReactants(void* r)
(uffi:def-function
 ("Reaction_getListOfReactants" reaction-get-list-of-reactants)
 ((r :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Reaction_getListOfProducts(void* r)
(uffi:def-function ("Reaction_getListOfProducts" reaction-get-list-of-products)
                   ((r :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Reaction_getListOfModifiers(void* r)
(uffi:def-function
 ("Reaction_getListOfModifiers" reaction-get-list-of-modifiers)
 ((r :pointer-void))
 :returning
 :pointer-void) 

;; Function void Reaction_addReactant(void* r, void* sr)
(uffi:def-function ("Reaction_addReactant" reaction-add-reactant)
                   ((r :pointer-void) (sr :pointer-void))
                   :returning
                   :void) 

;; Function void Reaction_addProduct(void* r, void* sr)
(uffi:def-function ("Reaction_addProduct" reaction-add-product)
                   ((r :pointer-void) (sr :pointer-void))
                   :returning
                   :void) 

;; Function void Reaction_addModifier(void* r, void* msr)
(uffi:def-function ("Reaction_addModifier" reaction-add-modifier)
                   ((r :pointer-void) (msr :pointer-void))
                   :returning
                   :void) 

;; Function void* Reaction_getReactant(void* r, unsigned int n)
(uffi:def-function ("Reaction_getReactant" reaction-get-reactant)
                   ((r :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Reaction_getReactantById(void* r, char* sid)
(uffi:def-function ("Reaction_getReactantById" reaction-get-reactant-by-id2125)
                   ((r :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun reaction-get-reactant-by-id (r sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-get-reactant-by-id2125 r sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Reaction_getProduct(void* r, unsigned int n)
(uffi:def-function ("Reaction_getProduct" reaction-get-product)
                   ((r :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Reaction_getProductById(void* r, char* sid)
(uffi:def-function ("Reaction_getProductById" reaction-get-product-by-id2126)
                   ((r :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun reaction-get-product-by-id (r sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-get-product-by-id2126 r sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Reaction_getModifier(void* r, unsigned int n)
(uffi:def-function ("Reaction_getModifier" reaction-get-modifier)
                   ((r :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Reaction_getModifierById(void* r, char* sid)
(uffi:def-function ("Reaction_getModifierById" reaction-get-modifier-by-id2127)
                   ((r :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun reaction-get-modifier-by-id (r sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-get-modifier-by-id2127 r sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function unsigned int Reaction_getNumReactants(void* r)
(uffi:def-function ("Reaction_getNumReactants" reaction-get-num-reactants)
                   ((r :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Reaction_getNumProducts(void* r)
(uffi:def-function ("Reaction_getNumProducts" reaction-get-num-products)
                   ((r :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Reaction_getNumModifiers(void* r)
(uffi:def-function ("Reaction_getNumModifiers" reaction-get-num-modifiers)
                   ((r :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void Reaction_unsetName(void* r)
(uffi:def-function ("Reaction_unsetName" reaction-unset-name)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; Function void Reaction_unsetKineticLaw(void* r)
(uffi:def-function ("Reaction_unsetKineticLaw" reaction-unset-kinetic-law)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; Function void Reaction_unsetFast(void* r)
(uffi:def-function ("Reaction_unsetFast" reaction-unset-fast)
                   ((r :pointer-void))
                   :returning
                   :void) 

;; Function int ReactionIdCmp(char* sid, void* r)
(uffi:def-function ("ReactionIdCmp" reaction-id-cmp2128)
                   ((sid :cstring) (r :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun reaction-id-cmp (sid r)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (reaction-id-cmp2128 sid r)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void EventAssignment_t
;; Function void* EventAssignment_create()
(uffi:def-function ("EventAssignment_create" event-assignment-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* EventAssignment_createWith(char* variable, void* math)
(uffi:def-function
 ("EventAssignment_createWith" event-assignment-create-with2129)
 ((variable :cstring) (math :pointer-void))
 :returning
 :pointer-void) 
(common-lisp:defun event-assignment-create-with (variable math)
  (common-lisp:setq variable (uffi:convert-to-cstring variable))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (event-assignment-create-with2129 variable math)
      common-lisp:nil
      (uffi:free-cstring variable)))) 

;; Function void EventAssignment_free(void* ea)
(uffi:def-function ("EventAssignment_free" event-assignment-free)
                   ((ea :pointer-void))
                   :returning
                   :void) 

;; Function char* EventAssignment_getVariable(void* ea)
(uffi:def-function
 ("EventAssignment_getVariable" event-assignment-get-variable2130)
 ((ea :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun event-assignment-get-variable (ea)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (event-assignment-get-variable2130 ea))
      common-lisp:nil))) 

;; Function void* EventAssignment_getMath(void* ea)
(uffi:def-function ("EventAssignment_getMath" event-assignment-get-math)
                   ((ea :pointer-void))
                   :returning
                   :pointer-void) 

;; Function int EventAssignment_isSetVariable(void* ea)
(uffi:def-function
 ("EventAssignment_isSetVariable" event-assignment-is-set-variable2131)
 ((ea :pointer-void))
 :returning
 :int) 
(common-lisp:defun event-assignment-is-set-variable (ea)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (event-assignment-is-set-variable2131 ea))
      common-lisp:nil))) 

;; Function int EventAssignment_isSetMath(void* ea)
(uffi:def-function
 ("EventAssignment_isSetMath" event-assignment-is-set-math2132)
 ((ea :pointer-void))
 :returning
 :int) 
(common-lisp:defun event-assignment-is-set-math (ea)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi::lboolm (event-assignment-is-set-math2132 ea))
      common-lisp:nil))) 

;; Function void EventAssignment_setVariable(void* ea, char* sid)
(uffi:def-function
 ("EventAssignment_setVariable" event-assignment-set-variable2133)
 ((ea :pointer-void) (sid :cstring))
 :returning
 :void) 
(common-lisp:defun event-assignment-set-variable (ea sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (event-assignment-set-variable2133 ea sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void EventAssignment_setMath(void* ea, void* math)
(uffi:def-function ("EventAssignment_setMath" event-assignment-set-math)
                   ((ea :pointer-void) (math :pointer-void))
                   :returning
                   :void) 

;;void Event_t
;; Function void* Event_create()
(uffi:def-function ("Event_create" event-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Event_createWith(char* sid, void* trigger)
(uffi:def-function ("Event_createWith" event-create-with2134)
                   ((sid :cstring) (trigger :pointer-void))
                   :returning
                   :pointer-void) 
(common-lisp:defun event-create-with (sid trigger)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (event-create-with2134 sid trigger)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Event_free(void* e)
(uffi:def-function ("Event_free" event-free)
                   ((e :pointer-void))
                   :returning
                   :void) 

;; Function char* Event_getId(void* e)
(uffi:def-function ("Event_getId" event-get-id2135)
                   ((e :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun event-get-id (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (event-get-id2135 e))
      common-lisp:nil))) 

;; Function char* Event_getName(void* e)
(uffi:def-function ("Event_getName" event-get-name2136)
                   ((e :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun event-get-name (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (event-get-name2136 e))
      common-lisp:nil))) 

;; Function void* Event_getTrigger(void* e)
(uffi:def-function ("Event_getTrigger" event-get-trigger)
                   ((e :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Event_getDelay(void* e)
(uffi:def-function ("Event_getDelay" event-get-delay)
                   ((e :pointer-void))
                   :returning
                   :pointer-void) 

;; Function char* Event_getTimeUnits(void* e)
(uffi:def-function ("Event_getTimeUnits" event-get-time-units2137)
                   ((e :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun event-get-time-units (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (event-get-time-units2137 e))
      common-lisp:nil))) 

;; Function int Event_isSetId(void* e)
(uffi:def-function ("Event_isSetId" event-is-set-id2138)
                   ((e :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun event-is-set-id (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (event-is-set-id2138 e))
      common-lisp:nil))) 

;; Function int Event_isSetName(void* e)
(uffi:def-function ("Event_isSetName" event-is-set-name2139)
                   ((e :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun event-is-set-name (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (event-is-set-name2139 e))
      common-lisp:nil))) 

;; Function int Event_isSetTrigger(void* e)
(uffi:def-function ("Event_isSetTrigger" event-is-set-trigger2140)
                   ((e :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun event-is-set-trigger (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (event-is-set-trigger2140 e))
      common-lisp:nil))) 

;; Function int Event_isSetDelay(void* e)
(uffi:def-function ("Event_isSetDelay" event-is-set-delay2141)
                   ((e :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun event-is-set-delay (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (event-is-set-delay2141 e))
      common-lisp:nil))) 

;; Function int Event_isSetTimeUnits(void* e)
(uffi:def-function ("Event_isSetTimeUnits" event-is-set-time-units2142)
                   ((e :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun event-is-set-time-units (e)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (event-is-set-time-units2142 e))
      common-lisp:nil))) 

;; Function void Event_setId(void* e, char* sid)
(uffi:def-function ("Event_setId" event-set-id2143)
                   ((e :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun event-set-id (e sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (event-set-id2143 e sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Event_setName(void* e, char* string)
(uffi:def-function ("Event_setName" event-set-name2144)
                   ((e :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun event-set-name (e string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (event-set-name2144 e string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Event_setTrigger(void* e, void* math)
(uffi:def-function ("Event_setTrigger" event-set-trigger)
                   ((e :pointer-void) (math :pointer-void))
                   :returning
                   :void) 

;; Function void Event_setDelay(void* e, void* math)
(uffi:def-function ("Event_setDelay" event-set-delay)
                   ((e :pointer-void) (math :pointer-void))
                   :returning
                   :void) 

;; Function void Event_setTimeUnits(void* e, char* sid)
(uffi:def-function ("Event_setTimeUnits" event-set-time-units2145)
                   ((e :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun event-set-time-units (e sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (event-set-time-units2145 e sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Event_unsetId(void* e)
(uffi:def-function ("Event_unsetId" event-unset-id)
                   ((e :pointer-void))
                   :returning
                   :void) 

;; Function void Event_unsetName(void* e)
(uffi:def-function ("Event_unsetName" event-unset-name)
                   ((e :pointer-void))
                   :returning
                   :void) 

;; Function void Event_unsetDelay(void* e)
(uffi:def-function ("Event_unsetDelay" event-unset-delay)
                   ((e :pointer-void))
                   :returning
                   :void) 

;; Function void Event_unsetTimeUnits(void* e)
(uffi:def-function ("Event_unsetTimeUnits" event-unset-time-units)
                   ((e :pointer-void))
                   :returning
                   :void) 

;; Function void Event_addEventAssignment(void* e, void* ea)
(uffi:def-function ("Event_addEventAssignment" event-add-event-assignment)
                   ((e :pointer-void) (ea :pointer-void))
                   :returning
                   :void) 

;; Function void* Event_getListOfEventAssignments(void* e)
(uffi:def-function
 ("Event_getListOfEventAssignments" event-get-list-of-event-assignments)
 ((e :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Event_getEventAssignment(void* e, unsigned int n)
(uffi:def-function ("Event_getEventAssignment" event-get-event-assignment)
                   ((e :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function unsigned int Event_getNumEventAssignments(void* e)
(uffi:def-function
 ("Event_getNumEventAssignments" event-get-num-event-assignments)
 ((e :pointer-void))
 :returning
 :unsigned-int) 

;; Function int EventIdCmp(char* sid, void* e)
(uffi:def-function ("EventIdCmp" event-id-cmp2146)
                   ((sid :cstring) (e :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun event-id-cmp (sid e)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (event-id-cmp2146 sid e)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;;void Model_t
;; Function void* Model_create()
(uffi:def-function ("Model_create" model-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* Model_createWith(char* sid)
(uffi:def-function ("Model_createWith" model-create-with2147)
                   ((sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-create-with (sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-create-with2147 sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_createWithName(char* string)
(uffi:def-function ("Model_createWithName" model-create-with-name2148)
                   ((string :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-create-with-name (string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-create-with-name2148 string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Model_free(void* m)
(uffi:def-function ("Model_free" model-free)
                   ((m :pointer-void))
                   :returning
                   :void) 

;; Function char* Model_getId(void* m)
(uffi:def-function ("Model_getId" model-get-id2149)
                   ((m :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun model-get-id (m)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (model-get-id2149 m))
      common-lisp:nil))) 

;; Function char* Model_getName(void* m)
(uffi:def-function ("Model_getName" model-get-name2150)
                   ((m :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun model-get-name (m)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (model-get-name2150 m))
      common-lisp:nil))) 

;; Function int Model_isSetId(void* m)
(uffi:def-function ("Model_isSetId" model-is-set-id2151)
                   ((m :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun model-is-set-id (m)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (model-is-set-id2151 m))
      common-lisp:nil))) 

;; Function int Model_isSetName(void* m)
(uffi:def-function ("Model_isSetName" model-is-set-name2152)
                   ((m :pointer-void))
                   :returning
                   :int) 
(common-lisp:defun model-is-set-name (m)
  (common-lisp:let ()
    (common-lisp:unwind-protect (uffi::lboolm (model-is-set-name2152 m))
      common-lisp:nil))) 

;; Function void Model_setId(void* m, char* sid)
(uffi:def-function ("Model_setId" model-set-id2153)
                   ((m :pointer-void) (sid :cstring))
                   :returning
                   :void) 
(common-lisp:defun model-set-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-set-id2153 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void Model_setName(void* m, char* string)
(uffi:def-function ("Model_setName" model-set-name2154)
                   ((m :pointer-void) (string :cstring))
                   :returning
                   :void) 
(common-lisp:defun model-set-name (m string)
  (common-lisp:setq string (uffi:convert-to-cstring string))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-set-name2154 m string)
      common-lisp:nil
      (uffi:free-cstring string)))) 

;; Function void Model_unsetId(void* m)
(uffi:def-function ("Model_unsetId" model-unset-id)
                   ((m :pointer-void))
                   :returning
                   :void) 

;; Function void Model_unsetName(void* m)
(uffi:def-function ("Model_unsetName" model-unset-name)
                   ((m :pointer-void))
                   :returning
                   :void) 

;; Function void* Model_createFunctionDefinition(void* m)
(uffi:def-function
 ("Model_createFunctionDefinition" model-create-function-definition)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_createUnitDefinition(void* m)
(uffi:def-function ("Model_createUnitDefinition" model-create-unit-definition)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createUnit(void* m)
(uffi:def-function ("Model_createUnit" model-create-unit)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createCompartment(void* m)
(uffi:def-function ("Model_createCompartment" model-create-compartment)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createSpecies(void* m)
(uffi:def-function ("Model_createSpecies" model-create-species)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createParameter(void* m)
(uffi:def-function ("Model_createParameter" model-create-parameter)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createAssignmentRule(void* m)
(uffi:def-function ("Model_createAssignmentRule" model-create-assignment-rule)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createRateRule(void* m)
(uffi:def-function ("Model_createRateRule" model-create-rate-rule)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createAlgebraicRule(void* m)
(uffi:def-function ("Model_createAlgebraicRule" model-create-algebraic-rule)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createCompartmentVolumeRule(void* m)
(uffi:def-function
 ("Model_createCompartmentVolumeRule" model-create-compartment-volume-rule)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_createParameterRule(void* m)
(uffi:def-function ("Model_createParameterRule" model-create-parameter-rule)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createSpeciesConcentrationRule(void* m)
(uffi:def-function
 ("Model_createSpeciesConcentrationRule"
  model-create-species-concentration-rule)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_createReaction(void* m)
(uffi:def-function ("Model_createReaction" model-create-reaction)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createReactant(void* m)
(uffi:def-function ("Model_createReactant" model-create-reactant)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createProduct(void* m)
(uffi:def-function ("Model_createProduct" model-create-product)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createModifier(void* m)
(uffi:def-function ("Model_createModifier" model-create-modifier)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createKineticLaw(void* m)
(uffi:def-function ("Model_createKineticLaw" model-create-kinetic-law)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createKineticLawParameter(void* m)
(uffi:def-function
 ("Model_createKineticLawParameter" model-create-kinetic-law-parameter)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_createEvent(void* m)
(uffi:def-function ("Model_createEvent" model-create-event)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_createEventAssignment(void* m)
(uffi:def-function
 ("Model_createEventAssignment" model-create-event-assignment)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void Model_addFunctionDefinition(void* m, void* fd)
(uffi:def-function
 ("Model_addFunctionDefinition" model-add-function-definition)
 ((m :pointer-void) (fd :pointer-void))
 :returning
 :void) 

;; Function void Model_addUnitDefinition(void* m, void* ud)
(uffi:def-function ("Model_addUnitDefinition" model-add-unit-definition)
                   ((m :pointer-void) (ud :pointer-void))
                   :returning
                   :void) 

;; Function void Model_addCompartment(void* m, void* c)
(uffi:def-function ("Model_addCompartment" model-add-compartment)
                   ((m :pointer-void) (c :pointer-void))
                   :returning
                   :void) 

;; Function void Model_addSpecies(void* m, void* s)
(uffi:def-function ("Model_addSpecies" model-add-species)
                   ((m :pointer-void) (s :pointer-void))
                   :returning
                   :void) 

;; Function void Model_addParameter(void* m, void* p)
(uffi:def-function ("Model_addParameter" model-add-parameter)
                   ((m :pointer-void) (p :pointer-void))
                   :returning
                   :void) 

;; Function void Model_addRule(void* m, void* r)
(uffi:def-function ("Model_addRule" model-add-rule)
                   ((m :pointer-void) (r :pointer-void))
                   :returning
                   :void) 

;; Function void Model_addReaction(void* m, void* r)
(uffi:def-function ("Model_addReaction" model-add-reaction)
                   ((m :pointer-void) (r :pointer-void))
                   :returning
                   :void) 

;; Function void Model_addEvent(void* m, void* e)
(uffi:def-function ("Model_addEvent" model-add-event)
                   ((m :pointer-void) (e :pointer-void))
                   :returning
                   :void) 

;; Function void* Model_getListOfFunctionDefinitions(void* m)
(uffi:def-function
 ("Model_getListOfFunctionDefinitions" model-get-list-of-function-definitions)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_getListOfUnitDefinitions(void* m)
(uffi:def-function
 ("Model_getListOfUnitDefinitions" model-get-list-of-unit-definitions)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_getListOfCompartments(void* m)
(uffi:def-function
 ("Model_getListOfCompartments" model-get-list-of-compartments)
 ((m :pointer-void))
 :returning
 :pointer-void) 

;; Function void* Model_getListOfSpecies(void* m)
(uffi:def-function ("Model_getListOfSpecies" model-get-list-of-species)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_getListOfParameters(void* m)
(uffi:def-function ("Model_getListOfParameters" model-get-list-of-parameters)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_getListOfRules(void* m)
(uffi:def-function ("Model_getListOfRules" model-get-list-of-rules)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_getListOfReactions(void* m)
(uffi:def-function ("Model_getListOfReactions" model-get-list-of-reactions)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_getListOfEvents(void* m)
(uffi:def-function ("Model_getListOfEvents" model-get-list-of-events)
                   ((m :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Model_getListOfByTypecode(void* m, SBMLTypeCode_t type)
(uffi:def-function ("Model_getListOfByTypecode" model-get-list-of-by-typecode)
                   ((m :pointer-void) (type :int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getFunctionDefinition(void* m, unsigned int n)
(uffi:def-function
 ("Model_getFunctionDefinition" model-get-function-definition)
 ((m :pointer-void) (n :unsigned-int))
 :returning
 :pointer-void) 

;; Function void* Model_getFunctionDefinitionById(void* m, char* sid)
(uffi:def-function
 ("Model_getFunctionDefinitionById" model-get-function-definition-by-id2155)
 ((m :pointer-void) (sid :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun model-get-function-definition-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-function-definition-by-id2155 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_getUnitDefinition(void* m, unsigned int n)
(uffi:def-function ("Model_getUnitDefinition" model-get-unit-definition)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getUnitDefinitionById(void* m, char* sid)
(uffi:def-function
 ("Model_getUnitDefinitionById" model-get-unit-definition-by-id2156)
 ((m :pointer-void) (sid :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun model-get-unit-definition-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-unit-definition-by-id2156 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_getCompartment(void* m, unsigned int n)
(uffi:def-function ("Model_getCompartment" model-get-compartment)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getCompartmentById(void* m, char* sid)
(uffi:def-function ("Model_getCompartmentById" model-get-compartment-by-id2157)
                   ((m :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-get-compartment-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-compartment-by-id2157 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_getSpecies(void* m, unsigned int n)
(uffi:def-function ("Model_getSpecies" model-get-species)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getSpeciesById(void* m, char* sid)
(uffi:def-function ("Model_getSpeciesById" model-get-species-by-id2158)
                   ((m :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-get-species-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-species-by-id2158 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_getParameter(void* m, unsigned int n)
(uffi:def-function ("Model_getParameter" model-get-parameter)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getParameterById(void* m, char* sid)
(uffi:def-function ("Model_getParameterById" model-get-parameter-by-id2159)
                   ((m :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-get-parameter-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-parameter-by-id2159 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_getRule(void* m, unsigned int n)
(uffi:def-function ("Model_getRule" model-get-rule)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getReaction(void* m, unsigned int n)
(uffi:def-function ("Model_getReaction" model-get-reaction)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getReactionById(void* m, char* sid)
(uffi:def-function ("Model_getReactionById" model-get-reaction-by-id2160)
                   ((m :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-get-reaction-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-reaction-by-id2160 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void* Model_getEvent(void* m, unsigned int n)
(uffi:def-function ("Model_getEvent" model-get-event)
                   ((m :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Model_getEventById(void* m, char* sid)
(uffi:def-function ("Model_getEventById" model-get-event-by-id2161)
                   ((m :pointer-void) (sid :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun model-get-event-by-id (m sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (model-get-event-by-id2161 m sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function unsigned int Model_getNumFunctionDefinitions(void* m)
(uffi:def-function
 ("Model_getNumFunctionDefinitions" model-get-num-function-definitions)
 ((m :pointer-void))
 :returning
 :unsigned-int) 

;; Function unsigned int Model_getNumUnitDefinitions(void* m)
(uffi:def-function
 ("Model_getNumUnitDefinitions" model-get-num-unit-definitions)
 ((m :pointer-void))
 :returning
 :unsigned-int) 

;; Function unsigned int Model_getNumCompartments(void* m)
(uffi:def-function ("Model_getNumCompartments" model-get-num-compartments)
                   ((m :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Model_getNumSpecies(void* m)
(uffi:def-function ("Model_getNumSpecies" model-get-num-species)
                   ((m :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Model_getNumSpeciesWithBoundaryCondition(void* m)
(uffi:def-function
 ("Model_getNumSpeciesWithBoundaryCondition"
  model-get-num-species-with-boundary-condition)
 ((m :pointer-void))
 :returning
 :unsigned-int) 

;; Function unsigned int Model_getNumParameters(void* m)
(uffi:def-function ("Model_getNumParameters" model-get-num-parameters)
                   ((m :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Model_getNumRules(void* m)
(uffi:def-function ("Model_getNumRules" model-get-num-rules)
                   ((m :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Model_getNumReactions(void* m)
(uffi:def-function ("Model_getNumReactions" model-get-num-reactions)
                   ((m :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int Model_getNumEvents(void* m)
(uffi:def-function ("Model_getNumEvents" model-get-num-events)
                   ((m :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void SBML_convertToL2(void* sb)
(uffi:def-function ("SBML_convertToL2" sbml-convert-to-l2)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function void SBML_convertNameToId(void* sb)
(uffi:def-function ("SBML_convertNameToId" sbml-convert-name-to-id)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function void SBML_convertReactionsInModelToL2(void* m)
(uffi:def-function
 ("SBML_convertReactionsInModelToL2" sbml-convert-reactions-in-model-to-l2)
 ((m :pointer-void))
 :returning
 :void) 

;; Function void SBML_addModifiersToReaction(void* r, void* m)
(uffi:def-function
 ("SBML_addModifiersToReaction" sbml-add-modifiers-to-reaction)
 ((r :pointer-void) (m :pointer-void))
 :returning
 :void) 

;;void ParseMessage_t
;; Function void* ParseMessage_create()
(uffi:def-function ("ParseMessage_create" parse-message-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* ParseMessage_createWith(char* message, unsigned int line, unsigned int column)
(uffi:def-function ("ParseMessage_createWith" parse-message-create-with2162)
                   ((message :cstring) (line :unsigned-int)
                    (column :unsigned-int))
                   :returning
                   :pointer-void) 
(common-lisp:defun parse-message-create-with (message line column)
  (common-lisp:setq message (uffi:convert-to-cstring message))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (parse-message-create-with2162 message line column)
      common-lisp:nil
      (uffi:free-cstring message)))) 

;; Function void ParseMessage_free(void* pm)
(uffi:def-function ("ParseMessage_free" parse-message-free)
                   ((pm :pointer-void))
                   :returning
                   :void) 

;; Function char* ParseMessage_getMessage(void* pm)
(uffi:def-function ("ParseMessage_getMessage" parse-message-get-message2163)
                   ((pm :pointer-void))
                   :returning
                   :cstring) 
(common-lisp:defun parse-message-get-message (pm)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring (parse-message-get-message2163 pm))
      common-lisp:nil))) 

;; Function unsigned int ParseMessage_getLine(void* pm)
(uffi:def-function ("ParseMessage_getLine" parse-message-get-line)
                   ((pm :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int ParseMessage_getColumn(void* pm)
(uffi:def-function ("ParseMessage_getColumn" parse-message-get-column)
                   ((pm :pointer-void))
                   :returning
                   :unsigned-int) 

;;void SBMLDocument_t
;; Function void* SBMLDocument_create()
(uffi:def-function ("SBMLDocument_create" sbmldocument-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void* SBMLDocument_createWith(unsigned int level, unsigned int version)
(uffi:def-function ("SBMLDocument_createWith" sbmldocument-create-with)
                   ((level :unsigned-int) (version :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* SBMLDocument_createModel(void* d)
(uffi:def-function ("SBMLDocument_createModel" sbmldocument-create-model)
                   ((d :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* SBMLDocument_createModelWith(void* d, char* sid)
(uffi:def-function
 ("SBMLDocument_createModelWith" sbmldocument-create-model-with2164)
 ((d :pointer-void) (sid :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun sbmldocument-create-model-with (d sid)
  (common-lisp:setq sid (uffi:convert-to-cstring sid))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbmldocument-create-model-with2164 d sid)
      common-lisp:nil
      (uffi:free-cstring sid)))) 

;; Function void SBMLDocument_free(void* d)
(uffi:def-function ("SBMLDocument_free" sbmldocument-free)
                   ((d :pointer-void))
                   :returning
                   :void) 

;; Function unsigned int SBMLDocument_getLevel(void* d)
(uffi:def-function ("SBMLDocument_getLevel" sbmldocument-get-level)
                   ((d :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int SBMLDocument_getVersion(void* d)
(uffi:def-function ("SBMLDocument_getVersion" sbmldocument-get-version)
                   ((d :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void* SBMLDocument_getWarning(void* d, unsigned int n)
(uffi:def-function ("SBMLDocument_getWarning" sbmldocument-get-warning)
                   ((d :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* SBMLDocument_getError(void* d, unsigned int n)
(uffi:def-function ("SBMLDocument_getError" sbmldocument-get-error)
                   ((d :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* SBMLDocument_getFatal(void* d, unsigned int n)
(uffi:def-function ("SBMLDocument_getFatal" sbmldocument-get-fatal)
                   ((d :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* SBMLDocument_getModel(void* d)
(uffi:def-function ("SBMLDocument_getModel" sbmldocument-get-model)
                   ((d :pointer-void))
                   :returning
                   :pointer-void) 

;; Function unsigned int SBMLDocument_getNumWarnings(void* d)
(uffi:def-function
 ("SBMLDocument_getNumWarnings" sbmldocument-get-num-warnings)
 ((d :pointer-void))
 :returning
 :unsigned-int) 

;; Function unsigned int SBMLDocument_getNumErrors(void* d)
(uffi:def-function ("SBMLDocument_getNumErrors" sbmldocument-get-num-errors)
                   ((d :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function unsigned int SBMLDocument_getNumFatals(void* d)
(uffi:def-function ("SBMLDocument_getNumFatals" sbmldocument-get-num-fatals)
                   ((d :pointer-void))
                   :returning
                   :unsigned-int) 

;; Function void SBMLDocument_printWarnings(void* d, FILE* stream)
(uffi:def-function ("SBMLDocument_printWarnings" sbmldocument-print-warnings)
                   ((d :pointer-void) (stream :pointer-void))
                   :returning
                   :void) 

;; Function void SBMLDocument_printErrors(void* d, FILE* stream)
(uffi:def-function ("SBMLDocument_printErrors" sbmldocument-print-errors)
                   ((d :pointer-void) (stream :pointer-void))
                   :returning
                   :void) 

;; Function void SBMLDocument_printFatals(void* d, FILE* stream)
(uffi:def-function ("SBMLDocument_printFatals" sbmldocument-print-fatals)
                   ((d :pointer-void) (stream :pointer-void))
                   :returning
                   :void) 

;; Function void SBMLDocument_setLevel(void* d, unsigned int level)
(uffi:def-function ("SBMLDocument_setLevel" sbmldocument-set-level)
                   ((d :pointer-void) (level :unsigned-int))
                   :returning
                   :void) 

;; Function void SBMLDocument_setVersion(void* d, unsigned int version)
(uffi:def-function ("SBMLDocument_setVersion" sbmldocument-set-version)
                   ((d :pointer-void) (version :unsigned-int))
                   :returning
                   :void) 

;; Function void SBMLDocument_setModel(void* d, void* m)
(uffi:def-function ("SBMLDocument_setModel" sbmldocument-set-model)
                   ((d :pointer-void) (m :pointer-void))
                   :returning
                   :void) 

;; Function unsigned int SBMLDocument_validate(void* d)
(uffi:def-function ("SBMLDocument_validate" sbmldocument-validate)
                   ((d :pointer-void))
                   :returning
                   :unsigned-int) 

;; enum{xml_schema_validation_none, xml_schema_validation_basic, xml_schema_validation_full} XMLSchemaValidation_t

(uffi:def-foreign-type xmlschema-validation-t :int) 
(common-lisp:defconstant +xml-schema-validation-none+ 0) 
(common-lisp:defconstant +xml-schema-validation-basic+ 1) 
(common-lisp:defconstant +xml-schema-validation-full+ 2) 
;;void SBMLReader_t
;; Function void* SBMLReader_create()
(uffi:def-function ("SBMLReader_create" sbmlreader-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void SBMLReader_free(void* sr)
(uffi:def-function ("SBMLReader_free" sbmlreader-free)
                   ((sr :pointer-void))
                   :returning
                   :void) 

;; Function char* SBMLReader_getSchemaFilenameL1v1(void* sr)
(uffi:def-function
 ("SBMLReader_getSchemaFilenameL1v1" sbmlreader-get-schema-filename-l1v12165)
 ((sr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun sbmlreader-get-schema-filename-l1v1 (sr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (sbmlreader-get-schema-filename-l1v12165 sr))
      common-lisp:nil))) 

;; Function char* SBMLReader_getSchemaFilenameL1v2(void* sr)
(uffi:def-function
 ("SBMLReader_getSchemaFilenameL1v2" sbmlreader-get-schema-filename-l1v22166)
 ((sr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun sbmlreader-get-schema-filename-l1v2 (sr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (sbmlreader-get-schema-filename-l1v22166 sr))
      common-lisp:nil))) 

;; Function char* SBMLReader_getSchemaFilenameL2v1(void* sr)
(uffi:def-function
 ("SBMLReader_getSchemaFilenameL2v1" sbmlreader-get-schema-filename-l2v12167)
 ((sr :pointer-void))
 :returning
 :cstring) 
(common-lisp:defun sbmlreader-get-schema-filename-l2v1 (sr)
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (uffi:convert-from-cstring
         (sbmlreader-get-schema-filename-l2v12167 sr))
      common-lisp:nil))) 

;; Function XMLSchemaValidation_t SBMLReader_getSchemaValidationLevel(void* sr)
(uffi:def-function
 ("SBMLReader_getSchemaValidationLevel" sbmlreader-get-schema-validation-level)
 ((sr :pointer-void))
 :returning
 :int) 

;; Function void* SBMLReader_readSBML(void* sr, char* filename)
(uffi:def-function ("SBMLReader_readSBML" sbmlreader-read-sbml2168)
                   ((sr :pointer-void) (filename :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun sbmlreader-read-sbml (sr filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbmlreader-read-sbml2168 sr filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function void* SBMLReader_readSBMLFromString(void* sr, char* xml)
(uffi:def-function
 ("SBMLReader_readSBMLFromString" sbmlreader-read-sbmlfrom-string2169)
 ((sr :pointer-void) (xml :cstring))
 :returning
 :pointer-void) 
(common-lisp:defun sbmlreader-read-sbmlfrom-string (sr xml)
  (common-lisp:setq xml (uffi:convert-to-cstring xml))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbmlreader-read-sbmlfrom-string2169 sr xml)
      common-lisp:nil
      (uffi:free-cstring xml)))) 

;; Function void* readSBML(char* filename)
(uffi:def-function ("readSBML" read-sbml2170)
                   ((filename :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun read-sbml (filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect (read-sbml2170 filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function void* readSBMLFromString(char* xml)
(uffi:def-function ("readSBMLFromString" read-sbmlfrom-string2171)
                   ((xml :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun read-sbmlfrom-string (xml)
  (common-lisp:setq xml (uffi:convert-to-cstring xml))
  (common-lisp:let ()
    (common-lisp:unwind-protect (read-sbmlfrom-string2171 xml)
      common-lisp:nil
      (uffi:free-cstring xml)))) 

;; Function void SBMLReader_setSchemaFilenameL1v1(void* sr, char* filename)
(uffi:def-function
 ("SBMLReader_setSchemaFilenameL1v1" sbmlreader-set-schema-filename-l1v12172)
 ((sr :pointer-void) (filename :cstring))
 :returning
 :void) 
(common-lisp:defun sbmlreader-set-schema-filename-l1v1 (sr filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (sbmlreader-set-schema-filename-l1v12172 sr filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function void SBMLReader_setSchemaFilenameL1v2(void* sr, char* filename)
(uffi:def-function
 ("SBMLReader_setSchemaFilenameL1v2" sbmlreader-set-schema-filename-l1v22173)
 ((sr :pointer-void) (filename :cstring))
 :returning
 :void) 
(common-lisp:defun sbmlreader-set-schema-filename-l1v2 (sr filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (sbmlreader-set-schema-filename-l1v22173 sr filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function void SBMLReader_setSchemaFilenameL2v1(void* sr, char* filename)
(uffi:def-function
 ("SBMLReader_setSchemaFilenameL2v1" sbmlreader-set-schema-filename-l2v12174)
 ((sr :pointer-void) (filename :cstring))
 :returning
 :void) 
(common-lisp:defun sbmlreader-set-schema-filename-l2v1 (sr filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect
        (sbmlreader-set-schema-filename-l2v12174 sr filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function void SBMLReader_setSchemaValidationLevel(void* sr, XMLSchemaValidation_t level)
(uffi:def-function
 ("SBMLReader_setSchemaValidationLevel" sbmlreader-set-schema-validation-level)
 ((sr :pointer-void) (level :int))
 :returning
 :void) 

;; enum{character_encoding_ascii, character_encoding_utf_8, character_encoding_utf_16, character_encoding_iso_8859_1, character_encoding_invalid} CharacterEncoding_t

(uffi:def-foreign-type character-encoding-t :int) 
(common-lisp:defconstant +character-encoding-ascii+ 0) 
(common-lisp:defconstant +character-encoding-utf-8+ 1) 
(common-lisp:defconstant +character-encoding-utf-16+ 2) 
(common-lisp:defconstant +character-encoding-iso-8859-1+ 3) 
(common-lisp:defconstant +character-encoding-invalid+ 4) 
;;struct{CharacterEncoding_t encoding} SBMLWriter_t
;; Function SBMLWriter_t* SBMLWriter_create()
(uffi:def-function ("SBMLWriter_create" sbmlwriter-create)
                   common-lisp:nil
                   :returning
                   :pointer-void) 

;; Function void SBMLWriter_free(SBMLWriter_t* sw)
(uffi:def-function ("SBMLWriter_free" sbmlwriter-free)
                   ((sw :pointer-void))
                   :returning
                   :void) 

;; Function void SBMLWriter_initDefaults(SBMLWriter_t* sw)
(uffi:def-function ("SBMLWriter_initDefaults" sbmlwriter-init-defaults)
                   ((sw :pointer-void))
                   :returning
                   :void) 

;; Function void SBMLWriter_setEncoding(SBMLWriter_t* sw, CharacterEncoding_t encoding)
(uffi:def-function ("SBMLWriter_setEncoding" sbmlwriter-set-encoding)
                   ((sw :pointer-void) (encoding :int))
                   :returning
                   :void) 

;; Function int SBMLWriter_writeSBML(SBMLWriter_t* sw, void* d, char* filename)
(uffi:def-function ("SBMLWriter_writeSBML" sbmlwriter-write-sbml2175)
                   ((sw :pointer-void) (d :pointer-void) (filename :cstring))
                   :returning
                   :int) 
(common-lisp:defun sbmlwriter-write-sbml (sw d filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbmlwriter-write-sbml2175 sw d filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function char* SBMLWriter_writeSBMLToString(SBMLWriter_t* sw, void* d)
(uffi:def-function
 ("SBMLWriter_writeSBMLToString" sbmlwriter-write-sbmlto-string2176)
 ((sw :pointer-void) (d :pointer-void))
 :returning
 :pointer-void) 
(common-lisp:defun sbmlwriter-write-sbmlto-string (sw d)
  (common-lisp:let (res2177)
    (common-lisp:unwind-protect
        (common-lisp:progn
         (common-lisp:setq res2177 (sbmlwriter-write-sbmlto-string2176 sw d))
         (uffi:convert-from-foreign-string res2177))
      common-lisp:nil
      (common-lisp:when res2177 (uffi::c-free res2177))))) 

;; Function int writeSBML(void* d, char* filename)
(uffi:def-function ("writeSBML" write-sbml2178)
                   ((d :pointer-void) (filename :cstring))
                   :returning
                   :int) 
(common-lisp:defun write-sbml (d filename)
  (common-lisp:setq filename (uffi:convert-to-cstring filename))
  (common-lisp:let ()
    (common-lisp:unwind-protect (write-sbml2178 d filename)
      common-lisp:nil
      (uffi:free-cstring filename)))) 

;; Function char* writeSBMLToString(void* d)
(uffi:def-function ("writeSBMLToString" write-sbmlto-string2179)
                   ((d :pointer-void))
                   :returning
                   :pointer-void) 
(common-lisp:defun write-sbmlto-string (d)
  (common-lisp:let (res2180)
    (common-lisp:unwind-protect
        (common-lisp:progn
         (common-lisp:setq res2180 (write-sbmlto-string2179 d))
         (uffi:convert-from-foreign-string res2180))
      common-lisp:nil
      (common-lisp:when res2180 (uffi::c-free res2180))))) 

;;struct{long sp, long capacity, void** stack} Stack_t
;; Function Stack_t* Stack_create(int capacity)
(uffi:def-function ("Stack_create" stack-create)
                   ((capacity :int))
                   :returning
                   :pointer-void) 

;; Function void Stack_free(Stack_t* s)
(uffi:def-function ("Stack_free" stack-free)
                   ((s :pointer-void))
                   :returning
                   :void) 

;; Function int Stack_find(Stack_t* s, void* item)
(uffi:def-function ("Stack_find" stack-find)
                   ((s :pointer-void) (item :pointer-void))
                   :returning
                   :int) 

;; Function void Stack_push(Stack_t* s, void* item)
(uffi:def-function ("Stack_push" stack-push)
                   ((s :pointer-void) (item :pointer-void))
                   :returning
                   :void) 

;; Function void* Stack_pop(Stack_t* s)
(uffi:def-function ("Stack_pop" stack-pop)
                   ((s :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Stack_popN(Stack_t* s, unsigned int n)
(uffi:def-function ("Stack_popN" stack-pop-n)
                   ((s :pointer-void) (n :unsigned-int))
                   :returning
                   :pointer-void) 

;; Function void* Stack_peek(Stack_t* s)
(uffi:def-function ("Stack_peek" stack-peek)
                   ((s :pointer-void))
                   :returning
                   :pointer-void) 

;; Function void* Stack_peekAt(Stack_t* s, int n)
(uffi:def-function ("Stack_peekAt" stack-peek-at)
                   ((s :pointer-void) (n :int))
                   :returning
                   :pointer-void) 

;; Function int Stack_size(Stack_t* s)
(uffi:def-function ("Stack_size" stack-size)
                   ((s :pointer-void))
                   :returning
                   :int) 

;; Function int Stack_capacity(Stack_t* s)
(uffi:def-function ("Stack_capacity" stack-capacity)
                   ((s :pointer-void))
                   :returning
                   :int) 

;; Function void* SBML_parseFormula(char* formula)
(uffi:def-function ("SBML_parseFormula" sbml-parse-formula2181)
                   ((formula :cstring))
                   :returning
                   :pointer-void) 
(common-lisp:defun sbml-parse-formula (formula)
  (common-lisp:setq formula (uffi:convert-to-cstring formula))
  (common-lisp:let ()
    (common-lisp:unwind-protect (sbml-parse-formula2181 formula)
      common-lisp:nil
      (uffi:free-cstring formula)))) 

;; Function long FormulaParser_getAction(long state, Token_t* token)
(uffi:def-function ("FormulaParser_getAction" formula-parser-get-action)
                   ((state :long) (token :pointer-void))
                   :returning
                   :long) 

;; Function long FormulaParser_getActionLength(TokenType_t type)
(uffi:def-function
 ("FormulaParser_getActionLength" formula-parser-get-action-length)
 ((type :int))
 :returning
 :long) 

;; Function long FormulaParser_getActionOffset(TokenType_t type)
(uffi:def-function
 ("FormulaParser_getActionOffset" formula-parser-get-action-offset)
 ((type :int))
 :returning
 :long) 

;; Function long FormulaParser_getGoto(long state, long rule)
(uffi:def-function ("FormulaParser_getGoto" formula-parser-get-goto)
                   ((state :long) (rule :long))
                   :returning
                   :long) 

;; Function void* FormulaParser_reduceStackByRule(Stack_t* stack, long rule)
(uffi:def-function
 ("FormulaParser_reduceStackByRule" formula-parser-reduce-stack-by-rule)
 ((stack :pointer-void) (rule :long))
 :returning
 :pointer-void) 

;;struct{unsigned long length, unsigned long capacity, char* buffer} StringBuffer_t
;; Function StringBuffer_t* StringBuffer_create(unsigned long capacity)
(uffi:def-function ("StringBuffer_create" string-buffer-create)
                   ((capacity :unsigned-long))
                   :returning
                   :pointer-void) 

;; Function void StringBuffer_free(StringBuffer_t* sb)
(uffi:def-function ("StringBuffer_free" string-buffer-free)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function void StringBuffer_reset(StringBuffer_t* sb)
(uffi:def-function ("StringBuffer_reset" string-buffer-reset)
                   ((sb :pointer-void))
                   :returning
                   :void) 

;; Function void StringBuffer_append(StringBuffer_t* sb, char* s)
(uffi:def-function ("StringBuffer_append" string-buffer-append2182)
                   ((sb :pointer-void) (s :cstring))
                   :returning
                   :void) 
(common-lisp:defun string-buffer-append (sb s)
  (common-lisp:setq s (uffi:convert-to-cstring s))
  (common-lisp:let ()
    (common-lisp:unwind-protect (string-buffer-append2182 sb s)
      common-lisp:nil
      (uffi:free-cstring s)))) 

;; Function void StringBuffer_appendChar(StringBuffer_t* sb, char c)
(uffi:def-function ("StringBuffer_appendChar" string-buffer-append-char)
                   ((sb :pointer-void) (c :char))
                   :returning
                   :void) 

;; Function void StringBuffer_appendInt(StringBuffer_t* sb, long i)
(uffi:def-function ("StringBuffer_appendInt" string-buffer-append-int)
                   ((sb :pointer-void) (i :long))
                   :returning
                   :void) 

;; Function void StringBuffer_appendReal(StringBuffer_t* sb, double r)
(uffi:def-function ("StringBuffer_appendReal" string-buffer-append-real)
                   ((sb :pointer-void) (r :double))
                   :returning
                   :void) 

;; Function void StringBuffer_ensureCapacity(StringBuffer_t* sb, unsigned long n)
(uffi:def-function
 ("StringBuffer_ensureCapacity" string-buffer-ensure-capacity)
 ((sb :pointer-void) (n :unsigned-long))
 :returning
 :void) 

;; Function void StringBuffer_grow(StringBuffer_t* sb, unsigned long n)
(uffi:def-function ("StringBuffer_grow" string-buffer-grow)
                   ((sb :pointer-void) (n :unsigned-long))
                   :returning
                   :void) 

;; Function char* StringBuffer_getBuffer(struct{unsigned long length, unsigned long capacity, char* buffer}* sb)
(uffi:def-function ("StringBuffer_getBuffer" string-buffer-get-buffer2183)
                   ((sb :pointer-void))
                   :returning
                   :pointer-void) 
(common-lisp:defun string-buffer-get-buffer (sb)
  (common-lisp:let (res2184)
    (common-lisp:unwind-protect
        (common-lisp:progn
         (common-lisp:setq res2184 (string-buffer-get-buffer2183 sb))
         (uffi:convert-from-foreign-string res2184))
      common-lisp:nil
      (common-lisp:when res2184 (uffi::c-free res2184))))) 

;; Function unsigned long StringBuffer_length(struct{unsigned long length, unsigned long capacity, char* buffer}* sb)
(uffi:def-function ("StringBuffer_length" string-buffer-length)
                   ((sb :pointer-void))
                   :returning
                   :unsigned-long) 

;; Function unsigned long StringBuffer_capacity(struct{unsigned long length, unsigned long capacity, char* buffer}* sb)
(uffi:def-function ("StringBuffer_capacity" string-buffer-capacity)
                   ((sb :pointer-void))
                   :returning
                   :unsigned-long) 

;; Function char* StringBuffer_toString(struct{unsigned long length, unsigned long capacity, char* buffer}* sb)
(uffi:def-function ("StringBuffer_toString" string-buffer-to-string2185)
                   ((sb :pointer-void))
                   :returning
                   :pointer-void) 
(common-lisp:defun string-buffer-to-string (sb)
  (common-lisp:let (res2186)
    (common-lisp:unwind-protect
        (common-lisp:progn
         (common-lisp:setq res2186 (string-buffer-to-string2185 sb))
         (uffi:convert-from-foreign-string res2186))
      common-lisp:nil
      (common-lisp:when res2186 (uffi::c-free res2186))))) 

;; Function char* SBML_formulaToString(void* tree)
(uffi:def-function ("SBML_formulaToString" sbml-formula-to-string2187)
                   ((tree :pointer-void))
                   :returning
                   :pointer-void) 
(common-lisp:defun sbml-formula-to-string (tree)
  (common-lisp:let (res2188)
    (common-lisp:unwind-protect
        (common-lisp:progn
         (common-lisp:setq res2188 (sbml-formula-to-string2187 tree))
         (uffi:convert-from-foreign-string res2188))
      common-lisp:nil
      (common-lisp:when res2188 (uffi::c-free res2188))))) 

;; Function int FormulaFormatter_isFunction(void* node)
(uffi:def-function
 ("FormulaFormatter_isFunction" formula-formatter-is-function)
 ((node :pointer-void))
 :returning
 :int) 

;; Function int FormulaFormatter_isGrouped(void* parent, void* child)
(uffi:def-function ("FormulaFormatter_isGrouped" formula-formatter-is-grouped)
                   ((parent :pointer-void) (child :pointer-void))
                   :returning
                   :int) 

;; Function void FormulaFormatter_format(StringBuffer_t* sb, void* node)
(uffi:def-function ("FormulaFormatter_format" formula-formatter-format)
                   ((sb :pointer-void) (node :pointer-void))
                   :returning
                   :void) 

;; Function void FormulaFormatter_formatFunction(StringBuffer_t* sb, void* node)
(uffi:def-function
 ("FormulaFormatter_formatFunction" formula-formatter-format-function)
 ((sb :pointer-void) (node :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_formatOperator(StringBuffer_t* sb, void* node)
(uffi:def-function
 ("FormulaFormatter_formatOperator" formula-formatter-format-operator)
 ((sb :pointer-void) (node :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_formatRational(StringBuffer_t* sb, void* node)
(uffi:def-function
 ("FormulaFormatter_formatRational" formula-formatter-format-rational)
 ((sb :pointer-void) (node :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_formatReal(StringBuffer_t* sb, void* node)
(uffi:def-function
 ("FormulaFormatter_formatReal" formula-formatter-format-real)
 ((sb :pointer-void) (node :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_visit(void* parent, void* node, StringBuffer_t* sb)
(uffi:def-function ("FormulaFormatter_visit" formula-formatter-visit)
                   ((parent :pointer-void) (node :pointer-void)
                    (sb :pointer-void))
                   :returning
                   :void) 

;; Function void FormulaFormatter_visitFunction(void* parent, void* node, StringBuffer_t* sb)
(uffi:def-function
 ("FormulaFormatter_visitFunction" formula-formatter-visit-function)
 ((parent :pointer-void) (node :pointer-void) (sb :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_visitLog10(void* parent, void* node, StringBuffer_t* sb)
(uffi:def-function
 ("FormulaFormatter_visitLog10" formula-formatter-visit-log10)
 ((parent :pointer-void) (node :pointer-void) (sb :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_visitSqrt(void* parent, void* node, StringBuffer_t* sb)
(uffi:def-function ("FormulaFormatter_visitSqrt" formula-formatter-visit-sqrt)
                   ((parent :pointer-void) (node :pointer-void)
                    (sb :pointer-void))
                   :returning
                   :void) 

;; Function void FormulaFormatter_visitUMinus(void* parent, void* node, StringBuffer_t* sb)
(uffi:def-function
 ("FormulaFormatter_visitUMinus" formula-formatter-visit-uminus)
 ((parent :pointer-void) (node :pointer-void) (sb :pointer-void))
 :returning
 :void) 

;; Function void FormulaFormatter_visitOther(void* parent, void* node, StringBuffer_t* sb)
(uffi:def-function
 ("FormulaFormatter_visitOther" formula-formatter-visit-other)
 ((parent :pointer-void) (node :pointer-void) (sb :pointer-void))
 :returning
 :void) 



(cl:defpackage "LIBSBMLC"
    (:use )
    (:export 
       "CBOOL"
       "LBOOL"
         "FORMULA-FORMATTER-VISIT-OTHER"
  "FORMULA-FORMATTER-VISIT-UMINUS"
  "FORMULA-FORMATTER-VISIT-SQRT"
  "FORMULA-FORMATTER-VISIT-LOG10"
  "FORMULA-FORMATTER-VISIT-FUNCTION"
  "FORMULA-FORMATTER-VISIT"
  "FORMULA-FORMATTER-FORMAT-REAL"
  "FORMULA-FORMATTER-FORMAT-RATIONAL"
  "FORMULA-FORMATTER-FORMAT-OPERATOR"
  "FORMULA-FORMATTER-FORMAT-FUNCTION"
  "FORMULA-FORMATTER-FORMAT"
  "FORMULA-FORMATTER-IS-GROUPED"
  "FORMULA-FORMATTER-IS-FUNCTION"
  "SBML-FORMULA-TO-STRING"
  "STRING-BUFFER-TO-STRING"
  "STRING-BUFFER-CAPACITY"
  "STRING-BUFFER-LENGTH"
  "STRING-BUFFER-GET-BUFFER"
  "STRING-BUFFER-GROW"
  "STRING-BUFFER-ENSURE-CAPACITY"
  "STRING-BUFFER-APPEND-REAL"
  "STRING-BUFFER-APPEND-INT"
  "STRING-BUFFER-APPEND-CHAR"
  "STRING-BUFFER-APPEND"
  "STRING-BUFFER-RESET"
  "STRING-BUFFER-FREE"
  "STRING-BUFFER-CREATE"
  "FORMULA-PARSER-REDUCE-STACK-BY-RULE"
  "FORMULA-PARSER-GET-GOTO"
  "FORMULA-PARSER-GET-ACTION-OFFSET"
  "FORMULA-PARSER-GET-ACTION-LENGTH"
  "FORMULA-PARSER-GET-ACTION"
  "SBML-PARSE-FORMULA"
  "STACK-CAPACITY"
  "STACK-SIZE"
  "STACK-PEEK-AT"
  "STACK-PEEK"
  "STACK-POP-N"
  "STACK-POP"
  "STACK-PUSH"
  "STACK-FIND"
  "STACK-FREE"
  "STACK-CREATE"
  "WRITE-SBMLTO-STRING"
  "WRITE-SBML"
  "SBMLWRITER-WRITE-SBMLTO-STRING"
  "SBMLWRITER-WRITE-SBML"
  "SBMLWRITER-SET-ENCODING"
  "SBMLWRITER-INIT-DEFAULTS"
  "SBMLWRITER-FREE"
  "SBMLWRITER-CREATE"
  "+CHARACTER-ENCODING-INVALID+"
  "+CHARACTER-ENCODING-ISO-8859-1+"
  "+CHARACTER-ENCODING-UTF-16+"
  "+CHARACTER-ENCODING-UTF-8+"
  "+CHARACTER-ENCODING-ASCII+"
  "CHARACTER-ENCODING-T"
  "SBMLREADER-SET-SCHEMA-VALIDATION-LEVEL"
  "SBMLREADER-SET-SCHEMA-FILENAME-L2V1"
  "SBMLREADER-SET-SCHEMA-FILENAME-L1V2"
  "SBMLREADER-SET-SCHEMA-FILENAME-L1V1"
  "READ-SBMLFROM-STRING"
  "READ-SBML"
  "SBMLREADER-READ-SBMLFROM-STRING"
  "SBMLREADER-READ-SBML"
  "SBMLREADER-GET-SCHEMA-VALIDATION-LEVEL"
  "SBMLREADER-GET-SCHEMA-FILENAME-L2V1"
  "SBMLREADER-GET-SCHEMA-FILENAME-L1V2"
  "SBMLREADER-GET-SCHEMA-FILENAME-L1V1"
  "SBMLREADER-FREE"
  "SBMLREADER-CREATE"
  "+XML-SCHEMA-VALIDATION-FULL+"
  "+XML-SCHEMA-VALIDATION-BASIC+"
  "+XML-SCHEMA-VALIDATION-NONE+"
  "XMLSCHEMA-VALIDATION-T"
  "SBMLDOCUMENT-VALIDATE"
  "SBMLDOCUMENT-SET-MODEL"
  "SBMLDOCUMENT-SET-VERSION"
  "SBMLDOCUMENT-SET-LEVEL"
  "SBMLDOCUMENT-PRINT-FATALS"
  "SBMLDOCUMENT-PRINT-ERRORS"
  "SBMLDOCUMENT-PRINT-WARNINGS"
  "SBMLDOCUMENT-GET-NUM-FATALS"
  "SBMLDOCUMENT-GET-NUM-ERRORS"
  "SBMLDOCUMENT-GET-NUM-WARNINGS"
  "SBMLDOCUMENT-GET-MODEL"
  "SBMLDOCUMENT-GET-FATAL"
  "SBMLDOCUMENT-GET-ERROR"
  "SBMLDOCUMENT-GET-WARNING"
  "SBMLDOCUMENT-GET-VERSION"
  "SBMLDOCUMENT-GET-LEVEL"
  "SBMLDOCUMENT-FREE"
  "SBMLDOCUMENT-CREATE-MODEL-WITH"
  "SBMLDOCUMENT-CREATE-MODEL"
  "SBMLDOCUMENT-CREATE-WITH"
  "SBMLDOCUMENT-CREATE"
  "PARSE-MESSAGE-GET-COLUMN"
  "PARSE-MESSAGE-GET-LINE"
  "PARSE-MESSAGE-GET-MESSAGE"
  "PARSE-MESSAGE-FREE"
  "PARSE-MESSAGE-CREATE-WITH"
  "PARSE-MESSAGE-CREATE"
  "SBML-ADD-MODIFIERS-TO-REACTION"
  "SBML-CONVERT-REACTIONS-IN-MODEL-TO-L2"
  "SBML-CONVERT-NAME-TO-ID"
  "SBML-CONVERT-TO-L2"
  "MODEL-GET-NUM-EVENTS"
  "MODEL-GET-NUM-REACTIONS"
  "MODEL-GET-NUM-RULES"
  "MODEL-GET-NUM-PARAMETERS"
  "MODEL-GET-NUM-SPECIES-WITH-BOUNDARY-CONDITION"
  "MODEL-GET-NUM-SPECIES"
  "MODEL-GET-NUM-COMPARTMENTS"
  "MODEL-GET-NUM-UNIT-DEFINITIONS"
  "MODEL-GET-NUM-FUNCTION-DEFINITIONS"
  "MODEL-GET-EVENT-BY-ID"
  "MODEL-GET-EVENT"
  "MODEL-GET-REACTION-BY-ID"
  "MODEL-GET-REACTION"
  "MODEL-GET-RULE"
  "MODEL-GET-PARAMETER-BY-ID"
  "MODEL-GET-PARAMETER"
  "MODEL-GET-SPECIES-BY-ID"
  "MODEL-GET-SPECIES"
  "MODEL-GET-COMPARTMENT-BY-ID"
  "MODEL-GET-COMPARTMENT"
  "MODEL-GET-UNIT-DEFINITION-BY-ID"
  "MODEL-GET-UNIT-DEFINITION"
  "MODEL-GET-FUNCTION-DEFINITION-BY-ID"
  "MODEL-GET-FUNCTION-DEFINITION"
  "MODEL-GET-LIST-OF-BY-TYPECODE"
  "MODEL-GET-LIST-OF-EVENTS"
  "MODEL-GET-LIST-OF-REACTIONS"
  "MODEL-GET-LIST-OF-RULES"
  "MODEL-GET-LIST-OF-PARAMETERS"
  "MODEL-GET-LIST-OF-SPECIES"
  "MODEL-GET-LIST-OF-COMPARTMENTS"
  "MODEL-GET-LIST-OF-UNIT-DEFINITIONS"
  "MODEL-GET-LIST-OF-FUNCTION-DEFINITIONS"
  "MODEL-ADD-EVENT"
  "MODEL-ADD-REACTION"
  "MODEL-ADD-RULE"
  "MODEL-ADD-PARAMETER"
  "MODEL-ADD-SPECIES"
  "MODEL-ADD-COMPARTMENT"
  "MODEL-ADD-UNIT-DEFINITION"
  "MODEL-ADD-FUNCTION-DEFINITION"
  "MODEL-CREATE-EVENT-ASSIGNMENT"
  "MODEL-CREATE-EVENT"
  "MODEL-CREATE-KINETIC-LAW-PARAMETER"
  "MODEL-CREATE-KINETIC-LAW"
  "MODEL-CREATE-MODIFIER"
  "MODEL-CREATE-PRODUCT"
  "MODEL-CREATE-REACTANT"
  "MODEL-CREATE-REACTION"
  "MODEL-CREATE-SPECIES-CONCENTRATION-RULE"
  "MODEL-CREATE-PARAMETER-RULE"
  "MODEL-CREATE-COMPARTMENT-VOLUME-RULE"
  "MODEL-CREATE-ALGEBRAIC-RULE"
  "MODEL-CREATE-RATE-RULE"
  "MODEL-CREATE-ASSIGNMENT-RULE"
  "MODEL-CREATE-PARAMETER"
  "MODEL-CREATE-SPECIES"
  "MODEL-CREATE-COMPARTMENT"
  "MODEL-CREATE-UNIT"
  "MODEL-CREATE-UNIT-DEFINITION"
  "MODEL-CREATE-FUNCTION-DEFINITION"
  "MODEL-UNSET-NAME"
  "MODEL-UNSET-ID"
  "MODEL-SET-NAME"
  "MODEL-SET-ID"
  "MODEL-IS-SET-NAME"
  "MODEL-IS-SET-ID"
  "MODEL-GET-NAME"
  "MODEL-GET-ID"
  "MODEL-FREE"
  "MODEL-CREATE-WITH-NAME"
  "MODEL-CREATE-WITH"
  "MODEL-CREATE"
  "EVENT-ID-CMP"
  "EVENT-GET-NUM-EVENT-ASSIGNMENTS"
  "EVENT-GET-EVENT-ASSIGNMENT"
  "EVENT-GET-LIST-OF-EVENT-ASSIGNMENTS"
  "EVENT-ADD-EVENT-ASSIGNMENT"
  "EVENT-UNSET-TIME-UNITS"
  "EVENT-UNSET-DELAY"
  "EVENT-UNSET-NAME"
  "EVENT-UNSET-ID"
  "EVENT-SET-TIME-UNITS"
  "EVENT-SET-DELAY"
  "EVENT-SET-TRIGGER"
  "EVENT-SET-NAME"
  "EVENT-SET-ID"
  "EVENT-IS-SET-TIME-UNITS"
  "EVENT-IS-SET-DELAY"
  "EVENT-IS-SET-TRIGGER"
  "EVENT-IS-SET-NAME"
  "EVENT-IS-SET-ID"
  "EVENT-GET-TIME-UNITS"
  "EVENT-GET-DELAY"
  "EVENT-GET-TRIGGER"
  "EVENT-GET-NAME"
  "EVENT-GET-ID"
  "EVENT-FREE"
  "EVENT-CREATE-WITH"
  "EVENT-CREATE"
  "EVENT-ASSIGNMENT-SET-MATH"
  "EVENT-ASSIGNMENT-SET-VARIABLE"
  "EVENT-ASSIGNMENT-IS-SET-MATH"
  "EVENT-ASSIGNMENT-IS-SET-VARIABLE"
  "EVENT-ASSIGNMENT-GET-MATH"
  "EVENT-ASSIGNMENT-GET-VARIABLE"
  "EVENT-ASSIGNMENT-FREE"
  "EVENT-ASSIGNMENT-CREATE-WITH"
  "EVENT-ASSIGNMENT-CREATE"
  "REACTION-ID-CMP"
  "REACTION-UNSET-FAST"
  "REACTION-UNSET-KINETIC-LAW"
  "REACTION-UNSET-NAME"
  "REACTION-GET-NUM-MODIFIERS"
  "REACTION-GET-NUM-PRODUCTS"
  "REACTION-GET-NUM-REACTANTS"
  "REACTION-GET-MODIFIER-BY-ID"
  "REACTION-GET-MODIFIER"
  "REACTION-GET-PRODUCT-BY-ID"
  "REACTION-GET-PRODUCT"
  "REACTION-GET-REACTANT-BY-ID"
  "REACTION-GET-REACTANT"
  "REACTION-ADD-MODIFIER"
  "REACTION-ADD-PRODUCT"
  "REACTION-ADD-REACTANT"
  "REACTION-GET-LIST-OF-MODIFIERS"
  "REACTION-GET-LIST-OF-PRODUCTS"
  "REACTION-GET-LIST-OF-REACTANTS"
  "REACTION-SET-FAST"
  "REACTION-SET-REVERSIBLE"
  "REACTION-SET-KINETIC-LAW"
  "REACTION-SET-NAME"
  "REACTION-SET-ID"
  "REACTION-IS-SET-FAST"
  "REACTION-IS-SET-KINETIC-LAW"
  "REACTION-IS-SET-NAME"
  "REACTION-IS-SET-ID"
  "REACTION-GET-FAST"
  "REACTION-GET-REVERSIBLE"
  "REACTION-GET-KINETIC-LAW"
  "REACTION-GET-NAME"
  "REACTION-GET-ID"
  "REACTION-INIT-DEFAULTS"
  "REACTION-FREE"
  "REACTION-CREATE-WITH"
  "REACTION-CREATE"
  "SPECIES-REFERENCE-SET-DENOMINATOR"
  "SPECIES-REFERENCE-SET-STOICHIOMETRY-MATH"
  "SPECIES-REFERENCE-SET-STOICHIOMETRY"
  "SPECIES-REFERENCE-SET-SPECIES"
  "SPECIES-REFERENCE-IS-SET-STOICHIOMETRY-MATH"
  "SPECIES-REFERENCE-IS-SET-SPECIES"
  "SPECIES-REFERENCE-GET-DENOMINATOR"
  "SPECIES-REFERENCE-GET-STOICHIOMETRY-MATH"
  "SPECIES-REFERENCE-GET-STOICHIOMETRY"
  "SPECIES-REFERENCE-GET-SPECIES"
  "SPECIES-REFERENCE-INIT-DEFAULTS"
  "SPECIES-REFERENCE-FREE"
  "SPECIES-REFERENCE-CREATE-WITH"
  "SPECIES-REFERENCE-CREATE"
  "MODIFIER-SPECIES-REFERENCE-SET-SPECIES"
  "MODIFIER-SPECIES-REFERENCE-IS-SET-SPECIES"
  "MODIFIER-SPECIES-REFERENCE-GET-SPECIES"
  "MODIFIER-SPECIES-REFERENCE-FREE"
  "MODIFIER-SPECIES-REFERENCE-CREATE-WITH"
  "MODIFIER-SPECIES-REFERENCE-CREATE"
  "SIMPLE-SPECIES-REFERENCE-CMP"
  "SIMPLE-SPECIES-REFERENCE-SET-SPECIES"
  "SIMPLE-SPECIES-REFERENCE-IS-SET-SPECIES"
  "SIMPLE-SPECIES-REFERENCE-GET-SPECIES"
  "KINETIC-LAW-UNSET-SUBSTANCE-UNITS"
  "KINETIC-LAW-UNSET-TIME-UNITS"
  "KINETIC-LAW-GET-NUM-PARAMETERS"
  "KINETIC-LAW-GET-PARAMETER"
  "KINETIC-LAW-ADD-PARAMETER"
  "KINETIC-LAW-SET-SUBSTANCE-UNITS"
  "KINETIC-LAW-SET-TIME-UNITS"
  "KINETIC-LAW-SET-MATH-FROM-FORMULA"
  "KINETIC-LAW-SET-MATH"
  "KINETIC-LAW-SET-FORMULA-FROM-MATH"
  "KINETIC-LAW-SET-FORMULA"
  "KINETIC-LAW-IS-SET-SUBSTANCE-UNITS"
  "KINETIC-LAW-IS-SET-TIME-UNITS"
  "KINETIC-LAW-IS-SET-MATH"
  "KINETIC-LAW-IS-SET-FORMULA"
  "KINETIC-LAW-GET-SUBSTANCE-UNITS"
  "KINETIC-LAW-GET-TIME-UNITS"
  "KINETIC-LAW-GET-LIST-OF-PARAMETERS"
  "KINETIC-LAW-GET-MATH"
  "KINETIC-LAW-GET-FORMULA"
  "KINETIC-LAW-FREE"
  "KINETIC-LAW-CREATE-WITH"
  "KINETIC-LAW-CREATE"
  "SPECIES-CONCENTRATION-RULE-SET-SPECIES"
  "SPECIES-CONCENTRATION-RULE-IS-SET-SPECIES"
  "SPECIES-CONCENTRATION-RULE-GET-SPECIES"
  "SPECIES-CONCENTRATION-RULE-FREE"
  "SPECIES-CONCENTRATION-RULE-CREATE-WITH"
  "SPECIES-CONCENTRATION-RULE-CREATE"
  "PARAMETER-RULE-UNSET-UNITS"
  "PARAMETER-RULE-SET-UNITS"
  "PARAMETER-RULE-SET-NAME"
  "PARAMETER-RULE-IS-SET-UNITS"
  "PARAMETER-RULE-IS-SET-NAME"
  "PARAMETER-RULE-GET-UNITS"
  "PARAMETER-RULE-GET-NAME"
  "PARAMETER-RULE-FREE"
  "PARAMETER-RULE-CREATE-WITH"
  "PARAMETER-RULE-CREATE"
  "COMPARTMENT-VOLUME-RULE-SET-COMPARTMENT"
  "COMPARTMENT-VOLUME-RULE-IS-SET-COMPARTMENT"
  "COMPARTMENT-VOLUME-RULE-GET-COMPARTMENT"
  "COMPARTMENT-VOLUME-RULE-FREE"
  "COMPARTMENT-VOLUME-RULE-CREATE-WITH"
  "COMPARTMENT-VOLUME-RULE-CREATE"
  "ALGEBRAIC-RULE-FREE"
  "ALGEBRAIC-RULE-CREATE-WITH-MATH"
  "ALGEBRAIC-RULE-CREATE-WITH"
  "ALGEBRAIC-RULE-CREATE"
  "RATE-RULE-SET-VARIABLE"
  "RATE-RULE-IS-SET-VARIABLE"
  "RATE-RULE-GET-VARIABLE"
  "RATE-RULE-FREE"
  "RATE-RULE-CREATE-WITH"
  "RATE-RULE-CREATE"
  "ASSIGNMENT-RULE-SET-VARIABLE"
  "ASSIGNMENT-RULE-SET-TYPE"
  "ASSIGNMENT-RULE-IS-SET-VARIABLE"
  "ASSIGNMENT-RULE-GET-VARIABLE"
  "ASSIGNMENT-RULE-GET-TYPE"
  "ASSIGNMENT-RULE-INIT-DEFAULTS"
  "ASSIGNMENT-RULE-FREE"
  "ASSIGNMENT-RULE-CREATE-WITH"
  "ASSIGNMENT-RULE-CREATE"
  "RULE-TYPE-TO-STRING"
  "RULE-TYPE-FOR-NAME"
  "+RULE-TYPE-INVALID+"
  "+RULE-TYPE-SCALAR+"
  "+RULE-TYPE-RATE+"
  "RULE-TYPE-T"
  "RULE-SET-MATH-FROM-FORMULA"
  "RULE-SET-MATH"
  "RULE-SET-FORMULA-FROM-MATH"
  "RULE-SET-FORMULA"
  "RULE-IS-SET-MATH"
  "RULE-IS-SET-FORMULA"
  "RULE-GET-MATH"
  "RULE-GET-FORMULA"
  "PARAMETER-ID-CMP"
  "PARAMETER-UNSET-UNITS"
  "PARAMETER-UNSET-VALUE"
  "PARAMETER-UNSET-NAME"
  "PARAMETER-SET-CONSTANT"
  "PARAMETER-SET-UNITS"
  "PARAMETER-SET-VALUE"
  "PARAMETER-SET-NAME"
  "PARAMETER-SET-ID"
  "PARAMETER-IS-SET-UNITS"
  "PARAMETER-IS-SET-VALUE"
  "PARAMETER-IS-SET-NAME"
  "PARAMETER-IS-SET-ID"
  "PARAMETER-GET-CONSTANT"
  "PARAMETER-GET-UNITS"
  "PARAMETER-GET-VALUE"
  "PARAMETER-GET-NAME"
  "PARAMETER-GET-ID"
  "PARAMETER-INIT-DEFAULTS"
  "PARAMETER-FREE"
  "PARAMETER-CREATE-WITH"
  "PARAMETER-CREATE"
  "SPECIES-ID-CMP"
  "SPECIES-UNSET-CHARGE"
  "SPECIES-UNSET-UNITS"
  "SPECIES-UNSET-SPATIAL-SIZE-UNITS"
  "SPECIES-UNSET-SUBSTANCE-UNITS"
  "SPECIES-UNSET-INITIAL-CONCENTRATION"
  "SPECIES-UNSET-INITIAL-AMOUNT"
  "SPECIES-UNSET-NAME"
  "SPECIES-SET-CONSTANT"
  "SPECIES-SET-CHARGE"
  "SPECIES-SET-BOUNDARY-CONDITION"
  "SPECIES-SET-HAS-ONLY-SUBSTANCE-UNITS"
  "SPECIES-SET-UNITS"
  "SPECIES-SET-SPATIAL-SIZE-UNITS"
  "SPECIES-SET-SUBSTANCE-UNITS"
  "SPECIES-SET-INITIAL-CONCENTRATION"
  "SPECIES-SET-INITIAL-AMOUNT"
  "SPECIES-SET-COMPARTMENT"
  "SPECIES-SET-NAME"
  "SPECIES-SET-ID"
  "SPECIES-IS-SET-CHARGE"
  "SPECIES-IS-SET-UNITS"
  "SPECIES-IS-SET-SPATIAL-SIZE-UNITS"
  "SPECIES-IS-SET-SUBSTANCE-UNITS"
  "SPECIES-IS-SET-INITIAL-CONCENTRATION"
  "SPECIES-IS-SET-INITIAL-AMOUNT"
  "SPECIES-IS-SET-COMPARTMENT"
  "SPECIES-IS-SET-NAME"
  "SPECIES-IS-SET-ID"
  "SPECIES-GET-CONSTANT"
  "SPECIES-GET-CHARGE"
  "SPECIES-GET-BOUNDARY-CONDITION"
  "SPECIES-GET-HAS-ONLY-SUBSTANCE-UNITS"
  "SPECIES-GET-UNITS"
  "SPECIES-GET-SPATIAL-SIZE-UNITS"
  "SPECIES-GET-SUBSTANCE-UNITS"
  "SPECIES-GET-INITIAL-CONCENTRATION"
  "SPECIES-GET-INITIAL-AMOUNT"
  "SPECIES-GET-COMPARTMENT"
  "SPECIES-GET-NAME"
  "SPECIES-GET-ID"
  "SPECIES-INIT-DEFAULTS"
  "SPECIES-FREE"
  "SPECIES-CREATE-WITH"
  "SPECIES-CREATE"
  "COMPARTMENT-ID-CMP"
  "COMPARTMENT-UNSET-OUTSIDE"
  "COMPARTMENT-UNSET-UNITS"
  "COMPARTMENT-UNSET-VOLUME"
  "COMPARTMENT-UNSET-SIZE"
  "COMPARTMENT-UNSET-NAME"
  "COMPARTMENT-SET-CONSTANT"
  "COMPARTMENT-SET-OUTSIDE"
  "COMPARTMENT-SET-UNITS"
  "COMPARTMENT-SET-VOLUME"
  "COMPARTMENT-SET-SIZE"
  "COMPARTMENT-SET-SPATIAL-DIMENSIONS"
  "COMPARTMENT-SET-NAME"
  "COMPARTMENT-SET-ID"
  "COMPARTMENT-IS-SET-OUTSIDE"
  "COMPARTMENT-IS-SET-UNITS"
  "COMPARTMENT-IS-SET-VOLUME"
  "COMPARTMENT-IS-SET-SIZE"
  "COMPARTMENT-IS-SET-NAME"
  "COMPARTMENT-IS-SET-ID"
  "COMPARTMENT-GET-CONSTANT"
  "COMPARTMENT-GET-OUTSIDE"
  "COMPARTMENT-GET-UNITS"
  "COMPARTMENT-GET-VOLUME"
  "COMPARTMENT-GET-SIZE"
  "COMPARTMENT-GET-SPATIAL-DIMENSIONS"
  "COMPARTMENT-GET-NAME"
  "COMPARTMENT-GET-ID"
  "COMPARTMENT-INIT-DEFAULTS"
  "COMPARTMENT-FREE"
  "COMPARTMENT-CREATE-WITH"
  "COMPARTMENT-CREATE"
  "UNIT-DEFINITION-ID-CMP"
  "UNIT-DEFINITION-GET-NUM-UNITS"
  "UNIT-DEFINITION-GET-UNIT"
  "UNIT-DEFINITION-GET-LIST-OF-UNITS"
  "UNIT-DEFINITION-ADD-UNIT"
  "UNIT-DEFINITION-UNSET-NAME"
  "UNIT-DEFINITION-SET-NAME"
  "UNIT-DEFINITION-SET-ID"
  "UNIT-DEFINITION-IS-SET-NAME"
  "UNIT-DEFINITION-IS-SET-ID"
  "UNIT-DEFINITION-GET-NAME"
  "UNIT-DEFINITION-GET-ID"
  "UNIT-DEFINITION-FREE"
  "UNIT-DEFINITION-CREATE-WITH-NAME"
  "UNIT-DEFINITION-CREATE-WITH"
  "UNIT-DEFINITION-CREATE"
  "UNIT-SET-OFFSET"
  "UNIT-SET-MULTIPLIER"
  "UNIT-SET-SCALE"
  "UNIT-SET-EXPONENT"
  "UNIT-SET-KIND"
  "UNIT-IS-SET-KIND"
  "UNIT-GET-OFFSET"
  "UNIT-GET-MULTIPLIER"
  "UNIT-GET-SCALE"
  "UNIT-GET-EXPONENT"
  "UNIT-GET-KIND"
  "UNIT-INIT-DEFAULTS"
  "UNIT-FREE"
  "UNIT-CREATE-WITH"
  "UNIT-CREATE"
  "UNIT-KIND-IS-VALID-UNIT-KIND-STRING"
  "UNIT-KIND-TO-STRING"
  "UNIT-KIND-FOR-NAME"
  "UNIT-KIND-EQUALS"
  "+UNIT-KIND-INVALID+"
  "+UNIT-KIND-WEBER+"
  "+UNIT-KIND-WATT+"
  "+UNIT-KIND-VOLT+"
  "+UNIT-KIND-TESLA+"
  "+UNIT-KIND-STERADIAN+"
  "+UNIT-KIND-SIEVERT+"
  "+UNIT-KIND-SIEMENS+"
  "+UNIT-KIND-SECOND+"
  "+UNIT-KIND-RADIAN+"
  "+UNIT-KIND-PASCAL+"
  "+UNIT-KIND-OHM+"
  "+UNIT-KIND-NEWTON+"
  "+UNIT-KIND-MOLE+"
  "+UNIT-KIND-METRE+"
  "+UNIT-KIND-METER+"
  "+UNIT-KIND-LUX+"
  "+UNIT-KIND-LUMEN+"
  "+UNIT-KIND-LITRE+"
  "+UNIT-KIND-LITER+"
  "+UNIT-KIND-KILOGRAM+"
  "+UNIT-KIND-KELVIN+"
  "+UNIT-KIND-KATAL+"
  "+UNIT-KIND-JOULE+"
  "+UNIT-KIND-ITEM+"
  "+UNIT-KIND-HERTZ+"
  "+UNIT-KIND-HENRY+"
  "+UNIT-KIND-GRAY+"
  "+UNIT-KIND-GRAM+"
  "+UNIT-KIND-FARAD+"
  "+UNIT-KIND-DIMENSIONLESS+"
  "+UNIT-KIND-COULOMB+"
  "+UNIT-KIND-CELSIUS+"
  "+UNIT-KIND-CANDELA+"
  "+UNIT-KIND-BECQUEREL+"
  "+UNIT-KIND-AMPERE+"
  "UNIT-KIND-T"
  "FUNCTION-DEFINITION-ID-CMP"
  "FUNCTION-DEFINITION-UNSET-NAME"
  "FUNCTION-DEFINITION-SET-MATH"
  "FUNCTION-DEFINITION-SET-NAME"
  "FUNCTION-DEFINITION-SET-ID"
  "FUNCTION-DEFINITION-IS-SET-MATH"
  "FUNCTION-DEFINITION-IS-SET-NAME"
  "FUNCTION-DEFINITION-IS-SET-ID"
  "FUNCTION-DEFINITION-GET-MATH"
  "FUNCTION-DEFINITION-GET-NAME"
  "FUNCTION-DEFINITION-GET-ID"
  "FUNCTION-DEFINITION-FREE"
  "FUNCTION-DEFINITION-CREATE-WITH"
  "FUNCTION-DEFINITION-CREATE"
  "ASTNODE-SWAP-CHILDREN"
  "ASTNODE-SET-TYPE"
  "ASTNODE-SET-REAL-WITH-EXPONENT"
  "ASTNODE-SET-REAL"
  "ASTNODE-SET-RATIONAL"
  "ASTNODE-SET-INTEGER"
  "ASTNODE-SET-NAME"
  "ASTNODE-SET-CHARACTER"
  "ASTNODE-IS-UNKNOWN"
  "ASTNODE-IS-UMINUS"
  "ASTNODE-IS-SQRT"
  "ASTNODE-IS-RELATIONAL"
  "ASTNODE-IS-REAL"
  "ASTNODE-IS-RATIONAL"
  "ASTNODE-IS-OPERATOR"
  "ASTNODE-IS-NUMBER"
  "ASTNODE-IS-NAME"
  "ASTNODE-IS-LOGICAL"
  "ASTNODE-IS-LOG10"
  "ASTNODE-IS-LAMBDA"
  "ASTNODE-IS-INTEGER"
  "ASTNODE-IS-FUNCTION"
  "ASTNODE-IS-CONSTANT"
  "ASTNODE-GET-TYPE"
  "ASTNODE-GET-PRECEDENCE"
  "ASTNODE-GET-EXPONENT"
  "ASTNODE-GET-MANTISSA"
  "ASTNODE-GET-REAL"
  "ASTNODE-GET-DENOMINATOR"
  "ASTNODE-GET-NUMERATOR"
  "ASTNODE-GET-NAME"
  "ASTNODE-GET-INTEGER"
  "ASTNODE-GET-CHARACTER"
  "ASTNODE-FILL-LIST-OF-NODES"
  "ASTNODE-GET-LIST-OF-NODES"
  "ASTNODE-GET-NUM-CHILDREN"
  "ASTNODE-GET-RIGHT-CHILD"
  "ASTNODE-GET-LEFT-CHILD"
  "ASTNODE-GET-CHILD"
  "ASTNODE-PREPEND-CHILD"
  "ASTNODE-ADD-CHILD"
  "ASTNODE-CANONICALIZE"
  "ASTNODE-FREE-NAME"
  "ASTNODE-FREE"
  "ASTNODE-CREATE-FROM-TOKEN"
  "ASTNODE-CREATE-WITH-TYPE"
  "ASTNODE-CREATE"
  "+AST-UNKNOWN+"
  "+AST-RELATIONAL-NEQ+"
  "+AST-RELATIONAL-LT+"
  "+AST-RELATIONAL-LEQ+"
  "+AST-RELATIONAL-GT+"
  "+AST-RELATIONAL-GEQ+"
  "+AST-RELATIONAL-EQ+"
  "+AST-LOGICAL-XOR+"
  "+AST-LOGICAL-OR+"
  "+AST-LOGICAL-NOT+"
  "+AST-LOGICAL-AND+"
  "+AST-FUNCTION-TANH+"
  "+AST-FUNCTION-TAN+"
  "+AST-FUNCTION-SINH+"
  "+AST-FUNCTION-SIN+"
  "+AST-FUNCTION-SECH+"
  "+AST-FUNCTION-SEC+"
  "+AST-FUNCTION-ROOT+"
  "+AST-FUNCTION-POWER+"
  "+AST-FUNCTION-PIECEWISE+"
  "+AST-FUNCTION-LOG+"
  "+AST-FUNCTION-LN+"
  "+AST-FUNCTION-FLOOR+"
  "+AST-FUNCTION-FACTORIAL+"
  "+AST-FUNCTION-EXP+"
  "+AST-FUNCTION-CSCH+"
  "+AST-FUNCTION-CSC+"
  "+AST-FUNCTION-COTH+"
  "+AST-FUNCTION-COT+"
  "+AST-FUNCTION-COSH+"
  "+AST-FUNCTION-COS+"
  "+AST-FUNCTION-CEILING+"
  "+AST-FUNCTION-ARCTANH+"
  "+AST-FUNCTION-ARCTAN+"
  "+AST-FUNCTION-ARCSINH+"
  "+AST-FUNCTION-ARCSIN+"
  "+AST-FUNCTION-ARCSECH+"
  "+AST-FUNCTION-ARCSEC+"
  "+AST-FUNCTION-ARCCSCH+"
  "+AST-FUNCTION-ARCCSC+"
  "+AST-FUNCTION-ARCCOTH+"
  "+AST-FUNCTION-ARCCOT+"
  "+AST-FUNCTION-ARCCOSH+"
  "+AST-FUNCTION-ARCCOS+"
  "+AST-FUNCTION-ABS+"
  "+AST-FUNCTION+"
  "+AST-LAMBDA+"
  "+AST-CONSTANT-TRUE+"
  "+AST-CONSTANT-PI+"
  "+AST-CONSTANT-FALSE+"
  "+AST-CONSTANT-E+"
  "+AST-NAME-TIME+"
  "+AST-NAME-DELAY+"
  "+AST-NAME+"
  "+AST-RATIONAL+"
  "+AST-REAL-E+"
  "+AST-REAL+"
  "+AST-INTEGER+"
  "+AST-POWER+"
  "+AST-DIVIDE+"
  "+AST-TIMES+"
  "+AST-MINUS+"
  "+AST-PLUS+"
  "ASTNODE-TYPE-T"
  "TOKEN-NEGATE-VALUE"
  "TOKEN-GET-REAL"
  "TOKEN-GET-INTEGER"
  "TOKEN-FREE"
  "TOKEN-CREATE"
  "FORMULA-TOKENIZER-NEXT-TOKEN"
  "FORMULA-TOKENIZER-FREE"
  "FORMULA-TOKENIZER-CREATE"
  "+TT-UNKNOWN+"
  "+TT-REAL-E+"
  "+TT-REAL+"
  "+TT-INTEGER+"
  "+TT-NAME+"
  "+TT-END+"
  "+TT-COMMA+"
  "+TT-RPAREN+"
  "+TT-LPAREN+"
  "+TT-POWER+"
  "+TT-DIVIDE+"
  "+TT-TIMES+"
  "+TT-MINUS+"
  "+TT-PLUS+"
  "TOKEN-TYPE-T"
  "LIST-OF-REMOVE"
  "LIST-OF-PREPEND"
  "LIST-OF-GET-NUM-ITEMS"
  "LIST-OF-GET"
  "LIST-OF-FIND"
  "LIST-OF-COUNT-IF"
  "LIST-OF-APPEND"
  "LIST-OF-FREE"
  "LIST-OF-CREATE"
  "LIST-SIZE"
  "LIST-REMOVE"
  "LIST-PREPEND"
  "LIST-GET"
  "LIST-FIND-IF"
  "LIST-FIND"
  "LIST-COUNT-IF"
  "LIST-ADD"
  "LIST-NODE-FREE"
  "LIST-FREE"
  "LIST-NODE-CREATE"
  "LIST-CREATE"
  "SBASE-UNSET-ANNOTATION"
  "SBASE-UNSET-NOTES"
  "SBASE-UNSET-META-ID"
  "SBASE-SET-ANNOTATION"
  "SBASE-SET-NOTES"
  "SBASE-SET-META-ID"
  "SBASE-IS-SET-ANNOTATION"
  "SBASE-IS-SET-NOTES"
  "SBASE-IS-SET-META-ID"
  "SBASE-GET-ANNOTATION"
  "SBASE-GET-NOTES"
  "SBASE-GET-META-ID"
  "SBASE-GET-LINE"
  "SBASE-GET-COLUMN"
  "SBASE-GET-TYPE-CODE"
  "SBASE-CLEAR"
  "SBASE-INIT"
  "+SBML-PARAMETER-RULE+"
  "+SBML-COMPARTMENT-VOLUME-RULE+"
  "+SBML-SPECIES-CONCENTRATION-RULE+"
  "+SBML-RATE-RULE+"
  "+SBML-ASSIGNMENT-RULE+"
  "+SBML-ALGEBRAIC-RULE+"
  "+SBML-UNIT+"
  "+SBML-UNIT-DEFINITION+"
  "+SBML-MODIFIER-SPECIES-REFERENCE+"
  "+SBML-SPECIES-REFERENCE+"
  "+SBML-SPECIES+"
  "+SBML-REACTION+"
  "+SBML-PARAMETER+"
  "+SBML-MODEL+"
  "+SBML-LIST-OF+"
  "+SBML-KINETIC-LAW+"
  "+SBML-FUNCTION-DEFINITION+"
  "+SBML-EVENT-ASSIGNMENT+"
  "+SBML-EVENT+"
  "+SBML-DOCUMENT+"
  "+SBML-COMPARTMENT+"
  "SBMLTYPE-CODE-T"
))