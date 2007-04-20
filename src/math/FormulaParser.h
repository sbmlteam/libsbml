/**
 * @file    FormulaParser.h
 * @brief   Parses an SBML formula string into an AST
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef FormulaParser_h
#define FormulaParser_h


#include <sbml/common/extern.h>
#include <sbml/util/Stack.h>

#include <sbml/math/ASTNode.h>
#include <sbml/math/FormulaTokenizer.h>


BEGIN_C_DECLS


/**
 * Parses the given SBML formula and returns a representation of it as an
 * Abstract Syntax Tree (AST).  The root node of the AST is returned.
 *
 * If the formula contains a grammatical error, NULL is returned.
 */
LIBSBML_EXTERN
ASTNode_t *
SBML_parseFormula (const char *formula);


#ifndef SWIG


/**
 * @return the action for the current state and token.
 *
 * ACCEPT_STATE and ERROR_STATE are special and should be tested for first.
 *
 * Postive actions less-than represent shifts.  Negative actions greater
 * than represent reductions by a grammar rule.
 */
long
FormulaParser_getAction (long state, Token_t *token);

/**
 * @return the number of consective tokens in the Action[] table for the
 * given token type.
 *
 * This function is machine-generated.  DO NOT EDIT.
 */
long
FormulaParser_getActionLength (TokenType_t type);

/**
 * @return the starting offset into the Action[] table for the given token
 * type.
 *
 * This function is machine-generated.  DO NOT EDIT.
 */
long
FormulaParser_getActionOffset (TokenType_t type);

/**
 * @return the next (or goto) state for the current state and grammar rule.
 *
 * ERROR_STATE is special and should be tested for first.
 */
long
FormulaParser_getGoto (long state, long rule);

/**
 * Reduces the given stack (containing SLR parser states and ASTNodes) by
 * the given grammar rule.
 */
ASTNode_t *
FormulaParser_reduceStackByRule (Stack_t *stack, long rule);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* FormulaParser_h */
