/**
 * Filename    : ASTNodeType.h
 * Description : Abstract Syntax Tree (AST) for representing formula trees
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef ASTNodeType_h
#define ASTNodeType_h


/**
 * Nodes of type AST_UNKNOWN are used internally as the AST is being
 * constructed.  Trees returned by SBML_parseFormula() will not contain
 * unknown nodes.
 */
typedef enum
{
    AST_PLUS    = '+'
  , AST_MINUS   = '-'
  , AST_TIMES   = '*'
  , AST_DIVIDE  = '/'
  , AST_POWER   = '^'

  , AST_INTEGER = 256
  , AST_REAL
  , AST_REAL_E
  , AST_RATIONAL

  , AST_NAME
  , AST_NAME_TIME

  , AST_CONSTANT_E
  , AST_CONSTANT_FALSE
  , AST_CONSTANT_PI
  , AST_CONSTANT_TRUE

  , AST_LAMBDA

  , AST_FUNCTION
  , AST_FUNCTION_ABS
  , AST_FUNCTION_ARCCOS
  , AST_FUNCTION_ARCCOSH
  , AST_FUNCTION_ARCCOT
  , AST_FUNCTION_ARCCOTH
  , AST_FUNCTION_ARCCSC
  , AST_FUNCTION_ARCCSCH
  , AST_FUNCTION_ARCSEC
  , AST_FUNCTION_ARCSECH
  , AST_FUNCTION_ARCSIN
  , AST_FUNCTION_ARCSINH
  , AST_FUNCTION_ARCTAN
  , AST_FUNCTION_ARCTANH
  , AST_FUNCTION_CEILING
  , AST_FUNCTION_COS
  , AST_FUNCTION_COSH
  , AST_FUNCTION_COT
  , AST_FUNCTION_COTH
  , AST_FUNCTION_CSC
  , AST_FUNCTION_CSCH
  , AST_FUNCTION_DELAY
  , AST_FUNCTION_EXP
  , AST_FUNCTION_FACTORIAL
  , AST_FUNCTION_FLOOR
  , AST_FUNCTION_LN
  , AST_FUNCTION_LOG
  , AST_FUNCTION_PIECEWISE
  , AST_FUNCTION_POWER
  , AST_FUNCTION_ROOT
  , AST_FUNCTION_SEC
  , AST_FUNCTION_SECH
  , AST_FUNCTION_SIN
  , AST_FUNCTION_SINH
  , AST_FUNCTION_TAN
  , AST_FUNCTION_TANH

  , AST_LOGICAL_AND
  , AST_LOGICAL_NOT
  , AST_LOGICAL_OR
  , AST_LOGICAL_XOR

  , AST_RELATIONAL_EQ
  , AST_RELATIONAL_GEQ
  , AST_RELATIONAL_GT
  , AST_RELATIONAL_LEQ
  , AST_RELATIONAL_LT
  , AST_RELATIONAL_NEQ

  , AST_UNKNOWN
} ASTNodeType_t;


#endif  /* ASTNodeType_h */
