/**
 * Filename    : FormulaFormatter.c
 * Description : Formats an AST formula tree as an SBML formula string
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-13
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/FormulaFormatter.h"


/**
 * @return the given formula AST as an SBML L1 string formula.  The caller
 * owns the returned string and is responsible for freeing it.
 */
LIBSBML_EXTERN
char *
SBML_formulaToString (const ASTNode_t *tree)
{
  StringBuffer_t *sb = StringBuffer_create(128);
  char           *s;


  FormulaFormatter_visit(NULL, tree, sb);
  s = StringBuffer_getBuffer(sb);
  safe_free(sb);

  return s;
}


/**
 * @return true (non-zero) if the given ASTNode is to formatted as a
 * function.
 */
int
FormulaFormatter_isFunction (const ASTNode_t *node)
{
  return
    ASTNode_isFunction  (node) ||
    ASTNode_isLambda    (node) ||
    ASTNode_isLogical   (node) ||
    ASTNode_isRelational(node);
}


/**
 * @return true (non-zero) if the given ASTNode should be grouped (with
 * parenthesis), false (0) otherwise.
 *
 * A node should be group if it is not an argument to a function and its
 * parent node has higher precedence than it.
 */
int
FormulaFormatter_isGrouped (const ASTNode_t *parent, const ASTNode_t *node)
{
  int group = 0;


  if (parent != NULL)
  {
    if (!FormulaFormatter_isFunction(parent))
    {
      group = (ASTNode_getPrecedence(parent) > ASTNode_getPrecedence(node));
    }
  }

  return group;
}


/**
 * Formats the given ASTNode as an SBML L1 token and appends the result to
 * the given StringBuffer.
 */
void
FormulaFormatter_format (StringBuffer_t *sb, const ASTNode_t *node)
{
  if (ASTNode_isOperator(node))
  {
    FormulaFormatter_formatOperator(sb, node);
  }
  else if (ASTNode_isFunction(node))
  {
    FormulaFormatter_formatFunction(sb, node);
  }
  else if (ASTNode_isInteger(node))
  {
    StringBuffer_appendInt(sb, ASTNode_getInteger(node));
  }
  else if (ASTNode_isRational(node))
  {
    FormulaFormatter_formatRational(sb, node);
  }
  else if (ASTNode_isReal(node))
  {
    FormulaFormatter_formatReal(sb, node);
  }
  else if ( !ASTNode_isUnknown(node) )
  {
    StringBuffer_append(sb, ASTNode_getName(node));
  }
}


/**
 * Formats the given ASTNode as an SBML L1 function name and appends the
 * result to the given StringBuffer.
 */
void
FormulaFormatter_formatFunction (StringBuffer_t *sb, const ASTNode_t *node)
{
  ASTNodeType_t type = ASTNode_getType(node);


  switch (type)
  {
    case AST_FUNCTION_ARCCOS:
      StringBuffer_append(sb, "acos");
      break;

    case AST_FUNCTION_ARCSIN:
      StringBuffer_append(sb, "asin");
      break;

    case AST_FUNCTION_ARCTAN:
      StringBuffer_append(sb, "atan");
      break;

    case AST_FUNCTION_CEILING:
      StringBuffer_append(sb, "ceil");
      break;

    case AST_FUNCTION_LN:
      StringBuffer_append(sb, "log");
      break;

    case AST_FUNCTION_POWER:
      StringBuffer_append(sb, "pow");
      break;

    default:
      StringBuffer_append(sb, ASTNode_getName(node));
      break;
  }
}


/**
 * Formats the given ASTNode as an SBML L1 operator and appends the result
 * to the given StringBuffer.
 */
void
FormulaFormatter_formatOperator (StringBuffer_t *sb, const ASTNode_t *node)
{
  ASTNodeType_t type = node->type;


  if (type != AST_POWER)
  {
    StringBuffer_appendChar(sb, ' ');
  }

  StringBuffer_appendChar(sb, ASTNode_getCharacter(node));

  if (type != AST_POWER)
  {
    StringBuffer_appendChar(sb, ' ');
  }
}


/**
 * Formats the given ASTNode as a rational number and appends the result to
 * the given StringBuffer.  For SBML L1 this amounts to:
 *
 *   "(numerator/denominator)"
 */
void
FormulaFormatter_formatRational (StringBuffer_t *sb, const ASTNode_t *node)
{
  StringBuffer_appendChar( sb, '(');
  StringBuffer_appendInt ( sb, ASTNode_getNumerator(node)   );
  StringBuffer_appendChar( sb, '/');
  StringBuffer_appendInt ( sb, ASTNode_getDenominator(node) );
  StringBuffer_appendChar( sb, ')');
}


/**
 * Formats the given ASTNode as a real number and appends the result to
 * the given StringBuffer.
 */
void
FormulaFormatter_formatReal (StringBuffer_t *sb, const ASTNode_t *node)
{
  double value = ASTNode_getReal(node);
  int    sign;


  if (isnan(value))
  {
    StringBuffer_append(sb, "NaN");
  }
  else if ((sign = util_isInf(value)) != 0)
  {
    if (sign == -1)
    {
      StringBuffer_appendChar(sb, '-');
    }

    StringBuffer_append(sb, "INF");
  }
  else if (util_isNegZero(value))
  {
    StringBuffer_append(sb, "-0");
  }
  else
  {
    StringBuffer_appendReal(sb, value);
  }
}


/**
 * Visits the given ASTNode node.  This function is really just a
 * dispatcher to either SBML_formulaToString_visitFunction() or
 * SBML_formulaToString_visitOther().
 */
void
FormulaFormatter_visit ( const ASTNode_t *parent,
                         const ASTNode_t *node,
                         StringBuffer_t  *sb )
{

  if (ASTNode_isLog10(node))
  {
    FormulaFormatter_visitLog10(parent, node, sb);
  }
  else if (ASTNode_isSqrt(node))
  {
    FormulaFormatter_visitSqrt(parent, node, sb);
  }
  else if (FormulaFormatter_isFunction(node))
  {
    FormulaFormatter_visitFunction(parent, node, sb);
  }
  else if (ASTNode_isUMinus(node))
  {
    FormulaFormatter_visitUMinus(parent, node, sb);
  }
  else
  {
    FormulaFormatter_visitOther(parent, node, sb);
  }
}


/**
 * Visits the given ASTNode as a function.  For this node only the
 * traversal is preorder.
 */
void
FormulaFormatter_visitFunction ( const ASTNode_t *parent,
                                 const ASTNode_t *node,
                                 StringBuffer_t  *sb )
{
  unsigned int numChildren = ASTNode_getNumChildren(node);
  unsigned int n;


  FormulaFormatter_format(sb, node);
  StringBuffer_appendChar(sb, '(');

  if (numChildren > 0)
  {
    FormulaFormatter_visit( node, ASTNode_getChild(node, 0), sb );
  }

  for (n = 1; n < numChildren; n++)
  {
    StringBuffer_appendChar(sb, ',');
    StringBuffer_appendChar(sb, ' ');
    FormulaFormatter_visit( node, ASTNode_getChild(node, n), sb );
  }

  StringBuffer_appendChar(sb, ')');
}


/**
 * Visits the given ASTNode as the function "log(10, x)" and in doing so,
 * formats it as "log10(x)" (where x is any subexpression).
 */
void
FormulaFormatter_visitLog10 ( const ASTNode_t *parent,
                              const ASTNode_t *node,
                              StringBuffer_t  *sb )
{
  StringBuffer_append(sb, "log10(");
  FormulaFormatter_visit(node, ASTNode_getChild(node, 1), sb);
  StringBuffer_appendChar(sb, ')');
}


/**
 * Visits the given ASTNode as the function "root(2, x)" and in doing so,
 * formats it as "sqrt(x)" (where x is any subexpression).
 */
void
FormulaFormatter_visitSqrt ( const ASTNode_t *parent,
                             const ASTNode_t *node,
                             StringBuffer_t  *sb )
{
  StringBuffer_append(sb, "sqrt(");
  FormulaFormatter_visit(node, ASTNode_getChild(node, 1), sb);
  StringBuffer_appendChar(sb, ')');
}


/**
 * Visits the given ASTNode as a unary minus.  For this node only the
 * traversal is preorder.
 */
void
FormulaFormatter_visitUMinus ( const ASTNode_t *parent,
                               const ASTNode_t *node,
                               StringBuffer_t  *sb )
{
  StringBuffer_appendChar(sb, '-');
  FormulaFormatter_visit ( node, ASTNode_getLeftChild(node), sb );
}


/**
 * Visits the given ASTNode and continues the inorder traversal.
 */
void
FormulaFormatter_visitOther ( const ASTNode_t *parent,
                              const ASTNode_t *node,
                              StringBuffer_t  *sb )
{
  unsigned int numChildren = ASTNode_getNumChildren(node);
  unsigned int group       = FormulaFormatter_isGrouped(parent, node);


  if (group)
  {
    StringBuffer_appendChar(sb, '(');
  }

  if (numChildren > 0)
  {
    FormulaFormatter_visit( node, ASTNode_getLeftChild(node), sb );
  }

  FormulaFormatter_format(sb, node);

  if (numChildren > 1)
  {
    FormulaFormatter_visit( node, ASTNode_getRightChild(node), sb );
  }

  if (group)
  {
    StringBuffer_appendChar(sb, ')');
  }
}
