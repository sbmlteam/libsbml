/**
 * @file    L3FormulaFormatter.c
 * @brief   Formats an AST formula tree as an SBML L3 formula string.
 * @author  Lucian Smith (from FormulaFormatter, by Ben Bornstein)
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/L3ParserSettings.h>
#include <sbml/math/ASTNode.h>
#include <cassert>

#include <sbml/util/util.h>

/** @cond doxygen-c-only */
/**
 * Converts an AST to a string representation of a formula using a syntax
 * basically derived from SBML Level&nbsp;1, but expanded.
 *
 * @if clike The text-string form of mathematical formulas produced by
 * SBML_formulaToL3String() and read by SBML_parseFormula() and SBML_parseL3Formula() 
 * are in a C-inspired infix notation.  A formula in
 * this text-string form therefore can be handed to a program that
 * understands SBML mathematical expressions, or used as part
 * of a formula translation system.  The syntax is described in detail in
 * the documentation for ASTNode. @endif@if java The text-string form of
 * mathematical formulas produced by <code><a
 * href="libsbml.html#formulaToString(org.sbml.libsbml.ASTNode)">
 * libsbml.formulaToString()</a></code> and read by
 * <code><a href="libsbml.html#parseFormula(java.lang.String)">
 * libsbml.parseFormula()</a></code> and
 * <code><a href="libsbml.html#parseL3Formula(java.lang.String)">
 * libsbml.parseL3Formula()</a></code> are in a 
 * simple C-inspired infix notation.  A
 * formula in this text-string form therefore can be handed to a program
 * that understands SBML mathematical expressions, or used as
 * part of a formula translation system.  The syntax is described in detail
 * in the documentation for ASTNode.   @endif
 *
 * Note that this facility is provided as a convenience by libSBML&mdash;the
 * MathML standard does not actually define a "string-form" equivalent to
 * MathML expression trees, so the choice of formula syntax is somewhat
 * arbitrary.  The approach taken by libSBML is to use the syntax defined by
 * SBML Level&nbsp;1 (which in fact used a text-string representation of
 * formulas and not MathML).  This formula syntax is based mostly on C
 * programming syntax, and may contain operators, function calls, symbols,
 * and white space characters.  The following table provides the precedence
 * rules for the different entities that may appear in formula strings.
 *
 * @htmlinclude math-precedence-table-l3.html
 * 
 * In the table above, @em operand implies the construct is an operand, @em
 * prefix implies the operation is applied to the following arguments, @em
 * unary implies there is one argument, and @em binary implies there are
 * two arguments.  The values in the <b>Precedence</b> column show how the
 * order of different types of operation are determined.  For example, the
 * expression <code>a * b + c</code> is evaluated as <code>(a * b) +
 * c</code> because the @c * operator has higher precedence.  The
 * <b>Associates</b> column shows how the order of similar precedence
 * operations is determined; for example, <code>a - b + c</code> is
 * evaluated as <code>(a - b) + c</code> because the @c + and @c -
 * operators are left-associative.
 *
 * @warning Note that there are a few differences between the above list,
 * and the list used for the L1 SBML_formulaToString():  the "^" operator
 * has a higher precedence here than the unary "-" operator, which is
 * the reverse case from the L1 formulas.  In addition, the "%" (modulo) 
 * operator has been added (and will be produced from 'piecewise' functions
 * of the appropriate form), and there are new operators for the logical
 * and relational ASTNodes.
 *
 * The function call syntax consists of a function name, followed by optional
 * white space, followed by an opening parenthesis token, followed by a
 * sequence of zero or more arguments separated by commas (with each comma
 * optionally preceded and/or followed by zero or more white space
 * characters, followed by a closing parenthesis token.  The function name
 * must be chosen from one of the pre-defined functions in SBML or a
 * user-defined function in the model.  The following table lists the names
 * of certain common mathematical functions; this table corresponds to
 * Table&nbsp;6 in the <a target="_blank" href="http://sbml.org/Documents/Specifications#SBML_Level_1_Version_2">SBML Level&nbsp;1 Version&nbsp;2 specification</a>, augmented with new functions added for SBML Levels 2 and 3:
 *
 * @htmlinclude string-functions-table-l3.html
 *
 * @warning Note that unlike SBML_formulaToString(), this function will
 * never produce the string 'log' as shorthand for either natural or 
 * base-10 logarithms.  Instead, it will always explicitly produce 
 * 'log10(x)' for base-10 logarithms, 'ln(x)' for natural logarhythms,
 * and 'log(x, y)' for logarithms of arbitrary bases.
 *
 * @warning Note also that for MathML n-ary functions that can take zero
 * or one arguments, the functional form of the operator will be produced.
 * This means that if the function is given an ASTNode of type AST_PLUS 
 * with one or no children, it will produce "plus(x)" and "plus()", respectively.
 * The same thing will happen for AST_TIMES, all logical and relational 
 * operators, and even AST_DIVIDE and AST_MINUS, though it is illegal to
 * have ASTNodes of some of the latter types with one or no children.
 *
 * In addition, ASTNodes that represent constants will be output as follows:
 *
 * @htmlinclude string-values-table-l3.html
 *
 * (with 'INF' and 'NaN' produced for the last two items.
 * 
 *
 * @param tree the AST to be converted.
 * 
 * @return the formula from the given AST as an SBML Level 3 text-string
 * mathematical formula.  The caller owns the returned string and is
 * responsible for freeing it when it is no longer needed.
 *
 * @see SBML_parseFormula()
 * @see SBML_parseL3Formula()
 * @see SBML_formualToString()
 * @see SBML_formulaToL3StringWithSettings()
 */
LIBSBML_EXTERN
char *
SBML_formulaToL3String (const ASTNode_t *tree)
{
  return SBML_formulaToL3StringWithSettings(tree, NULL);
}


/**
 * Converts an AST to a string representation of a formula using a syntax
 * basically derived from SBML Level&nbsp;1, with behavior modifiable with
 * custom settings.
 *
 * This function behaves identically to SBML_formulaToL3String(), but 
 * its behavior can be modified by two settings in the @param settings
 * object, namely:
 *
 * @li ParseUnits:  If this is set to 'true' (the default), the function will 
 *     write out the units of any numerical ASTNodes that have them, producing
 *     (for example) "3 mL", "(3/4) m", or "5.5e-10 M".  If this is set to
 *     'false', this function will only write out the number itself ("3",
 *     "(3/4)", and "5.5e-10", in the previous examples).
 *
 * @li CollapseMinus: If this is set to 'false' (the default), the function
 *     will write out explicitly any doubly-nested unary minus ASTNodes,
 *     producing (for example) "--x" or even "-----3.1".  If this is set
 *     to 'true', the function will collapse the nodes before producing the
 *     infix, producing "x" and "-3.1" in the previous examples.
 *
 * All other settings will not affect the behavior of this function:  the
 * 'parseLog' setting is ignored, and "log10(x)", "ln(x)", and "log(x, y)" 
 * are always produced.  Nothing in the Model object is used, and whether
 * Avogadro is a csymbol or not is immaterial to the produced infix.
 *
 * @param tree the AST to be converted.
 * @param settings the L3ParserSettings object used to modify behavior.
 * 
 * @return the formula from the given AST as an SBML Level 3 text-string
 * mathematical formula.  The caller owns the returned string and is
 * responsible for freeing it when it is no longer needed.
 *
 * @see SBML_parseFormula()
 * @see SBML_parseL3Formula()
 * @see SBML_formulaToL3String()
 */
LIBSBML_EXTERN
char *
SBML_formulaToL3StringWithSettings (const ASTNode_t *tree, const L3ParserSettings_t *settings)
{
  char           *s;
  StringBuffer_t *sb = StringBuffer_create(128);

  if (tree == NULL)
  {
    return NULL;
  }

  L3FormulaFormatter_visit(NULL, tree, sb, settings);
  s = StringBuffer_getBuffer(sb);
  safe_free(sb);
  return s;
}
/** @endcond */


/**
 * @cond doxygen-libsbml-internal
 * The rest of this file is internal code.
 */


/**
 * @return true (non-zero) if the given child ASTNode should be grouped
 * (with parenthesis), false (0) otherwise.
 *
 * A node should be group if it is not an argument to a function and
 * either:
 *
 *   - The parent node has higher precedence than the child, or
 *
 *   - If parent node has equal precedence with the child and the child is
 *     to the right.  In this case, operator associativity and right-most
 *     AST derivation enforce the grouping.
 */
int
L3FormulaFormatter_isGrouped (const ASTNode_t *parent, const ASTNode_t *child, const L3ParserSettings_t *settings)
{
  int pp, cp;
  int pt, ct;
  int group = 0;
  int parentmodulo = 0;


  if (parent != NULL)
  {
    parentmodulo = ASTNode_isModulo(parent);
    if (parentmodulo || !L3FormulaFormatter_isFunction(parent, settings))
    {
      group = 1;
      pp = ASTNode_getL3Precedence(parent);
      cp = ASTNode_getL3Precedence(child);

      if (pp < cp)
      {
        group = 0;
      }
      else if (pp == cp)
      {
        if (parentmodulo) {
          //Always group:  x * y % z -> (x * y) % z
          group = 1;
        }
        /**
         * Don't group only if i) child is the first on the list and ii) both parent and
         * child are the same type, or if they
         * should be associative operators (i.e. not AST_MINUS or
         * AST_DIVIDE).  That is, do not group a parent and left child
         * that are either both AST_PLUS or both AST_TIMES operators, nor the logical operators
         * that have the same precedence.
         */
        if (ASTNode_getLeftChild(parent) == child)
        {
          if (ASTNode_isLogical(parent) || ASTNode_isRelational(parent)) {
            group = 0;
          }
          else {
            pt = ASTNode_getType(parent);
            ct = ASTNode_getType(child);

            group = !((pt == ct) || (pt == AST_PLUS || pt == AST_TIMES));
          }
        }
      }
      else if (pp==7 && cp==6) {
        //If the parent is 'power' and the child is 'unary not' or 'unary minus', we only need
        // to group if the child is the *left* child:  '(-x)^y', but 'x^-y'.
        if (!(ASTNode_getLeftChild(parent) == child)) { 
          group = 0;
        }
      }
    }
  }

  return group;
}


/**
 * Formats the given ASTNode as an SBML L3 token and appends the result to
 * the given StringBuffer.
 */
void
L3FormulaFormatter_format (StringBuffer_t *sb, const ASTNode_t *node, const L3ParserSettings_t *settings)
{
  if (sb == NULL) return;
  if (L3FormulaFormatter_isFunction(node, settings))
  {
    L3FormulaFormatter_formatFunction(sb, node, settings);
  }
  else if (ASTNode_isOperator(node) || ASTNode_getType(node) == AST_FUNCTION_POWER)
  {
    L3FormulaFormatter_formatOperator(sb, node);
  }
  else if (ASTNode_isLogical(node) || ASTNode_isRelational(node))
  {
    L3FormulaFormatter_formatLogicalRelational(sb, node);
  }
  else if (ASTNode_isRational(node))
  {
    L3FormulaFormatter_formatRational(sb, node, settings);
  }
  else if (ASTNode_isInteger(node))
  {
    L3FormulaFormatter_formatReal(sb, node, settings);
  }
  else if (ASTNode_isReal(node))
  {
    L3FormulaFormatter_formatReal(sb, node, settings);
  }
  else if ( !ASTNode_isUnknown(node) )
  {
    StringBuffer_append(sb, ASTNode_getName(node));
  }
}


/**
 * Formats the given ASTNode as an SBML L3 function name and appends the
 * result to the given StringBuffer.
 */
void
L3FormulaFormatter_formatFunction (StringBuffer_t *sb, const ASTNode_t *node, const L3ParserSettings_t *settings)
{
  ASTNodeType_t type = ASTNode_getType(node);
  switch (type)
  {
  case AST_PLUS:
    StringBuffer_append(sb, "plus");
    break;
  case AST_TIMES:
    StringBuffer_append(sb, "times");
    break;
  case AST_MINUS:
    StringBuffer_append(sb, "minus");
    break;
  case AST_DIVIDE:
    StringBuffer_append(sb, "divide");
    break;
  case AST_POWER:
    StringBuffer_append(sb, "pow");
    break;
  case AST_FUNCTION_LN:
    StringBuffer_append(sb, "ln");
    break;

  default:
    FormulaFormatter_formatFunction(sb, node);
    break;
  }
}


/**
 * Formats the given ASTNode as an SBML L1 operator and appends the result
 * to the given StringBuffer.
 */
void
L3FormulaFormatter_formatOperator (StringBuffer_t *sb, const ASTNode_t *node)
{
  ASTNodeType_t type = ASTNode_getType(node);

  if (type == AST_FUNCTION_POWER ||
      type == AST_POWER) {
    StringBuffer_appendChar(sb, '^');
  }
  else 
  {
    StringBuffer_appendChar(sb, ' ');
    StringBuffer_appendChar(sb, ASTNode_getCharacter(node));
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
L3FormulaFormatter_formatRational (StringBuffer_t *sb, const ASTNode_t *node, const L3ParserSettings_t *settings)
{
  StringBuffer_appendChar( sb, '(');
  StringBuffer_appendInt ( sb, ASTNode_getNumerator(node)   );
  StringBuffer_appendChar( sb, '/');
  StringBuffer_appendInt ( sb, ASTNode_getDenominator(node) );
  StringBuffer_appendChar( sb, ')');

  if (L3ParserSettings_getParseUnits(settings)) {
    if (ASTNode_hasUnits(node)) {
      StringBuffer_appendChar( sb, ' ');
      StringBuffer_append( sb, ASTNode_getUnits(node));
    }
  }
}


/**
 * Formats the given ASTNode as a real number and appends the result to
 * the given StringBuffer.
 */
void
L3FormulaFormatter_formatReal (StringBuffer_t *sb, const ASTNode_t *node, const L3ParserSettings_t *settings)
{
  double value = ASTNode_getReal(node);
  int    sign;

  if (ASTNode_isInteger(node)) {
    value = ASTNode_getInteger(node);
  }

  if (util_isNaN(value))
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
    if (ASTNode_getType(node) == AST_REAL_E)
    {
      StringBuffer_appendExp(sb, value);
    }
    else
    {
      StringBuffer_appendReal(sb, value);
    }
  }
  if (L3ParserSettings_getParseUnits(settings)) {
    if (ASTNode_hasUnits(node)) {
      StringBuffer_appendChar( sb, ' ');
      StringBuffer_append( sb, ASTNode_getUnits(node));
    }
  }
}

/**
 * Formats the given ASTNode as an SBML L1 operator and appends the result
 * to the given StringBuffer.
 */
void
L3FormulaFormatter_formatLogicalRelational (StringBuffer_t *sb, const ASTNode_t *node)
{
  ASTNodeType_t type = ASTNode_getType(node);

  StringBuffer_appendChar(sb, ' ');
  switch(type)
  {
  case AST_LOGICAL_AND:
    StringBuffer_append(sb, "&&");
    break;
  case AST_LOGICAL_OR:
    StringBuffer_append(sb, "||");
    break;
  case AST_RELATIONAL_EQ:
    StringBuffer_append(sb, "==");
    break;
  case AST_RELATIONAL_GEQ:
    StringBuffer_append(sb, ">=");
    break;
  case AST_RELATIONAL_GT:
    StringBuffer_append(sb, ">");
    break;
  case AST_RELATIONAL_LEQ:
    StringBuffer_append(sb, "<=");
    break;
  case AST_RELATIONAL_LT:
    StringBuffer_append(sb, "<");
    break;
  case AST_RELATIONAL_NEQ:
    StringBuffer_append(sb, "!=");
    break;
  case AST_LOGICAL_NOT:
  case AST_LOGICAL_XOR:
  default:
    //Should never be called for these cases; unary not is
    // handled by checking 'isUNot' earlier; xor always
    // claims that it's a function, and is caught with 'isFunction'
    assert(0); 
    StringBuffer_append(sb, "!!");
    break;
  }
  StringBuffer_appendChar(sb, ' ');
}


/**
 * Visits the given ASTNode node.  This function is really just a
 * dispatcher to either SBML_formulaToL3String_visitFunction() or
 * SBML_formulaToL3String_visitOther().
 */
void
L3FormulaFormatter_visit ( const ASTNode_t *parent,
                           const ASTNode_t *node,
                           StringBuffer_t  *sb, 
                           const L3ParserSettings_t *settings )
{

  if (ASTNode_isLog10(node))
  {
    L3FormulaFormatter_visitLog10(parent, node, sb, settings);
  }
  else if (ASTNode_isSqrt(node))
  {
    L3FormulaFormatter_visitSqrt(parent, node, sb, settings);
  }
  else if (ASTNode_isModulo(node))
  {
    L3FormulaFormatter_visitModulo(parent, node, sb, settings);
  }
  else if (L3FormulaFormatter_isFunction(node, settings))
  {
    L3FormulaFormatter_visitFunction(parent, node, sb, settings);
  }
  else if (ASTNode_isUMinus(node))
  {
    L3FormulaFormatter_visitUMinus(parent, node, sb, settings);
  }
  else if (ASTNode_isUNot(node))
  {
    L3FormulaFormatter_visitUNot(parent, node, sb, settings);
  }
  else
  {
    L3FormulaFormatter_visitOther(parent, node, sb, settings);
  }
}


/**
 * Visits the given ASTNode as a function.  For this node only the
 * traversal is preorder.
 */
void
L3FormulaFormatter_visitFunction ( const ASTNode_t *parent,
                                   const ASTNode_t *node,
                                   StringBuffer_t  *sb, 
                                   const L3ParserSettings_t *settings )
{
  unsigned int numChildren = ASTNode_getNumChildren(node);
  unsigned int n;


  L3FormulaFormatter_format(sb, node, settings);
  StringBuffer_appendChar(sb, '(');

  if (numChildren > 0)
  {
    L3FormulaFormatter_visit( node, ASTNode_getChild(node, 0), sb, settings);
  }

  for (n = 1; n < numChildren; n++)
  {
    StringBuffer_appendChar(sb, ',');
    StringBuffer_appendChar(sb, ' ');
    L3FormulaFormatter_visit( node, ASTNode_getChild(node, n), sb, settings);
  }

  StringBuffer_appendChar(sb, ')');
}


/**
 * Visits the given ASTNode as the function "log(10, x)" and in doing so,
 * formats it as "log10(x)" (where x is any subexpression).
 */
void
L3FormulaFormatter_visitLog10 ( const ASTNode_t *parent,
                                const ASTNode_t *node,
                                StringBuffer_t  *sb, 
                                const L3ParserSettings_t *settings )
{
  StringBuffer_append(sb, "log10(");
  L3FormulaFormatter_visit(node, ASTNode_getChild(node, 1), sb, settings);
  StringBuffer_appendChar(sb, ')');
}


/**
 * Visits the given ASTNode as the function "root(2, x)" and in doing so,
 * formats it as "sqrt(x)" (where x is any subexpression).
 */
void
L3FormulaFormatter_visitSqrt ( const ASTNode_t *parent,
                               const ASTNode_t *node,
                               StringBuffer_t  *sb, 
                               const L3ParserSettings_t *settings )
{
  StringBuffer_append(sb, "sqrt(");
  L3FormulaFormatter_visit(node, ASTNode_getChild(node, 1), sb, settings);
  StringBuffer_appendChar(sb, ')');
}


/**
 * Visits the given ASTNode as a unary minus.  For this node only the
 * traversal is preorder.
 */
void
L3FormulaFormatter_visitUMinus ( const ASTNode_t *parent,
                                 const ASTNode_t *node,
                                 StringBuffer_t  *sb, 
                                 const L3ParserSettings_t *settings )
{
  //Unary minus is *not* the highest precedence, since it is superceded by 'power'
  unsigned int group;
  
  //If we are supposed to collapse minuses, do so.
  if (L3ParserSettings_getParseCollapseMinus(settings)) {
    if (ASTNode_getNumChildren(node) == 1 &&
        ASTNode_isUMinus(ASTNode_getLeftChild(node))) {
      L3FormulaFormatter_visit(parent, ASTNode_getLeftChild(ASTNode_getLeftChild(node)), sb, settings);
      return;
    }
  }
  
  group = L3FormulaFormatter_isGrouped(parent, node, settings);

  if (group)
  {
    StringBuffer_appendChar(sb, '(');
  }
  StringBuffer_appendChar(sb, '-');
  L3FormulaFormatter_visit ( node, ASTNode_getLeftChild(node), sb, settings);
  if (group)
  {
    StringBuffer_appendChar(sb, ')');
  }
}


/**
 * Visits the given ASTNode as a unary not.
 */
void
L3FormulaFormatter_visitUNot ( const ASTNode_t *parent,
                               const ASTNode_t *node,
                               StringBuffer_t  *sb, 
                               const L3ParserSettings_t *settings )
{
  //Unary not is also not the highest precedence, since it is superceded by 'power'
  unsigned int group       = L3FormulaFormatter_isGrouped(parent, node, settings);

  if (group)
  {
    StringBuffer_appendChar(sb, '(');
  }
  StringBuffer_appendChar(sb, '!');
  L3FormulaFormatter_visit ( node, ASTNode_getLeftChild(node), sb, settings);
  if (group)
  {
    StringBuffer_appendChar(sb, ')');
  }
}


/**
 * Visits the given ASTNode, translating the piecewise function
 * to the much simpler 'x % y' format.
 */
void
L3FormulaFormatter_visitModulo ( const ASTNode_t *parent,
                                 const ASTNode_t *node,
                                 StringBuffer_t  *sb, 
                                 const L3ParserSettings_t *settings )
{
  unsigned int group       = L3FormulaFormatter_isGrouped(parent, node, settings);
  const ASTNode_t* subnode = ASTNode_getLeftChild(node);
  if (group)
  {
    StringBuffer_appendChar(sb, '(');
  }

  //Get x and y from the first child of the piecewise function, 
  // then the first child of that (times), and the first child
  // of that (minus).
  L3FormulaFormatter_visit ( subnode, ASTNode_getLeftChild(subnode), sb, settings);
  StringBuffer_appendChar(sb, ' ');
  StringBuffer_appendChar(sb, '%');
  StringBuffer_appendChar(sb, ' ');
  subnode = ASTNode_getRightChild(subnode);
  L3FormulaFormatter_visit ( node, ASTNode_getLeftChild(subnode), sb, settings);

  if (group)
  {
    StringBuffer_appendChar(sb, ')');
  }
}


/**
 * Visits the given ASTNode and continues the inorder traversal.
 */
void
L3FormulaFormatter_visitOther ( const ASTNode_t *parent,
                                const ASTNode_t *node,
                                StringBuffer_t  *sb, 
                                const L3ParserSettings_t *settings )
{
  unsigned int numChildren = ASTNode_getNumChildren(node);
  unsigned int group       = L3FormulaFormatter_isGrouped(parent, node, settings);
  unsigned int n;


  if (group)
  {
    StringBuffer_appendChar(sb, '(');
  }

  if (numChildren == 0) {
    L3FormulaFormatter_format(sb, node, settings);
  }

  else if (numChildren == 1)
  {
    //I believe this would only be called for invalid ASTNode setups,
    // but this could in theory occur.  This is the safest 
    // behavior I can think of.
    L3FormulaFormatter_format(sb, node, settings);
    StringBuffer_appendChar(sb, '(');
    L3FormulaFormatter_visit( node, ASTNode_getChild(node, 0), sb, settings);
    StringBuffer_appendChar(sb, ')');
  }

  else {
    L3FormulaFormatter_visit( node, ASTNode_getChild(node, 0), sb, settings);

    for (n = 1; n < numChildren; n++)
    {
      L3FormulaFormatter_format(sb, node, settings);
      L3FormulaFormatter_visit( node, ASTNode_getChild(node, n), sb, settings);
    }
  }

  if (group)
  {
    StringBuffer_appendChar(sb, ')');
  }
}

//This function determines if the node in question should be
// expressed in the form "functioname(children)" or if
// it should be expressed as "child1 [symbol] child2 [symbol] child3"
// etc.
int
L3FormulaFormatter_isFunction (const ASTNode_t *node, 
                               const L3ParserSettings_t *settings)
{
  if (node==NULL) return 0;
  switch(ASTNode_getType(node))
  {
  case AST_PLUS:
  case AST_TIMES:
  case AST_LOGICAL_AND:
  case AST_LOGICAL_OR:
    if (ASTNode_getNumChildren(node) >= 2) {
      return 0;
    }
    return 1;

  case AST_RELATIONAL_EQ:
  case AST_RELATIONAL_GEQ:
  case AST_RELATIONAL_GT:
  case AST_RELATIONAL_LEQ:
  case AST_RELATIONAL_LT:
    if (ASTNode_getNumChildren(node) == 2) {
      return 0;
    }
    return 1;

  case AST_MINUS:
    if (ASTNode_getNumChildren(node) == 1) {
      return 0;
    }
  case AST_DIVIDE:
  case AST_RELATIONAL_NEQ:
  case AST_POWER:
  case AST_FUNCTION_POWER:
    if (ASTNode_getNumChildren(node) == 2) {
      return 0;
    }
    return 1;

  case AST_LOGICAL_NOT:
    if (ASTNode_getNumChildren(node) == 1) {
      return 0;
    }
    return 1;

  case AST_INTEGER:
  case AST_REAL:
  case AST_REAL_E:
  case AST_RATIONAL:
  case AST_NAME:
  case AST_NAME_AVOGADRO:
  case AST_NAME_TIME:
  case AST_CONSTANT_E:
  case AST_CONSTANT_FALSE:
  case AST_CONSTANT_PI:
  case AST_CONSTANT_TRUE:
    return 0;

  case AST_LOGICAL_XOR:
  case AST_LAMBDA:
  case AST_FUNCTION:
  case AST_FUNCTION_ABS:
  case AST_FUNCTION_ARCCOS:
  case AST_FUNCTION_ARCCOSH:
  case AST_FUNCTION_ARCCOT:
  case AST_FUNCTION_ARCCOTH:
  case AST_FUNCTION_ARCCSC:
  case AST_FUNCTION_ARCCSCH:
  case AST_FUNCTION_ARCSEC:
  case AST_FUNCTION_ARCSECH:
  case AST_FUNCTION_ARCSIN:
  case AST_FUNCTION_ARCSINH:
  case AST_FUNCTION_ARCTAN:
  case AST_FUNCTION_ARCTANH:
  case AST_FUNCTION_CEILING:
  case AST_FUNCTION_COS:
  case AST_FUNCTION_COSH:
  case AST_FUNCTION_COT:
  case AST_FUNCTION_COTH:
  case AST_FUNCTION_CSC:
  case AST_FUNCTION_CSCH:
  case AST_FUNCTION_DELAY:
  case AST_FUNCTION_EXP:
  case AST_FUNCTION_FACTORIAL:
  case AST_FUNCTION_FLOOR:
  case AST_FUNCTION_LN:
  case AST_FUNCTION_LOG:
  case AST_FUNCTION_PIECEWISE:
  case AST_FUNCTION_ROOT:
  case AST_FUNCTION_SEC:
  case AST_FUNCTION_SECH:
  case AST_FUNCTION_SIN:
  case AST_FUNCTION_SINH:
  case AST_FUNCTION_TAN:
  case AST_FUNCTION_TANH:
  case AST_UNKNOWN:
    return 1;
  }
  //Shouldn't ever get here
  assert(0);
  return 1;
}


/** @endcond */

