/**
 * Filename    : MathMLFormatter.cpp
 * Description : Formats MathML
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/MathMLFormatter.hpp"
#include "sbml/MathMLUnicodeConstants.hpp" 
#include "sbml/XMLUnicodeConstants.hpp"
#include "sbml/XMLUtil.hpp"


//
// Most MathML elements, indexed by (ASTNodeType - AST_FUNCTION_ABS).
//
static const XMLCh *MATHML_ELEMENTS[] =
{
    ELEM_ABS
  , ELEM_ARCCOS
  , ELEM_ARCCOSH
  , ELEM_ARCCOT
  , ELEM_ARCCOTH
  , ELEM_ARCCSC
  , ELEM_ARCCSCH
  , ELEM_ARCSEC
  , ELEM_ARCSECH
  , ELEM_ARCSIN
  , ELEM_ARCSINH
  , ELEM_ARCTAN
  , ELEM_ARCTANH
  , ELEM_CEILING
  , ELEM_COS
  , ELEM_COSH
  , ELEM_COT
  , ELEM_COTH
  , ELEM_CSC
  , ELEM_CSCH
  , ELEM_EXP
  , ELEM_FACTORIAL
  , ELEM_FLOOR
  , ELEM_LN
  , ELEM_LOG
  , ELEM_PIECEWISE
  , ELEM_POWER
  , ELEM_ROOT
  , ELEM_SEC
  , ELEM_SECH
  , ELEM_SIN
  , ELEM_SINH
  , ELEM_TAN
  , ELEM_TANH
  , ELEM_AND
  , ELEM_NOT
  , ELEM_OR
  , ELEM_XOR
  , ELEM_EQ
  , ELEM_GEQ
  , ELEM_GT
  , ELEM_LEQ
  , ELEM_LT
  , ELEM_NEQ
};


/**
 * Ctor
 *
 * Creates a new MathMLFormatter with the given character encoding.
 * If outputXMLDecl is true the output will begin with:
 *
 *   <?xml version="1.0" encoding="..."?>
 */
MathMLFormatter::MathMLFormatter ( const char*      outEncoding,
                                   XMLFormatTarget* target,
                                   bool             outputXMLDecl )
{
  //
  // Initialize() is static and may be called more than once safely.
  //
  XMLPlatformUtils::Initialize();

  fIndentLevel = 0;

  fTarget    = target;
  fFormatter = XMLUtil::createXMLFormatter(outEncoding, fTarget);

  if (outputXMLDecl)
  {
    *fFormatter
      << XML_DECL_1
      << fFormatter->getEncodingName()
      << XML_DECL_2;
  }
}


/**
 * Dtor
 */
MathMLFormatter::~MathMLFormatter ()
{
  delete fFormatter;
}




/* ----------------------------------------------------------------------
 *                          Insertion operator
 * ----------------------------------------------------------------------
 */


/**
 * Sends a '<math xmlns="http://www.w3.org/1998/Math/MathML">' to the
 * underlying XMLFormatTarget and then ups the indent level.
 */
void
MathMLFormatter::startMath ()
{
  openStartElement(ELEM_MATH);
  attribute(ATTR_XMLNS, XMLNS_MathML);
  closeStartElement();

  upIndent();
}


/**
 * Downs the indent level and sends '</math>' to the underlying
 * XMLFormatTarget.
 */
void
MathMLFormatter::endMath ()
{
  downIndent();

  indent();
  endElement(ELEM_MATH);
}


/**
 * MathMLDocument insertion operator
 */
MathMLFormatter&
MathMLFormatter::operator<< (const MathMLDocument_t* d)
{
  startMath();

  *this << d->math;

  endMath();

  return *this;
}


/**
 * ASTNode insertion operator
 */
MathMLFormatter&
MathMLFormatter::operator<< (const ASTNode_t* node)
{
  if (node == NULL) return *this;


  if ( ASTNode_isInteger(node) )
  {
    *this << ASTNode_getInteger(node);
  }
  else if ( ASTNode_isRational(node) )
  {
    doRational(node);
  }
  else if ( ASTNode_isReal(node) )
  {
    doReal(node);
  }
  else if ( ASTNode_isName(node) )
  {
    doName(node);
  }
  else if ( ASTNode_isConstant(node) )
  {
    doConstant(node);
  }
  else if ( ASTNode_isOperator(node) )
  {
    doOperator(node);
  }
  else if ( ASTNode_isLambda(node) )
  {
    doLambda(node);
  }
  else if ( ASTNode_getType(node) == AST_FUNCTION_PIECEWISE)
  {
    doPiecewise(node);
  }
  else if ( !ASTNode_isUnknown(node) )
  {
    doFunction(node);
  }

  return *this;
}


/**
 * Inserts the given string as a MathML <ci> element.
 */
MathMLFormatter&
MathMLFormatter::operator<< (const char* s)
{
  startElementSpace(ELEM_CI);

  characters(s);

  spaceEndElement(ELEM_CI);

  return *this;
}


/**
 * Inserts the given integer as a MathML <cn> element.
 */
MathMLFormatter&
MathMLFormatter::operator<< (long value)
{
  startElementCN(VAL_INTEGER);

  snprintf  (fNumberBuffer, NUMBER_BUFFER_SIZE, "%d", value);
  characters(fNumberBuffer);

  spaceEndElement(ELEM_CN);

  return *this;
}


/**
 * Inserts the given real as a MathML <cn> element.
 */
MathMLFormatter&
MathMLFormatter::operator<< (double value)
{
  int isInf = util_isInf(value);


  //
  // +/-Inf and NaN are not represented as <cn> elements in MathML.
  //
  if (isInf > 0)
  {
    startEndElement(ELEM_INFINITY);
    return *this;
  }
  else if (isInf < 0)
  {
    startElement(ELEM_APPLY);
    upIndent();

    startEndElement(ELEM_MINUS);
    startEndElement(ELEM_INFINITY);

    downIndent();

    indent();
    endElement(ELEM_APPLY);
    return *this;
  }
  else if (isnan(value))
  {
    startEndElement(ELEM_NOT_A_NUMBER);
    return *this;
  }

  //
  // Normal case
  //
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%g", value);

  FormulaTokenizer_t* tokenizer = FormulaTokenizer_create(fNumberBuffer);
  Token_t*            token     = FormulaTokenizer_nextToken(tokenizer);
  ASTNode_t*          node      = ASTNode_createFromToken(token);


  if (node->type == AST_REAL_E)
  {
    doENotation(node);
  }
  else
  {
    startElementSpace(ELEM_CN);

    characters(fNumberBuffer);

    spaceEndElement(ELEM_CN);
  }

  ASTNode_free(node);
  Token_free(token);
  FormulaTokenizer_free(tokenizer);

  return *this;
}




/* ----------------------------------------------------------------------
 *                 Insertion Operator Supporting Functions
 * ----------------------------------------------------------------------
 */


/**
 * Formats the given ASTNode as <cn type="real"> or <cn type='e-notation'>
 * as appropriate.
 */
void
MathMLFormatter::doReal (const ASTNode_t* node)
{
  if (ASTNode_getType(node) == AST_REAL_E)
  {
    doENotation(node);
  }
  else
  {
    *this << ASTNode_getReal(node);
  }
}


/**
 * Formats the given ASTNode as <cn type="e-notation"> %f <sep/> %ld </cn>.
 */
void
MathMLFormatter::doENotation (const ASTNode_t* node)
{
  double mantissa = ASTNode_getMantissa(node);
  long   exponent = ASTNode_getExponent(node);


  startElementCN(VAL_E_NOTATION);

  /**
   * Try %g first, as trailing zeros are removed from the fractional part
   * of the result.  However, if the result contains an exponent, fall-back
   * to %f.
   */
  snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%g", mantissa);

  if ( hasExponent(fNumberBuffer) )
  {
    snprintf(fNumberBuffer, NUMBER_BUFFER_SIZE, "%f", mantissa);
  }

  characters(fNumberBuffer);

  sepElement();

  snprintf  (fNumberBuffer, NUMBER_BUFFER_SIZE, "%ld", exponent);
  characters(fNumberBuffer);

  spaceEndElement(ELEM_CN);
}


/**
 * Formats the given ASTNode as <cn type="rational"> %ld <sep/> %ld </cn>.
 */
void
MathMLFormatter::doRational (const ASTNode_t* node)
{
  long numerator   = ASTNode_getNumerator  (node);
  long denominator = ASTNode_getDenominator(node);


  startElementCN(VAL_RATIONAL);

  snprintf  (fNumberBuffer, NUMBER_BUFFER_SIZE, "%ld", numerator);
  characters(fNumberBuffer);

  sepElement();

  snprintf  (fNumberBuffer, NUMBER_BUFFER_SIZE, "%ld", denominator);
  characters(fNumberBuffer);

  spaceEndElement(ELEM_CN);
}


/**
 * Formats the given ASTNode as a <ci> or <csymbol> element as appropriate.
 */
void
MathMLFormatter::doName (const ASTNode_t* node)
{
  ASTNodeType_t type = ASTNode_getType(node);


  if (type == AST_NAME_DELAY || type == AST_NAME_TIME)
  {
    doCSymbol(node);
  }
  else if (type == AST_NAME)
  {
    *this << ASTNode_getName(node);
  }
}


/**
 * Formats the given ASTNode as a <csymbol> time or delay element as
 * appropriate.
 */
void
MathMLFormatter::doCSymbol (const ASTNode_t* node)
{
  ASTNodeType_t type = ASTNode_getType(node);
  const XMLCh*  url  = NULL;


  if (type == AST_NAME_DELAY)
  {
    url = CSYMBOL_DEFINITION_URL_DELAY;
  }
  else if (type == AST_NAME_TIME)
  {
    url = CSYMBOL_DEFINITION_URL_TIME;
  }

  openStartElement(ELEM_CSYMBOL);

  attribute( ATTR_ENCODING      , VAL_TEXT );
  attribute( ATTR_DEFINITION_URL, url      );

  closeStartElementSpace();

  characters( ASTNode_getName(node) );

  spaceEndElement(ELEM_CSYMBOL);
}


/**
 * Formats the given ASTNode as a MathML constant.
 */
void
MathMLFormatter::doConstant (const ASTNode_t* node)
{
  switch (node->type)
  {
    case AST_CONSTANT_E:
      startEndElement(ELEM_EXPONENTIALE);
      break;

    case AST_CONSTANT_FALSE:
      startEndElement(ELEM_FALSE);
      break;

    case AST_CONSTANT_PI:
      startEndElement(ELEM_PI);
      break;

    case AST_CONSTANT_TRUE:
      startEndElement(ELEM_TRUE);
      break;

    default:
      break;
  }  
}


/**
 * Formats the given ASTNode as a <lambda> element.
 */
void
MathMLFormatter::doLambda (const ASTNode_t* node)
{
  unsigned int bvars = ASTNode_getNumChildren(node) - 1;
  unsigned int n;


  startElement(ELEM_LAMBDA);
  upIndent();

  for (n = 0; n < bvars; n++)
  {
    startElement(ELEM_BVAR);
    upIndent();

    *this << ASTNode_getChild(node, n);

    downIndent();
    indent();
    endElement(ELEM_BVAR);
  }

  *this << ASTNode_getChild(node, n);

  downIndent();
  indent();
  endElement(ELEM_LAMBDA);
}


/**
 * Formats the given ASTNode as a <apply> <op/> ... </apply>.
 */
void
MathMLFormatter::doOperator (const ASTNode_t* node)
{
  startElement(ELEM_APPLY);

  upIndent();

  switch (node->type)
  {
    case AST_PLUS:
      startEndElement(ELEM_PLUS);
      break;

    case AST_TIMES:
      startEndElement(ELEM_TIMES);
      break;

    case AST_MINUS:
      startEndElement(ELEM_MINUS);
      break;

    case AST_DIVIDE:
      startEndElement(ELEM_DIVIDE);
      break;

    case AST_POWER:
      startEndElement(ELEM_POWER);
      break;

    default:
      break;
  }

  doOperatorArgs(node);

  downIndent();

  indent();
  endElement(ELEM_APPLY);
}


/**
 * This function formats the children of the given ASTNode and is called by
 * doOperator().
 */
void
MathMLFormatter::doOperatorArgs (const ASTNode_t* node)
{
  ASTNodeType_t type  = ASTNode_getType(node);
  ASTNode_t*    left  = ASTNode_getLeftChild (node);
  ASTNode_t*    right = ASTNode_getRightChild(node);


  //
  // AST_PLUS and AST_TIMES nodes are always binary.  MathML, however,
  // allows n-ary <plus/> and <times/> operators.
  //
  // The recursive call to doOperatorArgs() has the effect of "unrolling"
  // multiple levels of binary AST_PLUS or AST_TIMES nodes into an n-ary
  // expression.
  //
  if (type == AST_PLUS || type == AST_TIMES)
  {
    if (ASTNode_getType(left) == type)
    {
      doOperatorArgs(left);
    }
    else
    {
      *this << left;
    }

    if (ASTNode_getType(right) == type)
    {
      doOperatorArgs(right);
    }
    else
    {
      *this << right;
    }
  }
  else
  {
    *this << left;
    *this << right;
  }
}


/**
 * Formats the given ASTNode as a <piecewise> element.
 */
void
MathMLFormatter::doPiecewise (const ASTNode_t* node)
{
  unsigned int numChildren = ASTNode_getNumChildren(node);
  unsigned int numPieces   = numChildren;
  unsigned int n;

  startElement(ELEM_PIECEWISE);

  upIndent();

  //
  // An odd number of children means the last element is an <otherwise>,
  // not a <piece>
  //
  if ((numChildren % 2) != 0)
  {
    numPieces--;
  }

  //
  // <piece>
  //
  for (n = 0; n < numPieces; n += 2)
  {
    startElement(ELEM_PIECE);

    upIndent();

    *this << ASTNode_getChild(node, n);
    *this << ASTNode_getChild(node, n + 1);

    downIndent();

    indent();
    endElement(ELEM_PIECE);
  }

  //
  // <otherwise>
  //
  if (numPieces < numChildren)
  {
    startElement(ELEM_OTHERWISE);
    upIndent();

    *this << ASTNode_getChild(node, numPieces);

    downIndent();

    indent();
    endElement(ELEM_OTHERWISE);
  }

  downIndent();

  indent();
  endElement(ELEM_PIECEWISE);
}


/**
 * Formats the given ASTNode as <apply> <fn/> ... </apply>.
 */
void
MathMLFormatter::doFunction (const ASTNode_t* node)
{
  ASTNodeType_t type        = ASTNode_getType(node);
  unsigned int  numChildren = ASTNode_getNumChildren(node);


  startElement(ELEM_APPLY);

  upIndent();

  if (type >= AST_FUNCTION && type < AST_UNKNOWN)
  {
    //
    // Format function name
    //
    if (type == AST_FUNCTION)
    {
      *this << ASTNode_getName(node);
    }
    else
    {
      startEndElement( MATHML_ELEMENTS[type - AST_FUNCTION_ABS] );
    }

    //
    // Format function arguments (children of this node)
    //
    if (type == AST_FUNCTION_LOG)
    {
      doFunctionLog(node);
    }
    else if (type == AST_FUNCTION_ROOT)
    {
      doFunctionRoot(node);
    }
    else
    {
      for (unsigned int c = 0; c < numChildren; c++)
      {
        *this << ASTNode_getChild(node, c);
      }
    }
  }

  downIndent();

  indent();
  endElement(ELEM_APPLY);
}


/**
 * Formats the two children of the given ASTNode.  The first child is
 * wrapped in a <logbase> element.
 */
void
MathMLFormatter::doFunctionLog (const ASTNode_t* node)
{
  startElement(ELEM_LOGBASE);

  upIndent();

  *this << ASTNode_getLeftChild(node);

  downIndent();

  indent();
  endElement(ELEM_LOGBASE);

  *this << ASTNode_getRightChild(node);
}


/**
 * Formats the children of the given ASTNode.  The first child is wrapped
 * in a <degree> element.
 */
void
MathMLFormatter::doFunctionRoot (const ASTNode_t* node)
{
  startElement(ELEM_DEGREE);

  upIndent();

  *this << ASTNode_getLeftChild(node);

  downIndent();

  indent();
  endElement(ELEM_DEGREE);

  *this << ASTNode_getRightChild(node);
}


/**
 * @return true if the string representation of number contains an 'e' or
 * 'E'.
 */
bool
MathMLFormatter::hasExponent (const char* number)
{
  bool result = false;


  while (*number++)
  {
    if (*number == 'e' || *number == 'E')
    {
      result = true;
      break;
    }
  }

  return result;
}




/* ----------------------------------------------------------------------
 *                      XML Elements and Attributes
 * ----------------------------------------------------------------------
 */


/**
 * Sends ' <sep/> ' to the underlying XMLFormatter.
 */
void
MathMLFormatter::sepElement ()
{
  *fFormatter << XMLFormatter::NoEscapes
              << chSpace
              << chOpenAngle << ELEM_SEP << chForwardSlash << chCloseAngle
              << chSpace;
}


/**
 * Sends '<name>\n' to the underlying XMLFormatter.
 */
void
MathMLFormatter::startElement (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes
              << chOpenAngle << name << chCloseAngle << chLF;
}


/**
 * Sends '<cn type="%s"> ' to the underlying XMLFormatter.
 */
void
MathMLFormatter::startElementCN (const XMLCh* type)
{
  openStartElement(ELEM_CN);
  attribute(ATTR_TYPE, type);
  *fFormatter << XMLFormatter::NoEscapes << chCloseAngle << chSpace;
}


/**
 * Sends '<name> ' to the underlying XMLFormatter.
 */
void
MathMLFormatter::startElementSpace (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes
              << chOpenAngle << name << chCloseAngle << chSpace;
}


/**
 * Sends '</name>\n' to the underlying XMLFormatter.
 */
void
MathMLFormatter::endElement (const XMLCh* name)
{
  *fFormatter << XMLFormatter::NoEscapes
              << chOpenAngle  << chForwardSlash << name << chCloseAngle
              << chLF;
}


/**
 * Sends ' </name>\n' to the underlying XMLFormatter.
 */
void
MathMLFormatter::spaceEndElement (const XMLCh* name)
{
  *fFormatter << XMLFormatter::NoEscapes
              << chSpace
              << chOpenAngle  << chForwardSlash << name << chCloseAngle
              << chLF;
}


/**
 * Sends '<name/>\n' to the underlying XMLFormatter.
 *
 * This is convenience function is equivalent to the following:
 *
 *  openStartElement(name);
 *  slashCloseStartElement(name);
 */
void
MathMLFormatter::startEndElement (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes
              << chOpenAngle  << name << chForwardSlash
              << chCloseAngle << chLF;
}


/**
 * Sends '<name' to the underlying XMLFormatter.  Use when name has one or
 * more attributes.
 *
 * See also closeStartElement() or slashCloseStartElement().
 */
void
MathMLFormatter::openStartElement (const XMLCh* name)
{
  indent();
  *fFormatter << XMLFormatter::NoEscapes << chOpenAngle << name;
}


/**
 * Sends '>\n' to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
MathMLFormatter::closeStartElement ()
{
  *fFormatter << XMLFormatter::NoEscapes << chCloseAngle << chLF;
}


/**
 * Sends '> ' to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
MathMLFormatter::closeStartElementSpace ()
{
  *fFormatter << XMLFormatter::NoEscapes << chCloseAngle << chSpace;
}


/**
 * Sends "/>\n" to the underlying XMLFormatter.
 *
 * See also openStartElement().
 */
void
MathMLFormatter::slashCloseStartElement ()
{
  *fFormatter << XMLFormatter::NoEscapes
              << chForwardSlash << chCloseAngle << chLF;
}


/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a C string).
 */
void
MathMLFormatter::attribute (const XMLCh* name, const char* value)
{
  XMLCh* s;


  if (value == NULL)
  {
    attribute(name, (const XMLCh*) NULL);
  }
  else
  {
    s = XMLString::transcode(value);
    attribute(name, s);

    delete [] s;
  }
}


/**
 * Sends ' name="%s" to the underlying XMLFormatter (where %s is a Unicode
 * string).
 */
void
MathMLFormatter::attribute (const XMLCh* name, const XMLCh* value)
{
  *fFormatter
    << XMLFormatter::NoEscapes
    << chSpace
    << name
    << chEqual
    << chDoubleQuote
    << XMLFormatter::AttrEscapes;

  if (value != NULL)
  {
    *fFormatter << value;
  }

  *fFormatter << XMLFormatter::NoEscapes << chDoubleQuote;
}


/**
 * Sends the given string of characters to the underlying XMLFormatter.
 */
void
MathMLFormatter::characters (const char* chars)
{
  if (chars == NULL) return;

  XMLCh* s = XMLString::transcode(chars);
  characters(s);

  delete [] s;
}


/**
 * Sends the given string of Unicode characters to the underlying
 * XMLFormatter.
 */
void
MathMLFormatter::characters (const XMLCh* chars)
{
  *fFormatter << XMLFormatter::CharEscapes << chars;
}


/**
 * Sends whitespace to the underlying XMLFormatter based on the current
 * indentation level.
 */
void
MathMLFormatter::indent ()
{
  for (unsigned int n = 0; n < fIndentLevel; n++)
  {
    *fFormatter << chSpace << chSpace;
  }
}
