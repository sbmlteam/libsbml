/**
 * Filename    : MathMLHandler.cpp
 * Description : MathML SAX2 Handler
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-05-06
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


#include <iostream>

#include <ctype.h>

#include "sbml/common.h"
#include "sbml/List.h"

#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/util/XMLString.hpp>

#include "sbml/MathMLUnicodeConstants.hpp"
#include "sbml/XMLUtil.hpp"

#include "sbml/MathMLHandler.hpp"


/**
 * This table Maps MathML tag codes to AST type codes, e.g:
 *
 *   MathMLTagCode_t tag  = getTagCode(uri, localname);
 *   ASTTypeCode_t   type = ACTION_TABLE[tag];
 *
 * A type of AST_UNKNOWN indicates a failed mapping (a few MathML tags do
 * not have a corresponding AST type).
 */
static const ASTNodeType_t AST_TYPE_TABLE[] =
{
    /* MathML tag           ASTTypeCode_t         */
    /* -----------------    --------------------- */
    /* <abs>            */  AST_FUNCTION_ABS
  , /* <and>            */  AST_LOGICAL_AND
  , /* <annotation>     */  AST_UNKNOWN
  , /* <annotation-xml> */  AST_UNKNOWN
  , /* <apply>          */  AST_FUNCTION
  , /* <arccos>         */  AST_FUNCTION_ARCCOS
  , /* <arccosh>        */  AST_FUNCTION_ARCCOSH
  , /* <arccot>         */  AST_FUNCTION_ARCCOT
  , /* <arccoth>        */  AST_FUNCTION_ARCCOTH
  , /* <arccsc>         */  AST_FUNCTION_ARCCSC
  , /* <arccsch>        */  AST_FUNCTION_ARCCSCH
  , /* <arcsec>         */  AST_FUNCTION_ARCSEC
  , /* <arcsech>        */  AST_FUNCTION_ARCSECH
  , /* <arcsin>         */  AST_FUNCTION_ARCSIN
  , /* <arcsinh>        */  AST_FUNCTION_ARCSINH
  , /* <arctan>         */  AST_FUNCTION_ARCTAN
  , /* <arctanh>        */  AST_FUNCTION_ARCTANH
  , /* <bvar>           */  AST_UNKNOWN
  , /* <ceiling>        */  AST_FUNCTION_CEILING
  , /* <ci>             */  AST_NAME
  , /* <cn>             */  AST_REAL
  , /* <cos>            */  AST_FUNCTION_COS
  , /* <cosh>           */  AST_FUNCTION_COSH
  , /* <cot>            */  AST_FUNCTION_COT
  , /* <coth>           */  AST_FUNCTION_COTH
  , /* <csc>            */  AST_FUNCTION_CSC
  , /* <csch>           */  AST_FUNCTION_CSCH
  , /* <csymbol>        */  AST_NAME
  , /* <degree>         */  AST_UNKNOWN
  , /* <divide>         */  AST_DIVIDE
  , /* <eq>             */  AST_RELATIONAL_EQ
  , /* <exp>            */  AST_FUNCTION_EXP
  , /* <exponentiale>   */  AST_CONSTANT_E
  , /* <factorial>      */  AST_FUNCTION_FACTORIAL
  , /* <false>          */  AST_CONSTANT_FALSE
  , /* <floor>          */  AST_FUNCTION_FLOOR
  , /* <geq>            */  AST_RELATIONAL_GEQ
  , /* <gt>             */  AST_RELATIONAL_GT
  , /* <infinity>       */  AST_REAL
  , /* <lambda>         */  AST_LAMBDA
  , /* <leq>            */  AST_RELATIONAL_LEQ
  , /* <ln>             */  AST_FUNCTION_LN
  , /* <log>            */  AST_FUNCTION_LOG
  , /* <logbase>        */  AST_UNKNOWN
  , /* <lt>             */  AST_RELATIONAL_LT
  , /* <math>           */  AST_UNKNOWN
  , /* <minus>          */  AST_MINUS
  , /* <neq>            */  AST_RELATIONAL_NEQ
  , /* <not>            */  AST_LOGICAL_NOT
  , /* <notanumber>     */  AST_REAL
  , /* <or>             */  AST_LOGICAL_OR
  , /* <otherwise>      */  AST_UNKNOWN
  , /* <pi>             */  AST_CONSTANT_PI
  , /* <piece>          */  AST_UNKNOWN
  , /* <piecewise>      */  AST_FUNCTION_PIECEWISE
  , /* <plus>           */  AST_PLUS
  , /* <power>          */  AST_FUNCTION_POWER
  , /* <root>           */  AST_FUNCTION_ROOT
  , /* <sec>            */  AST_FUNCTION_SEC
  , /* <sech>           */  AST_FUNCTION_SECH
  , /* <semantics>      */  AST_UNKNOWN
  , /* <sep>            */  AST_UNKNOWN
  , /* <sin>            */  AST_FUNCTION_SIN
  , /* <sinh>           */  AST_FUNCTION_SINH
  , /* <tan>            */  AST_FUNCTION_TAN
  , /* <tanh>           */  AST_FUNCTION_TANH
  , /* <times>          */  AST_TIMES
  , /* <true>           */  AST_CONSTANT_TRUE
  , /* <xor>            */  AST_LOGICAL_XOR
  , /* Unknown          */  AST_UNKNOWN
};


/* ----------------------------------------------------------------------
 *                         SAX2 Event Handlers
 * ----------------------------------------------------------------------
 */


void
MathMLHandler::startDocument ()
{
  fObjStack = Stack_create(7);
  fTagStack = Stack_create(7);
  fSeenSep  = false;
}


void
MathMLHandler::endDocument ()
{
  if (Stack_size(fObjStack) > 0)
  {
    fDocument->math = (ASTNode_t *) Stack_pop(fObjStack);
  }

  Stack_free( fObjStack );
  Stack_free( fTagStack );
}


void
MathMLHandler::startElement (const XMLCh* const  uri,
                             const XMLCh* const  localname,
                             const XMLCh* const  qname,
                             const Attributes&   attrs)
{
  MathMLTagCode_t currTag = getTagCode(uri, localname);
  MathMLTagCode_t prevTag = MATHML_TAG_UNKNOWN;
  ASTNodeType_t   type    = AST_TYPE_TABLE[currTag];
  ASTNode_t*      node    = NULL;


  if (Stack_size(fTagStack) > 0)
  {
    prevTag = (MathMLTagCode_t) Stack_peek(fTagStack);
  }

  //
  // When <apply> is seen, an AST_FUNCTION is created and pushed onto the
  // stack.  When the next tag is a builtin MathML function (e.g. <sin>),
  // the AST_FUNCTION node can be popped from the stack, its type made more
  // specific (e.g. AST_FUNCTION_SIN), in effect "renaming" the function,
  // and pushed on again.  This is what is happening inside the if-block
  // below.
  //
  // There is one exception to this rule which the if-block guards against:
  // A call to a user-defined function, e.g.:
  //
  //   MathML: <apply>        <ci> f </ci> ...
  //    Stack: (AST_FUNCTION) (AST_NAME)
  //
  // In this case the function name is not known until the full content of
  // the <ci> tag is seen.  The only thing we can do create and push an
  // AST_NAME (the else-if block).  The function name will have to wait
  // until reduceExpression().
  //
  if (prevTag == MATHML_TAG_APPLY && currTag != MATHML_TAG_CI)
  {
    Stack_pop(fTagStack);
    node = (ASTNode_t*) Stack_pop(fObjStack);

    ASTNode_setType(node, type);
  }

  //
  // Otherwise, instead of renaming an AST_FUNCTION node, create a new node
  // of the appropriate type.
  //
  else if (type != AST_UNKNOWN)
  {
    node = ASTNode_createWithType(type);
  }

  //
  // A few tags require additional handling.
  //
  switch (currTag)
  {
    case MATHML_TAG_CN:
      setTypeCN(node, attrs);
      break;

    case MATHML_TAG_CSYMBOL:
      setTypeCS(node, attrs);
      break;

    case MATHML_TAG_NOT_A_NUMBER:
      ASTNode_setReal(node, util_NaN());
      break;

    case MATHML_TAG_INFINITY:
      ASTNode_setReal(node, util_PosInf());
      break;
  }

  //
  // Push the new (or renamed AST_FUNCTION) node (back) onto the stack.
  //
  // NULL nodes result from either unrecognized tags (MATHML_TAG_UNKNOWN)
  // or tags that add little value to the parse (e.g. <bvar>, <degree>,
  // <logbase>).  These are simply ignored.
  //
  if (node != NULL)
  {
    Stack_push(fTagStack, (void*) currTag);
    Stack_push(fObjStack, node);
  }
}


void
MathMLHandler::endElement (const XMLCh* const  uri,
                           const XMLCh* const  localname,
                           const XMLCh* const  qname)
{
  MathMLTagCode_t tag  = getTagCode(uri, localname);
  ASTNode_t*      node = (ASTNode_t*) Stack_peek(fObjStack);


  switch (tag)
  {
    case MATHML_TAG_APPLY:
      checkFunctionArgs(node);
      reduceExpression();
      break;

    case MATHML_TAG_CI:
    case MATHML_TAG_CN:
    case MATHML_TAG_LAMBDA:
    case MATHML_TAG_EXPONENTIALE:
    case MATHML_TAG_FALSE:
    case MATHML_TAG_INFINITY:
    case MATHML_TAG_NOT_A_NUMBER:
    case MATHML_TAG_PI:
    case MATHML_TAG_TRUE:
      reduceExpression();
      fSeenSep = false;
      break;

    case MATHML_TAG_SEP:
      fSeenSep = true;
      break;
  }
}


/**
 * In MathML characters are used primarily within <cn> and <ci> elements.
 */
void
MathMLHandler::characters (const XMLCh* const  chars,
                           const unsigned int  length)
{
  MathMLTagCode_t tag = (MathMLTagCode_t) Stack_peek(fTagStack);
  const char*     s   = XMLString::transcode(chars);


  if (XMLString::isAllWhiteSpace(chars) == false)
  {
    //
    // If there is a <cn> or <ci> on top of the tag stack parse its character
    // data to populate the corresponding ASTNode on top of the object stack.
    //
    if (tag == MATHML_TAG_CN)
    {
      parseCN(s);
    }
    else if (tag == MATHML_TAG_CI || tag == MATHML_TAG_CSYMBOL)
    {
      parseCI(s);
    }

    delete [] s;
  }
}


void
MathMLHandler::setDocumentLocator (const Locator *const locator)
{
  fLocator = locator;
}




/* ----------------------------------------------------------------------
 *                         MathML Tag Handlers
 * ----------------------------------------------------------------------
 */


/**
 * Ensures the given ASTNode has the appropriate number of arguments.  If
 * arguments are missing, appropriate defaults (per the MathML 2.0
 * specification) are added:
 *
 *   log (x) -> log (10, x)
 *   root(x) -> root( 2, x)
 */
void
MathMLHandler::checkFunctionArgs (ASTNode_t* node)
{
  ASTNode_t* child;


  if (ASTNode_getNumChildren(node) == 1)
  {
    if (node->type == AST_FUNCTION_LOG)
    {
      child = ASTNode_create();
      ASTNode_setInteger(child, 10);

      ASTNode_prependChild(node, child);
    }
    else if (node->type == AST_FUNCTION_ROOT)
    {
      child = ASTNode_create();
      ASTNode_setInteger(child, 2);

      ASTNode_prependChild(node, child);
    }
  }
}


/**
 * @return the MathMLTagCode for the given namespace URI and element name
 * (localname).
 */
MathMLTagCode_t
MathMLHandler::getTagCode (const XMLCh* uri, const XMLCh* localname)
{
  MathMLTagCode_t tag = MATHML_TAG_UNKNOWN;


  if ( (XMLString::stringLen(uri) == 0) ||
       (XMLString::compareString(XMLNS_MathML, uri) == 0) )
  {
    tag = MathMLTagCode_forElement(localname);
  }

  return tag;
}


/**
 * Parses the character data (str) between <ci> ... </ci> and modifies the
 * ASTNode on the top of fObjStack accordingly.
 */
void
MathMLHandler::parseCI (const char *str)
{  
  ASTNode_t* ci = (ASTNode_t*) Stack_peek(fObjStack);


  ci->value.name = util_trim(str);
}


/**
 * Parses the character data (str) between <cn> ... </cn> and modifies the
 * ASTNode on the top of fObjStack accordingly.
 */
void
MathMLHandler::parseCN (const char* str)
{
  ASTNode_t*          cn        = (ASTNode_t*) Stack_peek(fObjStack);
  FormulaTokenizer_t* tokenizer = FormulaTokenizer_create(str);
  Token_t*            token     = FormulaTokenizer_nextToken(tokenizer);


  //
  // <cn type="rational">
  //
  // bSeenSep indicates whether or not a <sep/> tag has been encountered in
  // this <cn>.
  //
  if (cn->type == AST_RATIONAL)
  {
    if (fSeenSep == false)
    {
      cn->value.integer = token->value.integer;
    }
    else
    {
      cn->extra.denominator = token->value.integer;
    }
  }

  //
  // <cn type="e-notation">
  //
  // bSeenSep indicates whether or not a <sep/> tag has been encountered in
  // this <cn>.
  //
  else if (cn->type == AST_REAL_E)
  {
    if (fSeenSep == false)
    {
      cn->value.real = token->value.real;
    }
    else
    {
      cn->extra.exponent = token->value.integer;
    }
  }

  //
  // <cn type="real">
  //
  else if (cn->type == AST_REAL)
  {
    if (token->type == TT_REAL)
    {
      cn->value.real = token->value.real;
    }
    else if (token->type == TT_INTEGER)
    {
      cn->value.real = (double) token->value.integer;
    }
    else
    {
      cn->type = AST_UNKNOWN;
    }
  }

  //
  // <cn type="integer">
  //
  else if (cn->type == AST_INTEGER)
  {
    if (token->type == TT_INTEGER)
    {
      cn->value.integer = token->value.integer;
    }
    else
    {
      cn->type = AST_UNKNOWN;
    }
  }

  Token_free(token);
  FormulaTokenizer_free(tokenizer);
}


/**
 * Reduces the top-most expression on fObjStack and adjusts fTagStack to
 * match.
 */
void
MathMLHandler::reduceExpression ()
{
  ASTNode_t* child;
  ASTNode_t* parent;
  ASTNode_t* op;

  ASTNodeType_t parentType;


  if (Stack_size(fObjStack) >= 2)
  {
    child      = (ASTNode_t*) Stack_peekAt(fObjStack, 0);
    parent     = (ASTNode_t*) Stack_peekAt(fObjStack, 1);
    parentType = ASTNode_getType(parent);

    //
    // The case of a call to a user-defined function:
    //
    //   MathML: <apply>        <ci> f </ci> ...
    //    Stack: (AST_FUNCTION) (AST_NAME)
    //
    // Is handled as a special reduction where the parent takes on the name
    // of the child (i.e. the function recieves its name) and the child is
    // subsequently discared.
    //
    if ( (parentType == AST_FUNCTION) && (ASTNode_getName(parent) == NULL) )
    {
      ASTNode_setName(parent, ASTNode_getName(child));
      ASTNode_free(child);
    }

    //
    // In MathML <plus/> and <times/> are n-ary operators.
    //
    // The infix FormulaParser, however, represents them as binary (its an
    // SL Right-most derivation parser).  For the sake of consistency, this
    // special case reduction ensures AST_PLUS and AST_TIMES are stored as
    // binary.
    //
    else if ( (parentType == AST_PLUS || parentType == AST_TIMES) &&
              ASTNode_getNumChildren(parent) == 2 )
    {
      op = ASTNode_createWithType(parentType);
      ASTNode_addChild(op, (ASTNode_t *) List_remove(parent->children, 1));
      ASTNode_addChild(op, child);

      ASTNode_addChild(parent, op);
    }

    //
    // The normal case reduction: parent adopts child.
    //
    else
    {
      ASTNode_addChild(parent, child);
    }

    //
    // Remove child (pointer) from stack and discard its corresponding tag.
    // Note, child was either freed above or adopted by parent.
    //
    Stack_pop(fTagStack);
    Stack_pop(fObjStack);
  }
}


/**
 * Sets the node type based on the MathML <cn type="..."> attribute.
 */
void
MathMLHandler::setTypeCN (ASTNode_t* node, const Attributes& a)
{
  int index = a.getIndex(ATTR_TYPE);


  if (index >= 0)
  {
    const XMLCh* type = a.getValue(index);

    if ( !XMLString::compareString(type, VAL_INTEGER) )
    {
      node->type = AST_INTEGER;
    }
    else if ( !XMLString::compareString(type, VAL_RATIONAL) )
    {
      node->type = AST_RATIONAL;
    }
    else if ( !XMLString::compareString(type, VAL_E_NOTATION) )
    {
      node->type = AST_REAL_E;
    }
  }
}


/**
 * Sets the node type based on the MathML <csymbol definitionURL="...">
 * attribute.
 */
void
MathMLHandler::setTypeCS (ASTNode_t* node, const Attributes& a)
{
  int index = a.getIndex(ATTR_DEFINITION_URL);


  if (index >= 0)
  {
    const XMLCh* url = a.getValue(index);

    if ( !XMLString::compareString(url, CSYMBOL_DEFINITION_URL_TIME) )
    {
      node->type = AST_NAME_TIME;
    }
    else if ( !XMLString::compareString(url, CSYMBOL_DEFINITION_URL_DELAY) )
    {
      node->type = AST_NAME_DELAY;
    }
  }
}
