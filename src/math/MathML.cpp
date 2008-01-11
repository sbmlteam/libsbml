/**
 * @file    MathML.cpp
 * @brief   Utilities for reading and writing MathML to/from text strings.
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>

#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>

#include <sbml/util/util.h>

#include <sbml/common/common.h>

#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

static const char* URL_TIME  = "http://www.sbml.org/sbml/symbols/time";
static const char* URL_DELAY = "http://www.sbml.org/sbml/symbols/delay";

static const char* MATHML_ELEMENTS[] =
{
    "abs"
  , "and"
  , "annotation"
  , "annotation-xml"
  , "apply"
  , "arccos"
  , "arccosh"
  , "arccot"
  , "arccoth"
  , "arccsc"
  , "arccsch"
  , "arcsec"
  , "arcsech"
  , "arcsin"
  , "arcsinh"
  , "arctan"
  , "arctanh"
  , "bvar"
  , "ceiling"
  , "ci"
  , "cn"
  , "cos"
  , "cosh"
  , "cot"
  , "coth"
  , "csc"
  , "csch"
  , "csymbol"
  , "degree"
  , "divide"
  , "eq"
  , "exp"
  , "exponentiale"
  , "factorial"
  , "false"
  , "floor"
  , "geq"
  , "gt"
  , "infinity"
  , "lambda"
  , "leq"
  , "ln"
  , "log"
  , "logbase"
  , "lt"
  , "math"
  , "minus"
  , "neq"
  , "not"
  , "notanumber"
  , "or"
  , "otherwise"
  , "pi"
  , "piece"
  , "piecewise"
  , "plus"
  , "power"
  , "root"
  , "sec"
  , "sech"
  , "semantics"
  , "sep"
  , "sin"
  , "sinh"
  , "tan"
  , "tanh"
  , "times"
  , "true"
  , "xor"
};


static const char* MATHML_FUNCTIONS[] =
{
    "abs"
  , "arccos"
  , "arccosh"
  , "arccot"
  , "arccoth"
  , "arccsc"
  , "arccsch"
  , "arcsec"
  , "arcsech"
  , "arcsin"
  , "arcsinh"
  , "arctan"
  , "arctanh"
  , "ceiling"
  , "cos"
  , "cosh"
  , "cot"
  , "coth"
  , "csc"
  , "csch"
  , "csymbol"
  , "exp"
  , "factorial"
  , "floor"
  , "ln"
  , "log"
  , "piecewise"
  , "power"
  , "root"
  , "sec"
  , "sech"
  , "sin"
  , "sinh"
  , "tan"
  , "tanh"
  , "and"
  , "not"
  , "or"
  , "xor"
  , "eq"
  , "geq"
  , "gt"
  , "leq"
  , "lt"
  , "neq"
};


static const ASTNodeType_t MATHML_TYPES[] =
{
    AST_FUNCTION_ABS
  , AST_LOGICAL_AND
  , AST_UNKNOWN
  , AST_UNKNOWN
  , AST_FUNCTION
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
  , AST_UNKNOWN
  , AST_FUNCTION_CEILING
  , AST_NAME
  , AST_REAL
  , AST_FUNCTION_COS
  , AST_FUNCTION_COSH
  , AST_FUNCTION_COT
  , AST_FUNCTION_COTH
  , AST_FUNCTION_CSC
  , AST_FUNCTION_CSCH
  , AST_NAME
  , AST_UNKNOWN
  , AST_DIVIDE
  , AST_RELATIONAL_EQ
  , AST_FUNCTION_EXP
  , AST_CONSTANT_E
  , AST_FUNCTION_FACTORIAL
  , AST_CONSTANT_FALSE
  , AST_FUNCTION_FLOOR
  , AST_RELATIONAL_GEQ
  , AST_RELATIONAL_GT
  , AST_REAL
  , AST_LAMBDA
  , AST_RELATIONAL_LEQ
  , AST_FUNCTION_LN
  , AST_FUNCTION_LOG
  , AST_UNKNOWN
  , AST_RELATIONAL_LT
  , AST_UNKNOWN
  , AST_MINUS
  , AST_RELATIONAL_NEQ
  , AST_LOGICAL_NOT
  , AST_REAL
  , AST_LOGICAL_OR
  , AST_UNKNOWN
  , AST_CONSTANT_PI
  , AST_UNKNOWN
  , AST_FUNCTION_PIECEWISE
  , AST_PLUS
  , AST_FUNCTION_POWER
  , AST_FUNCTION_ROOT
  , AST_FUNCTION_SEC
  , AST_FUNCTION_SECH
  , AST_UNKNOWN
  , AST_UNKNOWN
  , AST_FUNCTION_SIN
  , AST_FUNCTION_SINH
  , AST_FUNCTION_TAN
  , AST_FUNCTION_TANH
  , AST_TIMES
  , AST_CONSTANT_TRUE
  , AST_LOGICAL_XOR
};


/**
 * @return s with whitespace removed from the beginning and end.
 */
static const string
trim (const string& s)
{
  static const string whitespace(" \t\r\n");

  string::size_type begin = s.find_first_not_of(whitespace);
  string::size_type end   = s.find_last_not_of (whitespace);

  return (begin == string::npos) ? "" : s.substr(begin, end - begin + 1);
}



/* ---------------------------------------------------------------------- */
/*                             MathML Input                               */
/* ---------------------------------------------------------------------- */

/**
 * Ensures the given ASTNode has the appropriate number of arguments.  If
 * arguments are missing, appropriate defaults (per the MathML 2.0
 * specification) are added:
 *
 *   log (x) -> log (10, x)
 *   root(x) -> root( 2, x)
 */
static void
checkFunctionArgs (ASTNode& node)
{
  if (node.getNumChildren() == 1)
  {
    if (node.getType() == AST_FUNCTION_LOG)
    {
      ASTNode* child = new ASTNode;
      child->setValue(10);

      node.prependChild(child);
    }
    else if (node.getType() == AST_FUNCTION_ROOT)
    {
      ASTNode* child = new ASTNode;
      child->setValue(2);

      node.prependChild(child);
    }
  }
}


/**
 * In MathML, &lt;plus/> and &lt;times/> are n-ary operators but the infix
 * FormulaParser represents them as binary operators.  To ensure a
 * consistent AST representation, this function is part of the n-ary to
 * binary reduction process.
 */
static void
reduceBinary (ASTNode& node)
{
  if (node.getNumChildren() == 2)
  {
    ASTNode* op = new ASTNode( node.getType() );
    node.swapChildren(op);
    node.prependChild(op);
  }
}


/**
 * Sets the type of an ASTNode based on the given MathML <ci> element.
 * Errors will be logged in the stream's SBMLErrorLog object.
 */
static void
setTypeCI (ASTNode& node, const XMLToken& element, XMLInputStream& stream)
{
  if (element.getName() == "csymbol")
  {
    string url;
    element.getAttributes().readInto("definitionURL", url);

         if ( url == URL_DELAY ) node.setType(AST_FUNCTION_DELAY);
    else if ( url == URL_TIME  ) node.setType(AST_NAME_TIME);
    else 
    {
      static_cast <SBMLErrorLog*>
	(stream.getErrorLog())->logError(BadCsymbolDefinitionURLValue);
    }
  }

  const string name = trim( stream.next().getCharacters() );
  node.setName( name.c_str() );
}


/**
 * Sets the type of an ASTNode based on the given MathML &lt;cn> element.
 * Errors will be logged in the stream's SBMLErrorLog object.
 */
static void
setTypeCN (ASTNode& node, const XMLToken& element, XMLInputStream& stream)
{
  string type = "real";
  element.getAttributes().readInto("type", type);

  if (type == "real")
  {
    double value;
    istringstream( stream.next().getCharacters() ) >> value;

    node.setValue(value);
  }

  else if (type == "integer")
  {
    long value;
    istringstream( stream.next().getCharacters() ) >> value;

    node.setValue(value);
  }

  else if (type == "e-notation")
  {
    double mantissa;
    long   exponent;
    istringstream( stream.next().getCharacters() ) >> mantissa;

    if (stream.peek().getName() == "sep")
    {
      stream.next();
      istringstream( stream.next().getCharacters() ) >> exponent;
    }

    node.setValue(mantissa, exponent);
  }

  else if (type == "rational")
  {
    long numerator;
    long denominator;

    istringstream( stream.next().getCharacters() ) >> numerator;

    if (stream.peek().getName() == "sep")
    {
      stream.next();
      istringstream( stream.next().getCharacters() ) >> denominator;
    }

    node.setValue(numerator, denominator);
  }
  else
  {
    static_cast <SBMLErrorLog*>
      (stream.getErrorLog())->logError(DisallowedMathTypeAttributeValue);
  }
}


/**
 * Sets the type of an ASTNode based on the given MathML element (not <ci>
 * or <cn>).
 */
static void
setTypeOther (ASTNode& node, const XMLToken& element, XMLInputStream& stream)
{
  static const int size = sizeof(MATHML_ELEMENTS) / sizeof(MATHML_ELEMENTS[0]);
  const char*      name = element.getName().c_str();

  int  index = util_bsearchStringsI(MATHML_ELEMENTS, name, 0, size - 1);
  bool found = (index < size);

  if (found) node.setType(MATHML_TYPES[index]);

}


/**
 * Sets the type of an ASTNode based on the given MathML element.
 */
static void
setType (ASTNode& node, const XMLToken& element, XMLInputStream& stream)
{
  const string& name = element.getName();

  if (name == "ci" || name == "csymbol")
  {
    setTypeCI(node, element, stream);
  }
  else if (name == "cn")
  {
    setTypeCN(node, element, stream);
  }
  else if (name == "notanumber")
  {
    node.setValue( numeric_limits<double>::quiet_NaN() );
  }

  else if (name == "infinity")
  {
    node.setValue( numeric_limits<double>::infinity() );
  }
  else
  {
    setTypeOther(node, element, stream);
  }
}


/**
 * Essentially an s-expression parser.
 * Errors will be logged in the stream's SBMLErrorLog object.
 */
static void
readMathML (ASTNode& node, XMLInputStream& stream)
{
  stream.skipText();
 
  const XMLToken elem = stream.next ();
  const string&  name = elem.getName();

  static const int size = sizeof(MATHML_ELEMENTS) / sizeof(MATHML_ELEMENTS[0]);

  int  index = util_bsearchStringsI(MATHML_ELEMENTS, name.c_str(), 0, size - 1);
  bool found = (index < size);

  if (!found)
  {
    static_cast <SBMLErrorLog*>
      (stream.getErrorLog())->logError(DisallowedMathMLSymbol);
  }

  string encoding;
  string type;
  string url;

  elem.getAttributes().readInto( "encoding"     , encoding );
  elem.getAttributes().readInto( "type"         , type     );
  elem.getAttributes().readInto( "definitionURL", url      );

  if ( !type.empty() && name != "cn")
  {
    static_cast <SBMLErrorLog*>
      (stream.getErrorLog())->logError(DisallowedMathTypeAttributeUse);
  }

  if ( !encoding.empty() && name != "csymbol")
  {
    static_cast <SBMLErrorLog*>
      (stream.getErrorLog())->logError(DisallowedMathMLEncodingUse);
  }

  // allow definition url on csymbol/semantics and bvar
  if ( !url.empty() && (name != "csymbol"
                     && name != "semantics"))
  {
    static_cast <SBMLErrorLog*>
      (stream.getErrorLog())->logError(DisallowedDefinitionURLUse);
  }



  if (name == "apply" || name == "lambda" || name == "piecewise")
  {
    if (name == "apply")
    {
      /* catch case where user has used <apply/> */
      if (elem.isStart() && elem.isEnd()) return;

      /* catch case where user has applied a function that
       * has no arguments 
       */
      if (elem.isEnd()) return;

      readMathML(node, stream);

      if (node.isName()) node.setType(AST_FUNCTION);

      /* there are several <apply> <...> constructs that are invalid
       * these need to caught here as they will mess up validation later
       * These are
       * <apply> <cn>
       * <apply> <true> OR <false>
       * <apply> <pi> OR <exponentiale>
       * <apply
       */
      if (node.isNumber())   
      {
        std::string message = "A number is not an operator and cannot be used ";
        message += "directly following an <apply> tag.";

        // the mathML reader doesnt know what level and version it is reading!
        // FIX ME
        static_cast <SBMLErrorLog*> (stream.getErrorLog())->logError(BadMathML,
                                                                     2, 1,
                                                                     message);
        return;

      }
      else if ((node.getType() == AST_CONSTANT_TRUE)
        || (node.getType() == AST_CONSTANT_FALSE)
        || (node.getType() == AST_CONSTANT_PI)
        || (node.getType() == AST_CONSTANT_E)
        ) 
      {
        std::string message = "<";
        message += node.getName();
        message += "> is not an operator and cannot be used directly following an";
        message += " <apply> tag.";

        // the mathML reader doesnt know what level and version it is reading!
        // FIX ME
        static_cast <SBMLErrorLog*> (stream.getErrorLog())->logError(BadMathML,
                                                                     2, 1,
                                                                     message);
        return;

      }
    }
    else if (name == "lambda")
    {
      node.setType(AST_LAMBDA);
    }
    else
    {
      node.setType(AST_FUNCTION_PIECEWISE);
    }

    while (stream.isGood() && stream.peek().isEndFor(elem) == false)
    {
      ASTNodeType_t type = node.getType();
      if (type == AST_PLUS || type == AST_TIMES) reduceBinary(node);
      if (type == AST_CONSTANT_TRUE || type == AST_CONSTANT_FALSE) break;

      /* it is possible to have a function that has no children
       * ie a lambda with no bvars
       * dont want to add the child since this makes it look like
       * it has a bvar
       */
      ASTNode* child = new ASTNode();
      readMathML(*child, stream);
      stream.skipText();
      if (stream.peek().getName() == "math") break;
      node.addChild(child);

      if (stream.peek().getName() == "piece" && stream.isGood()) stream.next();
    }
  }

  else if (name == "bvar")
  {
    readMathML(node, stream);
  }

  else if (name == "degree" || name == "logbase" ||
           name == "piece" || name == "otherwise" )
  {
    readMathML(node, stream);
    if (name == "piece") return;
  }

  else if (name == "semantics")
  {
    /** read in attributes */
    node.setDefinitionURL(elem.getAttributes());
    readMathML(node, stream);
    node.setSemanticsFlag();
    /** need to look for any annotation on the semantics element **/
    while ( stream.isGood() && !stream.peek().isEndFor(elem))
    {
      if (stream.peek().getName() == "annotation"
        || stream.peek().getName() == "annotation-xml")
      {
        XMLNode semanticAnnotation = XMLNode(stream);
        node.addSemanticsAnnotation(semanticAnnotation.clone());
      }
      else
      {
        stream.next();
      }
    }
  }
  else
  {
    setType(node, elem, stream);
  }

  checkFunctionArgs(node);

  stream.skipPastEnd(elem);
}



/* ---------------------------------------------------------------------- */
/*                             MathML Output                              */
/* ---------------------------------------------------------------------- */


static void writeNode      (const ASTNode&, XMLOutputStream&);
static void writeCSymbol   (const ASTNode&, XMLOutputStream&);
static void writeDouble    (const double& , XMLOutputStream&);
static void writeENotation (const double& , long, XMLOutputStream&);
static void writeENotation (const string& , const string&, XMLOutputStream&);


/**
 * Writes the given ASTNode as a <ci> or <csymbol> element as appropriate.
 */
static void
writeCI (const ASTNode& node, XMLOutputStream& stream)
{
  ASTNodeType_t type = node.getType();

  if (type == AST_FUNCTION_DELAY || type == AST_NAME_TIME)
  {
    writeCSymbol(node, stream);
  }
  else if (type == AST_NAME || type == AST_FUNCTION)
  {
    stream.startElement("ci");
    stream.setAutoIndent(false);

    stream << " " << node.getName() << " ";

    stream.endElement("ci");
    stream.setAutoIndent(true);
  }
}


/**
 * Writes the given ASTNode as <cn type="real">, <cn type='e-notation'>,
 * <cn type='integer'>, or <cn type='rational'> as appropriate.
 */
static void
writeCN (const ASTNode& node, XMLOutputStream& stream)
{
  if ( node.isNaN() )
  {
    stream.startEndElement("notanumber");
  }
  else if ( node.isInfinity() )
  {
    stream.startEndElement("infinity");
  }
  else if ( node.isNegInfinity() )
  {
    stream.startElement("apply");
    stream.setAutoIndent(false);
    stream << " ";
    stream.startEndElement("minus");
    stream << " ";
    stream.startEndElement("infinity");
    stream << " ";
    stream.endElement("apply");
    stream.setAutoIndent(true);
  }
  else
  {
    stream.startElement("cn");
    stream.setAutoIndent(false);

    if ( node.isInteger() )
    {
      static const string integer = "integer";
      stream.writeAttribute("type", integer);

      stream << " " << node.getInteger() << " ";
    }
    else if ( node.isRational() )
    {
      static const string rational = "rational";
      stream.writeAttribute("type", rational);

      stream << " " << node.getNumerator() << " ";
      stream.startEndElement("sep");
      stream << " " << node.getDenominator() << " ";
    }
    else if ( node.getType() == AST_REAL_E )
    {
      writeENotation( node.getMantissa(), node.getExponent(), stream );
    }
    else
    {
      writeDouble( node.getReal(), stream );
    }

    stream.endElement("cn");
    stream.setAutoIndent(true);
  }
}


/**
 * Writes the given ASTNode as a MathML constant.
 */
static void
writeConstant (const ASTNode& node, XMLOutputStream& stream)
{
  switch ( node.getType() )
  {
    case AST_CONSTANT_PI:    stream.startEndElement("pi");            break;
    case AST_CONSTANT_TRUE:  stream.startEndElement("true");          break;
    case AST_CONSTANT_FALSE: stream.startEndElement("false");         break;
    case AST_CONSTANT_E:     stream.startEndElement("exponentiale");  break;
    default:  break;
  }
}


/**
 * Writes the given ASTNode as a <csymbol> time or delay element as
 * appropriate.
 */
static void
writeCSymbol (const ASTNode& node, XMLOutputStream& stream)
{
  ASTNodeType_t type = node.getType();
  string url;

       if (type == AST_FUNCTION_DELAY) url = URL_DELAY;
  else if (type == AST_NAME_TIME)      url = URL_TIME;

  stream.startElement("csymbol");
  stream.setAutoIndent(false);
  
  static const string text = "text";
  stream.writeAttribute( "encoding"     , text );
  stream.writeAttribute( "definitionURL", url  );

  stream << " " << node.getName() << " ";

  stream.endElement("csymbol");
  stream.setAutoIndent(true);
}


/**
 * Writes the given double precision value.  This function handles the
 * special case where the value, converted to a string, contains an
 * exponent part.
 */
static void
writeDouble (const double& value, XMLOutputStream& stream)
{
  ostringstream output;

  output.precision(LIBSBML_DOUBLE_PRECISION);
  output << value;

  string            value_string = output.str();
  string::size_type position     = value_string.find('e');

  if (position == string::npos)
  {
    stream << " " << value_string << " ";
  }
  else
  {
    const string mantissa_string = value_string.substr(0, position);
    const string exponent_string = value_string.substr(position + 1);

    double mantissa = strtod(mantissa_string.c_str(), 0);
    long   exponent = strtol(exponent_string.c_str(), 0, 10);

    writeENotation(mantissa, exponent, stream);
  }
}


/**
 * Writes the given mantissa and exponent.  This function handles the
 * special case where the mantissa, converted to a string, contains an
 * exponent part.
 */
static void
writeENotation (  const double&    mantissa
                , long             exponent
                , XMLOutputStream& stream )
{
  ostringstream output;

  output.precision(LIBSBML_DOUBLE_PRECISION);
  output << mantissa;

  const string      value_string = output.str();
  string::size_type position     = value_string.find('e');

  if (position != string::npos)
  {
    const string exponent_string = value_string.substr(position + 1);
    exponent += strtol(exponent_string.c_str(), NULL, 10);
  }

  output.str("");
  output << exponent;

  const string mantissa_string = value_string.substr(0, position);
  const string exponent_string = output.str();

  writeENotation(mantissa_string, exponent_string, stream);
}


/**
 * Writes the given string mantissa and exponent.
 */
static void
writeENotation (  const string&    mantissa
                , const string&    exponent
                , XMLOutputStream& stream )
{
  static const string enotation = "e-notation";
  stream.writeAttribute("type", enotation);

  stream << " " << mantissa << " ";
  stream.startEndElement("sep");
  stream << " " << exponent << " ";
}


/**
 * Writes the two children of the given ASTNode.  The first child is
 * wrapped in a <logbase> element.
 */
static void
writeFunctionLog (const ASTNode& node, XMLOutputStream& stream)
{
  if ( node.getNumChildren() > 1 )
  {
    stream.startElement("logbase");

    if ( node.getLeftChild() )  writeNode(*node.getLeftChild(), stream);

    stream.endElement("logbase");
  }

  if ( node.getRightChild() ) writeNode(*node.getRightChild(), stream);
}


/**
 * Writes the children of the given ASTNode.  The first child is wrapped
 * in a <degree> element.
 */
static void
writeFunctionRoot (const ASTNode& node, XMLOutputStream& stream)
{
  if ( node.getNumChildren() > 1 )
  {
    stream.startElement("degree");

    if ( node.getLeftChild() )  writeNode(*node.getLeftChild(), stream);

    stream.endElement("degree");
  }

  if ( node.getRightChild() ) writeNode(*node.getRightChild(), stream);
}


/**
 * Writes the given ASTNode as <apply> <fn/> ... </apply>.
 */
static void
writeFunction (const ASTNode& node, XMLOutputStream& stream)
{
  ASTNodeType_t type        = node.getType();
  unsigned int  numChildren = node.getNumChildren();


  stream.startElement("apply");

  if (type >= AST_FUNCTION && type < AST_UNKNOWN)
  {
    //
    // Format function name
    //
    if (type == AST_FUNCTION)
    {
      writeCI(node, stream);
    }
    else if (type == AST_FUNCTION_DELAY)
    {
      writeCSymbol(node, stream);
    }
    else
    {
      const char* name = MATHML_FUNCTIONS[type - AST_FUNCTION_ABS];
      stream.startEndElement(name);
    }

    //
    // Format function arguments (children of this node)
    //
    if (type == AST_FUNCTION_LOG)
    {
      writeFunctionLog(node, stream);
    }
    else if (type == AST_FUNCTION_ROOT)
    {
      writeFunctionRoot(node, stream);
    }
    else
    {
      for (unsigned int c = 0; c < numChildren; c++)
      {
        writeNode(*node.getChild(c), stream);
      }
    }
  }

  stream.endElement("apply");
}


/**
 * Writes the given ASTNode as a <lambda> element.
 */
static void
writeLambda (const ASTNode& node, XMLOutputStream& stream)
{
  unsigned int bvars = node.getNumChildren() - 1;
  unsigned int n;

  stream.startElement("lambda");

  for (n = 0; n < bvars; n++)
  {
    stream.startElement("bvar");
    if (node.getChild(n)->getDefinitionURL())
      stream.writeAttribute("definitionURL", 
      node.getChild(n)->getDefinitionURL()->getValue(0));
    writeNode(*node.getChild(n), stream);
    stream.endElement("bvar");
  }

  writeNode( *node.getChild(n), stream );

  stream.endElement("lambda");
}


/**
 * This function formats the children of the given ASTNode and is called by
 * doOperator().
 */
static void
writeOperatorArgs (const ASTNode& node, XMLOutputStream& stream)
{
  ASTNodeType_t type  = node.getType();
  ASTNode*      left  = node.getLeftChild();
  ASTNode*      right = node.getRightChild();


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
    if (left)
    {
      if (left->getType() == type) writeOperatorArgs(*left, stream);
      else writeNode(*left, stream);
    }

    if (right)
    {
      if (right->getType() == type) writeOperatorArgs(*right, stream);
      else writeNode(*right, stream);
    }
  }
  else
  {
    if (left)  writeNode(*left , stream);
    if (right) writeNode(*right, stream);
  }
}


/**
 * Writes the given ASTNode as a <apply> <op/> ... </apply>.
 */
static void
writeOperator (const ASTNode& node, XMLOutputStream& stream)
{
  stream.startElement("apply");

  switch ( node.getType() )
  {
    case AST_PLUS  :  stream.startEndElement( "plus"   );  break;
    case AST_TIMES :  stream.startEndElement( "times"  );  break;
    case AST_MINUS :  stream.startEndElement( "minus"  );  break;
    case AST_DIVIDE:  stream.startEndElement( "divide" );  break;
    case AST_POWER :  stream.startEndElement( "power"  );  break;
    default:  break;
  }

  writeOperatorArgs(node, stream);

  stream.endElement("apply");
}


/**
 * Formats the given ASTNode as a <piecewise> element.
 */
static void
writePiecewise (const ASTNode& node, XMLOutputStream& stream)
{
  unsigned int numChildren = node.getNumChildren();
  unsigned int numPieces   = numChildren;

  //
  // An odd number of children means the last element is an <otherwise>,
  // not a <piece>
  //
  if ((numChildren % 2) != 0) numPieces--;

  stream.startElement("piecewise");

  for (unsigned int n = 0; n < numPieces; n += 2)
  {
    stream.startElement("piece");

    writeNode( *node.getChild(n)    , stream );
    writeNode( *node.getChild(n + 1), stream );

    stream.endElement("piece");
  }

  if (numPieces < numChildren)
  {
    stream.startElement("otherwise");

    writeNode( *node.getChild(numPieces), stream );

    stream.endElement("otherwise");
  }

  stream.endElement("piecewise");
}


/**
 * Formats the given ASTNode as a <semantics> element.
 */
static void
writeSemantics(const ASTNode& node, XMLOutputStream& stream, bool &inSemantics)
{
  inSemantics = true;
  stream.startElement("semantics");
  if (node.getDefinitionURL())
    stream.writeAttribute("definitionURL", 
                            node.getDefinitionURL()->getValue(0));
  writeNode(node, stream);

  for (unsigned int n = 0; n < node.getNumSemanticsAnnotations(); n++)
  {
    stream << *node.getSemanticsAnnotation(n);
  }
  stream.endElement("semantics");
  inSemantics = false;
}


/**
 * Writes the given ASTNode (and its children) to the XMLOutputStream as
 * MathML.
 */
static void
writeNode (const ASTNode& node, XMLOutputStream& stream)
{
  static bool inSemantics = false;
  
  if (node.getSemanticsFlag() && !inSemantics)
                     writeSemantics(node, stream, inSemantics);

  else if (  node.isNumber   () ) writeCN       (node, stream);
  else if (  node.isName     () ) writeCI       (node, stream);
  else if (  node.isConstant () ) writeConstant (node, stream);
  else if (  node.isOperator () ) writeOperator (node, stream);
  else if (  node.isLambda   () ) writeLambda   (node, stream);
  else if (  node.isPiecewise() ) writePiecewise(node, stream);
  else if ( !node.isUnknown  () ) writeFunction (node, stream);
}


/**
 * Reads the MathML from the given XMLInputStream, constructs a corresponding
 * abstract syntax tree and returns a pointer to the root of the tree.
 */
LIBSBML_EXTERN
ASTNode*
readMathML (XMLInputStream& stream)
{
  stream.skipText();

  ASTNode*      node = new ASTNode;
  const string& name = stream.peek().getName();

  if (name == "apply" || name == "math")
  {
    const XMLToken elem = stream.next();
      
    if (elem.isStart() && elem.isEnd()) return node;

    readMathML(*node, stream);
    stream.skipPastEnd(elem);
  }
  else
  {
    readMathML(*node, stream);
  }

  return node;
}


/**
 * Writes the given ASTNode (and its children) to the XMLOutputStream as
 * MathML.
 */
LIBSBML_EXTERN
void
writeMathML (const ASTNode* node, XMLOutputStream& stream)
{
  static const string uri = "http://www.w3.org/1998/Math/MathML";

  stream.startElement("math");
  stream.writeAttribute("xmlns", uri);

  if (node) writeNode(*node, stream);

  stream.endElement("math");
}


/** @endcond doxygen-libsbml-internal */

/* ---------------------------------------------------------------------- */
/*                           Public Functions                             */
/* ---------------------------------------------------------------------- */


/**
 * Reads MathML from a text string containing XML and returns the
 * corresponding AST representation of the mathematical formula.
 *
 * Note that the content should be a complete self-contained MathML
 * formula, enclosed by
 * <code>&lt;math xmlns="http://www.w3.org/1998/Math/MathML"></code>
 * and <code>&lt;/math></code> tags.
 *
 * @param xml the MathML to be converted, stored in a character string.
 *
 * @return an ASTnode (the root of the AST representing the mathematical
 * formula in the given XML string).
 */
LIBSBML_EXTERN
ASTNode_t *
readMathMLFromString (const char *xml)
{
  if (xml == 0) return 0;
  const char* dummy_xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  const char* xmlstr_c;
  
  if (!strncmp(xml, dummy_xml, 14))
  {
    xmlstr_c = xml;
  }
  else
  {
    std::ostringstream oss;
    const char* dummy_xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";

    oss << dummy_xml;
    oss << xml;


    xmlstr_c = safe_strdup(oss.str().c_str());
  }

  XMLInputStream stream(xmlstr_c, false);
  SBMLErrorLog   log;

  stream.setErrorLog(&log);

  return readMathML(stream);
}


/**
 * Writes the given ASTNode (and its children) as MathML to a string, and
 * returns the string.
 *
 * @param node the AST to be converted to MathML
 *
 * @return the XML representation of the given mathematical expression.
 * The string is owned by the caller and should be freed (with
 * <code>free()</code>) when it is no longer needed.
 */
LIBSBML_EXTERN
char *
writeMathMLToString (const ASTNode* node)
{
  ostringstream   os;
  XMLOutputStream stream(os);

  char * result = 0;

  if (node)
  {
    writeMathML(node, stream);
    result = safe_strdup( os.str().c_str() );
  }

  return result;
}

