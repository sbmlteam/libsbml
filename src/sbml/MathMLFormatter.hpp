/**
 * Filename    : MathMLFormatter.hpp
 * Description : Formats MathML
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Stefan Hoops
 */


#ifndef MathMLFormatter_hpp
#define MathMLFormatter_hpp


#include "common.hpp"


#ifdef USE_EXPAT
#  include "ExpatFormatter.hpp"
#else
#  include <xercesc/framework/XMLFormatter.hpp>
#  include <xercesc/framework/MemBufFormatTarget.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#  include <xercesc/util/XMLString.hpp>
#  include <xercesc/util/XMLUniDefs.hpp>
#endif  // USE_EXPAT


#include "ASTNode.hpp"
#include "MathMLDocument.hpp" 


//
// MathMLFormatter is meant to act like a C++ output stream.  Creating an
// MathMLFormatter requires a character encoding and an underlying
// XMLFormatTarget, which can be either in-memory (with MemBufFormatTarget)
// or file (FileFormatTarget), to be specified.  Once created, inserting
// ASTNode objects (C structs) into the stream (with <<) will cause them to
// be formatted in the character encoding for the XMLFormatTarget.
//
// Currently, this class is meant to be used internally by libsbml.
//
class MathMLFormatter
{

public:

  /**
   * Ctor
   *
   * Creates a new MathMLFormatter with the given character encoding.
   * If outputXMLDecl is true the output will begin with:
   *
   *   <?xml version="1.0" encoding="..."?>
   */
  MathMLFormatter (const char*      outEncoding,
                   XMLFormatTarget* target,
                   bool             outputXMLDecl);

  /**
   * Dtor
   */
  ~MathMLFormatter ();

  /**
   * Sets the current indentation level for this MathMLFormatter.
   */
  void setIndentLevel (unsigned int n) { mIndentLevel = n; }

  /**
   * Sends a '<math xmlns="http://www.w3.org/1998/Math/MathML">' to the
   * underlying XMLFormatTarget and then ups the indent level.
   */
  void startMath ();

  /**
   * Downs the indent level and sends '</math>' to the underlying
   * XMLFormatTarget.
   */
  void endMath ();

  /**
   * MathMLDocument insertion operator
   */
  MathMLFormatter& operator<< (const MathMLDocument* d);

  /**
   * ASTNode insertion operator
   */
  MathMLFormatter& operator<< (const ASTNode* node);

  /**
   * ASTNode_t insertion operator (for backward compatibility).
   */
  MathMLFormatter& operator<< (const ASTNode_t* node);

  /**
   * Inserts the given string as a MathML <ci> element.
   */
  MathMLFormatter& operator<< (const char* s);

  /**
   * Inserts the given integer as a MathML <cn> element.
   */
  MathMLFormatter& operator<< (long value);

  /**
   * Inserts the given real as a MathML <cn> element.
   */
  MathMLFormatter& operator<< (double value);


private:


  /* ----------------------------------------------------------------------
   *                 Insertion Operator Supporting Functions
   * ----------------------------------------------------------------------
   */

  /**
   * Formats the given ASTNode as:
   *
   *   <cn type="e-notation"> %f <sep/> %ld </cn>.
   */
  void doENotation (const ASTNode* node);

  /**
   * Formats the given mantissa and exponent as:
   *
   *   <cn type="e-notation"> %f <sep/> %ld </cn>
   */
  void doENotation (double mantissa, long exponent);

  /**
   * Formats the given mantissa and exponent as:
   *
   *   <cn type="e-notation"> %s <sep/> %s </cn>
   */
  void doENotation (const char* mantissa, const char* exponent);

  /**
   * Formats the given ASTNode as
   * <cn type="rational"> %ld <sep/> %ld </cn>.
   */
  void doRational (const ASTNode* node);

  /**
   * Formats the given ASTNode as <cn type="real"> or <cn type='e-notation'>
   * as appropriate.
   */
  void doReal (const ASTNode* node);

  /**
   * Formats the given ASTNode as a <ci> or <csymbol> element as appropriate.
   */
  void doName (const ASTNode* node);

  /**
   * Formats the given ASTNode as a <csymbol> time or delay element as
   * appropriate.
   */
  void doCSymbol (const ASTNode* node);

  /**
   * Formats the given ASTNode as a MathML constant.
   */
  void doConstant (const ASTNode* node);

  /**
   * Formats the given ASTNode as a <lambda> element.
   */
  void doLambda (const ASTNode* node);

  /**
   * Outputs MathML appropriate for NaN (<notanumber/>).
   */
  void doNaN();

  /**
   * Outputs MathML appropriate for negative infinity.
   */
  void doNegInfinity ();

  /**
   * Outputs MathML appropriate for infinity (<infinity/>).
   */
  void doPosInfinity ();

  /**
   * Formats the given ASTNode as a <apply> <op/> ... </apply>.
   */
  void doOperator (const ASTNode* node);

  /**
   * This function formats the children of the given ASTNode and is called
   * by doOperator().
   */
  void doOperatorArgs (const ASTNode* node);

  /**
   * Formats the given ASTNode as a <piecewise> element.
   */
  void doPiecewise (const ASTNode* node);

  /**
   * Formats the given ASTNode as <apply> <fn/> ... </apply>.
   */
  void doFunction (const ASTNode* node);

  /**
   * Formats the two children of the given ASTNode.  The first child is
   * wrapped in a <logbase> element.
   */
  void doFunctionLog (const ASTNode* node);

  /**
   * Formats the children of the given ASTNode.  The first child is wrapped
   * in a <degree> element.
   */
  void doFunctionRoot (const ASTNode* node);

  /**
   * @return a string representation of the given integer value.
   */
  char* toString (long value);

  /**
   * @return a string representation of the given real value.
   */ 
  char* toString (double value);

  /**
   * Splits the string representation of number into mantissa and exponent
   * parts.
   *
   * The 'e' or 'E' in the string is overwritten with a NULL character and
   * a pointer to the exponent part is returned.  If the string does not
   * contain an exponent, number is not modified and a NULL pointer is
   * returned.
   */
  char* splitExponent (char* number);




  /* ----------------------------------------------------------------------
   *                      XML Elements and Attributes
   * ----------------------------------------------------------------------
   */

  /**
   * Sends '<sep/>' to the underlying XMLFormatter.
   */
  inline void sepElement ();

  /**
   * Sends '<name>\n' to the underlying XMLFormatter.
   */
  inline void startElement (const XMLCh* name);

  /**
   * Sends '<cn type="%s"> ' to the underlying XMLFormatter.
   */
  void startElementCN (const XMLCh* type);

  /**
   * Sends '<name> ' to the underlying XMLFormatter.
   */
  void startElementSpace (const XMLCh* name);

  /**
   * Sends '</name>\n' to the underlying XMLFormatter.
   */
  void endElement (const XMLCh* name);

  /**
   * Sends ' </name>\n' to the underlying XMLFormatter.
   */
  void spaceEndElement (const XMLCh* name);

  /**
   * Sends '<name/>\n' to the underlying XMLFormatter.
   *
   * This is convenience function is equivalent to the following:
   *
   *  openStartElement(name);
   *  slashCloseStartElement(name);
   */
  inline void startEndElement (const XMLCh* name);

  /**
   * Sends '<name' to the underlying XMLFormatter.  Use when name has one or
   * more attributes.
   *
   * See also closeStartElement() or slashCloseStartElement().
   */
  inline void openStartElement (const XMLCh* name);

  /**
   * Sends '>\n' to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void closeStartElement ();

  /**
   * Sends '> ' to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void closeStartElementSpace ();

  /**
   * Sends "/>\n" to the underlying XMLFormatter.
   *
   * See also openStartElement().
   */
  inline void slashCloseStartElement ();

  //
  // Sends ' name=value' to the underlying XMLFormatter where value is an
  // appropriate string representation for the given type.
  //
#ifndef USE_EXPAT
  void attribute ( const XMLCh* name, const char*  value );
#endif  // !USE_EXPAT
  void attribute ( const XMLCh* name, const XMLCh* value );

  //
  // Sends the given string of characters to the underlying XMLFormatter.
  //
#ifndef USE_EXPAT
  void characters (const char*  chars);
#endif  // !USE_EXPAT
  void characters (const XMLCh* chars);

  /**
   * Sends whitespace to the underlying XMLFormatter based on the current
   * indentation level.
   */
  void indent ();

  inline void upIndent   () { mIndentLevel++; }
  inline void downIndent () { mIndentLevel--; }


  XMLFormatter*     mFormatter;
  XMLFormatTarget*  mTarget;
  unsigned int      mIndentLevel;

  char* mFloatBuffer;
  char* mIntBuffer;

  static const unsigned int NUMBER_BUFFER_SIZE;
  static const XMLCh*       MATHML_ELEMENTS[];
};


#endif  // MathMLFormatter_hpp
