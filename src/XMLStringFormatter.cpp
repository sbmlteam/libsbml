/**
 * Filename    : XMLStringFormatter.cpp
 * Description : Formats XML Strings from SAX2 parse events
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-25
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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


#include "sbml/common.hpp"


#ifdef USE_EXPAT
#  include "ExpatFormatter.hpp"
#else
#  include <xercesc/util/PlatformUtils.hpp>
#endif  // USE_EXPAT


#include "sbml/XMLStringFormatter.hpp"
#include "sbml/XMLUnicodeConstants.hpp"
#include "sbml/XMLUtil.hpp"


//
// Ctor
//
// Creates a new XMLStringFormatter
//
// The underlying character encoding is configurable.
//
XMLStringFormatter::XMLStringFormatter (const char* outEncoding)
{
  //
  // Initialize() is static, but may be called more than once safely.
  //
#ifndef USE_EXPAT
  XMLPlatformUtils::Initialize();
#endif  // !USE_EXPAT

  target    = new MemBufFormatTarget();
  formatter = XMLUtil::createXMLFormatter(outEncoding, target);
}


//
// Dtor
//
// Destroys this XMLStringFormatter
//
XMLStringFormatter::~XMLStringFormatter (void)
{
  delete formatter;
  delete target;
}


//
// Formats a string of the form:
//   '<qname attrs1="value1" ... attrsN="valueN">'.
//
void
XMLStringFormatter::startElement (const XMLCh* const  qname,
                                  const Attributes&   attrs)
{
  *formatter << XMLFormatter::NoEscapes << chOpenAngle;
  *formatter << qname;


  unsigned int length = attrs.getLength();
  for (unsigned int index = 0; index < length; index++)
  {
    *formatter 
      << XMLFormatter::NoEscapes
      << chSpace 
      << attrs.getQName(index)
      << chEqual
      << chDoubleQuote
      << XMLFormatter::AttrEscapes
      << attrs.getValue(index)
      << XMLFormatter::NoEscapes
      << chDoubleQuote;
  }

  *formatter << chCloseAngle;
}


//
// Formats a string of the form '</qname>'
//
void
XMLStringFormatter::endElement (const XMLCh* const  qname)
{
  *formatter << XMLFormatter::NoEscapes
             << chOpenAngle
             << chForwardSlash
             << qname
             << chCloseAngle;
}


//
// Formats a string composed of the chars.
//
void
XMLStringFormatter::characters (const XMLCh* const  chars,
                                const unsigned int  length)
{
  formatter->formatBuf(chars, length, XMLFormatter::CharEscapes);
}


//
// Formats a string composed of whitespace chars.
//
void
XMLStringFormatter::ignorableWhitespace (const XMLCh* const  chars,
                                         const unsigned int  length)
{
  formatter->formatBuf(chars, length, XMLFormatter::CharEscapes);
}


//
// Resets (empties) the internal character buffer.  Use this method when
// you want the formatter to begin creating a new XML string.
//
void
XMLStringFormatter::reset (void)
{
  target->reset();
}


//
// Returns the length of the current XML string.
//
unsigned int
XMLStringFormatter::getLength (void) const
{
  return target->getLen();
}


//
// Returns a the underlying formatted XML string.  The caller does not
// own this string and therefore is not allowed to modify it.
//
const char*
XMLStringFormatter::getString (void)
{
  return (char *) target->getRawBuffer();
}
