/**
 * Filename    : XMLUtil.cpp
 * Description : Utility functions to help manipulate XML data
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-22
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <iostream>

#include "sbml/common.h"

#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>

#include "sbml/SBMLUnicodeConstants.hpp"
#include "sbml/XMLUnicodeConstants.hpp"
#include "sbml/XMLUtil.hpp"


/**
 * Creates a new XMLStringFormatter and returns a pointer to it.  This
 * method hides the constructor API differences between Xerces 2.3.0 and
 * earlier versions.
 */
XMLFormatter*
XMLUtil::createXMLFormatter ( const char*      outEncoding,
                              XMLFormatTarget* target )
{
  XMLFormatter* formatter;


  //
  // Starting with Xerces-C 2.3.0 _XERCES_VERSION (20300 == 2.03.00 ==
  // 2.3.0), the XMLFormatter class takes a new constructor argument, the
  // version of XML to write (1.0 or 1.1).
  //
#if _XERCES_VERSION >= 20300

  formatter = new XMLFormatter( outEncoding,
                                "1.0",
                                target,
                                XMLFormatter::NoEscapes,
                                XMLFormatter::UnRep_CharRef );
#else

  formatter = new XMLFormatter( outEncoding,
                                target,
                                XMLFormatter::NoEscapes,
                                XMLFormatter::UnRep_CharRef );

#endif

  return formatter;
}


/**
 * Searches for an attribute with name.  If found, and the corresponding
 * value can be interpreted as a boolean, it is stored through the last
 * pointer argument, analogous to C's scanf.  Otherwise, the last argument
 * is left unassigned.
 *
 * Valid booleans according to the W3C Schema recommendation are "true",
 * "false", "1", and "0" (case-insensitive).
 *
 * Returns true if the last argument was assigned, false otherwise.
 */
bool
XMLUtil::scanAttr (const Attributes& attrs, const XMLCh* name, bool* value)
{
  bool         assigned  = false;
  const XMLCh* toConvert = attrs.getValue(name);


  //
  // Proceed iff toConvert is not NULL and not an empty string
  //
  if ( (toConvert) && (*toConvert) )
  {
    XMLCh* trimmed = XMLString::replicate(toConvert);
    XMLString::trim(trimmed);

    unsigned int trimmedLen = XMLString::stringLen(trimmed);

    //
    // Proceed iff toConvert is more than just whitespace.
    //
    if (trimmedLen > 0)
    {
      if ( !XMLString::compareIString(trimmed, VAL_0) ||
           !XMLString::compareIString(trimmed, VAL_FALSE) )
      {
        *value   = false;
        assigned = true;
      }

      else if ( !XMLString::compareIString(trimmed, VAL_1) ||
                !XMLString::compareIString(trimmed, VAL_TRUE) )
      {
        *value   = true;
        assigned = true;
      }
    }

    delete [] trimmed;
  }

  return assigned;
}


/**
 * Searches for an attribute with name.  If found, and the corresponding
 * value can be interpreted as an integer, it is stored through the last
 * pointer argument, analogous to C's scanf.  Otherwise, the last argument
 * is left unassigned.
 *
 * Returns true if the last argument was assigned, false otherwise.
 */
bool
XMLUtil::scanAttr (const Attributes& attrs, const XMLCh* name, int* value)
{
  bool         assigned  = false;
  const XMLCh* toConvert = attrs.getValue(name);


  //
  // Proceed iff toConvert is not NULL and not an empty string
  //
  if ( (toConvert) && (*toConvert) )
  {
    XMLCh* trimmed = XMLString::replicate(toConvert);
    XMLString::trim(trimmed);

    unsigned int trimmedLen = XMLString::stringLen(trimmed);

    //
    // Proceed iff toConvert is more than just whitespace.
    //
    if (trimmedLen > 0)
    {
      errno        = 0;
      char* endptr = 0;
      char* nptr   = XMLString::transcode(trimmed);
      long  result = strtol(nptr, &endptr, 10);
      int   len    = endptr - nptr;

      if ( (len == (int) trimmedLen) && (errno != ERANGE) )
      {
        *value   = (int) result;
        assigned = true;
      }

      delete [] nptr;
    }

    delete [] trimmed;
  }

  return assigned;
}


/**
 * Searches for an attribute with name.  If found, and the corresponding
 * value can be interpreted as a positive integer, it is stored through the
 * last pointer argument, analogous to C's scanf.  Otherwise, the last
 * argument is left unassigned.
 *
 * Returns true if the last argument was assigned, false otherwise.
 */
bool
XMLUtil::scanAttr (const Attributes& attrs,
                   const XMLCh*      name,
                   unsigned int*     value )
{
  bool         assigned  = false;
  const XMLCh* toConvert = attrs.getValue(name);


  //
  // Proceed iff toConvert is not NULL and not an empty string
  //
  if ( (toConvert) && (*toConvert) )
  {
    XMLCh* trimmed = XMLString::replicate(toConvert);
    XMLString::trim(trimmed);

    unsigned int trimmedLen = XMLString::stringLen(trimmed);

    //
    // Proceed iff toConvert is more than just whitespace.
    //
    if (trimmedLen > 0)
    {
      errno        = 0;
      char* endptr = 0;
      char* nptr   = XMLString::transcode(trimmed);
      long  result = strtol(nptr, &endptr, 10);
      int   len    = endptr - nptr;

      if ( (len == (int) trimmedLen) && (errno != ERANGE) && (result > 0) )
      {
        *value   = (unsigned int) result;
        assigned = true;
      }

      delete [] nptr;
    }

    delete [] trimmed;
  }

  return assigned;
}


/**
 * Searches for an attribute with name.  If found, and the corresponding
 * value can be interpreted as a double, it is stored through the last
 * pointer argument, analogous to C's scanf.  Otherwise, the last argument
 * is left unassigned.
 *
 * Returns true if the last argument was assigned, false otherwise.
 */
bool
XMLUtil::scanAttr (const Attributes& attrs, const XMLCh* name, double* value)
{
  bool         assigned  = false;
  const XMLCh* toConvert = attrs.getValue(name);


  //
  // Proceed iff toConvert is not NULL and not an empty string
  //
  if ( (toConvert) && (*toConvert) )
  {
    XMLCh* trimmed = XMLString::replicate(toConvert);
    XMLString::trim(trimmed);

    unsigned int trimmedLen = XMLString::stringLen(trimmed);

    //
    // Proceed iff toConvert is more than just whitespace.
    //
    if (trimmedLen > 0)
    {
      errno        = 0;
      char* endptr = 0;
      char* nptr   = XMLString::transcode(trimmed);


      //
      // The forms "-0.", "-0.0" and "+Inf" are not valid according to the
      // documentation for XML Schema datatype double (Section 3.2.5):
      //
      //   http://www.w3.org/TR/xmlschema-2/#double
      // 
      // However, they are common notation and easy to support.  Adding
      // checks for them does no real harm.  Any more sophistication,
      // however, (e.g. "- Inf") would require either a mini-parser be
      // implemented here or use FormulaParser.
      //
      if ( !strcmp_insensitive(nptr, "-Inf") )
      {
        *value   = util_NegInf();
        assigned = true;
      }
      else if ( !strcmp(nptr, "-0"  ) ||
                !strcmp(nptr, "-0." ) ||
                !strcmp(nptr, "-0.0") )
      {
        *value   = util_NegZero();
        assigned = true;
      }
      else if ( (strcmp_insensitive(nptr, "Inf")  == 0) ||
                (strcmp_insensitive(nptr, "+Inf") == 0) )
      {
        *value   = util_PosInf();
        assigned = true;
      }
      else if (strcmp_insensitive(nptr, "NaN") == 0)
      {
        *value   = util_NaN();
        assigned = true;
      }
      else
      {
        double result = strtod(nptr, &endptr);
        int    len    = endptr - nptr;

        if ( (len == (int) trimmedLen) && (errno != ERANGE) )
        {
          *value   = result;
          assigned = true;
        }
      }

      delete [] nptr;
    }

    delete [] trimmed;
  }

  return assigned;
}


/**
 * Stores the string value of the attribute at the given index.  The string
 * is stored through the last pointer argument, analogous to C's scanf.
 * If index is out of range, the last argument is left unassigned.
 *
 * The space for the string is allocated by this function with malloc() and
 * must be freed with free(), i.e. it is meant to be used in C data
 * structures / programs.
 *
 * Returns true if the last argument was assigned, false otherwise.
 */
bool
XMLUtil::scanAttrCStr ( const Attributes&  attrs,
                        const unsigned int index,
                        char**             value )
{
  char* s        = XMLString::transcode( attrs.getValue(index) );
  bool  assigned = false;
  int   size;


  if (s != NULL)
  {
    size   = strlen(s) + 1;
    *value = (char*) safe_malloc(size * sizeof(char));

    strncpy(*value, s, size);
    assigned = true;

    delete [] s;
  }

  return assigned;
}


/**
 * Searches for an attribute with name.  If found, the corresponding string
 * is stored through the last pointer argument, analogous to C's scanf.
 * Otherwise, the last argument is left unassigned.
 *
 * The space for the string is allocated by this function with malloc() and
 * must be freed with free(), i.e. it is meant to be used in C data
 * structures / programs.
 *
 * Returns true if the last argument was assigned, false otherwise.
 */
bool
XMLUtil::scanAttrCStr ( const Attributes& attrs,
                        const XMLCh*      name ,
                        char**            value )
{

  int index = attrs.getIndex(name);


  return (index >= 0) ? scanAttrCStr(attrs, index, value) : false;
}
