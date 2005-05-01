/**
 * \file    SAX2AttributesMock.cpp
 * \brief   SAX2 Attributes mock object (to assist unit tests)
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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


#include <iostream>
#include "xml/common.h"


#ifdef USE_EXPAT
#  include "ExpatXMLString.h"
#else
#  include <xercesc/util/PlatformUtils.hpp>
   using namespace xercesc;
#endif // USE_EXPAT


#include "SAX2AttributesMock.h"


/**
 * Creates an implementation of a SAX2 Attributes class (adhering to the
 * Attributes interface) capable of holding at most max Attributes.
 */
SAX2AttributesMock::SAX2AttributesMock (unsigned int max) :
    mLocalNames( new XMLCh*[max] )
  , mValues    ( new XMLCh*[max] )
  , mMax       ( max )
  , mLength    ( 0 )
{
  XML_PLATFORM_UTILS_INIT();
}


SAX2AttributesMock::~SAX2AttributesMock ()
{
  for (unsigned int i = 0; i < mLength; i++)
  {
    delete [] mLocalNames[i];
    delete [] mValues[i];
  }
}


/**
 * @return The number of attributes in the list.
 */
unsigned int
SAX2AttributesMock::getLength () const
{
  return mLength;
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
const XMLCh*
SAX2AttributesMock::getURI (const unsigned int index) const
{
  return 0;
}


/**
 * @return The local name of the indexed attribute, or null if the index is
 * out of range.
 */
const XMLCh*
SAX2AttributesMock::getLocalName (const unsigned int index) const
{
  if (index >= 0 && index < mLength)
  {
    return mLocalNames[index];
  }
  else
  {
    return 0;
  }
}


/**
 * @return getLocalName(index)
 */
const XMLCh*
SAX2AttributesMock::getQName (const unsigned int index) const
{
  return getLocalName(index);
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
const XMLCh*
SAX2AttributesMock::getType (const unsigned int index) const
{
  return 0;
}


/**
 * @return The attribute value as a string, or null if the index is out
 * of range.
 */
const XMLCh*
SAX2AttributesMock::getValue (const unsigned int index) const
{
  if (index >= 0 && index < mLength)
  {
    return mValues[index];
  }
  else
  {
    return 0;
  }
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
int
SAX2AttributesMock::getIndex (const XMLCh* const  uri,
                              const XMLCh* const  localPart) const
{
  return 0;
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
int
SAX2AttributesMock::getIndex (const XMLCh* const qname) const
{
  return 0;
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
const XMLCh*
SAX2AttributesMock::getType (const XMLCh* const  uri,
                             const XMLCh* const  localPart) const
{
  return 0;
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
const XMLCh*
SAX2AttributesMock::getType (const XMLCh* const qname) const
{
  
  return 0;
}


/**
 * @return 0.  CURRENTLY UNIMPLEMENTED.
 */
const XMLCh*
SAX2AttributesMock::getValue (const XMLCh* const  uri,
                              const XMLCh* const  localPart) const
{
  return 0;
}


/**
 * @return The attribute value as a string, or null if the attribute is not
 * in the list or if qualified names are not available.
 */
const XMLCh*
SAX2AttributesMock::getValue (const XMLCh* const qname) const
{
  for (unsigned int n = 0; n < mLength; n++)
  {
    if ( XMLString::compareString(mLocalNames[n], qname) == 0)
    {
      return mValues[n];
    }
  }

  return 0;
}


/**
 * If getLength() is less-than the maximum number of attributes (supplied
 * during construction), adds an Attribute with the given localname and
 * value to this set of Attributes, otherwise does nothing.
 */
void
SAX2AttributesMock::add (const char* localname, const char* value)
{
  if (mLength < mMax)
  {
    mLocalNames[mLength] = XMLString::transcode( localname );
    mValues    [mLength] = XMLString::transcode( value     );
    mLength++;
  }
}
