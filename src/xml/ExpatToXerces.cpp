/**
 * \file    ExpatToXerces.cpp
 * \brief   Maps Expat XML events to a Xerces-C++ XML SAX2 event handler
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
 */


#include <string>
#include <vector>

#include <cstring>

#include "common.h"
#include "XMLUtil.h"

#include "Expat.h"
#include "ExpatAttributes.h"
#include "ExpatUnicodeChars.h"
#include "ExpatToXerces.h"


using namespace std;


/**
 * Separates the namespace URI from its element name.  This may any single
 * character that cannot be part of a legal URI (see Internet RFC 2396 for
 * URI syntax).  A single space (ASCII 32) will do nicely.
 */
const char ExpatToXerces::SepChar = chSpace;


/**
 * Creates a new ExpatToXerces interface.
 */
ExpatToXerces::ExpatToXerces () : Expat()
{
  create(NULL, &SepChar);

  enableCharacterDataHandler();

  enableStartNamespaceDeclHandler();
  enableNamespaceTriplets();
}


/**
 * Destroys this ExpatToXerces interface.
 */
ExpatToXerces::~ExpatToXerces ()
{
}


/**
 * Start element handler
 *
 * @param  const XML_Char*   name
 * @param  const XML_Char**  pAttrs
 */
void
ExpatToXerces::onStartElement (const XML_Char* name, const XML_Char** pAttrs)
{
  XML_Char*        elem  = const_cast<XML_Char*>(name);
  const XML_Char** attrs = pAttrs;

  XML_Char* uri;
  XML_Char* localname;
  XML_Char* prefix;


  XMLUtil::splitNamespaceTriplets(elem, &uri, &localname, &prefix, SepChar);

  //
  // Create new attrs with xmlns:prefix="URI" attributes prepended.
  //
  if ( !mNamespaceAttrNames.empty() )
  {
    attrs = prependNamespaceAttrs(pAttrs);
  }

  if (prefix)
  {
    setQName(prefix, localname);
    startElement(uri, localname, mQName.c_str(), Attributes(attrs, SepChar));
  }
  else
  {
    startElement(uri, localname, localname, Attributes(attrs, SepChar));
  }

  //
  // Now that the xmlns attributes have been reports to the Xerces-C++
  // startElement() handler, we can discard them.
  //
  if ( !mNamespaceAttrNames.empty() )
  {
    delete [] attrs;
    mNamespaceAttrNames .clear();
    mNamespaceAttrValues.clear();
  }

  XMLUtil::unsplitNamespaceTriplets(&uri, &localname, &prefix, SepChar);
}


/**
 * End element handler
 *
 * @param  const XML_Char*  name
 */
void
ExpatToXerces::onEndElement (const XML_Char* name)
{
  XML_Char* elem = const_cast<XML_Char*>(name);

  XML_Char* uri;
  XML_Char* localname;
  XML_Char* prefix;


  XMLUtil::splitNamespaceTriplets(elem, &uri, &localname, &prefix, SepChar);

  if (prefix)
  {
    setQName(prefix, localname);
    endElement(uri, localname, mQName.c_str());
  }
  else
  {
    endElement(uri, localname, localname);
  }

  XMLUtil::unsplitNamespaceTriplets(&uri, &localname, &prefix, SepChar);
}


/**
 * Character data handler
 *
 * @param  const XML_Char*  data
 * @param  int              length
 */
void
ExpatToXerces::onCharacterData (const XML_Char* data, int length)
{
  //
  // The Xerces-C++ SAX2 interface calls for length to be unsigned.
  //
  if (length < 0)
  {
    characters(data, 0);
  }
  else
  {
    characters(data, length);
  }
}


/**
 * Start namespace declaration handler
 *
 * @param  const XML_Char*  prefix
 * @param  const XML_Char*  URI
 */
void
ExpatToXerces::onStartNamespaceDecl ( const XML_Char* prefix,
                                      const XML_Char* URI )
{
  //
  // The Xerces-C++ SAX2 interfaces expects xmlns[:prefix]="URI"
  // declarations to be stored in Attributes passed to startElment().  To
  // accomodate this, we intercept and record them here and then construct
  // add them to Attributes in onStartElement().
  //

  string xmlns = "xmlns";

  if (prefix)
  {
    xmlns.append(":");
    xmlns.append(prefix);
  }

  mNamespaceAttrNames .push_back(xmlns);
  mNamespaceAttrValues.push_back(URI);
}


/**
 * @return a new set of raw Expat attributes with all xmlns:prefix="URI"
 * attributes followed by attributes in oldAttrs.
 */
const XML_Char**
ExpatToXerces::prependNamespaceAttrs (const XML_Char** oldAttrs)
{
  unsigned int i, j;
  unsigned int newAttrsSize = mNamespaceAttrNames.size();
  unsigned int oldAttrsSize = 0;

  const XML_Char** newAttrs;


  while (oldAttrs[2 * oldAttrsSize]) oldAttrsSize++;

  newAttrs = new const XML_Char*[2 * (newAttrsSize + oldAttrsSize) + 1];

  for (i = 0; i < newAttrsSize; ++i)
  {
    newAttrs[2 * i]     = mNamespaceAttrNames [i].c_str();
    newAttrs[2 * i + 1] = mNamespaceAttrValues[i].c_str();
  }

  for (j = 0; j < oldAttrsSize; ++i, ++j)
  {
    newAttrs[2 * i]     = oldAttrs[2 * j];
    newAttrs[2 * i + 1] = oldAttrs[2 * j + 1];
  }

  newAttrs[2 * i] = 0;

  return newAttrs;
}


/**
 * Sets mQName to prefix + ':' + localname
 */
void
ExpatToXerces::setQName (XML_Char* prefix, XML_Char* localname)
{
  mQName.erase();

  mQName.append(prefix);
  mQName.append(":");
  mQName.append(localname);
}
