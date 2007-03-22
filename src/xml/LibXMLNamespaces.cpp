/**
 * \file    LibXMLNamespaces.cpp
 * \brief   Extracts XML namespace declarations from LibXML prefix/URI pairs.
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/xml/LibXMLTranscode.h>
#include <sbml/xml/LibXMLNamespaces.h>


using namespace std;


/**
 * Creates a new list of XML namespaces declarations from a "raw" LibXML
 * prefix/URI pairs.
 */
LibXMLNamespaces::LibXMLNamespaces (  const xmlChar**     namespaces
                                    , const unsigned int& size )
{
  mNamespaces.reserve(size);

  for (unsigned int n = 0; n < size; ++n)
  {
    const string prefix = LibXMLTranscode( namespaces[2 * n]     );
    const string uri    = LibXMLTranscode( namespaces[2 * n + 1] );

    add(uri, prefix);
  }
}


/**
 * Destroys this list of XML namespace declarations.
 */
LibXMLNamespaces::~LibXMLNamespaces ()
{
}
