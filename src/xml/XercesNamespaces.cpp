/**
 * @file    XercesNamespaces.cpp
 * @brief   Extracts XML namespace declarations from Xerces-C++ attributes.
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

#include <sbml/xml/XercesTranscode.h>
#include <sbml/xml/XercesNamespaces.h>

/** @cond doxygen-ignored */

using namespace std;
using namespace xercesc;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

/**
 * Creates a new list of XML namespaces declarations from a "raw" Xerces-C++
 * Attributes set.
 */
XercesNamespaces::XercesNamespaces (const xercesc::Attributes& attrs)
{
  unsigned int size = attrs.getLength();
  mNamespaces.reserve(size);


  for (unsigned int n = 0; n < size; ++n)
  {
    const string            name  = XercesTranscode( attrs.getLocalName(n) );
    const string            qname = XercesTranscode( attrs.getQName    (n) );
    const string            value = XercesTranscode( attrs.getValue    (n) );
    const string::size_type pos   = qname.find(":", 0);

    const string prefix = (pos != string::npos) ? qname.substr(0, pos) : "";

         if (prefix == "xmlns") add( value, name );
    else if (name   == "xmlns") add( value );
  }
}


/**
 * Destroys this list of XML namespace declarations.
 */
XercesNamespaces::~XercesNamespaces ()
{
}

/** @endcond doxygen-libsbml-internal */
