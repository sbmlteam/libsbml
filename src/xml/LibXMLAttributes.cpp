/**
 * @file    LibXMLAttributes.cpp
 * @brief   Creates new XMLAttributes from "raw" LibXML attributes.
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

#include <sbml/xml/LibXMLTranscode.h>
#include <sbml/xml/LibXMLAttributes.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/** @cond doxygen-libsbml-internal */

/**
 * Creates a new XMLAttributes set from the given "raw" LibXML attributes.
 */
LibXMLAttributes::LibXMLAttributes (  const xmlChar**     attributes
                                    , const unsigned int& size )
{
  mNames .reserve(size);
  mValues.reserve(size);

  for (unsigned int n = 0; n < size; ++n)
  {
    const string name    = LibXMLTranscode( attributes[5 * n]     );
    const string prefix  = LibXMLTranscode( attributes[5 * n + 1] );
    const string uri     = LibXMLTranscode( attributes[5 * n + 2] );

    const xmlChar* start = attributes[5 * n + 3];
    const xmlChar* end   = attributes[5 * n + 4];
    int length           = (end - start) / sizeof(xmlChar);

    const string value   =  LibXMLTranscode((length > 0) ? start : 0, length);

    mNames .push_back( XMLTriple(name, uri, prefix) );
    mValues.push_back( value );
  }
}


/**
 * Destroys this Attribute set.
 */
LibXMLAttributes::~LibXMLAttributes ()
{
}

/** @endcond doxygen-libsbml-internal */
