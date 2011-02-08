/**
 * \file    EchoHandler.cpp
 * \brief   Example XMLHandler that simply echos (prints out) an XML document.
 * \author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <iostream>

#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>

#include "EchoHandler.h"

/** @cond doxygen-ignored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


/**
 * Creates a new EchoHandler.
 */
EchoHandler::EchoHandler () : mInStart(false)
{
}


/**
 * Destroys this XMLEchoHandler.
 */
EchoHandler::~EchoHandler ()
{
}


/**
 * Receive notification of the XML declaration.
 */
void
EchoHandler::XML (const string& version, const string& encoding)
{
  cout << "<?xml version=\"" << version << "\" encoding=\"" << encoding << '"'
       << "?>" << endl;
}


/**
 * Receive notification of the beginning of the document.
 */
void
EchoHandler::startDocument ()
{
  // cout << "<!-- startDocument() -->" << endl;
}


/**
 * Receive notification of the start of an element.
 */
void
EchoHandler::startElement (const XMLToken& element)
{
  if (mInStart)
  {
    mInStart = false;
    cout << ">";
  }

  mInStart = true;
  const XMLAttributes& attributes = element.getAttributes();
  const XMLNamespaces& namespaces = element.getNamespaces();

  cout << '<';

  if (element.getPrefix().empty() == false)
  {
    cout << element.getPrefix() << ':';
  }

  cout << element.getName();

  for (int n = 0; n < namespaces.getLength(); ++n)
  {
    cout << " xmlns";

    if (namespaces.getPrefix(n).empty() == false)
    {
      cout << ':' << namespaces.getPrefix(n);
    }
    
    cout << '=' << '"' << namespaces.getURI(n) << '"';
  }

  for (int n = 0; n < attributes.getLength(); ++n)
  {
    cout << ' ' << attributes.getName (n) << '='
         << '"' << attributes.getValue(n) << '"';
  }
}


/**
 * Receive notification of the end of the document.
 */
void
EchoHandler::endDocument ()
{
  // cout << "<!-- endDocument() -->" << endl;
}


/**
 * Receive notification of the end of an element.
 */
void
EchoHandler::endElement (const XMLToken& element)
{
  if (mInStart)
  {
    mInStart = false;
    cout << "/>";
  }
  else
  {
    cout << "</";

    if (element.getPrefix().empty() == false)
    {
      cout << element.getPrefix() << ':';
    }

    cout << element.getName() << '>';
  }
}


/**
 * Receive notification of character data inside an element.
 */
void
EchoHandler::characters (const XMLToken& data)
{
  if (mInStart)
  {
    mInStart = false;
    cout << '>';
  }

  const string& chars = data.getCharacters();
  for (string::const_iterator c = chars.begin(); c != chars.end(); ++c)
  {
    switch (*c)
    {
      case '&' : cout << "&amp;" ; break;
      case '\'': cout << "&apos;"; break;
      case '<' : cout << "&lt;"  ; break;
      case '>' : cout << "&gt;"  ; break;
      case '"' : cout << "&quot;"; break;
      default  : cout << *c      ; break;
    }
  }
}
