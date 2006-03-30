/**
 * \file    EchoHandler.cpp
 * \brief   Example XMLHandler that simply echos (prints out) an XML document.
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


#include <iostream>

#include <XMLToken.h>
#include <XMLAttributes.h>

#include "EchoHandler.h"


using namespace std;


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
