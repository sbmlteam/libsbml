/**
 * @file    ExpatHandler.cpp
 * @brief   Redirect Expat events to an XMLHandler
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

#include <expat.h>

#include <sbml/xml/XMLHandler.h>
#include <sbml/xml/XMLTriple.h>
#include <sbml/xml/XMLToken.h>

#include <sbml/xml/ExpatAttributes.h>
#include <sbml/xml/ExpatHandler.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

/*
 * The functions below are internal to this file.  They simply redirect to
 * the corresponding ExpatHandler method (assuming UserData contains a
 * pointer to ExpatHandler).  I first saw this redirect scheme used in
 * Stefan Hoops' ExpatParser class.
 */

static void
XML (void* userData, const XML_Char* version, const XML_Char* encoding, int)
{
  if (version == 0) return;
  static_cast<ExpatHandler*>(userData)->XML(version, encoding);
}

static void
startElement (void* userData, const XML_Char* name, const XML_Char** attrs)
{
  static_cast<ExpatHandler*>(userData)->startElement(name, attrs);
}

static void
startNamespace (void* userData, const XML_Char* prefix, const XML_Char* uri)
{
  static_cast<ExpatHandler*>(userData)->startNamespace(prefix, uri);
}

static void
endElement (void* userData, const XML_Char* name)
{
  static_cast<ExpatHandler*>(userData)->endElement(name);
}

static void
characters (void* userData, const XML_Char* chars, int length)
{
  static_cast<ExpatHandler*>(userData)->characters(chars, length);
}

static int
unknownEncodingHandler(void *encodingHandlerData,
		       const XML_Char *name,
		       XML_Encoding *info)
{
  return XML_STATUS_ERROR;
}

/**
 * Creates a new ExpatHandler.  Expat events will be redirected to the
 * given XMLHandler.
 */
ExpatHandler::ExpatHandler (XML_Parser parser, XMLHandler& handler) :
   mParser ( parser  )
 , mHandler( handler )
{
  XML_SetXmlDeclHandler      ( mParser, ::XML                        );
  XML_SetElementHandler      ( mParser, ::startElement, ::endElement );
  XML_SetCharacterDataHandler( mParser, ::characters                 );
  XML_SetNamespaceDeclHandler( mParser, ::startNamespace, 0          );
  XML_SetUserData            ( mParser, static_cast<void*>(this)     );
}


/**
 * Destroys this ExpatHandler.
 */
ExpatHandler::~ExpatHandler ()
{
}


/**
 * Receive notification of the beginning of the document.
 */
void
ExpatHandler::startDocument ()
{
  mHandler.startDocument();
}


/**
 * Receive notification of the XML declaration, i.e.
 * <?xml version="1.0" encoding="UTF-8"?>
 */
void
ExpatHandler::XML (const XML_Char* version, const XML_Char* encoding)
{
  XML_SetUnknownEncodingHandler( mParser, &unknownEncodingHandler, NULL );
  mHandler.XML(version, encoding);
}


/**
 * Receive notification of the start of an element.
 *
 * @param  name   The element name
 * @param  attrs  The specified or defaulted attributes
 */
void
ExpatHandler::startElement (const XML_Char* name, const XML_Char** attrs)
{
  const XMLTriple        triple    ( name  );
  const ExpatAttributes  attributes( attrs );
  const XMLToken         element   ( triple, attributes, mNamespaces,
                                     getLine(), getColumn() );

  mHandler.startElement(element);
  mNamespaces.clear();
}


/**
 * Receive notification of the start of an XML namespace.
 *
 * @param  prefix  The namespace prefix or NULL (for xmlns="...")
 * @param  uri     The namespace uri    or NULL (for xmlns="")
 */
void
ExpatHandler::startNamespace (const XML_Char* prefix, const XML_Char* uri)
{
  mNamespaces.add(uri ? uri : "", prefix ? prefix : "");
}


/**
 * Receive notification of the end of the document.
 */
void
ExpatHandler::endDocument ()
{
  mHandler.endDocument();
}


/**
 * Receive notification of the end of an element.
 *
 * @param  name  The element name
 */
void
ExpatHandler::endElement (const XML_Char* name)
{
  const XMLTriple  triple ( name );
  const XMLToken   element( triple, getLine(), getColumn() );

  mHandler.endElement(element);
}


/**
 * Receive notification of character data inside an element.
 *
 * @param  chars   The characters
 * @param  length  The number of characters to use from the character array
 */
void
ExpatHandler::characters (const XML_Char* chars, int length)
{
  XMLToken data( string(chars, length) );
  mHandler.characters(data);
}


/**
 * @return the column number of the current XML event.
 */
unsigned int
ExpatHandler::getColumn () const
{
  return static_cast<unsigned int>( XML_GetCurrentColumnNumber(mParser) );
}


/**
 * @return the line number of the current XML event.
 */
unsigned int
ExpatHandler::getLine () const
{
  return static_cast<unsigned int>( XML_GetCurrentLineNumber(mParser) );
}

/** @endcond doxygen-libsbml-internal */
