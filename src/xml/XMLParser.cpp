/**
 * \file    XMLParser.cpp
 * \brief   XMLParser interface and factory
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


#ifdef USE_EXPAT
#include <sbml/xml/ExpatParser.h>
#endif

#ifdef USE_LIBXML
#include <sbml/xml/LibXMLParser.h>
#endif

#ifdef USE_XERCES
#include <sbml/xml/XercesParser.h>
#endif

#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLParser.h>


using namespace std;


/**
 * Creates a new XMLParser.  The parser will notify the given XMLHandler
 * of parse events and errors.
 */
XMLParser::XMLParser () : mErrorLog(0)
{

}


/**
 * Destroys this XMLParser.
 */
XMLParser::~XMLParser ()
{
}


/**
 * Creates a new XMLParser.  The parser will notify the given XMLHandler
 * of parse events and errors.
 *
 * The library parameter indicates the underlying XML library to use if
 * the XML compatibility layer has been linked against multiple XML
 * libraries.  It may be one of: "expat" (default), "libxml", or
 * "xerces".
 *
 * If the XML compatibility layer has been linked against only a single
 * XML library, the library parameter is ignored.
 */
XMLParser*
XMLParser::create (XMLHandler& handler, const string library)
{
#ifdef USE_EXPAT
  if (library.empty() || library == "expat")  return new ExpatParser(handler);
#endif

#ifdef USE_LIBXML
  if (library.empty() || library == "libxml") return new LibXMLParser(handler);
#endif

#ifdef USE_XERCES
  if (library.empty() || library == "xerces") return new XercesParser(handler);
#endif

  return 0;
}


/**
 * @return an XMLErrorLog which can be used to log XML parse errors and
 * other validation errors (and messages).
 */
XMLErrorLog*
XMLParser::getErrorLog ()
{
  return mErrorLog;
}


/**
 * Sets the XMLErrorLog this parser will use to log errors.
 */
void
XMLParser::setErrorLog (XMLErrorLog* log)
{
  mErrorLog = log;
  if (mErrorLog) mErrorLog->setParser(this);
}
