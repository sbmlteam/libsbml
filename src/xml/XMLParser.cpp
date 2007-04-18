/**
 * @file    XMLParser.cpp
 * @brief   XMLParser interface and factory
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 */
/* Copyright 2007 California Institute of Technology.
 * Copyright 2006 California Institute of Technology and 
 *                Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution.  
 * It is also available online at http://sbml.org/software/libsbml/license.html
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


/**
 * Table mapping error codes to messages.  The error code numbers are not
 * contiguous, hence the table has to map code numbers to strings rather than
 * simply being an array of strings.  The table is an array of vectors of
 * items [code, string], where `code' is an error code taken from the
 * enumeration XMLParser::errorCodes, and `string' is an appropriate error
 * string.
 *
 * Some parsers have a lot of possible error messages (e.g., Xerces has >200).
 * Not all are applicable, and not all the parsers define errors at the same
 * level of fine granularity.  The following therefore tries to abstract out
 * a number of categories of errors, with the intention that parser-specific
 * errors be mapped to a smaller number of errors here.  Sometimes this means
 * that the error messages here are very general (such-and-such is malformed
 * or incorrect).  Unfortunately, it's not clear what else we can do, given
 * the differences among the XML parsers.
 */
static struct errorMessageTable {
  enum XMLParser::errorCodes code;
  const char *               string;
} errorMessages[] = {
  { XMLParser::NoError,                       "" },
  { XMLParser::ErrorNoXMLDecl,                "Missing XML declaration" },
  { XMLParser::ErrorBadXMLDecl,               "XML declaration is malformed or incorrect" },
  { XMLParser::ErrorInvalidChar,              "Invalid character in XML content" },
  { XMLParser::ErrorNotWellFormed,            "Badly formed XML" },
  { XMLParser::ErrorUnclosedToken,            "Unclosed token" },
  { XMLParser::ErrorInvalidConstruct,         "XML construct is invalid or not permitted" },
  { XMLParser::ErrorTagMismatch,              "Element tag mismatch or missing tag" },
  { XMLParser::ErrorDupAttribute,             "Duplicate attribute" },
  { XMLParser::ErrorBadDOCTYPE,               "Invalid or incorrect XML DOCTYPE declaration" },
  { XMLParser::ErrorUndefinedEntity,          "Undefined XML entity" },
  { XMLParser::ErrorBadProcessingInstruction, "Invalid XML processing instruction" },
  { XMLParser::ErrorBadPrefixDefinition,      "Invalid XML Namespace prefix" },
  { XMLParser::ErrorBadPrefixValue,           "Invalid XML prefix value" },
  { XMLParser::ErrorOutOfMemory,              "Out of memory" },
  // Always make this next one last in this table:
  { XMLParser::UnknownError,                  "Unknown error encountered in XML parser" }
};


/**
 * Returns a string message given an error code taken from the
 * 'errorCodes' enumeration.
 */
const char *
XMLParser::getErrorMessage (enum XMLParser::errorCodes code)
{
  unsigned int tableSize = sizeof(errorMessages)/sizeof(errorMessages[0]);

  // Iterate through the table, searching for a match for the code.
  // Yes, this is inefficient, but if we're already in an exception,
  // who cares how efficient the error look-up is?

  if (code > 0 && code < XMLParser::UnknownError)
  {
    for (unsigned int i = 0; i < tableSize; i++)
    {
      if (errorMessages[i].code == code)
	return errorMessages[i].string;
    };
  }

  // Something's wrong with the code given as argument.  We should never
  // get here if all were going properly.  (Note: the following relies on
  // the assumption that XMLParser::UnknownError is the last entry in
  // the error table.)

  return errorMessages[tableSize-1].string;
}
