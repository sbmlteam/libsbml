/**
 * \file    XMLErrorLog.cpp
 * \brief   Stores errors (and messages) encountered during an XML parse
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


#include <sstream>

#include "XMLError.h"
#include "XMLParser.h"

#include "XMLErrorLog.h"


using namespace std;


/**
 * Creates a new empty XMLErrorLog.  The XMLParser will be used to obtain
 * the current line and column number as XMLErrors are logged (if they have
 * a line and column number of zero).
 */
XMLErrorLog::XMLErrorLog (const XMLParser& parser) : mParser(parser)
{
}


/**
 * Destroys this XMLErrorLog.
 */
XMLErrorLog::~XMLErrorLog ()
{
}


/**
 * Logs the given XMLError.
 */
void
XMLErrorLog::add (const XMLError& error)
{
  mErrors.push_back(error);

  if (error.getLine() == 0 && error.getColumn() == 0)
  {
    XMLError& e = mErrors.back();

    e.setLine  ( mParser.getLine()   );
    e.setColumn( mParser.getColumn() );
  }
}


/**
 * Logs an attribute format error.
 *
 * @param  name  Name of the attribute
 * @param  type  The datatype of the attribute value.
 */
void
XMLErrorLog::attributeTypeError (  const string&         name
                                 , XMLErrorLog::DataType type )
{
  ostringstream message;

  message << "The ";

  if ( !mElement.empty() ) message << mElement << ' ';
  message << name;

  switch (type)
  {
    case Boolean:
      message <<
        " attribute must have a value of either \"true\" or \"false\""
        " (all lowercase).  The numbers \"1\" (true) and \"0\" (false) are"
        " also allowed, but not preferred.  For more information, see:"
        " http://www.w3.org/TR/xmlschema-2/#boolean.";
      break;

    case Double:
      message <<
        " attribute must be a double (decimal number).  To represent"
        " infinity use \"INF\", negative infinity use \"-INF\", and"
        " not-a-number use \"NaN\".  For more information, see:"
        " http://www.w3.org/TR/xmlschema-2/#double.";
      break;

    case Integer:
      message <<
        " attribute must be an integer (whole number).  For more"
        " information, see: http://www.w3.org/TR/xmlschema-2/#integer.";
      break;
  }

  add( XMLError(100, message.str()) );
}


/**
 * Logs an error indicating a required attribute was missing.
 *
 * @param  name  Name of the attribute
 */
void
XMLErrorLog::attributeRequired (const string& name)
{
  ostringstream message;

  if ( !mElement.empty() ) message << mElement << ' ';

  message << name << " attribute is required.";

  add( XMLError(101, message.str()) );
}


/**
 * @return the nth XMLError in this log.
 */
const XMLError&
XMLErrorLog::getError (unsigned int n) const
{
  return mErrors[n];
}


/**
 * @return the number of errors that have been logged.
 */
unsigned int
XMLErrorLog::getNumErrors () const
{
  return mErrors.size();
}


/**
 * Sets the element name to use when logging attributeTypeError() and
 * attributeRequired() errors (optional).
 */
void
XMLErrorLog::setElement (const string& name)
{
  mElement = name;
}
