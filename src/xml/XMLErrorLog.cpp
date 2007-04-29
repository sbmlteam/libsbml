/**
 * @file    XMLErrorLog.cpp
 * @brief   Stores errors (and messages) encountered during an XML parse
 * @author  Ben Bornstein
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

#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLParser.h>

#include <sbml/xml/XMLErrorLog.h>


using namespace std;


/**
 * Creates a new empty XMLErrorLog.
 */
XMLErrorLog::XMLErrorLog ():mParser(NULL)
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
    unsigned int line, column;
    if(mParser!=NULL)
    {
      try
      {
        line = mParser->getLine();
        column = mParser->getColumn();
      }
      catch (...)
      {
        line = 1;
        column = 1;
      }
    }
    else
    {
      line = 1;
      column = 1;
    }

    XMLError& e = mErrors.back();

    e.setLine  ( line   );
    e.setColumn( column );
  }
}


/**
 * Logs (copies) the XMLErrors in the given XMLError list to this
 * XMLErrorLog.
 */
void
XMLErrorLog::add (const list<XMLError>& errors)
{
  list<XMLError>::const_iterator end = errors.end();
  list<XMLError>::const_iterator iter;

  for (iter = errors.begin(); iter != end; ++iter) add( XMLError(*iter) );
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
const XMLError*
XMLErrorLog::getError (unsigned int n) const
{
  return (n < mErrors.size()) ? &mErrors[n] : 0;
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


/**
 * Sets the XMLParser for this XMLErrorLog.
 *
 * The XMLParser will be used to obtain the current line and column
 * number as XMLErrors are logged (if they have a line and column number
 * of zero).
 */
void
XMLErrorLog::setParser (const XMLParser* p)
{
  mParser = p;
}

/** @cond doxygen-c-only */

/**
 * Creates a new empty XMLErrorLog_t structure and returns it.
 *
 * @return the new XMLErrorLog_t structure.
 **/
LIBLAX_EXTERN
XMLErrorLog_t *
XMLErrorLog_create (void)
{
  return new(nothrow) XMLErrorLog;
}


/**
 * Frees the given XMLError_t structure.
 *
 * @param log XMLErrorLog_t, the error log to be freed.
 */
LIBLAX_EXTERN
void
XMLErrorLog_free (XMLErrorLog_t *log)
{
    delete static_cast<XMLErrorLog*>(log);
}


/**
 * Logs the given XMLError_t structure.
 *
 * @param log XMLErrorLog_t, the error log to be added to.
 * @param error XMLError_t, the error to be logged.
 */
LIBLAX_EXTERN
void
XMLErrorLog_add (XMLErrorLog_t *log, const XMLError_t *error)
{
  log->add(*error);
}


/**
 * Logs an attribute datatype error.
 *
 * @param log XMLErrorLog_t, the error log to be added to.
 * @param  name  Name of the attribute
 * @param  type  The datatype of the attribute value.
 */
LIBLAX_EXTERN
void
XMLErrorLog_attributeTypeError (XMLErrorLog_t *log,
				const char *name, 
				XMLErrorLog_DataType type)
{
  log->attributeTypeError(name, static_cast<XMLErrorLog::DataType>(type));
}


/**
 * Logs an error indicating a required attribute was missing.
 *
 * @param log XMLErrorLog_t, the error log to be added to.
 * @param  name  Name of the attribute
 */
LIBLAX_EXTERN
void
XMLErrorLog_attributeRequired (XMLErrorLog_t *log, const char *name)
{
  log->attributeRequired(name);
}


/**
 * Returns the nth XMLError_t in this log.
 *
 * @param log XMLErrorLog_t, the error log to be queried.
 * @param n unsigned int number of the error to retrieve.
 *
 * @return the nth XMLError_t in this log.
 */
LIBLAX_EXTERN
const XMLError_t *
XMLErrorLog_getError (const XMLErrorLog_t *log, unsigned int n)
{
  return log->getError(n);
}


/**
 * Returns the number of errors that have been logged.
 *
 * @param log XMLErrorLog_t, the error log to be queried.
 *
 * @return the number of errors that have been logged.
 */
LIBLAX_EXTERN
unsigned int
XMLErrorLog_getNumErrors (const XMLErrorLog_t *log)
{
  return log->getNumErrors();
}


/**
 * Sets the element name to use when logging attributeTypeError() and
 * attributeRequired() errors (optional).
 *
 * @param log XMLErrorLog_t, the error log whose element name is to be set.
 * @param name string, the name of the element.
 */
LIBLAX_EXTERN
void
XMLErrorLog_setElement (XMLErrorLog_t *log, const char *name)
{
  log->setElement(name);
}
/** @endcond doxygen-c-only */
