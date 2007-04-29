/**
 * @file    XMLError.cpp
 * @brief   Represents errors (and messages) encountered during an XML parse
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


#include <iostream>
#include <sstream>

#include <sbml/xml/XMLError.h>


using namespace std;


/**
 * Creates a new XMLError to report that something occurred at the given
 * line and column.  Each XMLError also has an identification number, a
 * category, and a severity level associated with it.
 */
XMLError::XMLError (  unsigned int   id
                    , const string&  message
                    , Severity       severity
                    , const string&  category
                    , unsigned int   line
                    , unsigned int   column ) :
   mId      ( id       )
 , mMessage ( message  )
 , mSeverity( severity )
 , mCategory( category )
 , mLine    ( line     )
 , mColumn  ( column   )
{
}


/**
 * Destroys this XMLError.
 */
XMLError::~XMLError ()
{
}


/**
 * @return the id of this XMLError.
 */
unsigned int
XMLError::getId () const
{
  return mId;
}


/**
 * @return the message text of this XMLError.
 */
const string&
XMLError::getMessage () const
{
  return mMessage;
}


/**
 * @return the line number where this XMLError ocurred.
 */
unsigned int
XMLError::getLine () const
{
  return mLine;
}


/**
 * @return the column number where this XMLError occurred.
 */
unsigned int
XMLError::getColumn () const
{
  return mColumn;
}


/**
 * @return the severity of this XMLError.  XMLError severity levels
 * correspond to those defined in the XML specification (with the addition
 * of Info for informational messages).
 */
XMLError::Severity
XMLError::getSeverity () const
{
  return mSeverity;
}


/**
 * @return the category of this XMLError.  A category is a string, similiar
 * in spirit to an XML namespace, which can be used to partition errors
 * into distinct groups.  Among other things, this can be used to prevent
 * id conflicts by uniquely identifying an XMLError by both id and
 * category.
 */
const string&
XMLError::getCategory () const
{
  return mCategory;
}


/**
 * @return true if this XMLError is for informational purposes only,
 * false otherwise.
 */
bool
XMLError::isInfo () const
{
  return (mSeverity == Info);
}


/**
 * @return true if this XMLError is a warning, false otherwise.
 */
bool
XMLError::isWarning () const
{
  return (mSeverity == Warning);
}


/**
 * @return true if this XMLError is an error, false otherwise.
 */
bool
XMLError::isError () const
{
  return (mSeverity == Error);
}


/**
 * @return true if this XMLError is a fatal error, false otherwise.
 */
bool
XMLError::isFatal () const
{
  return (mSeverity == Fatal);
}


/**
 * Sets the line number where this XMLError occurred.
 */
void
XMLError::setLine (unsigned int line)
{
  mLine = line;
}


/**
 * Sets the column number where this XMLError occurred.
 */
void
XMLError::setColumn (unsigned int column)
{
  mColumn = column;
}


/**
 * Outputs this XMLError to stream in the following format (and followed by
 * a newline):
 *
 *   line: (id) message
 */
ostream& operator<< (ostream& s, const XMLError& error)
{
  s << error.mLine << ": (" << error.mId << ") " << error.mMessage << endl;
  return s;
}


/** @cond doxygen-c-only */

/**
 * Creates a new XMLError to report that something occurred.
 */
LIBLAX_EXTERN
XMLError_t*
XMLError_create (void)
{
  return new(nothrow) XMLError;
}


/**
 * Creates a new XMLError with the identification number
 * and message set.
 *
 * @param id an unsigned int, the identification number of the error.
 * @param message a string, the error message.
 *
 */
LIBLAX_EXTERN
XMLError_t*
XMLError_createWithIdAndMessage (unsigned int id, const char * message)
{
  return new(nothrow) XMLError(id, message);
}

/**
 * Creates a new XMLError to report that something occurred at the given
 * line and column.  Each XMLError also has an identification number, a
 * category, and a severity level associated with it.
 *
 * @param id an unsigned int, the identification number of the error.
 * @param message a string, the error message.
 * @param severity XMLError_Severity, severity of the error.
 * @param category a string, the category to which the error belongs.
 * @param line an unsigned int, the line number at which the error occurs.
 * @param column an unsigned int, the column number at which the error occurs.
 *
 */


//LIBLAX_EXTERN
//XMLError_t*
//XMLError_createWithAll (unsigned int id, const char * message, XMLError_Severity severity,
//                        const char * category, unsigned int line, unsigned int column)
//{
//  XMLError::Severity s;
//  switch (severity)
//  {
//  case Info:
//    s = XMLError::Severity::Info;
//    break;
//  case Warning:
//    s = XMLError::Severity::Warning;
//    break;
//  case Error:
//    s = XMLError::Severity::Error;
//    break;
//  case Fatal:
//    s = XMLError::Severity::Fatal;
//    break;
//  default:
//    s = XMLError::Severity::Error;
//    break;
//  }
//  return new(nothrow) XMLError(id, message, s, category, line, column);
//}


/**
 * Frees the given XMLError_t structure.
 *
 * @param error the XMLError_t structure to be freed.
 **/
LIBLAX_EXTERN
void
XMLError_free(XMLError_t* error)
{
  delete static_cast<XMLError*>(error);
}

/**
 * Returns the id of this XMLError.
 *
 * @param error the XMLError_t from which to return the id.
 *
 * @return the id of this XMLError.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getId (const XMLError_t *error)
{
  return error->getId();
}


/**
 * Returns the message text of this XMLError.
 *
 * @param error the XMLError_t from which to return the message.
 *
 * @return the message text of this XMLError.
 */
LIBLAX_EXTERN
const char *
XMLError_getMessage (const XMLError_t *error)
{
  const string msg = error->getMessage();


  return msg.empty() ? 0 : msg.c_str();
}


/**
 * Return the line number where this XMLError occurred.
 *
 * @param error the XMLError_t from which to return the line number.
 *
 * @return the line number where this XMLError occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getLine (const XMLError_t *error)
{
  return error->getLine();
}


/**
 * Return the column number where this XMLError occurred.
 *
 * @param error the XMLError_t from which to return the column number.
 *
 * @return the column number where this XMLError occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getColumn (const XMLError_t *error)
{
  return error->getColumn();
}


/**
 * Return the severity of this XMLError.  XMLErrors severity levels
 * correspond to those defined in the XML specification (with the addition
 * of Info for informational messages).
 *
 * @li  0 - Info
 * @li  1 - Warning
 * @li  2 - Error
 * @li  3 - Fatal
 *
 * @param error the XMLError_t from which to return the severity.
 *
 * @return the severity of this XMLError.
 */
LIBLAX_EXTERN
XMLError_Severity
XMLError_getSeverity (const XMLError_t *error)
{
  return static_cast<XMLError_Severity>( error->getSeverity() );
}


/**
 * Return the category of this XMLError.  A category is a string, similiar
 * in spirit to an XML namespace, which can be used to partition errors
 * into distinct groups.  Among other things, this can be used to prevent
 * id conflicts by uniquely identifying an XMLError by both id and
 * category.
 *
 * @param error the XMLError_t from which to return the category.
 *
 * @return the category of this XMLError.
 */
LIBLAX_EXTERN
const char *
XMLError_getCategory (const XMLError_t *error)
{
  const std::string c = error->getCategory();


  return c.empty() ? 0 : c.c_str();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is for information only.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is for informational purposes
 * only, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isInfo (const XMLError_t *error)
{
  return static_cast<int>( error->isInfo() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is a warning.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is a warning, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isWarning (const XMLError_t *error)
{
  return static_cast<int>( error->isWarning() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is an error.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is an error, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isError (const XMLError_t *error)
{
  return static_cast<int>( error->isError() );
}



/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is a fatal error.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is a fatal error, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isFatal (const XMLError_t *error)
{
  return static_cast<int>( error->isFatal() );
}


/**
 * Outputs this XMLError to stream in the following format (and
 * followed by a newline):
 *
 *   line: (id) message
 *
 * @param error, the XMLError_t structure to write.
 * @param stream, the stream to write to.
 */
LIBLAX_EXTERN
void
XMLError_print (const XMLError_t *error, FILE *stream)
{
  ostringstream os;
  os << *(static_cast<const XMLError*>(error));

  fprintf(stream, "%s", os.str().c_str());

}
/** @endcond doxygen-c-only */
