/**
 * \file    XMLError.cpp
 * \brief   Represents errors (and messages) encountered during an XML parse
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




/**
 * @return the id of this XMLError.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getId (const XMLError_t *xe)
{
  return xe->getId();
}


/**
 * @return the message text of this XMLError.
 */
LIBLAX_EXTERN
const char *
XMLError_getMessage (const XMLError_t *xe)
{
  const string msg = xe->getMessage();


  return msg.empty() ? 0 : msg.c_str();
}


/**
 * @return the line number where this XMLError ocurred.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getLine (const XMLError_t *xe)
{
  return xe->getLine();
}


/**
 * @return the column number where this XMLError occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getColumn (const XMLError_t *xe)
{
  return xe->getColumn();
}


/**
 * @return the severity of this XMLError.  XMLErrors severity levels
 * correspond to those defined in the XML specification (with the addition
 * of Info for informational messages).
 *
 *   0 - Info
 *   1 - Warning
 *   2 - Error
 *   3 - Fatal
 */
LIBLAX_EXTERN
XMLError_Severity
XMLError_getSeverity (const XMLError_t *xe)
{
  return static_cast<XMLError_Severity>( xe->getSeverity() );
}


/**
 * @return the category of this XMLError.  A category is a string, similiar
 * in spirit to an XML namespace, which can be used to partition errors
 * into distinct groups.  Among other things, this can be used to prevent
 * id conflicts by uniquely identifying an XMLError by both id and
 * category.
 */
LIBLAX_EXTERN
const char *
XMLError_getCategory (const XMLError_t *xe)
{
  const std::string c = xe->getCategory();


  return c.empty() ? 0 : c.c_str();
}


/**
 * @return true (non-zero) if this XMLError is for informational purposes
 * only, false (0) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isInfo (const XMLError_t *xe)
{
  return static_cast<int>( xe->isInfo() );
}


/**
 * @return true (non-zero) if this XMLError is a warning, false (0)
 * otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isWarning (const XMLError_t *xe)
{
  return static_cast<int>( xe->isWarning() );
}


/**
 * @return true (non-zero) if this XMLError is an error, false (0) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isError (const XMLError_t *xe)
{
  return static_cast<int>( xe->isError() );
}



/**
 * @return true (non-zero) if this XMLError is a fatal error, false (0)
 * otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isFatal (const XMLError_t *xe)
{
  return static_cast<int>( xe->isFatal() );
}


/**
 * Outputs this XMLError to stream in the following format (and
 * followed by a newline):
 *
 *   line: (id) message
 */
LIBLAX_EXTERN
void
XMLError_print (const XMLError_t *xe, FILE *stream)
{
  ostringstream os;
  os << *(static_cast<const XMLError*>(xe));

  fprintf(stream, "%s", os.str().c_str());

}
