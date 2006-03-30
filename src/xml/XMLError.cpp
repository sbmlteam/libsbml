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


#include "XMLError.h"


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
