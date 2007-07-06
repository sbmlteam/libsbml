/**
 * @file    XMLErrorLog.cpp
 * @brief   Stores errors (and messages) encountered while processing XML.
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

#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLParser.h>

#include <sbml/xml/XMLErrorLog.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */
/**
 * Creates a new empty XMLErrorLog.
 */
XMLErrorLog::XMLErrorLog ():mParser(NULL)
{
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Destroys this XMLErrorLog.
 */
XMLErrorLog::~XMLErrorLog ()
{
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
    if (mParser != NULL)
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

    // Can't modify 'error' directly because it's const, and the const
    // declaration is needed to allow add() to be called in certain
    // contexts.  The following cheats the const, in effect, by getting
    // back the object after it's been added to the mErrors vector.

    XMLError& e = mErrors.back();

    e.setLine  ( line   );
    e.setColumn( column );
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Logs (copies) the XMLErrors in the given XMLError list to this
 * XMLErrorLog.
 */
void
XMLErrorLog::add (const std::list<XMLError>& errors)
{
  list<XMLError>::const_iterator end = errors.end();
  list<XMLError>::const_iterator iter;

  for (iter = errors.begin(); iter != end; ++iter) add( XMLError(*iter) );
}
/** @endcond doxygen-libsbml-internal */


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


/** @cond doxygen-libsbml-internal */
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
/** @endcond doxygen-libsbml-internal */


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



/** @endcond doxygen-c-only */
