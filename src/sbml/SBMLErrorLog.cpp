/**
 * @file    SBMLErrorLog.cpp
 * @brief   Stores errors (and messages) encountered while processing SBML.
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <string>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLParser.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>


/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

/**
 * Creates a new empty SBMLErrorLog.
 */
SBMLErrorLog::SBMLErrorLog ()
{
}


/**
 * Destroys this SBMLErrorLog.
 */
SBMLErrorLog::~SBMLErrorLog ()
{
}


/*
 * See SBMLError for a list of SBML error codes and XMLError
 * for a list of system and XML-level error codes.
 */
void
SBMLErrorLog::logError ( const unsigned int  errorId 
                       , const unsigned int level
                       , const unsigned int version
                       , const std::string&  details
                       , const unsigned int  line
                       , const unsigned int  column
                       , SBMLError::SBMLSeverity severity
                       , SBMLError::SBMLCategory category )
{
  add( SBMLError( errorId, level, version, details, line, column, 
                                                         severity, category ));
}


/**
 * Adds the given SBMLError to the log.
 *
 * @param error SBMLError, the error to be logged.
 */
void
SBMLErrorLog::add (const SBMLError& error)
{
  XMLErrorLog::add(dynamic_cast<const XMLError&>(error));
}


/**
 * Logs (copies) the SBMLErrors in the given SBMLError list to this
 * SBMLErrorLog.
 *
 * @param errors list, a list of SBMLError to be added to the log.
 */
void
SBMLErrorLog::add (const std::list<SBMLError>& errors)
{
  list<SBMLError>::const_iterator end = errors.end();
  list<SBMLError>::const_iterator iter;

  for (iter = errors.begin(); iter != end; ++iter)
    XMLErrorLog::add( XMLError(*iter) );
}


/** @endcond doxygen-libsbml-internal */


/**
 * Returns the nth SBMLError in this log.
 *
 * @param n unsigned int number of the error to retrieve.
 *
 * @return the nth SBMLError in this log.
 */
const SBMLError*
SBMLErrorLog::getError (unsigned int n) const
{
  return static_cast<const SBMLError*>(XMLErrorLog::getError(n));
}

