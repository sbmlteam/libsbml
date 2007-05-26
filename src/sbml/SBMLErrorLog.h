/**
 * @file    SBMLErrorLog.h
 * @brief   Stores errors (and messages) encountered during an SBML parse
 * @author  Ben Bornstein
 * @author  Michael Hucka
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

#ifndef SBMLErrorLog_h
#define SBMLErrorLog_h


#include <sbml/common/extern.h>
#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/SBMLError.h>


class LIBSBML_EXTERN SBMLErrorLog : public XMLErrorLog
{
public:

  /**
   * Creates a new empty SBMLErrorLog.
   */
  SBMLErrorLog ();


  /**
   * Destroys this SBMLErrorLog.
   */
  virtual ~SBMLErrorLog ();


  /**
   * Convenience function that combines creating an SBMLError object and
   * adding it to the log.
   */
  void logError
  (
      const unsigned int id              = 0
    , const std::string& details         = ""
    , const unsigned int line            = 0
    , const unsigned int column          = 0
    , const SBMLError::SBMLSeverity severity = SBMLError::Error
    , const SBMLError::SBMLCategory category = SBMLError::SBML
  );


  /**
   * Adds the given SBMLError to the log.
   *
   * @param error SBMLError, the error to be logged.
   */
  void add (const SBMLError& error);


  /**
   * Logs (copies) the SBMLErrors in the given SBMLError list to this
   * SBMLErrorLog.
   *
   * @param errors list, a list of SBMLError to be added to the log.
   */
  void add (const std::list<SBMLError>& errors);


  /**
   * Returns the nth SBMLError in this log.
   *
   * @param n unsigned int number of the error to retrieve.
   *
   * @return the nth SBMLError in this log.
   */
  const SBMLError* getError (unsigned int n) const;

};


#endif  /* SBMLErrorLog_h */
