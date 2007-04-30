/**
 * @file    SBMLErrorLog.h
 * @brief   Stores errors (and messages) encountered during an SBML parse
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

#ifndef SBMLErrorLog_h
#define SBMLErrorLog_h


#include <sbml/common/extern.h>
#include <sbml/xml/XMLErrorLog.h>


class XMLToken;


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
   * Logs an error message for the given SBML error code.
   */
  void logError (unsigned int error);

  /**
   * Logs an error message indicating the XML element is unrecognized.
   */
  void unrecognizedElement (const XMLToken& element);
};


#endif  /* SBMLErrorLog_h */
