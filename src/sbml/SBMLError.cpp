/**
 * @file    SBMLError.cpp
 * @brief   Represents SBML errors and other diagnostics
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

#include <string>
#include <iostream>
#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorTable.h>


/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */



SBMLError::SBMLError (  const unsigned int            errorId
                      , const unsigned int            level
                      , const unsigned int            version 
                      , const std::string&            details
                      , const unsigned int            line
                      , const unsigned int            column
                      , const SBMLError::SBMLSeverity severity
                      , const SBMLError::SBMLCategory category ):
    XMLError(errorId, details, line, column, severity, category)
{
  // Check if the given id is one we have in our table of error codes.  If
  // it is, fill in the fields of the error object with the appropriate
  // content.  If it's not in the table, take the content as-is.

  mErrorId     = errorId;
  mLine   = line;
  mColumn = column;

  if ( errorId >= 0 && errorId < XMLError::ErrorCodesUpperBound )
  {
    // then error was caught during the XML read
    return;
  }
  else if ( errorId > XMLError::ErrorCodesUpperBound
       && errorId < SBMLError::SBMLCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);    

    for ( unsigned int i = 0; i < tableSize; i++ )
    {
      if ( errorTable[i].code == errorId )
      {
        switch (level)
        {
        case 1:
          switch (version)
          {
          case 1:
            mSeverity = errorTable[i].l1v1_severity;
            break;
          case 2:
            mSeverity = errorTable[i].l1v2_severity;
          default:
            break;
          }
          break;
        case 2:
        default:
          switch (version)
          {
          case 1:
            mSeverity = errorTable[i].l2v1_severity;
            break;
          case 2:
            mSeverity = errorTable[i].l2v2_severity;
            break;
          case 3:
          default:
            mSeverity = errorTable[i].l2v3_severity;
            break;
          }
          break;
        }


        ostringstream newMsg;
        if (mSeverity == SBMLError::SchemaError)
        {
          mErrorId = 10103;
          mSeverity = SBMLError::Error;

          newMsg << errorTable[3].message
                 << " " << errorTable[i].message << endl;
        }
        else if (mSeverity == SBMLError::GeneralWarning)
        {
          newMsg << "General warning: Although SBML Level " << level 
            << " Version " << version
            << " does not explicitly define this as an error, more recent "
            << "versions of SBML do. " << endl;
        }
        else
        {
          newMsg << errorTable[i].message;
        }

        /* report all these as 10501 */
        if (mErrorId == 10502
          || mErrorId == 10503
          || mErrorId == 10504)
        {
          mErrorId = 10501;
        }

        if (!details.empty())
        {
          newMsg << " " << details << endl;
        }
        else
        {
          newMsg << endl;
        }          
      
        mMessage  = newMsg.str();
        mCategory = errorTable[i].category;
        return;
      }
    }
      
    //if (fromValidator == 1)
    //{
      mMessage  = details;
      mSeverity = severity;
      mCategory = category;
      return;
    //}


    // The id is in the range of error numbers that are supposed to be in
    // the SBML layer, but it's NOT in our table. This is an internal error.
    // Unfortunately, we don't have an error log or anywhere to report it
    // except the measure of last resort: the standard error output.
    
    cerr << "Internal error: unknown error code '" << errorId
          << "' encountered while processing error" << endl;
  }

  // It's not an error code in the XML layer, so assume the caller has
  // filled in all the relevant additional data.  (If they didn't, the
  // following merely assigns the defaults.)

  mMessage  = details;
  mSeverity = severity;
  mCategory = category;
}
