/**
 * @file    SBMLError.cpp
 * @brief   Represents SBML errors and other diagnostics
 * @author  Michael Hucka
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
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
#include <iomanip>
#include <sstream>

#include <sbml/xml/XMLError.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorTable.h>


/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal **/
/** 
 * Helper function for SBMLError().  Takes an index, SBML level and version,
 * and returns the appropriate field for the severity code out of the
   errorTable entry.
 */
static const unsigned int
getSeverityForEntry(unsigned int index,
                    unsigned int level,
                    unsigned int version)
{
  if ( level == 1 )
  {
    switch (version)
    {
    case 1:
      return errorTable[index].l1v1_severity;

    case 2: 
    default:
      return errorTable[index].l1v2_severity;
    }
  }
  else   // Must be level 2.
  {
    switch (version)
    {
    case 1:
      return errorTable[index].l2v1_severity;

    case 2:
      return errorTable[index].l2v2_severity;

    case 3:
      return errorTable[index].l2v3_severity;

    case 4:
    default:
      return errorTable[index].l2v4_severity;
    }
  }
}


/*
 * Table of strings corresponding to the values from SBMLErrorCategory_t.
 * The enumeration starts at a number higher than 0, so each entry is keyed
 * by its enum value.
 *
 * A similar table for severity strings is currently unnecessary because
 * libSBML never returns anything more than the XMLSeverityCode_t values.
 */
static struct sbmlCategoryString {
  unsigned int catCode;
  const char * catString;
} sbmlCategoryStringTable[] = {
  { LIBSBML_CAT_SBML,                   "General SBML conformance"    },
  { LIBSBML_CAT_SBML_L1_COMPAT,         "Translation to SBML L1V2"    },
  { LIBSBML_CAT_SBML_L2V1_COMPAT,	"Translation to SBML L2V1"    },
  { LIBSBML_CAT_SBML_L2V2_COMPAT,	"Translation to SBML L2V2"    },
  { LIBSBML_CAT_GENERAL_CONSISTENCY,	"SBML component consistency"  },
  { LIBSBML_CAT_IDENTIFIER_CONSISTENCY,	"SBML identifier consistency" },
  { LIBSBML_CAT_UNITS_CONSISTENCY,	"SBML unit consistency"       },
  { LIBSBML_CAT_MATHML_CONSISTENCY,     "MathML consistency"          },
  { LIBSBML_CAT_SBO_CONSISTENCY,        "SBO term consistency"        },
  { LIBSBML_CAT_OVERDETERMINED_MODEL,   "Overdetermined model"        },
  { LIBSBML_CAT_SBML_L2V3_COMPAT,	"Translation to SBML L2V3"    },
  { LIBSBML_CAT_MODELING_PRACTICE,      "Modeling practice"           },
  { LIBSBML_CAT_INTERNAL_CONSISTENCY,   "Internal consistency"        },
  { LIBSBML_CAT_SBML_L2V4_COMPAT,	"Translation to SBML L2V4"    }
};

static unsigned int sbmlCategoryStringTableSize
  = sizeof(sbmlCategoryStringTable)/sizeof(sbmlCategoryStringTable[0]);


/** @endcond doxygen-libsbml-internal **/


SBMLError::SBMLError (  const unsigned int errorId
                      , const unsigned int level
                      , const unsigned int version 
                      , const std::string& details
                      , const unsigned int line
                      , const unsigned int column
                      , const unsigned int severity
                      , const unsigned int category ):
    XMLError(errorId, details, line, column, severity, category)
{
  // Check if the given id is one we have in our table of error codes.  If
  // it is, fill in the fields of the error object with the appropriate
  // content.  If it's not in the table, take the content as-is.

  mErrorId = errorId;
  mLine    = line;
  mColumn  = column;

  if ( errorId >= 0 && errorId < XMLErrorCodesUpperBound )
  {
    // then error was caught during the XML read
    return;
  }
  else if ( errorId > XMLErrorCodesUpperBound
            && errorId < SBMLCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);
    unsigned int index = 0;

    for ( unsigned int i = 0; i < tableSize; i++ )
    {
      if ( errorId == errorTable[i].code )
      {
        index = i;
        break;
      }
    }

    if ( index == 0 && ! (errorId > LibSBMLAdditionalCodesLowerBound
                          && errorId < SBMLCodesUpperBound) )
    {
      // The id is in the range of error numbers that are supposed to be in
      // the SBML layer, but it's NOT in our table. This is an internal error.
      // Unfortunately, we don't have an error log or anywhere to report it
      // except the measure of last resort: the standard error output.
    
      cerr << "Internal error: unknown error code '" << errorId
           << "' encountered while processing error." << endl;
      return;
    }

    // The rest of this block massages the results to account for how some
    // internal bookkeeping is done in libSBML 3, and also to provide
    // additional info in the messages.

    if ( mErrorId == InconsistentArgUnitsWarnings
         || mErrorId == InconsistentPowerUnitsWarnings
         || mErrorId == InconsistentExponUnitsWarnings )
    {
      mErrorId = InconsistentArgUnits;
    }

    ostringstream newMsg;
    mSeverity = getSeverityForEntry(index, level, version);

    if (mSeverity == LIBSBML_SEV_SCHEMA_ERROR)
    {
      // Prior to L2v3, many possible errors were not listed separately as
      // validation rules because they were assumed to be caught by a
      // schema-aware XML parser.  In L2v3, we stopped relying on this and
      // listed schema errors separately.  This poses a problem for how
      // libSBML should errors for documents having levels/versions < L2v3.
      // LibSBML handles this internally by using the special severity code
      // SchemaError in SBMLErrorTable.h for those SBML level/version
      // combinations that didn't have separate validation rules, then
      // here, we translate the errors into the same basic error code and
      // add some elaboration to the error text message.

      mErrorId  = NotSchemaConformant;
      mSeverity = LIBSBML_SEV_ERROR;
      newMsg << errorTable[3].message << " "; // FIXME
    }
    else if (mSeverity == LIBSBML_SEV_GENERAL_WARNING)
    {
      // General warnings are our internal code for non-XML-schema issues
      // that were not defined as errors in prior levels/versions, but then
      // were defined as errors at some later time.  Like with SchemaError,
      // we use the GeneralWarning code for those cases in SBMLErrorTable.h
      // and then here we translate them into regular warnings.

      mSeverity = LIBSBML_SEV_WARNING;
      newMsg << "[Although SBML Level " << level
             << " Version " << version << " does not explicitly define the "
             << "following as an error, other Levels and/or Versions "
             << "of SBML do.] " << endl;
    }

    newMsg << errorTable[index].message;
    if (!details.empty())
    {
      newMsg << " " << details;
    }      
    newMsg << endl;
      
    mMessage        = newMsg.str();
    mCategory       = errorTable[index].category;

    // If it's got a category from the XML layer, then it will already have
    // been assigned a category string by the constructor.  Only go
    // assigning one if it's in the SBML range of categories.

    if ( mCategory >= LIBSBML_CAT_SBML )
    {
      for ( unsigned int i = 0; i < sbmlCategoryStringTableSize; i++ )
        if ( sbmlCategoryStringTable[i].catCode == mCategory )
        {
          mCategoryString = sbmlCategoryStringTable[i].catString;
          break;
        }
    }

    return;
  }

  // It's not an error code in the SBML layer, so assume the caller has
  // filled in all the relevant additional data.  (If they didn't, the
  // following merely assigns the defaults.)

  mMessage  = details;
  mSeverity = severity;
  mCategory = category;
}


/*
 * Outputs this SBMLError to stream in the following format (and followed by
 * a newline):
 *
 *   line: (error_id [severity]) message
 */
ostream& operator<< (ostream& s, const SBMLError& error)
{
  s << "line " << error.getLine() << ": ("
    << setfill('0') << setw(5) << error.getErrorId()
    << " [";

  switch (error.getSeverity())
  {
  case LIBSBML_SEV_INFO:            s << "Advisory"; break;
  case LIBSBML_SEV_WARNING:         s << "Warning";  break;
  case LIBSBML_SEV_FATAL:           s << "Fatal";    break;
  case LIBSBML_SEV_ERROR:           s << "Error";    break;
  case LIBSBML_SEV_SCHEMA_ERROR:    s << "Error";    break;
  case LIBSBML_SEV_GENERAL_WARNING: s << "Warning";  break;
  }

  s << "]) " << error.getMessage() << endl;
  return s;
}
