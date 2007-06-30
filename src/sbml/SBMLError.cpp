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


typedef struct {
  unsigned int            code;
  SBMLError::SBMLCategory category;
  SBMLError::SBMLSeverity severity;
  const char*             message;
} sbmlErrorTableEntry_old;

static const sbmlErrorTableEntry_old errorTable_old[] =
{
  // 0
  { SBMLError::UnknownError, SBMLError::SBML, SBMLError::Fatal,
    "Unrecognized error encountered" },

  // 10308
  { SBMLError::InvalidSBOTermSyntax, SBMLError::SBML, SBMLError::Error,
    "The value of a sboTerm attribute must have the data type SBOTerm, "
    "which is a string consisting of the characters 'S', 'B', 'O', ':' "
    "followed by exactly seven digits. (References: L2V2 Section 3.1.8.)"},

  // 10309
  { SBMLError::InvalidMetaidSyntax, SBMLError::SBML, SBMLError::Error,
    "The syntax of 'metaid' attribute values must conform to the syntax of the "
    "XML type 'ID'. (References: L2V2 Sections 3.3.1 and 3.1.6.)" },

  // 10310
  { SBMLError::InvalidIdSyntax, SBMLError::SBML, SBMLError::Error,
    "The syntax of 'id' attribute values must conform to the syntax of the SBML "
    "type 'SId'. (References: L2V2 Sections 3.1.7.)" },

  // 10401
  { SBMLError::MissingAnnotationNamespace, SBMLError::SBML, SBMLError::Error,
    "Every top-level element within an annotation element must have a "
    "namespace declared. (References: L2V2 Section 3.3.3.)" },

  // 10402
  { SBMLError::DuplicateAnnotationNamespaces, SBMLError::SBML, SBMLError::Error,
    "There cannot be more than one top-level element using a given namespace "
    "inside a given annotation element. (References: L2V2 Section 3.3.3.)" },

  // 10403
  { SBMLError::SBMLNamespaceInAnnotation, SBMLError::SBML, SBMLError::Error,
    "Top-level elements within an annotation element cannot use any SBML "
    "namespace, whether explicitly (by declaring the namespace to be one of "
    "the URIs, or implicitly (by failing to declare any namespace). "
    "(References: L2V2 Section 3.3.3.)" },

  // 10801
  { SBMLError::NotesNotInXHTMLNamespace, SBMLError::SBML, SBMLError::Error,
    "The contents of the notes element must be explicitly placed in the XHTML "
    "XML namespace. (References: L2V3 Section 3.2.3.)" },

  // 10802
  { SBMLError::NotesContainsXMLDecl, SBMLError::SBML, SBMLError::Error,
    "The contents of the notes element must not contain an XML declaration "
    "(i.e., a string of the form \"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\" "
    "or similar). (References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 10803
  { SBMLError::NotesContainsDOCTYPE, SBMLError::SBML, SBMLError::Error,
    "The contents of the notes element must not contain an XML DOCTYPE "
    "declaration (i.e., a string beginning with the characters \"<!DOCTYPE\". "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },

  // 10804
  { SBMLError::InvalidNotesContent, SBMLError::SBML, SBMLError::Error,
    "The XHTML content inside a notes element can only take one of the "
    "following general forms: (1) a complete XHTML document beginning with "
    "the element <html> and ending with </html>; (2) the \"body\" portion of "
    "a document beginning with the element <body> and ending with </body>; or "
    "(3) XHTML content that is permitted within a <body> ... </body> elements. "
    "(References: L2V2 Section 3.3.2; L2V3 Section 3.2.3.)" },






  // 21102
  { SBMLError::IncorrectOrderInReaction, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within Reaction must be the following: "
    "listOfReactants (optional), listOfProducts (optional), "
    "listOfModifiers (optional), kineticLaw. (References: L2V2 Section 4.13.)" },

  // 21103
  { SBMLError::EmptyListInReaction, SBMLError::SBML, SBMLError::Error,
    "The following containers are all optional in a <reaction>, but if any "
    "present is, it must not be empty: 'listOfReactants', 'listOfProducts', "
    "'listOfModifiers', 'kineticLaw'. (References: L2V2 Section 4.13.)" },

  // 21104
  { SBMLError::InvalidReactantsProductsList, SBMLError::SBML, SBMLError::Error,
    "The list of reactants ('listOfReactants') and list of products "
    "('listOfProducts') in a <reaction> can only contain 'speciesReference' "
    "elements. (References: L2V1 Section 4.9; L2V2 Section 4.13.)" },

  // 21105
  { SBMLError::InvalidModifiersList, SBMLError::SBML, SBMLError::Error,
    "The list of modifiers ('listOfModifiers') in a <reaction> can only "
    "contain 'modifierSpeciesReference' elements. (References: L2V1 Section "
    "4.9; L2V2 Section 4.13.)" },

  // 21122
  { SBMLError::IncorrectOrderInKineticLaw, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within KineticLaw must be the following: math, "
    "listOfParameters. The listOfParameters is optional, but if present, must "
    "follow math. (References: L2V2 Section 4.13.9.)." },

  // 21123
  { SBMLError::EmptyListInKineticLaw, SBMLError::SBML, SBMLError::Error,
    "If present, the 'listOfParameters' in a <kineticLaw> must not be an "
    "empty list. (References: L2V2 Section 4.13.)" },

  // 21205
  { SBMLError::IncorrectOrderInEvent, SBMLError::SBML, SBMLError::Error,
    "The order of subelements within Event must be the following: trigger, "
    "delay, listOfEventAssignments. The delay element is optional, but if "
    "present, must follow trigger. (References: L2V2 Section 4.14.)" },
};





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

	        newMsg << "This fails to satisfy SBML validation rule number 10103: " 
            << errorTable[3].message << endl;
          newMsg << errorTable[i].message << endl;
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
	        newMsg << "This fails to satisfy SBML validation rule number " <<
            errorTable[i].code << ": " << errorTable[i].message << endl;
        }
        if (!details.empty())
        {
          newMsg << details << "." << endl;
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
