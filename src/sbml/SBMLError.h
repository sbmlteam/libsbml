/**
 * @file    SBMLError.h
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

#ifndef SBMLError_h
#define SBMLError_h

#include <sbml/common/extern.h>
#include <sbml/xml/XMLError.h>


class LIBSBML_EXTERN SBMLError : public XMLError
{
public:

  /**
   * Codes for all SBML-level errors and warnings.  These are distinguished
   * from the XML layer (LIBLAX) error codes by being numbered > 10000, while
   * the XML layer's codes are < 9999.
   */
  enum SBMLCode
  {
    UnknownError                     =     0
  , NotUTF8                          = 10101
  , UnrecognizedElement              = 10102
  , NotSchemaConformant              = 10103
  , InvalidMathElement               = 10201
  , DisallowedMathMLSymbol           = 10202
  , DisallowedMathMLEncodingUse      = 10203
  , DisallowedDefinitionURLUse       = 10204
  , BadCsymbolDefinitionURLValue     = 10205
  , DisallowedMathTypeAttributeUse   = 10206
  , DisallowedMathTypeAttributeValue = 10207
  , InvalidSBOTermSyntax             = 10308
  , InvalidMetaidSyntax              = 10309
  , InvalidIdSyntax                  = 10310
  , MissingAnnotationNamespace       = 10401
  , DuplicateAnnotationNamespaces    = 10402
  , SBMLNamespaceInAnnotation        = 10403
  , NotesNotInXHTMLNamespace         = 10801
  , NotesContainsXMLDecl             = 10802
  , NotesContainsDOCTYPE             = 10803
  , InvalidNotesContent              = 10804
  , InvalidNamespaceOnSBML           = 20101
  , MissingOrInconsistentLevel       = 20102
  , MissingOrInconsistentVersion     = 20103
  , AnnotationNotesNotAllowedLevel1  = 20141
  , MissingModel                     = 20201
  , IncorrectOrderInModel            = 20202
  , EmptyListElement                 = 20203
  , EmptyListOfUnits                 = 20409
  , IncorrectOrderInConstraint       = 21002
  , ConstraintNotInXHTMLNamespace    = 21003
  , ConstraintContainsXMLDecl        = 21004
  , ConstraintContainsDOCTYPE        = 21005
  , InvalidConstraintContent         = 21006
  , IncorrectOrderInReaction         = 21102
  , EmptyListInReaction              = 21103
  , InvalidReactantsProductsList     = 21104
  , InvalidModifiersList             = 21105
  , IncorrectOrderInKineticLaw       = 21122
  , EmptyListInKineticLaw            = 21123
  , IncorrectOrderInEvent            = 21205
    // Bounds
  , SBMLCodesUpperBound              = 99999
  };


  enum SBMLCategory
  {
    SBML                      = 3
  , SBMLL1Compatibility       = 4
  , SBMLL2v1Compatibility     = 5 
  , SBMLL2v2Compatibility     = 6
  , SBMLConsistency           = 7
  , SBMLConsistencyIdentifier = 8
  , SBMLConsistencyUnits      = 9
  , SBMLConsistencyMathML     = 10
  , SBMLConsistencySBO        = 11
  };


  enum SBMLSeverity
  {
    Info    = XMLError::Info
  , Warning = XMLError::Warning
  , Error   = XMLError::Error
  , Fatal   = XMLError::Fatal
  };


  SBMLError
  (
     const unsigned int  errorId      = 0
   , const std::string& details       = ""
   , const unsigned int line          = 0
   , const unsigned int column        = 0
   , const SBMLSeverity severity      = Error
   , const SBMLCategory category      = SBML
   , const unsigned int fromValidator = 0
  );

};


#endif /* SBMLError_h */
