/**
 * @file    SyntaxChecker.h
 * @brief   Syntax checking functions
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->
 *
 * @class SyntaxChecker
 * @brief Methods for checking syntax of ids, metaids and units
 *
 */

#ifndef SyntaxChecker_h
#define SyntaxChecker_h


#include <sbml/common/extern.h>
#include <sbml/SBase.h>
#include <sbml/util/util.h>

#ifdef __cplusplus


#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

class LIBSBML_EXTERN SyntaxChecker
{
public:

  /**
   * Predicate returning @c true or @c false depending on whether the
   * argument string conforms to the SBML type SId. 
   *
   * @param sid string to be checked for conformance
   *
   * @return @c true if the string conforms to type SId, @c false otherwise.
   *
   * @note The literal representation of SBML type SId consists of strings 
   * of characters restricted to:
   *
   *  - letter ::= 'a'..'z','A'..'Z'
   *  - digit  ::= '0'..'9'
   *  - idChar ::= letter | digit | '_'
   *  - SId    ::= ( letter | '_' ) idChar*
   */  
  static bool isValidSBMLSId(std::string sid);

  
  /**
   * Predicate returning @c true or @c false depending on whether the
   * argument string conforms to the XML 1.0 type ID. 
   *
   * @param id string to be checked for conformance
   *
   * @return @c true if the string conforms to type ID, @c false otherwise.
   *
   * @note The literal representation of XML 1.0 type ID consists of strings 
   * of characters restricted to:
   *
   *  - NCNameChar ::= letter | digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
   *  - ID ::= ( letter | '_' | ':' ) NCNameChar*
   */  
  static bool isValidXMLID(std::string id);


  /**
   * Predicate returning @c true or @c false depending on whether the
   * argument string conforms to the SBML type UnitSId. 
   *
   * @param units string to be checked for conformance
   *
   * @return @c true if the string conforms to type UnitSId, 
   * @c false otherwise.
   *
   * @note The literal representation of SBML type UniySId consists of strings 
   * of characters restricted to:
   *
   *  - letter ::= 'a'..'z','A'..'Z'
   *  - digit  ::= '0'..'9'
   *  - idChar ::= letter | digit | '_'
   *  - UnitSId    ::= ( letter | '_' ) idChar*
   */
   static bool isValidUnitSId(std::string units);


  /**
   * Predicate returning @c true or @c false depending on whether the
   * argument XMLNode represents XHTML that conforms to the 
   * requirements of the SBML specification.
   *
   * @param xhtml the XMLNode to be checked for conformance.
   * @param sbmlns the SBMLNamespaces associated with the object.
   *
   * @return @c true if the XMLNode conforms, @c false otherwise.
   *
   * @note the optional SBMLNamespaces argument can be used to
   * check for the declaration of the XHTML namespace at the top-level
   * within an SBMLDocument.
   */
  static bool hasExpectedXHTMLSyntax(const XMLNode * xhtml, 
                                     SBMLNamespaces * sbmlns = NULL); 


  /** @cond doxygen-libsbml-internal */

  /*
   * return true if element is an allowed xhtml element
   */
  static bool isAllowedElement(const XMLNode &node);

  /** @endcond doxygen-libsbml-internal */

  /** @cond doxygen-libsbml-internal */

  /*
   * return true has the xhtml ns correctly declared
   */
  static bool hasDeclaredNS(const XMLNode &node, const XMLNamespaces* toplevelNS);

  /** @endcond doxygen-libsbml-internal */

  /** @cond doxygen-libsbml-internal */

  /*
   * return true if the html tag contains both a title
   * and a body tag 
   */
  static bool isCorrectHTMLNode(const XMLNode &node);

  /** @endcond doxygen-libsbml-internal */

protected:  
  /** @cond doxygen-libsbml-internal */
  /**
   * Checks if a character is part of the Unicode Letter set.
   * @return true if the character is a part of the set, false otherwise.
   */
  static bool isUnicodeLetter(std::string::iterator, unsigned int);


  /**
   * Checks if a character is part of the Unicode Digit set.
   * @return true if the character is a part of the set, false otherwise.
   */
  static bool isUnicodeDigit(std::string::iterator, unsigned int);


  /**
   * Checks if a character is part of the Unicode CombiningChar set.
   * @return true if the character is a part of the set, false otherwise.
   */
  static bool isCombiningChar(std::string::iterator, unsigned int);


  /**
   * Checks if a character is part of the Unicode Extender set.
   * @return true if the character is a part of the set, false otherwise.
   */
  static bool isExtender(std::string::iterator, unsigned int);

  /** @endcond doxygen-libsbml-internal */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


#ifndef SWIG

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/
LIBSBML_EXTERN
int
SyntaxChecker_isValidSBMLSId(const char * sid);


LIBSBML_EXTERN
int
SyntaxChecker_isValidXMLID(const char * id);


LIBSBML_EXTERN
int
SyntaxChecker_isValidUnitSId(const char * units);


LIBSBML_EXTERN
int
SyntaxChecker_hasExpectedXHTMLSyntax(XMLNode_t * node, 
                                     SBMLNamespaces_t * sbmlns);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* !SWIG */
#endif  /* SyntaxChecker_h */
