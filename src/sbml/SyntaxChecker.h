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
 * @brief Methods for checking syntax of SBML identifiers and other strings.
 * 
 * @htmlinclude libsbml-not-sbml-warning.html
 * 
 * This utility class provides static methods for checking the syntax of
 * identifiers and other text used in an SBML model.  The methods allow
 * callers to verify that strings such as SBML identifiers and XHTML notes
 * text conform to the SBML specifications.
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
   * Returns true @c true or @c false depending on whether the argument
   * string conforms to the syntax of SBML identifiers.
   *
   * In SBML, identifiers that are the values of <code>id</code> attributes
   * on objects must conform to a data type called <code>SId</code> in the
   * SBML specifications.  LibSBML does not provide an explicit
   * <code>SId</code> data type; instead, it tests for identifier validity
   * at various times, such as when reading in models from files and data
   * streams.  To enable calling programs to test that the identifiers they
   * generate also conform to the SBML identifier syntax, libSBML provides
   * this method.
   *
   * @param sid string to be checked for conformance to SBML identifier
   * syntax.
   *
   * @return @c true if the string conforms to type SBML data type
   * <code>SId</code>, @c false otherwise.
   *
   * @note @htmlinclude id-syntax.html
   *
   * @see isValidUnitSId(std::string sid)
   * @see isValidXMLID(std::string sid)
   */  
  static bool isValidSBMLSId(std::string sid);

  
  /**
   * Returns @c true or @c false depending on whether the argument string
   * conforms to the XML data type <code>ID</code>.
   *
   * In SBML, identifiers that are the values of <code>metaid</code>
   * attributes on objects must conform to the <a target="_blank" 
   * href="http://www.w3.org/TR/REC-xml/#id">XML ID</a> data type.  LibSBML
   * does not provide an explicit XML <code>ID</code> data type; instead,
   * it tests for identifier validity at various times, such as when
   * reading in models from files and data streams.  To enable calling
   * programs to test that the identifiers they generate also conform to
   * the SBML identifier syntax, libSBML provides this method.
   *
   * @param id string to be checked for conformance to the syntax of
   * <a target="_blank" href="http://www.w3.org/TR/REC-xml/#id">XML ID</a>.
   *
   * @return @c true if the string is a syntactically-valid value for the
   * XML type <a target="_blank"
   * href="http://www.w3.org/TR/REC-xml/#id">ID</a>, @c false otherwise.
   *
   * @note @htmlinclude xmlid-syntax.html
   * 
   * @see isValidSBMLSId(std::string sid)
   * @see isValidUnitSId(std::string sid)
   */  
  static bool isValidXMLID(std::string id);


  /**
   * Returns @c true or @c false depending on whether the argument string
   * conforms to the syntax of SBML unit identifiers.
   *
   * In SBML, the identifiers of units (of both the predefined units and
   * user-defined units) must conform to a data type called
   * <code>UnitSId</code> in the SBML specifications.  LibSBML does not
   * provide an explicit <code>UnitSId</code> data type; instead, it tests
   * for identifier validity at various times, such as when reading in
   * models from files and data streams.  To enable calling programs to
   * test that the unit identifiers they generate also conform to the SBML
   * identifier syntax, libSBML provides this method.
   *
   * @param units string to be checked for conformance to SBML unit
   * identifier syntax.
   *
   * @return @c true if the string conforms to type SBML data type
   * <code>UnitSId</code>, @c false otherwise.
   *
   * @note @htmlinclude unitid-syntax.html
   *
   * @see isValidSBMLSId(std::string sid)
   * @see isValidXMLID(std::string sid)
   */
   static bool isValidUnitSId(std::string units);


  /**
   * Returns @c true or @c false depending on whether the given XMLNode
   * object contains valid XHTML content.
   *
   * In SBML, the content of the "notes" subelement available on SBase, as
   * well as the "message" subelement available on Constraint, must conform
   * to <a target="_blank"
   * href="http://www.w3.org/TR/xhtml1/">XHTML&nbsp;1.0</a> (which is
   * simply an XML-ized version of HTML).  However, the content cannot be
   * @em entirely free-form; it must satisfy certain requirements defined in
   * the <a target="_blank"
   * href="http://sbml.org/Documents/Specifications">SBML
   * specifications</a> for specific SBML Levels.  This method implements a
   * verification process that lets callers check whether the content of a
   * given XMLNode object conforms to the SBML requirements for "notes" and
   * "message" structure.
   *
   * An aspect of XHTML validity is that the content is declared to be in
   * the XML namespace for XHTML&nbsp;1.0.  There is more than one way in
   * which this can be done in XML.  In particular, a model might not
   * contain the declaration within the "notes" or "message" subelement
   * itself, but might instead place the declaration on an enclosing
   * element and use an XML namespace prefix within the "notes" element to
   * refer to it.  In other words, the following is valid:
   * @verbatim
<sbml xmlns="http://www.sbml.org/sbml/level2/version3" level="2" version="3"
      xmlns:xhtml="http://www.w3.org/1999/xhtml">
  <model>
    <notes>
      <xhtml:body>
        <xhtml:center><xhtml:h2>A Simple Mitotic Oscillator</xhtml:h2></xhtml:center>
        <xhtml:p>A minimal cascade model for the mitotic oscillator.</xhtml:p>
      </xhtml:body>
    </notes>
  ... rest of model ...
</sbml>
@endverbatim
   * Contrast the above with the following, self-contained version, which
   * places the XML namespace declaration within the <code>&lt;notes&gt;</code>
   * element itself:
   * @verbatim
<sbml xmlns="http://www.sbml.org/sbml/level2/version3" level="2" version="3">
  <model>
    <notes>
      <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
          <title/>
        </head>
        <body>
          <center><h2>A Simple Mitotic Oscillator</h2></center>
          <p>A minimal cascade model for the mitotic oscillator.</p>
        </body>
      </html>
    </notes>
  ... rest of model ...
</sbml>
@endverbatim
   *
   * Both of the above are valid XML.  The purpose of the @p sbmlns
   * argument to this method is to allow callers to check the validity of
   * "notes" and "message" subelements whose XML namespace declarations
   * have been put elsewhere in the manner illustrated above.  Callers can
   * can pass in the SBMLNamespaces object of a higher-level model
   * component if the XMLNode object does not itself have the XML namespace
   * declaration for XHTML&nbsp;1.0.
   * 
   * @param xhtml the XMLNode to be checked for conformance.
   * @param sbmlns the SBMLNamespaces associated with the object.
   *
   * @return @c true if the XMLNode content conforms, @c false otherwise.
   *
   * @if notcpp @docnote @htmlinclude libsbml-warn-default-args-in-docs.html @endif
   */
  static bool hasExpectedXHTMLSyntax(const XMLNode * xhtml, 
                                     SBMLNamespaces * sbmlns = NULL); 


  /** @cond doxygen-libsbml-internal */

  /*
   * return true if element is an allowed xhtml element
   */
  static bool isAllowedElement(const XMLNode &node);

  /** @endcond */

  /** @cond doxygen-libsbml-internal */

  /*
   * return true has the xhtml ns correctly declared
   */
  static bool hasDeclaredNS(const XMLNode &node, const XMLNamespaces* toplevelNS);

  /** @endcond */

  /** @cond doxygen-libsbml-internal */

  /*
   * return true if the html tag contains both a title
   * and a body tag 
   */
  static bool isCorrectHTMLNode(const XMLNode &node);

  /** @endcond */

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

  /** @endcond */
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
