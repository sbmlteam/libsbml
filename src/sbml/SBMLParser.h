/**
 * \file    SBMLParser.h
 * \brief   Parses an SBMLDocument (used internally by libSBML)
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef SBMLParser_h
#define SBMLParser_h


#include "common/libsbml-config.h"


#ifdef __cplusplus


/* The libSBML Expat interface does not *currently* use SBMLParser. */
#ifndef USE_EXPAT


#include <xercesc/parsers/SAX2XMLReaderImpl.hpp>

#include "SBMLHandler.h"
#include "xml/XMLSchemaValidation.h"


using xercesc::SAX2XMLReaderImpl;
using xercesc::XMLErrorReporter;


class SBMLDocument;

class NotXMLException          { };
class UnknownEncodingException { };
class NotSBMLException         { };
class UnknownSBMLException     { };


/**
 * SBMLParser
 *
 * SBMLParser extends SAX2XMLReaderImpl to redirect low-level error
 * information to SBMLHandler.  This allows SBMLHandler to capture
 * Xerces-C++ error codes.  SBMLParser also enscapulates the logic
 * necessary to switch XML Schemas depending on the SBML Level/Version
 * combination.
 */
class SBMLParser : public SAX2XMLReaderImpl
{
public:

  /**
   * Creates a new SBMLParser.  Call parse() to begin parsing content which
   * in turn will populate SBMLDocument.  Content is treated either as a
   * filename containing XML or an XML string, depending on isFile.
   */
  SBMLParser (SBMLDocument* d, const char* content, bool isFile = true);

  virtual ~SBMLParser ();


  /**
   * Called to report errors from the scanner or validator.
   *
   * This method is called back on by the scanner or validator (or any
   * other internal parser component which might need to report an error in
   * the future.) It contains all the information that the client code
   * might need to report or log the error.
   */
  virtual
  void error (  const unsigned int                code
              , const XMLCh* const                domain
              , const XMLErrorReporter::ErrTypes  severity
              , const XMLCh* const                message
              , const XMLCh* const                sysId
              , const XMLCh* const                pubId
              , const XMLSSize_t                  line
              , const XMLSSize_t                  col );

  /**
   * Same as above, but with reasonable defaults for some parameters.
   */
  void error (  const unsigned int                code
              , const XMLErrorReporter::ErrTypes  severity
              , const char*                       message
              , const unsigned int                line = 0
              , const unsigned int                col  = 0 );


  /**
   * Starts the SBML parse.
   */
  void parse ();

  /**
   * Sets the schema validation level used by this SBMLParser.
   *
   * The levels are:
   *
   *   XML_SCHEMA_VALIDATION_NONE (0) turns schema validation off.
   *
   *   XML_SCHEMA_VALIDATION_BASIC (1) validates an XML instance document
   *   against an XML Schema.  Those who wish to perform schema checking on
   *   SBML documents should use this option.
   *
   *   XML_SCHEMA_VALIDATION_FULL (2) validates both the instance document
   *   itself *and* the XML Schema document.  The XML Schema document is
   *   checked for violation of particle unique attribution constraints and
   *   particle derivation restrictions, which is both time-consuming and
   *   memory intensive.
   */
  void setSchemaValidation (XMLSchemaValidation_t level);

  /**
   * This method is used to report the XML decl scanned by the parser.
   * Refer to the XML specification to see the meaning of parameters.
   */
  virtual
  void XMLDecl (  const XMLCh* const version
                , const XMLCh* const encoding
                , const XMLCh* const standalone
                , const XMLCh* const autoEncoding );


protected:

  SBMLDocument* mSBML;
  InputSource*  mInput;
  SBMLHandler*  mHandler;

  XMLSchemaValidation_t mValidationLevel;
  bool                  mSkipEncodingCheck;
};


#endif  /* !USE_EXPAT   */
#endif  /* __cplusplus  */
#endif  /* SBMLParser_h */
