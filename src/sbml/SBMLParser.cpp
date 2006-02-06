/**
 * \file    SBMLParser.cpp
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


#include <sstream>
#include "common/common.h"


/* The libSBML Expat interface does not *currently* use SBMLParser. */
#ifndef USE_EXPAT


#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/util/XMLUniDefs.hpp>

#include "SBMLDocument.h"
#include "SBMLReader.h"
#include "SBMLParser.h"


using namespace std;
using namespace xercesc;


static const unsigned int XERCESC_FILE_NOT_FOUND   =  74;
static const unsigned int XERCESC_NOT_XML          = 186;
static const unsigned int XERCESC_UNKNOWN_ENCODING = 292;


/**
 * "libSBML"
 */
static const XMLCh LIBSBML[] =
{
  chLatin_l, chLatin_i, chLatin_b, chLatin_S, chLatin_B, chLatin_M, chLatin_L,
  chNull
};


/**
 * "UTF-8"
 */
static const XMLCh UTF_8[] =
{
  chLatin_U, chLatin_T, chLatin_F, chDash, chDigit_8, chNull
};


/**
 * "http://www.sbml.org/sbml/level1 sbml-l1v1.xsd"
 */
static const XMLCh SBML_SCHEMA_LOCATION_L1V1[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chForwardSlash,
  chLatin_l, chLatin_e, chLatin_v, chLatin_e, chLatin_l, chDigit_1,

  chSpace,

  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chDash,
  chLatin_l, chDigit_1, chLatin_v, chDigit_1,
  chPeriod,
  chLatin_x, chLatin_s, chLatin_d,

  chNull
};


/**
 * "http://www.sbml.org/sbml/level1 sbml-l1v2.xsd"
 */
static const XMLCh SBML_SCHEMA_LOCATION_L1V2[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chForwardSlash,
  chLatin_l, chLatin_e, chLatin_v, chLatin_e, chLatin_l, chDigit_1,

  chSpace,

  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chDash,
  chLatin_l, chDigit_1, chLatin_v, chDigit_2,
  chPeriod,
  chLatin_x, chLatin_s, chLatin_d,

  chNull
};


/**
 * "http://www.sbml.org/sbml/level2 sbml-l2v1.xsd"
 */
static const XMLCh SBML_SCHEMA_LOCATION_L2V1[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chForwardSlash,
  chLatin_l, chLatin_e, chLatin_v, chLatin_e, chLatin_l, chDigit_2,

  chSpace,

  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chDash,
  chLatin_l, chDigit_2, chLatin_v, chDigit_1,
  chPeriod,
  chLatin_x, chLatin_s, chLatin_d,

  chNull
};


/**
 * Creates a new SBMLParser.  Call parse() to begin parsing content which
 * in turn will populate SBMLDocument.  Content is treated either as a
 * filename containing XML or an XML string, depending on isFile.
 */
SBMLParser::SBMLParser (SBMLDocument* d, const char* content, bool isFile) :
    SAX2XMLReaderImpl (                            )
  , mSBML             ( d                          )
  , mHandler          ( new SBMLHandler(d)         )
  , mSkipEncodingCheck( false                      )
  , mValidationLevel  ( XML_SCHEMA_VALIDATION_NONE )
{
  if (isFile)
  {
    XMLCh* filename = XMLString::transcode(content);
    mInput          = new LocalFileInputSource(filename);
    XMLString::release(&filename);
  }
  else
  {
    const XMLByte*     bytes = (const XMLByte*) content;
    const unsigned int size  = strlen(content);
    mInput = new MemBufInputSource(bytes, size, "FromString", false);
  }

  setContentHandler(mHandler);
  setErrorHandler  (mHandler);
  setEntityResolver(mHandler);

  setFeature( XMLUni::fgSAX2CoreNameSpaces       , true );
  setFeature( XMLUni::fgSAX2CoreNameSpacePrefixes, true );
}


/**
 * Destroys this SBMLParser.
 */
SBMLParser::~SBMLParser ()
{
  delete mInput;
}


/**
 * Called to report errors from the scanner or validator.
 *
 * This method is called back on by the scanner or validator (or any other
 * internal parser component which might need to report an error in the
 * future.) It contains all the information that the client code might need
 * to report or log the error.
 */
void
SBMLParser::error (  const unsigned int                code
                   , const XMLCh* const                domain
                   , const XMLErrorReporter::ErrTypes  severity
                   , const XMLCh* const                message
                   , const XMLCh* const                sysId
                   , const XMLCh* const                pubId
                   , const XMLSSize_t                  line
                   , const XMLSSize_t                  col )
{
  if (code == XERCESC_NOT_XML)
  {
    throw NotXMLException();
  }
  else if (code == XERCESC_UNKNOWN_ENCODING)
  {
    throw UnknownEncodingException();
  }
  else
  {
    mHandler->error(code, domain, severity, message, sysId, pubId, line, col);
  }
}


/**
 * Same as above, but with reasonable defaults for some parameters.
 */
void
SBMLParser::error (  const unsigned int                code
                   , const XMLErrorReporter::ErrTypes  severity
                   , const char*                       message
                   , const unsigned int                line
                   , const unsigned int                col )
{
  XMLCh* msg = XMLString::transcode(message);

  error(code, LIBSBML, severity, msg, mInput->getSystemId(),
        mInput->getPublicId(), line, col);

  XMLString::release(&msg);
}


/**
 * Starts the SBML parse.
 */
void
SBMLParser::parse ()
{
  XMLPScanToken token;
  const XMLCh*  location;
 
  if (parseFirst(*mInput, token)) parseNext(token);

  parseReset(token);
  mSkipEncodingCheck = true;

  if (mHandler->sawSBML() == false)
  {
    throw NotSBMLException();
  }
  if (mSBML->getLevel() == 1 && mSBML->getVersion() == 1)
  {
    location = SBML_SCHEMA_LOCATION_L1V1;
  }
  else if (mSBML->getLevel() == 1 && mSBML->getVersion() == 2)
  {
    location = SBML_SCHEMA_LOCATION_L1V2;
  }
  else if (mSBML->getLevel() == 2 && mSBML->getVersion() == 1)
  {
    location = SBML_SCHEMA_LOCATION_L2V1;
  }
  else
  {
    throw UnknownSBMLException();
  }

  if (mValidationLevel == XML_SCHEMA_VALIDATION_NONE)
  {
    setFeature( XMLUni::fgXercesSchema            , false );
    setFeature( XMLUni::fgXercesSchemaFullChecking, false );
  }
  else
  {
    bool   doFull = (mValidationLevel == XML_SCHEMA_VALIDATION_FULL);
    XMLCh* value  = const_cast<XMLCh*>( location );

    setFeature ( XMLUni::fgSAX2CoreValidation                , true   );
    setFeature ( XMLUni::fgXercesSchema                      , true   );
    setFeature ( XMLUni::fgXercesSchemaFullChecking          , doFull );
    setProperty( XMLUni::fgXercesSchemaExternalSchemaLocation, value  );
  }

  SAX2XMLReaderImpl::parse(*mInput);
}


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
void
SBMLParser::setSchemaValidation (XMLSchemaValidation_t level)
{
  mValidationLevel = level;
}


/**
 * This method is used to report the XML decl scanned by the parser.
 * Refer to the XML specification to see the meaning of parameters.
 */
void
SBMLParser::XMLDecl (  const XMLCh* const version
                     , const XMLCh* const encoding
                     , const XMLCh* const standalone
                     , const XMLCh* const autoEncoding )
{
  if (mSkipEncodingCheck) return;

  XMLErrorReporter::ErrTypes severity = XMLErrorReporter::ErrType_Error;
  const XMLCh* toCheck = encoding;


  if ( !(toCheck && *toCheck) )
  {
    SBMLReadError_t code    = SBML_READ_ERROR_NO_ENCODING;
    const char*     message =
      "No character encoding was specified.  Will attempt to auto-detect.";

    error(code, severity, message);

    toCheck = autoEncoding;
  }

  if (toCheck && *toCheck)
  {
    if (XMLString::equals(toCheck, UTF_8) == false)
    {
      ostringstream message;

      SBMLReadError_t code     = SBML_READ_ERROR_NOT_UTF_8;
      char*           current  = XMLString::transcode(encoding);


      message << "SBML documents must be encoded as UTF-8.  The current "
              << "encoding is: '" << current << "'.";

      XMLString::release(&current);

      error(code, severity, message.str().c_str());
    }
  }
}


#endif  /* !USE_EXPAT */
