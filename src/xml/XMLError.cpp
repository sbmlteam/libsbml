/**
 * @file    XMLError.cpp
 * @brief   Represents errors (and messages) encountered during an XML parse
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <iostream>
#include <iomanip>
#include <sstream>

#include <sbml/xml/XMLError.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


static const xmlErrorTableEntry errorTable[] =
{
  // 0
  { XMLError::UnknownError, XMLError::Internal, XMLError::Fatal,
    "Unrecognized error encountered internally" },


  // System diagnostics:

  // 0001
  { XMLError::OutOfMemory, XMLError::System, XMLError::Fatal,
    "Out of memory" },

  // 0002
  { XMLError::FileUnreadable, XMLError::System, XMLError::Error,
    "File unreadable" },

  // 0003
  { XMLError::FileUnwritable, XMLError::System, XMLError::Error,
    "File unwritable" },

  // 0004
  { XMLError::FileOperationError, XMLError::System, XMLError::Error,
    "Error encountered while attempting file operation" },

  // 0005
  { XMLError::NetworkAccessError, XMLError::System, XMLError::Error,
    "Network access error" },


  // Internal diagnostics:

  // 0101
  { XMLError::InternalParserError, XMLError::Internal, XMLError::Fatal,
    "Internal XML parser state error" },

  // 0102
  { XMLError::UnrecognizedParserCode, XMLError::Internal, XMLError::Fatal,
    "XML parser returned an unrecognized error code" },

  // 0102
  { XMLError::TranscoderError, XMLError::Internal, XMLError::Fatal,
    "Character transcoder error" },


  // Diagnostics about XML content:

  // 1001
  { XMLError::MissingXMLDecl, XMLError::XML, XMLError::Error,
    "Missing XML declaration at beginning of XML input" },

  // 1002
  { XMLError::MissingXMLEncoding, XMLError::XML, XMLError::Error,
    "Missing encoding attribute in XML declaration" },

  // 1003
  { XMLError::BadXMLDecl, XMLError::XML, XMLError::Error,
    "Invalid or unrecognized XML declaration or XML encoding" },

  // 1004
  { XMLError::BadDOCTYPE, XMLError::XML, XMLError::Error,
    "Invalid, malformed or unrecognized XML DOCTYPE declaration" },

  // 1005
  { XMLError::InvalidChar, XMLError::XML, XMLError::Error,
    "Invalid character in XML content" },

  // 1006
  { XMLError::NotWellFormed, XMLError::XML, XMLError::Error,
    "Badly formed XML" },

  // 1007
  { XMLError::UnclosedToken, XMLError::XML, XMLError::Error,
    "Unclosed token" },

  // 1008
  { XMLError::InvalidConstruct, XMLError::XML, XMLError::Error,
    "XML construct is invalid or not permitted" },

  // 1009
  { XMLError::TagMismatch, XMLError::XML, XMLError::Error,
    "Element tag mismatch or missing tag" },

  // 1010
  { XMLError::DuplicateAttribute, XMLError::XML, XMLError::Error,
    "Duplicate attribute" },

  // 1011
  { XMLError::UndefinedEntity, XMLError::XML, XMLError::Error,
    "Undefined XML entity" },

  // 1012
  { XMLError::BadProcessingInstruction, XMLError::XML, XMLError::Error,
    "Invalid, malformed or unrecognized XML processing instruction" },

  // 1013
  { XMLError::BadPrefix, XMLError::XML, XMLError::Error,
    "Invalid or undefined XML Namespace prefix" },

  // 1014
  { XMLError::BadPrefixValue, XMLError::XML, XMLError::Error,
    "Invalid XML Namespace prefix value" },

  // 1015
  { XMLError::MissingRequiredAttribute, XMLError::XML, XMLError::Error,
    "Required attribute is missing" },

  // 1016
  { XMLError::AttributeTypeMismatch, XMLError::XML, XMLError::Error,
    "Data type mismatch for attribute value" },

  // 1017
  { XMLError::BadUTF8Content, XMLError::XML, XMLError::Error,
    "Invalid UTF8 content" },

  // 1018
  { XMLError::MissingAttributeValue, XMLError::XML, XMLError::Error,
    "Missing or improperly formed attribute value" },

  // 1019
  { XMLError::BadAttributeValue, XMLError::XML, XMLError::Error,
    "Invalid or unrecognizable attribute value" },

  // 1020
  { XMLError::BadAttribute, XMLError::XML, XMLError::Error,
    "Invalid, unrecognized or malformed attribute" },

  // 1021
  { XMLError::UnrecognizedElement, XMLError::XML, XMLError::Error,
    "Element either not recognized or not permitted" },

  // 1022
  { XMLError::BadXMLComment, XMLError::XML, XMLError::Error,
    "Badly formed XML comment" },

  // 1023
  { XMLError::BadXMLDeclLocation, XMLError::XML, XMLError::Error,
    "XML declaration not permitted in this location" },

  // 1024
  { XMLError::UnexpectedEOF, XMLError::XML, XMLError::Error,
    "Reached end of input unexpectedly" },

  // 1025
  { XMLError::BadXMLIDValue, XMLError::XML, XMLError::Error,
    "Value is invalid for XML ID, or has already been used" },

  // 1026
  { XMLError::BadXMLIDRef, XMLError::XML, XMLError::Error,
    "XML ID value was never declared" },

  // 1027
  { XMLError::UninterpretableContent, XMLError::XML, XMLError::Error,
    "Unable to interpret content" },

  // 1028
  { XMLError::BadDocumentStructure, XMLError::XML, XMLError::Error,
    "Bad XML document structure" },

  // 1029
  { XMLError::InvalidAfterContent, XMLError::XML, XMLError::Error,
    "Encountered invalid content after expected content" },

  // 1031
  { XMLError::ExpectedQuotedString, XMLError::XML, XMLError::Error,
    "Expected to find a quoted string" },

  // 1032
  { XMLError::EmptyValueNotPermitted, XMLError::XML, XMLError::Error,
    "An empty value is not permitted in this context" },

  // 1033
  { XMLError::BadNumber, XMLError::XML, XMLError::Error,
    "Invalid or unrecognized number" },

  // 1034
  { XMLError::BadColon, XMLError::XML, XMLError::Error,
    "Colon characters are invalid in this context" },

  // 1035
  { XMLError::MissingElements, XMLError::XML, XMLError::Error,
    "One or more expected elements are missing" },

  // 1036
  { XMLError::EmptyXML, XMLError::XML, XMLError::Error,
    "Main XML content is empty" },

};


/*
 * XMLErrorLog will check if line & column = 0 and attempt to fill in
 * the line and column by consulting the parser.  This constructor
 * purposefully doesn't do that.
 */
XMLError::XMLError (  const int errorId
                    , const std::string& details
                    , const unsigned int line
                    , const unsigned int column
                    , const unsigned int severity
                    , const unsigned int category ) :
    mErrorId( errorId )
  , mLine   ( line    )
  , mColumn ( column  )
{
  // Check if the given id is one we have in our table of error codes.  If
  // it is, fill in the fields of the error object with the appropriate
  // content.  If it's not in the table, take the content as-is.

  if ( errorId >= 0 && errorId < XMLError::ErrorCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);    

    for ( unsigned int i = 0; i < tableSize; i++ )
    {
      if ( errorTable[i].code == errorId )
      {
        mMessage  = errorTable[i].message;

        if ( !details.empty() )
        {
          mMessage.append(": ");
          mMessage.append(details);
        }

        mSeverity = errorTable[i].severity;
        mCategory = errorTable[i].category;
        return;
      }
    }

    // The id is in the range of error numbers that are supposed to be in
    // the XML layer, but it's NOT in our table.  This is an internal error.
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


/**
 * Destroys this XMLError.
 */
XMLError::~XMLError ()
{
}


/**
 * @return the id of this XMLError.
 */
const unsigned int
XMLError::getId () const
{
  return mErrorId;
}


/**
 * @return the message text of this XMLError.
 */
const string&
XMLError::getMessage () const
{
  return mMessage;
}


/**
 * @return the line number where this XMLError ocurred.
 */
unsigned int
XMLError::getLine () const
{
  return mLine;
}


/**
 * @return the column number where this XMLError occurred.
 */
unsigned int
XMLError::getColumn () const
{
  return mColumn;
}


/**
 * @return the severity of this XMLError.  XMLError severity levels
 * correspond to those defined in the XML specification (with the addition
 * of Info for informational messages).
 */
unsigned int
XMLError::getSeverity () const
{
  return mSeverity;
}


/**
 * @return the category of this XMLError.  A category is a string, similiar
 * in spirit to an XML namespace, which can be used to partition errors
 * into distinct groups.  Among other things, this can be used to prevent
 * id conflicts by uniquely identifying an XMLError by both id and
 * category.
 */
unsigned int
XMLError::getCategory () const
{
  return mCategory;
}


/**
 * @return true if this XMLError is for informational purposes only,
 * false otherwise.
 */
bool
XMLError::isInfo () const
{
  return (mSeverity == Info);
}


/**
 * @return true if this XMLError is a warning, false otherwise.
 */
bool
XMLError::isWarning () const
{
  return (mSeverity == Warning);
}


/**
 * @return true if this XMLError is an error, false otherwise.
 */
bool
XMLError::isError () const
{
  return (mSeverity == Error);
}


/**
 * @return true if this XMLError is a fatal error, false otherwise.
 */
bool
XMLError::isFatal () const
{
  return (mSeverity == Fatal);
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError resulted from the operating system.
 *
 * @return @c true or @c false
 */
bool
XMLError::isSystem () const
{
  return (mCategory == System);
}


/**
 * Predicate returning @c true or @c false depending on whether this
 * XMLError resulted from a problem at the raw XML level (e.g., an XML
 * syntax error).
 *
 * @return @c true or @c false
 */
bool
XMLError::isXML () const
{
  return (mCategory == XML);
}


/**
 * Predicate returning @c true or @c false depending on whether this
 * XMLError resulted from an internal program error.
 *
 * @return @c true or @c false
 */
bool
XMLError::isInternal () const
{
  return (mCategory == Internal);
}


/**
 * Sets the line number where this XMLError occurred.
 */
void
XMLError::setLine (unsigned int line)
{
  mLine = line;
}


/**
 * Sets the column number where this XMLError occurred.
 */
void
XMLError::setColumn (unsigned int column)
{
  mColumn = column;
}


/**
 * Given an XMLError::Code, return a copy of the error text.
 * 
 * @return the message text 
 */
const string
XMLError::getStandardMessage (const int code)
{
  string msg;

  if ( code >= 0 && code < XMLError::ErrorCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);    

    for ( unsigned int i = 0; i < tableSize; i++ )
      if ( errorTable[i].code == code )
        msg.append(errorTable[i].message);
  }
  
  return msg;
}


/**
 * Outputs this XMLError to stream in the following format (and followed by
 * a newline):
 *
 *   line: (id) message
 */
ostream& operator<< (ostream& s, const XMLError& error)
{
  s << "line " << error.mLine << ": ("
    << setfill('0') << setw(5) << error.mErrorId
    << ") " << error.mMessage << endl;
  return s;
}


/** @cond doxygen-c-only */


/**
 * Creates a new XMLError to report that something occurred.
 */
LIBLAX_EXTERN
XMLError_t*
XMLError_create (void)
{
  return new(nothrow) XMLError;
}


/**
 * Creates a new XMLError with the identification number
 * and detailed message set.
 *
 * If the identifier is < 10000, it must be one of the predefined XML layer
 * error codes.
 *
 * @param errorId an unsigned int, the identification number of the error.
 * @param message a string, the error message.
 *
 */
LIBLAX_EXTERN
XMLError_t*
XMLError_createWithIdAndMessage (unsigned int errorId, const char * message)
{
  return new(nothrow) XMLError(errorId, message);
}

/**
 * Creates a new XMLError to report that something occurred at the given
 * line and column.  Each XMLError also has an identification number, a
 * category, and a severity level associated with it.
 *
 * @param errorId an unsigned int, the identification number of the error.
 * @param message a string, the error message.
 * @param severity XMLError_Severity, severity of the error.
 * @param category a string, the category to which the error belongs.
 * @param line an unsigned int, the line number at which the error occurs.
 * @param column an unsigned int, the column number at which the error occurs.
 *
 */


//LIBLAX_EXTERN
//XMLError_t*
//XMLError_createWithAll (unsigned int errorId, const char * message, XMLError_Severity severity,
//                        const char * category, unsigned int line, unsigned int column)
//{
//  XMLError::Severity s;
//  switch (severity)
//  {
//  case Info:
//    s = XMLError::Severity::Info;
//    break;
//  case Warning:
//    s = XMLError::Severity::Warning;
//    break;
//  case Error:
//    s = XMLError::Severity::Error;
//    break;
//  case Fatal:
//    s = XMLError::Severity::Fatal;
//    break;
//  default:
//    s = XMLError::Severity::Error;
//    break;
//  }
//  return new(nothrow) XMLError(errorId, message, s, category, line, column);
//}


/**
 * Frees the given XMLError_t structure.
 *
 * @param error the XMLError_t structure to be freed.
 **/
LIBLAX_EXTERN
void
XMLError_free(XMLError_t* error)
{
  delete static_cast<XMLError*>(error);
}

/**
 * Returns the id of this XMLError.
 *
 * @param error the XMLError_t from which to return the id.
 *
 * @return the id of this XMLError.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getId (const XMLError_t *error)
{
  return error->getId();
}


/**
 * Returns the message text of this XMLError.
 *
 * @param error the XMLError_t from which to return the message.
 *
 * @return the message text of this XMLError.
 */
LIBLAX_EXTERN
const char *
XMLError_getMessage (const XMLError_t *error)
{
  const string msg = error->getMessage();


  return msg.empty() ? 0 : msg.c_str();
}


/**
 * Return the line number where this XMLError occurred.
 *
 * @param error the XMLError_t from which to return the line number.
 *
 * @return the line number where this XMLError occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getLine (const XMLError_t *error)
{
  return error->getLine();
}


/**
 * Return the column number where this XMLError occurred.
 *
 * @param error the XMLError_t from which to return the column number.
 *
 * @return the column number where this XMLError occurred.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getColumn (const XMLError_t *error)
{
  return error->getColumn();
}


/**
 * Return the severity of this XMLError.  XMLErrors severity levels
 * correspond to those defined in the XML specification (with the addition
 * of Info for informational messages).
 *
 * @li  0 - Info
 * @li  1 - Warning
 * @li  2 - Error
 * @li  3 - Fatal
 *
 * @param error the XMLError_t from which to return the severity.
 *
 * @return the severity of this XMLError.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getSeverity (const XMLError_t *error)
{
  return error->getSeverity();
}


/**
 * Return the category of this XMLError.  A category is a string, similiar
 * in spirit to an XML namespace, which can be used to partition errors
 * into distinct groups.  Among other things, this can be used to prevent
 * id conflicts by uniquely identifying an XMLError by both id and
 * category.
 *
 * @param error the XMLError_t from which to return the category.
 *
 * @return the category of this XMLError.
 */
LIBLAX_EXTERN
unsigned int
XMLError_getCategory (const XMLError_t *error)
{
  return error->getCategory();
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is for information only.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is for informational purposes
 * only, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isInfo (const XMLError_t *error)
{
  return static_cast<int>( error->isInfo() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is a warning.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is a warning, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isWarning (const XMLError_t *error)
{
  return static_cast<int>( error->isWarning() );
}


/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is an error.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is an error, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isError (const XMLError_t *error)
{
  return static_cast<int>( error->isError() );
}



/**
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError_t structure is a fatal error.
 *
 * @param error the XMLError_t.
 *
 * @return @c non-zero (true) if this XMLError is a fatal error, @c zero (false) otherwise.
 */
LIBLAX_EXTERN
int
XMLError_isFatal (const XMLError_t *error)
{
  return static_cast<int>( error->isFatal() );
}


/**
 * Outputs this XMLError to stream in the following format (and
 * followed by a newline):
 *
 *   line: (id) message
 *
 * @param error, the XMLError_t structure to write.
 * @param stream, the stream to write to.
 */
LIBLAX_EXTERN
void
XMLError_print (const XMLError_t *error, FILE *stream)
{
  ostringstream os;
  os << *(static_cast<const XMLError*>(error));

  fprintf(stream, "%s", os.str().c_str());

}

/** @endcond doxygen-c-only */
