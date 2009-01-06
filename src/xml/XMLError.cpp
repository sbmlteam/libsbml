/**
 * @file    XMLError.cpp
 * @brief   Represents errors (and messages) encountered during an XML parse
 * @author  Ben Bornstein
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

/** @cond doxygen-libsbml-internal **/

static const xmlErrorTableEntry errorTable[] =
{
  // 0
  { XMLUnknownError, LIBSBML_CAT_INTERNAL, LIBSBML_SEV_FATAL,
    "Unrecognized error encountered internally." },


  // System diagnostics:

  // 0001
  { XMLOutOfMemory, LIBSBML_CAT_SYSTEM, LIBSBML_SEV_FATAL,
    "Out of memory." },

  // 0002
  { XMLFileUnreadable, LIBSBML_CAT_SYSTEM, LIBSBML_SEV_ERROR,
    "File unreadable." },

  // 0003
  { XMLFileUnwritable, LIBSBML_CAT_SYSTEM, LIBSBML_SEV_ERROR,
    "File unwritable." },

  // 0004
  { XMLFileOperationError, LIBSBML_CAT_SYSTEM, LIBSBML_SEV_ERROR,
    "Error encountered while attempting file operation." },

  // 0005
  { XMLNetworkAccessError, LIBSBML_CAT_SYSTEM, LIBSBML_SEV_ERROR,
    "Network access error." },


  // Internal diagnostics:

  // 0101
  { InternalXMLParserError, LIBSBML_CAT_INTERNAL, LIBSBML_SEV_FATAL,
    "Internal XML parser state error." },

  // 0102
  { UnrecognizedXMLParserCode, LIBSBML_CAT_INTERNAL, LIBSBML_SEV_FATAL,
    "XML parser returned an unrecognized error code." },

  // 0102
  { XMLTranscoderError, LIBSBML_CAT_INTERNAL, LIBSBML_SEV_FATAL,
    "Character transcoder error." },


  // Diagnostics about XML content:

  // 1001
  { MissingXMLDecl, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Missing XML declaration at beginning of XML input." },

  // 1002
  { MissingXMLEncoding, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Missing encoding attribute in XML declaration." },

  // 1003
  { BadXMLDecl, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid or unrecognized XML declaration or XML encoding." },

  // 1004
  { BadXMLDOCTYPE, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid, malformed or unrecognized XML DOCTYPE declaration." },

  // 1005
  { InvalidCharInXML, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid character in XML content." },

  // 1006
  { BadlyFormedXML, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "XML content is not well-formed." },

  // 1007
  { UnclosedXMLToken, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Unclosed token." },

  // 1008
  { InvalidXMLConstruct, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "XML construct is invalid or not permitted." },

  // 1009
  { XMLTagMismatch, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Element tag mismatch or missing tag." },

  // 1010
  { DuplicateXMLAttribute, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Duplicate attribute." },

  // 1011
  { UndefinedXMLEntity, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Undefined XML entity." },

  // 1012
  { BadProcessingInstruction, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid, malformed or unrecognized XML processing instruction." },

  // 1013
  { BadXMLPrefix, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid or undefined XML Namespace prefix." },

  // 1014
  { BadXMLPrefixValue, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid XML Namespace prefix value." },

  // 1015
  { MissingXMLRequiredAttribute, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Missing a required attribute." },

  // 1016
  { XMLAttributeTypeMismatch, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Data type mismatch for the value of an attribute." },

  // 1017
  { XMLBadUTF8Content, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid UTF8 content." },

  // 1018
  { MissingXMLAttributeValue, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Missing or improperly formed attribute value." },

  // 1019
  { BadXMLAttributeValue, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid or unrecognizable attribute value." },

  // 1020
  { BadXMLAttribute, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid, unrecognized or malformed attribute." },

  // 1021
  { UnrecognizedXMLElement, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Element either not recognized or not permitted." },

  // 1022
  { BadXMLComment, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Badly formed XML comment." },

  // 1023
  { BadXMLDeclLocation, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "XML declaration not permitted in this location." },

  // 1024
  { XMLUnexpectedEOF, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Reached end of input unexpectedly." },

  // 1025
  { BadXMLIDValue, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Value is invalid for XML ID, or has already been used." },

  // 1026
  { BadXMLIDRef, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "XML ID value was never declared." },

  // 1027
  { UninterpretableXMLContent, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Unable to interpret content." },

  // 1028
  { BadXMLDocumentStructure, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Bad XML document structure." },

  // 1029
  { InvalidAfterXMLContent, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Encountered invalid content after expected content." },

  // 1031
  { XMLExpectedQuotedString, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Expected to find a quoted string." },

  // 1032
  { XMLEmptyValueNotPermitted, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "An empty value is not permitted in this context." },

  // 1033
  { XMLBadNumber, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Invalid or unrecognized number." },

  // 1034
  { XMLBadColon, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Colon characters are invalid in this context." },

  // 1035
  { MissingXMLElements, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "One or more expected elements are missing." },

  // 1036
  { XMLContentEmpty, LIBSBML_CAT_XML, LIBSBML_SEV_ERROR,
    "Main XML content is empty." },

};


/*
 * Table of strings corresponding to the values from XMLErrorSeverity_t.
 * Be careful that the enum is used to index into this table.
 * This approach is not very object oriented and someday we should do better.
 */
const char * xmlSeverityStringTable[] =
{
  "Informational"                       // LIBSBML_SEV_INFO
, "Warning"                             // LIBSBML_SEV_WARNING
, "Error"                               // LIBSBML_SEV_ERROR
, "Fatal"                               // LIBSBML_SEV_FATAL
};


/*
 * Table of strings corresponding to the values from XMLErrorCategory_t.
 * Be careful that the enum is used to index into this table.
 * This approach is not very object oriented and someday we should do better.
 */
const char * xmlCategoryStringTable[] =
{
  "Internal"                            // LIBSBML_CAT_INTERNAL
, "Operating system"                    // LIBSBML_CAT_SYSTEM
, "XML content"                         // LIBSBML_CAT_XML
};

/** @endcond doxygen-libsbml-internal **/


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

  if ( errorId >= 0 && errorId < XMLErrorCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);    

    for ( unsigned int i = 0; i < tableSize; i++ )
    {
      if ( errorTable[i].code == errorId )
      {
        mMessage  = errorTable[i].message;

        if ( !details.empty() )
        {
          mMessage.append(" ");
          mMessage.append(details);
        }

        mSeverity = errorTable[i].severity;
        mCategory = errorTable[i].category;
        
        mSeverityString = xmlSeverityStringTable[mSeverity];
        mCategoryString = xmlCategoryStringTable[mCategory];

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
  // following ends up merely assigning the defaults.)

  mMessage  = details;

  mSeverity = severity;
  mCategory = category;

  if ( mSeverity >= LIBSBML_SEV_INFO && mSeverity <= LIBSBML_SEV_FATAL)
    mSeverityString = xmlSeverityStringTable[mSeverity];

  if ( mCategory >= LIBSBML_CAT_INTERNAL && mCategory <= LIBSBML_CAT_XML )
    mCategoryString = xmlCategoryStringTable[mCategory];
}


/*
 * Destroys this XMLError.
 */
XMLError::~XMLError ()
{
}


/*
 * @return the id of this XMLError.
 */
const unsigned int
XMLError::getErrorId () const
{
  return mErrorId;
}


/*
 * @return the message text of this XMLError.
 */
const string&
XMLError::getMessage () const
{
  return mMessage;
}


/*
 * @return the line number where this XMLError ocurred.
 */
unsigned int
XMLError::getLine () const
{
  return mLine;
}


/*
 * @return the column number where this XMLError occurred.
 */
unsigned int
XMLError::getColumn () const
{
  return mColumn;
}


/*
 * @return the severity of this XMLError.  XMLError severity levels
 * correspond to those defined in the XML specification (with the addition
 * of Info for informational messages).
 */
unsigned int
XMLError::getSeverity () const
{
  return mSeverity;
}


/*
 * @return a string explaining the severity code for this XMLError.
 * XMLError severity levels correspond to those defined in the XML
 * specification (with the addition of Info for informational messages).
 */
const std::string 
XMLError::getSeverityAsString() const
{
  return mSeverityString;
}


/*
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


/*
 * @return a string explaining the category code for this XMLError.
 */
const std::string 
XMLError::getCategoryAsString() const
{
  return mCategoryString;
}


/*
 * @return true if this XMLError is for informational purposes only,
 * false otherwise.
 */
bool
XMLError::isInfo () const
{
  return (mSeverity == LIBSBML_SEV_INFO);
}


/*
 * @return true if this XMLError is a warning, false otherwise.
 */
bool
XMLError::isWarning () const
{
  return (mSeverity == LIBSBML_SEV_WARNING);
}


/*
 * @return true if this XMLError is an error, false otherwise.
 */
bool
XMLError::isError () const
{
  return (mSeverity == LIBSBML_SEV_ERROR);
}


/*
 * @return true if this XMLError is a fatal error, false otherwise.
 */
bool
XMLError::isFatal () const
{
  return (mSeverity == LIBSBML_SEV_FATAL);
}


/*
 * Predicate returning @c true or @c false depending on whether 
 * this XMLError resulted from the operating system.
 *
 * @return @c true or @c false
 */
bool
XMLError::isSystem () const
{
  return (mCategory == LIBSBML_CAT_SYSTEM);
}


/*
 * Predicate returning @c true or @c false depending on whether this
 * XMLError resulted from a problem at the raw XML level (e.g., an XML
 * syntax error).
 *
 * @return @c true or @c false
 */
bool
XMLError::isXML () const
{
  return (mCategory == LIBSBML_CAT_XML);
}


/*
 * Predicate returning @c true or @c false depending on whether this
 * XMLError resulted from an internal program error.
 *
 * @return @c true or @c false
 */
bool
XMLError::isInternal () const
{
  return (mCategory == LIBSBML_CAT_INTERNAL);
}


/*
 * Sets the line number where this XMLError occurred.
 */
void
XMLError::setLine (unsigned int line)
{
  mLine = line;
}


/*
 * Sets the column number where this XMLError occurred.
 */
void
XMLError::setColumn (unsigned int column)
{
  mColumn = column;
}


/*
 * Given an XMLError::Code, return a copy of the error text.
 * 
 * @return the message text 
 */
const string
XMLError::getStandardMessage (const int code)
{
  string msg;

  if ( code >= 0 && code < XMLErrorCodesUpperBound )
  {
    unsigned int tableSize = sizeof(errorTable)/sizeof(errorTable[0]);    

    for ( unsigned int i = 0; i < tableSize; i++ )
      if ( errorTable[i].code == code )
        msg.append(errorTable[i].message);
  }
  
  return msg;
}


/*
 * Outputs this XMLError to stream in the following format (and followed by
 * a newline):
 *
 *   line: (error_id [severity]) message
 *
 * Implementation note:
 * The following code was originally written before the implementation of
 * xmlSeverityStringTable[] and getSeverityAsString().  It previously used
 * hard-code strings, and for LIBSBML_SEV_INFO, it printed "Advisory"
 * instead of "Information".  However, an inspection of the libSBML code
 * showed that nothing ever used LIBSBML_SEV_INFO!  Therefore, changing the 
 * severity string used here to be the same as what getSeverityAsString()
 * returns should not break any caller code.
 */
ostream& operator<< (ostream& s, const XMLError& error)
{
  s << "line " << error.mLine << ": ("
    << setfill('0') << setw(5) << error.mErrorId
    << " [" << error.getSeverityAsString() << "]) "
    << error.mMessage << endl;

  return s;
}

/* 
 * since the severity of errors can be drawn from a table
 * and therefore changed the unsigned int and string
 * may go out of sync
 */
void
XMLError::syncSeverityString()
{
  if ( mSeverity >= LIBSBML_SEV_INFO && mSeverity <= LIBSBML_SEV_FATAL)
    mSeverityString = xmlSeverityStringTable[mSeverity];
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

/*
 * Creates a new XMLError to report that something occurred at the given
 * line and column.  Each XMLError also has an identification number, a
 * category, and a severity level associated with it.
 *
 * @param errorId an unsigned int, the identification number of the error.
 * @param message a string, the error message.
 * @param severity XMLErrorSeverity_t, severity of the error.
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
XMLError_getErrorId (const XMLError_t *error)
{
  return error->getErrorId();
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
  return error->getMessage().empty() ? 0 : error->getMessage().c_str();
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
 * Return the severity of this XMLError.  The possible values (for the XML
 * layer) are those from the enumeration XMLErrorSeverity_t.
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
 * Return the severity of this XMLError as a string.erity_t.
 *
 * @param error the XMLError_t from which to return the severity.
 *
 * @return string representing the severity of this XMLError.
 */
LIBLAX_EXTERN
const char *
XMLError_getSeverityAsString (const XMLError_t *error)
{
  return error->getSeverityAsString().empty() ? 0 : 
      safe_strdup(error->getSeverityAsString().c_str());
}


/**
 * Return the category of this XMLError.  The possible values (for the XML
 * layers) are those from the enumeration XMLErrorCategory_t.
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
 * Return the category of this XMLError as a string.
 *
 * @param error the XMLError_t from which to return the category.
 *
 * @return string representing the category of this XMLError.
 */
LIBLAX_EXTERN
const char *
XMLError_getCategoryAsString (const XMLError_t *error)
{
  return error->getCategoryAsString().empty() ? 0 : 
              safe_strdup(error->getCategoryAsString().c_str());
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
