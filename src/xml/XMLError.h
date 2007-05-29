/**
 * @file    XMLError.h
 * @brief   Represents errors (and messages) encountered during an XML parse
 * @author  Ben Bornstein
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * @class XMLError
 * @brief Abstraction and processing of errors in the XML layer.
 *
 * LibSBML can be configured to use any of a number of XML parsers; at the
 * time of this writing, libSBML supported Xerces 2.4&ndash;2.7, Expat
 * 1.95.x, and libxml2.  These parsers each report different status codes
 * for the various exceptions that can occur during XML processing.
 * The XMLError object class abstracts away from the particular diagnostics
 * reported by the different parsers and presents a single uniform
 * interface and set of status codes, along with operations for
 * manipulating the error objects.
 */


#ifndef XMLError_h
#define XMLError_h


#include <sbml/xml/XMLExtern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <iosfwd>
#include <string>



class LIBLAX_EXTERN XMLError
{
public:

  /**
   * Canonical error codes returned for low-level XML parser errors.
   *
   * These codes are 4 digits long, less than 10000, to distinguish them
   * from 5-digit SBML error codes > 10000.  The codes are an abstraction
   * of errors from the multiple parsers (Xerces, Expat, libxml2) supported
   * by libSBML.
   */
  enum Code
  {
    UnknownError             =    0

    // System diagnostics: numbers below 100
  , OutOfMemory              =    1
  , FileUnreadable           =    2
  , FileUnwritable           =    3
  , FileOperationError       =    4
  , NetworkAccessError       =    5

    // Internal diagnostics: numbers about 100 and below 1000
  , InternalParserError      =  101
  , UnrecognizedParserCode   =  102
  , TranscoderError          =  103

    // Content errors: numbers about 1000 and below 9999
  , MissingXMLDecl           = 1001
  , MissingXMLEncoding       = 1002
  , BadXMLDecl               = 1003
  , BadDOCTYPE               = 1004
  , InvalidChar              = 1005
  , NotWellFormed            = 1006
  , UnclosedToken            = 1007
  , InvalidConstruct         = 1008
  , TagMismatch              = 1009
  , DuplicateAttribute       = 1010
  , UndefinedEntity          = 1011
  , BadProcessingInstruction = 1012
  , BadPrefix                = 1013
  , BadPrefixValue           = 1014
  , MissingRequiredAttribute = 1015
  , AttributeTypeMismatch    = 1016
  , BadUTF8Content           = 1017
  , MissingAttributeValue    = 1018
  , BadAttributeValue        = 1019
  , BadAttribute             = 1020
  , UnrecognizedElement      = 1021
  , BadXMLComment            = 1022
  , BadXMLDeclLocation       = 1023
  , UnexpectedEOF            = 1024
  , BadXMLIDValue            = 1025
  , BadXMLIDRef              = 1026
  , UninterpretableContent   = 1027
  , BadDocumentStructure     = 1028
  , InvalidAfterContent      = 1029
  , ExpectedQuotedString     = 1030
  , EmptyValueNotPermitted   = 1031
  , BadNumber                = 1032
  , BadColon                 = 1033
  , MissingElements          = 1034
  , EmptyXML                 = 1035

    // Bounds
  , ErrorCodesUpperBound     = 9999

  };


  /**
   * Severity codes for errors in the XML layer
   *
   * These severity levels correspond to those defined in the XML
   * specification, with the addition of Info for informational messages.
   *
   */
  enum Severity
  {
    Info = 0,    /*!< The error is actually informational and
   	          * not necessarily a serious problem. */

    Warning = 1, /*!< The error object represents a problem that is not
		  * serious enough to necessarily stop the problem, but
		  * applications should take note of the problem and
		  * evaluate what its implications may be. */

    Error = 2,   /*!< The error object represents a serious error.  The
		  * application may continue running but it is unlikely to
		  * be able to continue processing the same XML file or
		  * data stream. */

    Fatal = 3    /*!< A serious error occurred, such as an out-of-memory
		  * condition, and the software should terminate
		  * immediately. */
  };


  /**
   * Category codes for errors in the XML layer.
   */
  enum Category
  {
    Internal = 0, /*!< A problem involving the libSBML software itself or
		   * the underlying XML parser.  This almost certainly 
		   * indicates a software defect (i.e., bug).  Please
		   * report instances of this to the libSBML developers. */

    System = 1,   /*!< A problem reported by the operating system, such as
		   * an inability to read or write a file.  This indicates
		   * something that is not a program error but is outside
		   * of the control of the software. */

    XML = 2      /*!< A problem in the XML content itself.  This usually
		  * arises from malformed XML or the use of constructs not
		  * permitted in SBML. */
  };


  /**
   * Creates a new XMLError to report that something occurred during XML
   * processing.
   *
   * XMLError objects have identification numbers to indicate the nature of
   * the exception.  These numbers are drawn from the enumeration
   * XMLError::Code.  The argument @p errorId to this constructor @em can be
   * (but does not have to be) a value from this enumeration.  If it is a
   * value from XMLError::Code, the XMLError class assumes it the error is
   * a low-level system or XML layer error and prepends a predefined error
   * message to any string passed in @p details.  In addition, all
   * XMLError::Code errors have associated severity and category codes, and
   * these fields are filled-in as well from the enumerations
   * XMLError::Severity and XMLError::Category, respectively
   *
   * If the error identifier @p errorId is a number greater than 9999, the
   * XMLError class assumes the error was generated from another part of
   * the software and does not do additional filling in of values beyond
   * the default in the constructor itself.  This allows XMLError to serve
   * as a base class for other errors (and is used in this way elsewhere in
   * libSBML).  Callers should fill in all the parameters with suitable
   * values if generating errors with codes greater than 9999 to make
   * maximum use of the XMLError facilities.
   *
   * As mentioned above, there are two other enumerations,
   * XMLError::Severity and XMLError::Category, used for indicating the
   * severity and category of error for the predefined XMLError codes.  The
   * values passed in @p severity and @p category override the defaults
   * assigned based on the error code.  If the error identifier is a code
   * number from XMLError::Code, callers do not need to fill in @p severity
   * and @p category.  Conversely, if @p errorId is not a value from
   * XMLError::Code, callers can use other values (not just those from
   * XMLError::Severity and XMLError::Category, but their own special
   * values) for @p severity and @p category.
   *
   * @param errorId an unsigned int, the identification number of the error.
   * 
   * @param line an unsigned int, the line number at which the error occured.
   * 
   * @param column an unsigned int, the column number at which the error occured.
   * 
   * @param message a string, the error message.  If the error code in @p
   * errorId is one that is recognized by XMLError, the given message is appended
   * to a predefined message associated with the given code.  If the error
   * code is not recognized, the message is stored as-is as the text of the
   * error.
   * 
   * @param severity an integer indicating severity of the error.
   * 
   * @param category an integer indicating the category to which the error
   * belongs.
   */
  XMLError
  (
      const int errorId           = 0
    , const std::string& details  = ""
    , const unsigned int line     = 0
    , const unsigned int column   = 0
    , const unsigned int severity = Fatal
    , const unsigned int category = Internal
  );


  /**
   * Destroys this XMLError.
   */
  virtual ~XMLError ();


  /**
   * Returns the identifier of this error.
   *
   * @return the id of this XMLError.
   */
  const unsigned int getId () const;


  /**
   * Returns the message text of this error.
   *
   * @return the message text
   */
  const std::string& getMessage () const;


  /**
   * Return the line number in the XML input where the error occurred.
   *
   * @return the line number 
   */
  unsigned int getLine () const;


  /**
   * Return the column number in the XML input where the error occurred.
   *
   * @return the column number
   */
  unsigned int getColumn () const;


  /**
   * Return the severity of this error.
   *
   * XMLError defines an enumeration of severity codes for the XML layer.
   * Applications that build on XMLError by subclassing it may add their
   * own severity codes with numbers higher than those in the
   * XMLError::Severity enumeration.
   *
   * @return the severity of this XMLError.
   */
  unsigned int getSeverity () const;


  /**
   * Return the category of this error.
   *
   * XMLError defines an enumeration of category codes for the XML layer.
   * Applications that build on XMLError by subclassing it may add their
   * own categoreis with numbers higher than those in the
   * XMLError::Category enumeration.
   *
   * Categories can be used to partition errors into distinct groups.
   * Among other things, this can be used to prevent id conflicts by
   * uniquely identifying an XMLError by both id and category.
   *
   * @return the category of this XMLError.
   */
  unsigned int getCategory () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * error object is for information purposes only.
   *
   * @return @c true if this XMLError is for informational purposes only,
   * @c false otherwise.
   */
  bool isInfo () const;


  /**
   * Predicate returning @c true or @c false depending on whether 
   * this error object is a warning.
   *
   * @return @c true if this error is a warning, @c false otherwise.
   */
  bool isWarning () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * error is a significant error.
   *
   * @return @c true if this error is an error, @c false otherwise.
   */
  bool isError () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * error is a fatal run-time error.
   *
   * @return @c true if this error is a fatal error, @c false otherwise.
   */
  bool isFatal () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * error resulted from an internal program error.
   *
   * @return @c true or @c false
   */
  bool isInternal () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * error was generated by the operating system.
   *
   * @return @c true or @c false
   */
  bool isSystem () const;


  /**
   * Predicate returning @c true or @c false depending on whether this
   * error resulted from a problem in the XML input (e.g., an XML syntax
   * error).
   *
   * @return @c true or @c false
   */
  bool isXML () const;


  /**
   * Sets the line number where this error occurred.
   * 
   * @param line an unsigned int, the line number to set.
   */
  void setLine (unsigned int line);


  /**
   * Sets the column number where this error occurred.
   * 
   * @param column an unsigned int, the column number to set.
   */
  void setColumn (unsigned int column);

  
  /**
   * Returns a copy of the message string associated with the given
   * predefined XMLError code.
   *
   * @param code the error code whose message is sought; it must be a
   * predefined value from XMLError::Code
   */
  static const std::string getStandardMessage (const XMLError::Code code);


#ifndef SWIG

  /**
   * Outputs this XMLError to stream in the following format (and followed
   * by a newline):
   *
   *   line: (id) message
   *
   * @param stream the output stream to write to.
   * @param error the XMLError to write.
   */
  LIBLAX_EXTERN
  friend
  std::ostream& operator<< (std::ostream& stream, const XMLError& error);

#endif  /* !SWIG */


protected:
  /** @cond doxygen-libsbml-internal */

  unsigned int mErrorId;

  std::string  mMessage;

  unsigned int mSeverity;
  unsigned int mCategory;

  unsigned int mLine;
  unsigned int mColumn;

  /** @endcond doxygen-libsbml-internal */
};


/** @cond doxygen-libsbml-internal */
/**
 * The structured used in constructing tables of predefined error codes and
 * their associated messages, severities and categories. 
 */
typedef struct {
  int                code;
  XMLError::Category category;
  XMLError::Severity severity;
  const char*        message;
} xmlErrorTableEntry;
/** @endcond doxygen-libsbml-internal */



#endif  /* __cplusplus */


#ifndef SWIG

BEGIN_C_DECLS

typedef enum { Info, Warning, Error, Fatal } XMLError_Severity;

typedef enum { System, XML, SBML, Internal } XMLError_Category;


/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


//#ifndef __cplusplus
//typedef struct XMLError_t;
//#endif

LIBLAX_EXTERN
XMLError_t*
XMLError_create (void);

LIBLAX_EXTERN
XMLError_t*
XMLError_createWithIdAndMessage (unsigned int errorId, const char * message);

//LIBLAX_EXTERN
//XMLError_t*
//XMLError_createWithAll (unsigned int id, const char * message, XMLError_Severity severity,
//                        const char * category, unsigned int line, unsigned int column);
//
LIBLAX_EXTERN
void
XMLError_free(XMLError_t* error);


LIBLAX_EXTERN
unsigned int
XMLError_getId (const XMLError_t *error);


LIBLAX_EXTERN
const char *
XMLError_getMessage (const XMLError_t *error);


LIBLAX_EXTERN
unsigned int
XMLError_getLine (const XMLError_t *error);


LIBLAX_EXTERN
unsigned int
XMLError_getColumn (const XMLError_t *error);


LIBLAX_EXTERN
unsigned int
XMLError_getSeverity (const XMLError_t *error);


LIBLAX_EXTERN
unsigned int
XMLError_getCategory (const XMLError_t *error);


LIBLAX_EXTERN
int
XMLError_isInfo (const XMLError_t *error);


LIBLAX_EXTERN
int
XMLError_isWarning (const XMLError_t *error);


LIBLAX_EXTERN
int
XMLError_isError (const XMLError_t *error);


LIBLAX_EXTERN
int
XMLError_isFatal (const XMLError_t *error);


LIBLAX_EXTERN
void
XMLError_print (const XMLError_t *error, FILE *stream);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* XMLError_h */
