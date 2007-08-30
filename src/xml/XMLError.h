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
 * @brief Representation of errors, warnings and other diagnostics
 *
 * LibSBML can be configured to use any of a number of XML parsers; at the
 * time of this writing, libSBML supports Xerces 2.4&ndash;2.7, Expat
 * 1.95.x, and libxml2 2.6.16 or higher.  These parsers each report
 * different status codes for the various exceptions that can occur during
 * XML processing.  The XMLError object class abstracts away from the
 * particular diagnostics reported by the different parsers and presents a
 * single uniform interface and set of status codes, along with operations
 * for manipulating the error objects.
 *
 * When the libSBML XML parser layer encounters an error in the XML content
 * being processed, or when there is something else wrong (such as an
 * out-of-memory condition), the problems are reported as XMLError objects.
 *
 * Each XMLError object instance has an identification number that
 * identifies the nature of the problem.  This number will be up to five
 * digits long.  Applications can use the error identifiers as a means of
 * recognizing and switching behavior based on the error encountered.
 *
 * XMLError also logs a text message describing the nature of the error.
 * The text message is suitable for displaying to humans.
 *
 * Each XMLError object also contains a @em category code, drawn
 * from the enumeration XMLCategory.  Categories are used to provide more
 * information about the nature of a given error, such as whether it is
 * a system problem or a problem with the XML content.
 *
 * Each XMLError object also has a @em severity code, drawn from the
 * enumeration XMLSeverity.  Severity levels currently range from
 * informational (XMLError::Info) to fatal errors (XMLError::Fatal).
 *
 * Finally, XMLError objects record the line and column near where the
 * problem occurred in the XML content.  We say "near", because a lot of
 * factors affect how accurate the line/column information ultimately is.
 * For example, different XML parsers have different conventions for which
 * line and column number they report for a particular problem (which makes
 * a difference when a problem involves an opening XML tag on one line and
 * a closing tag on another line).  When communicating problems to humans,
 * it is generally best to provide all three pieces of information
 * (message, line, column), to help them determine the actual error.
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
    UnknownError             =    0 /*!< Unknown error encountered. */

    // System diagnostics: numbers below 100
  , OutOfMemory              =    1 /*!< LibSBML unexpected encountered an out
				     *   of memory condition from the operating
				     *   system. */

  , FileUnreadable           =    2 /*!< Could not open or read the file. */

  , FileUnwritable           =    3 /*!< Could not write to the file. */

  , FileOperationError       =    4 /*!< Error encountered while attempting
				     *   a file operation. */

  , NetworkAccessError       =    5 /*!< Error encountered while attempting
				     *   a network access. */

    // Internal diagnostics: numbers about 100 and below 1000

  , InternalParserError      =  101 /*!< Internal error in XML parser. */

  , UnrecognizedParserCode   =  102 /*!< The XML parser returned an error
				     *   code that is not recognized by
				     *   libSBML. */

  , TranscoderError          =  103 /*!< The character transcoder reported
				     *   an error. */

    // Content errors: numbers about 1000 and below 9999

  , MissingXMLDecl           = 1001 /*!< Missing XML declaration at beginning
				     *   of XML input. */

  , MissingXMLEncoding       = 1002 /*!< Missing encoding attribute in
				     *   XML declaration. */

  , BadXMLDecl               = 1003 /*!< Invalid or unrecognized XML
				     *   declaration or XML encoding. */

  , BadDOCTYPE               = 1004 /*!< Invalid, malformed or unrecognized
				     *   XML DOCTYPE declaration. */

  , InvalidChar              = 1005 /*!< Invalid character in XML content. */

  , NotWellFormed            = 1006 /*!< Badly formed XML. */

  , UnclosedToken            = 1007 /*!< Unclosed token. */

  , InvalidConstruct         = 1008 /*!< XML construct is invalid or
				     *   not permitted. */

  , TagMismatch              = 1009 /*!< Element tag mismatch or missing tag.*/

  , DuplicateAttribute       = 1010 /*!< Duplicate attribute. */

  , UndefinedEntity          = 1011 /*!< Undefined XML entity. */

  , BadProcessingInstruction = 1012 /*!< Invalid, malformed or unrecognized
				     *   XML processing instruction. */

  , BadPrefix                = 1013 /*!< Invalid or undefined XML
				     *   Namespace prefix. */

  , BadPrefixValue           = 1014 /*!< Invalid XML Namespace prefix value. */

  , MissingRequiredAttribute = 1015 /*!< Required attribute is missing. */

  , AttributeTypeMismatch    = 1016 /*!< Data type mismatch for attribute
				     *   value. */

  , BadUTF8Content           = 1017 /*!< Invalid UTF8 content. */

  , MissingAttributeValue    = 1018 /*!< Missing or improperly formed
				     *   attribute value. */

  , BadAttributeValue        = 1019 /*!< Invalid or unrecognizable attribute
				     *   value. */

  , BadAttribute             = 1020 /*!< Invalid, unrecognized or malformed
				     *   attribute. */

  , UnrecognizedElement      = 1021 /*!< Element either not recognized or
				     *   not permitted. */

  , BadXMLComment            = 1022 /*!< Badly formed XML comment. */

  , BadXMLDeclLocation       = 1023 /*!< XML declaration not permitted in
				     *   this location. */

  , UnexpectedEOF            = 1024 /*!< Reached end of input unexpectedly. */

  , BadXMLIDValue            = 1025 /*!< Value is invalid for XML ID, or has
				     *   already been used. */

  , BadXMLIDRef              = 1026 /*!< XML ID value was never declared. */

  , UninterpretableContent   = 1027 /*!< Unable to interpret content. */

  , BadDocumentStructure     = 1028 /*!< Bad XML document structure. */

  , InvalidAfterContent      = 1029 /*!< Encountered invalid content after
				     *   expected content. */

  , ExpectedQuotedString     = 1030 /*!< Expected to find a quoted string. */

  , EmptyValueNotPermitted   = 1031 /*!< An empty value is not permitted in
				     *   this context. */

  , BadNumber                = 1032 /*!< Invalid or unrecognized number. */

  , BadColon                 = 1033 /*!< Colon characters are invalid in
				     *   this context. */

  , MissingElements          = 1034 /*!< One or more expected elements
				     *   are missing. */

  , EmptyXML                 = 1035 /*!< Main XML content is empty. */

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
		   * indicates a software defect (i.e., bug) in libSBML.
		   * Please report instances of this to the libSBML
		   * developers. */

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
   * value from XMLError::Code, the XMLError class assumes the error is
   * a low-level system or XML layer error and prepends a predefined error
   * message to any string passed in @p details.  In addition, all
   * XMLError::Code errors have associated severity and category codes, and
   * these fields are filled-in as well from the enumerations
   * XMLError::Severity and XMLError::Category, respectively.
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
   * @param details a string containing additional details about the error.
   * If the error code in @p errorId is one that is recognized by XMLError,
   * the given message is @em appended to a predefined message associated
   * with the given code.  If the error code is not recognized, the message
   * is stored as-is as the text of the error.
   * 
   * @param line an unsigned int, the line number at which the error occured.
   * 
   * @param column an unsigned int, the column number at which the error occured.
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
  const unsigned int getErrorId () const;


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
  static const std::string getStandardMessage (const int code);


#ifndef SWIG

  /**
   * Outputs this XMLError to stream in the following format (and followed
   * by a newline):
   *
   *   line: (error id) message
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
XMLError_getErrorId (const XMLError_t *error);


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
