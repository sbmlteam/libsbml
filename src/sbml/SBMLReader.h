/**
 * @file    SBMLReader.h
 * @brief   Reads an SBML Document into memory
 * @author  Ben Bornstein
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
 *------------------------------------------------------------------------- -->
 *
 * @class SBMLReader
 * @brief Methods for reading SBML from files and text strings.
 *
 * @htmlinclude libsbml-not-sbml-warning.html
 *
 * The SBMLReader class provides the main interface for reading SBML
 * content from files and strings.  The methods for reading SBML all return
 * an SBMLDocument object representing the results.
 *
 * In the case of failures (such as if the SBML contains errors or a file
 * cannot be read), the errors will be recorded with the SBMLErrorLog
 * object kept in the SBMLDocument returned by SBMLReader.  Consequently,
 * immediately after calling a method on SBMLReader, callers should always
 * check for errors and warnings using the methods for this purpose
 * provided by SBMLDocument.
 *
 * For convenience as well as easy access from other languages besides C++,
 * this file also defines two global functions, readSBMLFromFile() and
 * readSBMLFromString().  They are equivalent to creating an SBMLReader
 * object and then calling
 * the @if clike SBMLReader::readSBML() @endif@if java SBMLReader::readSBML(String filename) @endif or
 * @if clike SBMLReader::readSBMLFromString() @endif@if java SBMLReader::readSBMLFromString(String xml)  @endif methods, respectively.
 *
 * LibSBML also provides support for reading and writing compressed files
 * and data streams.  It does this automatically and transparently if
 * libSBML was configured and compiled with the compression support
 * enabled.  If the given filename ends with the suffix @c ".gz" (for
 * example, @c "myfile.xml.gz"), then the file is assumed to be compressed
 * in @em gzip format and will be automatically decompressed upon reading.
 * Similarly, if the given filename ends with @c ".zip" or @c ".bz2", the
 * file is assumed to be compressed in @em zip or @em bzip2 format
 * (respectively).  Files whose names lack these suffixes will be read
 * uncompressed.
 *
 * Applications may sometimes need to query whether the copy of the libSBML
 * library that they're linked against does support reading and writing
 * compressed files.  SBMLReader provides the methods hasZlib() and
 * hasBzip2() and for this purpose.
 */

#ifndef SBMLReader_h
#define SBMLReader_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/util/util.h>


#ifdef __cplusplus


#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLDocument;


class LIBSBML_EXTERN SBMLReader
{
public:

  /**
   * Creates a new SBMLReader and returns it. 
   */
  SBMLReader ();


  /**
   * Destroys this SBMLReader.
   */
  virtual ~SBMLReader ();


  /**
   * Reads an SBML document from a file.
   *
   * If the file named @p filename does not exist or its content is not
   * valid SBML, one or more errors will be logged with the SBMLDocument
   * object returned by this method.  Callers can use the methods on
   * SBMLDocument such as SBMLDocument::getNumErrors() and
   * @if clike SBMLDocument::getError() @endif@if java SBMLDocument::getError(long n) @endif to get the errors.  The object returned by
   * @if clike SBMLDocument::getError() @endif@if java SBMLDocument::getError(long n) @endif is an SBMLError object, and it has methods to
   * get the error code, category, and severity level of the problem, as
   * well as a textual description of the problem.  The possible severity
   * levels range from informational messages to fatal errors; see the
   * documentation for SBMLError for more information.
   *
   * If the file @p filename could not be read, the file-reading error will
   * appear first.  The error code (a value drawn from the enumeration
   * #XMLErrorCode_t) can provide a clue about what happened.  For example,
   * a file might be unreadable (either because it does not actually exist
   * or because the user does not have the necessary access priviledges to
   * read it) or some sort of file operation error may have been reported
   * by the underlying operating system.  Callers can check for these
   * situations using a program fragment such as the following:
   * @code
   * SBMLReader* reader = new SBMLReader();
   * SBMLDocument* doc  = reader.readSBML(filename);
   *
   * if (doc->getNumErrors() > 0)
   * {
   *   if (doc->getError(0)->getId() == XMLError::FileUnreadable)
   *   {
   *     // Handle case of unreadable file here.
   *   } 
   *   else if (doc->getError(0)->getId() == XMLError::FileOperationError)
   *   {
   *     // Handle case of other file error here.
   *   }
   *   else
   *   {
   *     // Handle other cases -- see error codes defined in XMLErrorCode_t
   *     // for other possible cases to check.
   *   }
   * }
   * @endcode
   *
   * If the given filename ends with the suffix @c ".gz" (for example, @c
   * "myfile.xml.gz"), the file is assumed to be compressed in @em gzip
   * format and will be automatically decompressed upon reading.
   * Similarly, if the given filename ends with @c ".zip" or @c ".bz2", the
   * file is assumed to be compressed in @em zip or @em bzip2 format
   * (respectively).  Files whose names lack these suffixes will be read
   * uncompressed.  Note that if the file is in @em zip format but the
   * archive contains more than one file, only the first file in the
   * archive will be read and the rest ignored.
   *
   * @param filename the name or full pathname of the file to be read.
   *
   * @return a pointer to the SBMLDocument created from the SBML content.
   *
   * @see SBMLError
   *
   * @note LibSBML versions 2.x and later versions behave differently in
   * error handling in several respects.  One difference is how early some
   * errors are caught and whether libSBML continues processing a file in
   * the face of some early errors.  In general, libSBML versions after 2.x
   * stop parsing SBML inputs sooner than libSBML version 2.x in the face
   * of XML errors, because the errors may invalidate any further SBML
   * content.  For example, a missing XML declaration at the beginning of
   * the file was ignored by libSBML 2.x but in version 3.x and later, it
   * will cause libSBML to stop parsing the rest of the input altogether.
   * While this behavior may seem more severe and intolerant, it was
   * necessary in order to provide uniform behavior regardless of which
   * underlying XML parser (Expat, Xerces, libxml2) is being used by
   * libSBML.  The XML parsers themselves behave differently in their error
   * reporting, and sometimes libSBML has to resort to the lowest common
   * denominator.
   * <br><br>
   * @note To read a gzip/zip file, libSBML needs to be configured and
   * linked with the <a href="http://www.zlib.net/">zlib</a> library at
   * compile time.  It also needs to be linked with the <a
   * href="">bzip2</a> library to read files in @em bzip2 format.  (Both of
   * these are the default configurations for libSBML.)  Errors about
   * unreadable files will be logged if a compressed filename is given and
   * libSBML was @em not linked with the corresponding required library.
   * <br><br>
   * @note SBMLReader::hasZlib() and SBMLReader::hasBzip2() can be used to
   * check whether libSBML has been linked with each library.
   */
  SBMLDocument* readSBML (const std::string& filename);


  /**
   * Reads an SBML document from a file.
   *
   * If the file named @p filename does not exist or its content is not
   * valid SBML, one or more errors will be logged with the SBMLDocument
   * object returned by this method.  Callers can use the methods on
   * SBMLDocument such as SBMLDocument::getNumErrors() and
   * @if clike SBMLDocument::getError() @endif@if java SBMLDocument::getError(long n) @endif to get the errors.  The object returned by
   * @if clike SBMLDocument::getError() @endif@if java SBMLDocument::getError(long n) @endif is an SBMLError object, and it has methods to
   * get the error code, category, and severity level of the problem, as
   * well as a textual description of the problem.  The possible severity
   * levels range from informational messages to fatal errors; see the
   * documentation for SBMLError for more information.
   *
   * If the file @p filename could not be read, the file-reading error will
   * appear first.  The error code (a value drawn from the enumeration
   * #XMLErrorCode_t) can provide a clue about what happened.  For example,
   * a file might be unreadable (either because it does not actually exist
   * or because the user does not have the necessary access priviledges to
   * read it) or some sort of file operation error may have been reported
   * by the underlying operating system.  Callers can check for these
   * situations using a program fragment such as the following:
   * @code
   * SBMLReader* reader = new SBMLReader();
   * SBMLDocument* doc  = reader.readSBML(filename);
   *
   * if (doc->getNumErrors() > 0)
   * {
   *   if (doc->getError(0)->getId() == XMLError::FileUnreadable)
   *   {
   *     // Handle case of unreadable file here.
   *   } 
   *   else if (doc->getError(0)->getId() == XMLError::FileOperationError)
   *   {
   *     // Handle case of other file error here.
   *   }
   *   else
   *   {
   *     // Handle other cases -- see error codes defined in XMLErrorCode_t
   *     // for other possible cases to check.
   *   }
   * }
   * @endcode
   *
   * If the given filename ends with the suffix @c ".gz" (for example, @c
   * "myfile.xml.gz"), the file is assumed to be compressed in @em gzip
   * format and will be automatically decompressed upon reading.
   * Similarly, if the given filename ends with @c ".zip" or @c ".bz2", the
   * file is assumed to be compressed in @em zip or @em bzip2 format
   * (respectively).  Files whose names lack these suffixes will be read
   * uncompressed.  Note that if the file is in @em zip format but the
   * archive contains more than one file, only the first file in the
   * archive will be read and the rest ignored.
   *
   * @param filename the name or full pathname of the file to be read.
   *
   * @return a pointer to the SBMLDocument created from the SBML content.
   *
   * @see SBMLError
   *
   * @note LibSBML versions 2.x and later versions behave differently in
   * error handling in several respects.  One difference is how early some
   * errors are caught and whether libSBML continues processing a file in
   * the face of some early errors.  In general, libSBML versions after 2.x
   * stop parsing SBML inputs sooner than libSBML version 2.x in the face
   * of XML errors, because the errors may invalidate any further SBML
   * content.  For example, a missing XML declaration at the beginning of
   * the file was ignored by libSBML 2.x but in version 3.x and later, it
   * will cause libSBML to stop parsing the rest of the input altogether.
   * While this behavior may seem more severe and intolerant, it was
   * necessary in order to provide uniform behavior regardless of which
   * underlying XML parser (Expat, Xerces, libxml2) is being used by
   * libSBML.  The XML parsers themselves behave differently in their error
   * reporting, and sometimes libSBML has to resort to the lowest common
   * denominator.
   * <br><br>
   * @note To read a gzip/zip file, libSBML needs to be configured and
   * linked with the <a href="http://www.zlib.net/">zlib</a> library at
   * compile time.  It also needs to be linked with the <a
   * href="">bzip2</a> library to read files in @em bzip2 format.  (Both of
   * these are the default configurations for libSBML.)  Errors about
   * unreadable files will be logged if a compressed filename is given and
   * libSBML was @em not linked with the corresponding required library.
   * <br><br>
   * @note SBMLReader::hasZlib() and SBMLReader::hasBzip2() can be used to
   * check whether libSBML has been linked with each library.
   */
  SBMLDocument* readSBMLFromFile (const std::string& filename);


  /**
   * Reads an SBML document from the given XML string.
   *
   * This method is flexible with respect to the presence of an XML
   * declaration at the beginning of the string.  In particular, if the
   * string in @p xml does not begin with the XML declaration
   * <code>&lt;?xml version='1.0' encoding='UTF-8'?&gt;</code>, then this
   * method will automatically prepend the declaration to @p xml.
   *
   * This method will log a fatal error if the content given in the
   * parameter @p xml is not SBML.  See the method documentation for
   * @if clike SBMLReader::readSBML() @endif@if java SBMLReader::readSBML(String filename) @endif for an example of code for testing the returned
   * error code.
   *
   * @param xml a string containing a full SBML model
   *
   * @return a pointer to the SBMLDocument created from the SBML content.
   *
   * @if clike @see SBMLReader::readSBML() @endif@if java @see SBMLReader::readSBML(String filename) @endif
   */
  SBMLDocument* readSBMLFromString (const std::string& xml);


  /**
   * Predicate returning @c true or @c false depending on whether this copy
   * of libSBML supports <i>gzip</I> and <i>zip</i> format compression.
   *
   * @return @c true if libSBML has been linked with the <i>zlib</i>
   * library, @c false otherwise.
   *
   * @see hasBzip2()
   */
  static bool hasZlib();


  /**
   * Predicate returning @c true or @c false depending on whether
   * this copy of libSBML supports <i>bzip2</i> format compression.
   *
   * @return @c true if libSBML has been linked with the <i>bzip2</i>
   * libraries, @c false otherwise.
   *
   * @see hasZlib()
   */
  static bool hasBzip2();


protected:
  /** @cond doxygen-libsbml-internal */

  /**
   * Used by readSBML() and readSBMLFromString().
   *
   * @if notcpp @docnote @htmlinclude libsbml-warn-default-args-in-docs.html @endif
   */
  SBMLDocument* readInternal (const char* content, bool isFile = true);

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


#ifndef SWIG


/**
 * Creates a new SBMLReader and returns it.  By default XML Schema
 * validation is off.
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create (void);

/**
 * Frees the given SBMLReader.
 */
LIBSBML_EXTERN
void
SBMLReader_free (SBMLReader_t *sr);


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, an error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLReader_t   *sr;\n
 *   SBMLDocument_t *d;
 *
 *   sr = SBMLReader_create();
 *
 *   d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumErrors(d) > 0)\n
 *   {\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_FILE_NOT_FOUND)\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_NOT_SBML)\n
 *   }\n
 * </code>
 *
 * If the given filename ends with the suffix @c ".gz" (for example, @c
 * "myfile.xml.gz"), the file is assumed to be compressed in @em gzip
 * format and will be automatically decompressed upon reading.
 * Similarly, if the given filename ends with @c ".zip" or @c ".bz2", the
 * file is assumed to be compressed in @em zip or @em bzip2 format
 * (respectively).  Files whose names lack these suffixes will be read
 * uncompressed.  Note that if the file is in @em zip format but the
 * archive contains more than one file, only the first file in the
 * archive will be read and the rest ignored.
 *
 * @note LibSBML versions 2.x and 3.x behave differently in error
 * handling in several respects.  One difference is how early some errors
 * are caught and whether libSBML continues processing a file in the face
 * of some early errors.  In general, libSBML 3.x stops parsing SBML
 * inputs sooner than libSBML 2.x in the face of XML errors because the
 * errors may invalidate any further SBML content.  For example, a
 * missing XML declaration at the beginning of the file was ignored by
 * libSBML 2.x but in version 3.x, it will cause libSBML to stop parsing
 * the rest of the input altogether.  While this behavior may seem more
 * severe and intolerant, it was necessary in order to provide uniform
 * behavior regardless of which underlying XML parser (Expat, Xerces,
 * libxml2) is being used by libSBML.  The XML parsers themselves behave
 * differently in their error reporting, and sometimes libSBML has to
 * resort to the lowest common denominator.
 *
 * @note To read a gzip/zip file, libSBML needs to be configured and
 * linked with the <a href="http://www.zlib.net/">zlib</a> library at
 * compile time.  It also needs to be linked with the <a
 * href="">bzip2</a> library to read files in @em bzip2 format.  (Both of
 * these are the default configurations for libSBML.)  Errors about
 * unreadable files will be logged if a compressed filename is given and
 * libSBML was @em not linked with the corresponding required library.
 *
 * @note SBMLReader::hasZlib() and SBMLReader::hasBzip2() can be used to
 * check whether libSBML has been linked with each library.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename);

LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromFile (SBMLReader_t *sr, const char *filename);

/**
 * Reads an SBML document from the given XML string.
 *
 * If the string does not begin with XML declaration:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * it will be prepended.
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml);


/**
 * Predicate returning @c non-zero or @c zero depending on whether
 * underlying libSBML is linked with..
 *
 * @return @c non-zero if libSBML is linked with zlib, @c zero otherwise.
 */
LIBSBML_EXTERN
int
SBMLReader_hasZlib ();


/**
 * Predicate returning @c non-zero or @c zero depending on whether
 * libSBML is linked with bzip2.
 *
 * @return @c non-zero if libSBML is linked with bzip2, @c zero otherwise.
 */
LIBSBML_EXTERN
int
SBMLReader_hasBzip2 ();

#endif  /* !SWIG */


/**
 * Reads an SBML document from the given file @p filename.
 *
 * If @p filename does not exist, or it is not an SBML file, an error will
 * be logged in the error log of the SBMLDocument object returned by this
 * method.  Calling programs can inspect this error log to determine
 * the nature of the problem.  Please refer to the definition of
 * SBMLDocument for more information about the error reporting mechanism.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBML (const char *filename);


LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromFile (const char *filename);


/**
 * Reads an SBML document from a string assumed to be in XML format.
 *
 * If the string does not begin with XML declaration,
 *@verbatim
<?xml version='1.0' encoding='UTF-8'?>
@endverbatim
 *
 * an XML declaration string will be prepended.
 *
 * This method will report an error if the given string @p xml is not SBML.
 * The error will be logged in the error log of the SBMLDocument object
 * returned by this method.  Calling programs can inspect this error log to
 * determine the nature of the problem.  Please refer to the definition of
 * SBMLDocument for more information about the error reporting mechanism.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* SBMLReader_h */
