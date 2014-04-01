/**
 * @file    SBMLReader.h
 * @brief   Reads an SBML Document into memory
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class SBMLReader
 * @sbmlbrief{core} Methods for reading SBML from files and text strings.
 *
 * @htmlinclude not-sbml-warning.html
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
 * this file also defines two global functions,
 * libsbml::readSBML(@if java String filename@endif)
 * and libsbml::readSBMLFromString(@if java String xml@endif).
 * They are equivalent to creating an SBMLReader
 * object and then calling the
 * SBMLReader::readSBML(@if java String filename@endif) or
 * SBMLReader::readSBMLFromString(@if java String xml@endif)
 * methods, respectively.
 *
 * @section compression Support for reading compressed files
 *
 * LibSBML provides support for reading (as well as writing) compressed
 * SBML files.  The process is transparent to the calling
 * application&mdash;the application does not need to do anything
 * deliberate to invoke the functionality.  If a given SBML filename ends
 * with an extension for the @em gzip, @em zip or @em bzip2 compression
 * formats (respectively, @c .gz, @c .zip, or @c .bz2), then the methods
 * SBMLReader::readSBML(@if java String filename@endif) and
 * SBMLWriter::writeSBML(@if java SBMLDocument d, String filename@endif)
 * will automatically decompress and compress the file while writing and
 * reading it.  If the filename has no such extension, it
 * will be read and written uncompressed as normal.
 *
 * The compression feature requires that the @em zlib (for @em gzip and @em
 * zip formats) and/or @em bzip2 (for @em bzip2 format) be available on the
 * system running libSBML, and that libSBML was configured with their
 * support compiled-in.  Please see the libSBML @if clike <a href="libsbml-installation.html">installation instructions</a> @endif@if python <a href="libsbml-installation.html">installation instructions</a> @endif@if java  <a href="../../../libsbml-installation.html">installation instructions</a> @endif@~ for more information about this.  The methods
 * @if java SBMLReader::hasZlib()@else hasZlib()@endif@~ and
 * @if java SBMLReader::hasBzip2()@else hasBzip2()@endif@~
 * can be used by an application to query at run-time whether support
 * for the compression libraries is available in the present copy of
 * libSBML.
 *
 * Support for compression is not mandated by the SBML standard, but
 * applications may find it helpful, particularly when large SBML models
 * are being communicated across data links of limited bandwidth.
 */

/**
 * <!-- ~ ~ ~ ~ ~ Start of common documentation strings ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
 * The following text is used as common documentation blocks copied multiple
 * times elsewhere in this file.  The use of @class is a hack needed because
 * Doxygen's @copydetails command has limited functionality.  Symbols
 * beginning with "doc_" are marked as ignored in our Doxygen configuration.
 * ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~  -->
 *
 * @class doc_sbmlreader_if_compressed
 *
 * @par
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
 * @class doc_note_sbmlreader_error_handling
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
   *
   * The libSBML SBMLReader objects offer methods for reading SBML in
   * XML form from files and text strings.
   */
  SBMLReader ();


  /**
   * Destroys this SBMLReader.
   */
  virtual ~SBMLReader ();


  /**
   * Reads an SBML document from a file.
   *
   * This method is identical to SBMLReader::readSBMLFromFile(@if java String filename@endif).
   *
   * If the file named @p filename does not exist or its content is not
   * valid SBML, one or more errors will be logged with the SBMLDocument
   * object returned by this method.  Callers can use the methods on
   * SBMLDocument such as SBMLDocument::getNumErrors() and
   * SBMLDocument::getError(@if java long n@endif) to get the errors.  The object returned by
   * SBMLDocument::getError(@if java long n@endif) is an SBMLError object, and it has methods to
   * get the error code, category, and severity level of the problem, as
   * well as a textual description of the problem.  The possible severity
   * levels range from informational messages to fatal errors; see the
   * documentation for SBMLError for more information.
   *
   * If the file @p filename could not be read, the file-reading error will
   * appear first.  The error code @if clike (a value drawn from the enumeration
   * #XMLErrorCode_t) @endif@~ can provide a clue about what happened.  For example,
   * a file might be unreadable (either because it does not actually exist
   * or because the user does not have the necessary access priviledges to
   * read it) or some sort of file operation error may have been reported
   * by the underlying operating system.  Callers can check for these
   * situations using a program fragment such as the following:
   * @if clike
 @verbatim
 SBMLReader reader;
 SBMLDocument* doc  = reader.readSBMLFromFile(filename);
 
 if (doc->getNumErrors() > 0)
 {
   if (doc->getError(0)->getErrorId() == XMLError::XMLFileUnreadable)
   {
     // Handle case of unreadable file here.
   } 
   else if (doc->getError(0)->getErrorId() == XMLError::XMLFileOperationError)
   {
     // Handle case of other file operation error here.
   }
   else
   {
     // Handle other cases -- see error codes defined in XMLErrorCode_t
     // for other possible cases to check.
   }
 }
 @endverbatim
 @endif@if java
 @verbatim
 SBMLReader reader = new SBMLReader();
 SBMLDocument doc  = reader.readSBMLFromFile(filename);
 
 if (doc.getNumErrors() > 0)
 {
     if (doc.getError(0).getErrorId() == libsbmlConstants.XMLFileUnreadable)
     {
         // Handle case of unreadable file here.
     } 
     else if (doc.getError(0).getErrorId() == libsbmlConstants.XMLFileOperationError)
     {
         // Handle case of other file operation error here.
     }
     else
     {
         // Handle other error cases.
     }
 }
 @endverbatim
 @endif@if python
 @verbatim
 reader = SBMLReader()
 doc    = reader.readSBMLFromFile(filename)
 
 if doc.getNumErrors() > 0:
   if doc.getError(0).getErrorId() == libsbml.XMLFileUnreadable:
     # Handle case of unreadable file here.
   elif doc.getError(0).getErrorId() == libsbml.XMLFileOperationError:
     # Handle case of other file error here.
   else:
     # Handle other error cases here.
   
 @endverbatim
 @endif@if csharp
 @verbatim
 SBMLReader reader = new SBMLReader();
 SBMLDocument doc = reader.readSBMLFromFile(filename);

 if (doc.getNumErrors() > 0)
 {
     if (doc.getError(0).getErrorId() == libsbmlcs.libsbml.XMLFileUnreadable)
     {
          // Handle case of unreadable file here.
     }
     else if (doc.getError(0).getErrorId() == libsbmlcs.libsbml.XMLFileOperationError)
     {
          // Handle case of other file operation error here.
     }
     else
     {
          // Handle other cases -- see error codes defined in XMLErrorCode_t
          // for other possible cases to check.
     }
  }
 @endverbatim
 @endif@~
   *
   * @copydetails doc_sbmlreader_if_compressed
   *
   * @copydetails doc_config_for_reading_zipped_files
   *
   * @param filename the name or full pathname of the file to be read.
   *
   * @return a pointer to the SBMLDocument created from the SBML content.
   *
   * @copydetails doc_note_sbmlreader_error_handling
   *
   * @see SBMLError
   * @see SBMLDocument
   */
  SBMLDocument* readSBML (const std::string& filename);


  /**
   * Reads an SBML document from a file.
   *
   * This method is identical to SBMLReader::readSBML(@if java String filename@endif).
   *
   * If the file named @p filename does not exist or its content is not
   * valid SBML, one or more errors will be logged with the SBMLDocument
   * object returned by this method.  Callers can use the methods on
   * SBMLDocument such as SBMLDocument::getNumErrors() and
   * SBMLDocument::getError(@if java long n@endif) to get the errors.  The object returned by
   * SBMLDocument::getError(@if java long n@endif) is an SBMLError object, and it has methods to
   * get the error code, category, and severity level of the problem, as
   * well as a textual description of the problem.  The possible severity
   * levels range from informational messages to fatal errors; see the
   * documentation for SBMLError for more information.
   *
   * If the file @p filename could not be read, the file-reading error will
   * appear first.  The error code @if clike (a value drawn from the enumeration
   * #XMLErrorCode_t)@endif@~ can provide a clue about what happened.  For example,
   * a file might be unreadable (either because it does not actually exist
   * or because the user does not have the necessary access priviledges to
   * read it) or some sort of file operation error may have been reported
   * by the underlying operating system.  Callers can check for these
   * situations using a program fragment such as the following:
   * @if clike
 @verbatim
 SBMLReader* reader = new SBMLReader();
 SBMLDocument* doc  = reader.readSBML(filename);
 
 if (doc->getNumErrors() > 0)
 {
   if (doc->getError(0)->getErrorId() == XMLError::FileUnreadable)
   {
     // Handle case of unreadable file here.
   } 
   else if (doc->getError(0)->getErrorId() == XMLError::FileOperationError)
   {
     // Handle case of other file operation error here.
   }
   else
   {
     // Handle other cases -- see error codes defined in XMLErrorCode_t
     // for other possible cases to check.
   }
 }
 @endverbatim
 @endif@if java
 @verbatim
 SBMLReader reader = new SBMLReader();
 SBMLDocument doc  = reader.readSBMLFromFile(filename);
 
 if (doc.getNumErrors() > 0)
 {
     if (doc.getError(0).getErrorId() == libsbmlConstants.XMLFileUnreadable)
     {
         // Handle case of unreadable file here.
     } 
     else if (doc.getError(0).getErrorId() == libsbmlConstants.XMLFileOperationError)
     {
         // Handle case of other file operation error here.
     }
     else
     {
         // Handle other error cases.
     }
 }
 @endverbatim
 @endif@if python
 @verbatim
 reader = SBMLReader()
 doc    = reader.readSBMLFromFile(filename)
 
 if doc.getNumErrors() > 0:
   if doc.getError(0).getErrorId() == libsbml.XMLFileUnreadable:
     # Handle case of unreadable file here.
   elif doc.getError(0).getErrorId() == libsbml.XMLFileOperationError:
     # Handle case of other file error here.
   else:
     # Handle other error cases here.
   
 @endverbatim
 @endif@~
   *
   * @copydetails doc_sbmlreader_if_compressed
   *
   * @copydetails doc_config_for_reading_zipped_files
   *
   * @param filename the name or full pathname of the file to be read.
   *
   * @return a pointer to the SBMLDocument created from the SBML content.
   *
   * @copydetails doc_note_sbmlreader_error_handling
   *
   * @see SBMLError
   * @see SBMLDocument
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
   * SBMLReader::readSBML(@if java String filename@endif)
   * for an example of code for testing the returned error code.
   *
   * @param xml a string containing a full SBML model
   *
   * @return a pointer to the SBMLDocument created from the SBML content.
   *
   * @note When using this method to read an SBMLDocument that uses 
   * the SBML L3 Hierarchical Model Composition package (comp) the
   * document location cannot be set automatically. Thus, if the model
   * contains references to ExternalModelDefinitions, it will be necessary
   * to manually set the document URI location (setLocationURI) in order 
   * to facilitate resolving these models.
   *
   * @see SBMLReader::readSBML(@if java String filename@endif)
   */
  SBMLDocument* readSBMLFromString (const std::string& xml);


  /**
   * Static method; returns @c true if this copy of libSBML supports
   * <i>gzip</I> and <i>zip</i> format compression.
   *
   * @return @c true if libSBML has been linked with the <i>zlib</i>
   * library, @c false otherwise.
   *
   * @copydetails doc_note_static_methods
   *
   * @see @if clike hasBzip2() @else SBMLReader::hasBzip2() @endif@~
   */
  static bool hasZlib();


  /**
   * Static method; returns @c true if this copy of libSBML supports
   * <i>bzip2</i> format compression.
   *
   * @return @c true if libSBML is linked with the <i>bzip2</i>
   * libraries, @c false otherwise.
   *
   * @copydetails doc_note_static_methods
   *
   * @see @if clike hasZlib() @else SBMLReader::hasZlib() @endif@~
   */
  static bool hasBzip2();


protected:
  /** @cond doxygenLibsbmlInternal */

  /**
   * Used by readSBML() and readSBMLFromString().
   *
   * @if notcpp @htmlinclude warn-default-args-in-docs.html @endif@~
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
 * Creates a new SBMLReader_t and returns it.  By default, XML Schema
 * validation is off.
 *
 * @memberof SBMLReader_t
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create (void);

/**
 * Frees the given SBMLReader_t.
 *
 * @memberof SBMLReader_t
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
 * @param sr The SBMLReader_t structure to use
 *
 * @param filename a string giving the path to a file where the XML
 * content is to be found.
 *
 * @copydetails doc_sbmlreader_if_compressed 
 * 
 * @copydetails doc_config_for_reading_zipped_files
 *
 * @return a pointer to the SBMLDocument read.
 *
 * @copydetails doc_note_sbmlreader_error_handling
 *
 * @memberof SBMLReader_t
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename);

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
 * @param sr The SBMLReader_t structure to use
 *
 * @param filename a string giving the path to a file where the XML
 * content is to be found.
 *
 * @return a pointer to the SBMLDocument read.
 *
 * @if conly
 * @memberof SBMLReader_t
 * @endif
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromFile (SBMLReader_t *sr, const char *filename);

/**
 * Reads an SBML document from the given @p xml XML string.
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
 * @param sr the SBMLReader_t structure to use
 *
 * @param xml the input xml string.
 *
 * @return a pointer to the SBMLDocument read.
 *
 * @note When using this method to read an SBMLDocument that uses 
 * the SBML L3 Hierarchical Model Composition package (comp) the
 * document location cannot be set automatically. Thus, if the model
 * contains references to ExternalModelDefinitions, it will be necessary
 * to manually set the document URI location (setLocationURI) in order 
 * to facilitate resolving these models.
 *
 * @memberof SBMLReader_t
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml);


/**
 * Predicate returning @c non-zero or @c zero depending on whether
 * underlying libSBML is linked with zlib or not.
 *
 * @return @c non-zero if libSBML is linked with zlib, @c zero otherwise.
 *
 * @memberof SBMLReader_t
 */
LIBSBML_EXTERN
int
SBMLReader_hasZlib ();


/**
 * Predicate returning @c non-zero or @c zero depending on whether
 * underlying libSBML is linked with bzip2 or not.
 *
 * @return @c non-zero if libSBML is linked with bzip2, @c zero otherwise.
 *
 * @memberof SBMLReader_t
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
 * SBMLDocument_t for more information about the error reporting mechanism.
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
 * If the filename ends with @em .gz, the file will be read as a @em gzip file.
 * Similary, if the filename ends with @em .zip or @em .bz2, the file will be
 * read as a @em zip or @em bzip2 file, respectively. Otherwise, the fill will be
 * read as an uncompressed file.
 * If the filename ends with @em .zip, only the first file in the archive will
 * be read if the zip archive contains two or more files.
 *
 * To read a gzip/zip file, underlying libSBML needs to be linked with zlib
 * at compile time. Also, underlying libSBML needs to be linked with bzip2 
 * to read a bzip2 file. File unreadable error will be logged if a compressed 
 * file name is given and underlying libSBML is not linked with the corresponding 
 * required library.
 * SBMLReader_hasZlib() and SBMLReader_hasBzip2() can be used to check 
 * whether libSBML is linked with each library.
 *
 * @return a pointer to the SBMLDocument read.
 *
 * @if conly
 * @memberof SBMLReader_t
 * @endif
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBML (const char *filename);


/**
 * Reads an SBML document from the given file @p filename.
 *
 * If @p filename does not exist, or it is not an SBML file, an error will
 * be logged in the error log of the SBMLDocument object returned by this
 * method.  Calling programs can inspect this error log to determine
 * the nature of the problem.  Please refer to the definition of
 * SBMLDocument_t for more information about the error reporting mechanism.
 *
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
 * If the filename ends with @em .gz, the file will be read as a @em gzip file.
 * Similary, if the filename ends with @em .zip or @em .bz2, the file will be
 * read as a @em zip or @em bzip2 file, respectively. Otherwise, the fill will be
 * read as an uncompressed file.
 * If the filename ends with @em .zip, only the first file in the archive will
 * be read if the zip archive contains two or more files.
 *
 * To read a gzip/zip file, underlying libSBML needs to be linked with zlib
 * at compile time. Also, underlying libSBML needs to be linked with bzip2 
 * to read a bzip2 file. File unreadable error will be logged if a compressed 
 * file name is given and underlying libSBML is not linked with the corresponding 
 * required library.
 * SBMLReader_hasZlib() and SBMLReader_hasBzip2() can be used to check 
 * whether libSBML is linked with each library.
 *
 * @return a pointer to the SBMLDocument read.
 *
 * @if conly
 * @memberof SBMLReader_t
 * @endif
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromFile (const char *filename);


/**
 * Reads an SBML document from the given XML string @p xml.
 *
 * If the string does not begin with XML declaration,
 *@verbatim
<?xml version='1.0' encoding='UTF-8'?>
@endverbatim
 *
 * an XML declaration string will be prepended.
 *
 * This method will report an error if the given string @p xml is not SBML.
 * The error will be logged in the error log of the SBMLDocument_t structure
 * returned by this method.  Calling programs can inspect this error log to
 * determine the nature of the problem.  Please refer to the definition of
 * SBMLDocument for more information about the error reporting mechanism.
 *
 * @return a pointer to the SBMLDocument_t read.
 *
 * @note When using this method to read an SBMLDocument that uses 
 * the SBML L3 Hierarchical Model Composition package (comp) the
 * document location cannot be set automatically. Thus, if the model
 * contains references to ExternalModelDefinitions, it will be necessary
 * to manually set the document URI location (setLocationURI) in order 
 * to facilitate resolving these models.
 *
 * @if conly
 * @memberof SBMLReader_t
 * @endif
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* SBMLReader_h */
