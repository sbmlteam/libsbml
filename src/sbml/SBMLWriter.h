/**
 * @file    SBMLWriter.h
 * @brief   Writes an SBML Document to file or in-memory string
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
 * @class SBMLWriter
 * @brief Methods for writing SBML to files and text strings.
 *
 * @htmlinclude libsbml-not-sbml-warning.html
 *
 * The SBMLWriter class is the converse of SBMLReader, and provides the
 * main interface for serializing SBML models into XML and writing the
 * result to files and text strings.  The methods for writing SBML all take
 * an SBMLDocument object and a destination.  They return a boolean value
 * to indicate success or failure.
 */

#ifndef SBMLWriter_h
#define SBMLWriter_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <iosfwd>
#include <string>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLDocument;


class LIBSBML_EXTERN SBMLWriter
{
public:

  /**
   * Creates a new SBMLWriter.
   */
  SBMLWriter  ();


  /**
   * Destroys this SBMLWriter.
   */
  ~SBMLWriter ();


  /**
   * Sets the name of this program, i.e., the program that is about to
   * write out the SBMLDocument.
   *
   * If the program name and version are set (setProgramVersion()), the
   * following XML comment, intended for human consumption, will be written
   * at the beginning of the document:
   * @verbatim
   <!-- Created by <program name> version <program version>
   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
@endverbatim
   *
   * @param name the name of this program (where "this program" refers to
   * program in which libSBML is embedded, not libSBML itself!)
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   * 
   * @see setProgramVersion(const std::string& version)
   */
  int setProgramName (const std::string& name);


  /**
   * Sets the version of this program, i.e., the program that is about to
   * write out the SBMLDocument.
   *
   * If the program version and name are set (setProgramName()), the
   * following XML comment, intended for human consumption, will be written
   * at the beginning of the document:
   * @verbatim
   <!-- Created by <program name> version <program version>
   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
@endverbatim
   *
   * @param version the version of this program (where "this program"
   * refers to program in which libSBML is embedded, not libSBML itself!)
   *
   * @return integer value indicating success/failure of the
   * function.  @if clike The value is drawn from the
   * enumeration #OperationReturnValues_t. @endif The possible values
   * returned by this function are:
   * @li @link OperationReturnValues_t#LIBSBML_OPERATION_SUCCESS LIBSBML_OPERATION_SUCCESS @endlink
   *
   * @see setProgramName(const std::string& name)
   */
  int setProgramVersion (const std::string& version);


  /**
   * Writes the given SBML document to filename.
   *
   * If the given filename ends with the suffix @c ".gz" (for example, @c
   * "myfile.xml.gz"), libSBML assumes the caller wants the file to be
   * written compressed in @em gzip.  Similarly, if the given filename ends
   * with @c ".zip" or @c ".bz2", libSBML assumes the caller wants the file
   * to be compressed in @em zip or @em bzip2 format (respectively).  Files
   * whose names lack these suffixes will be written uncompressed.
   * <em>Special considerations for the zip format</em>: If the given
   * filename ends with @c ".zip", the file placed in the zip archive will
   * have the suffix @c ".xml" or @c ".sbml".  For example, the file in
   * the zip archive will be named @c "test.xml" if the given filename is
   * @c "test.xml.zip" or @c "test.zip".  Similarly, the filename in the
   * archive will be @c "test.sbml" if the given filename is @c
   * "test.sbml.zip".
   *
   * @note To write a gzip/zip file, libSBML needs to be configured and
   * linked with the <a href="http://www.zlib.net/">zlib</a> library at
   * compile time.  It also needs to be linked with the <a
   * href="">bzip2</a> library to write files in @em bzip2 format.  (Both
   * of these are the default configurations for libSBML.)  Errors about
   * unreadable files will be logged and this method will return @c false
   * if a compressed filename is given and libSBML was @em not linked with
   * the corresponding required library.
   *
   * @note SBMLReader::hasZlib() and SBMLReader::hasBzip2() can be used to
   * check whether libSBML has been linked with each library.
   *
   * @param d the SBML document to be written
   *
   * @param filename the name or full pathname of the file where the SBML
   * is to be written. 
   *
   * @return @c true on success and @c false if the filename could not be
   * opened for writing.
   */
  bool writeSBML (const SBMLDocument* d, const std::string& filename);


  /**
   * Writes the given SBML document to the output stream.
   *
   * @param d the SBML document to be written
   *
   * @param stream the stream object where the SBML is to be written.
   *
   * @return @c true on success and @c false if one of the underlying
   * parser components fail (rare).
   */
  bool writeSBML (const SBMLDocument* d, std::ostream& stream);


  /** @cond doxygen-libsbml-internal */

  /**
   * Writes the given SBML document to an in-memory string and returns a
   * pointer to it.
   *
   * The string is owned by the caller and should be freed (with @c free())
   * when no longer needed.
   *
   * @param d the SBML document to be written
   *
   * @return the string on success and @c 0 if one of the underlying parser
   * components fail.
   */
  char* writeToString (const SBMLDocument* d);

  /** @endcond */

  /**
   * Writes the given SBML document to filename.
   *
   * If the given filename ends with the suffix @c ".gz" (for example, @c
   * "myfile.xml.gz"), libSBML assumes the caller wants the file to be
   * written compressed in @em gzip.  Similarly, if the given filename ends
   * with @c ".zip" or @c ".bz2", libSBML assumes the caller wants the file
   * to be compressed in @em zip or @em bzip2 format (respectively).  Files
   * whose names lack these suffixes will be written uncompressed.
   * <em>Special considerations for the zip format</em>: If the given
   * filename ends with @c ".zip", the file placed in the zip archive will
   * have the suffix @c ".xml" or @c ".sbml".  For example, the file in
   * the zip archive will be named @c "test.xml" if the given filename is
   * @c "test.xml.zip" or @c "test.zip".  Similarly, the filename in the
   * archive will be @c "test.sbml" if the given filename is @c
   * "test.sbml.zip".
   *
   * @note To write a gzip/zip file, libSBML needs to be configured and
   * linked with the <a href="http://www.zlib.net/">zlib</a> library at
   * compile time.  It also needs to be linked with the <a
   * href="">bzip2</a> library to write files in @em bzip2 format.  (Both
   * of these are the default configurations for libSBML.)  Errors about
   * unreadable files will be logged and this method will return @c false
   * if a compressed filename is given and libSBML was @em not linked with
   * the corresponding required library.
   *
   * @note SBMLReader::hasZlib() and SBMLReader::hasBzip2() can be used to
   * check whether libSBML has been linked with each library.
   *
   * @param d the SBML document to be written
   *
   * @param filename the name or full pathname of the file where the SBML
   * is to be written. 
   *
   * @return @c true on success and @c false if the filename could not be
   * opened for writing.
   */
  bool writeSBMLToFile (const SBMLDocument* d, const std::string& filename);


  /**
   * Writes the given SBML document to an in-memory string and returns a
   * pointer to it.
   *
   * The string is owned by the caller and should be freed (with @c free())
   * when no longer needed.
   *
   * @param d the SBML document to be written
   *
   * @return the string on success and @c 0 if one of the underlying parser
   * components fail.
   */
  char* writeSBMLToString (const SBMLDocument* d);


  /**
   * Predicate returning @c true if
   * underlying libSBML is linked with zlib.
   *
   * LibSBML supports reading and writing files compressed with either
   * bzip2 or zip/gzip compression.  The facility depends on libSBML having
   * been compiled with the necessary support libraries.  This method
   * allows a calling program to inquire whether that is the case for the
   * copy of libSBML it is running.
   *
   * @return @c true if libSBML is linked with zlib, @c false otherwise.
   */
  static bool hasZlib();


  /**
   * Predicate returning @c true if
   * underlying libSBML is linked with bzip2.
   *
   * LibSBML supports reading and writing files compressed with either
   * bzip2 or zip/gzip compression.  The facility depends on libSBML having
   * been compiled with the necessary support libraries.  This method
   * allows a calling program to inquire whether that is the case for the
   * copy of libSBML it is running.
   *
   * @return @c true if libSBML is linked with bzip2, @c false otherwise.
   */
  static bool hasBzip2();


 protected:
  /** @cond doxygen-libsbml-internal */

  std::string mProgramName;
  std::string mProgramVersion;

  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */


LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


#ifndef SWIG


/**
 * Creates a new SBMLWriter and returns a pointer to it.
 */
LIBSBML_EXTERN
SBMLWriter_t *
SBMLWriter_create (void);

/**
 * Frees the given SBMLWriter.
 */
LIBSBML_EXTERN
void
SBMLWriter_free (SBMLWriter_t *sw);

/**
 * Sets the name of this program. i.\ e.\ the one about to write out the
 * SBMLDocument.  If the program name and version are set
 * (setProgramVersion()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
int
SBMLWriter_setProgramName (SBMLWriter_t *sw, const char *name);

/**
 * Sets the version of this program. i.\ e.\ the one about to write out the
 * SBMLDocument.  If the program version and name are set
 * (setProgramName()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
int
SBMLWriter_setProgramVersion (SBMLWriter_t *sw, const char *version);

/**
 * Writes the given SBML document to filename.
 *
 * If the given filename ends with the suffix @c ".gz" (for example, @c
 * "myfile.xml.gz"), libSBML assumes the caller wants the file to be
 * written compressed in @em gzip.  Similarly, if the given filename ends
 * with @c ".zip" or @c ".bz2", libSBML assumes the caller wants the file
 * to be compressed in @em zip or @em bzip2 format (respectively).  Files
 * whose names lack these suffixes will be written uncompressed.
 * <em>Special considerations for the zip format</em>: If the given
 * filename ends with @c ".zip", the file placed in the zip archive will
 * have the suffix @c ".xml" or @c ".sbml".  For example, the file in
 * the zip archive will be named @c "test.xml" if the given filename is
 * @c "test.xml.zip" or @c "test.zip".  Similarly, the filename in the
 * archive will be @c "test.sbml" if the given filename is @c
 * "test.sbml.zip".
 *
 * @note To write a gzip/zip file, libSBML needs to be configured and
 * linked with the <a href="http://www.zlib.net/">zlib</a> library at
 * compile time.  It also needs to be linked with the <a
 * href="">bzip2</a> library to write files in @em bzip2 format.  (Both
 * of these are the default configurations for libSBML.)  Errors about
 * unreadable files will be logged and this method will return @c false
 * if a compressed filename is given and libSBML was @em not linked with
 * the corresponding required library.
 *
 * @note SBMLReader::hasZlib() and SBMLReader::hasBzip2() can be used to
 * check whether libSBML has been linked with each library.
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.
 */
LIBSBML_EXTERN
int
SBMLWriter_writeSBML ( SBMLWriter_t         *sw,
                       const SBMLDocument_t *d,
                       const char           *filename );

LIBSBML_EXTERN
int
SBMLWriter_writeSBMLToFile ( SBMLWriter_t         *sw,
                       const SBMLDocument_t *d,
                       const char           *filename );
/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return the string on success and @c NULL if one of the underlying parser
 * components fail (rare).
 */
LIBSBML_EXTERN
char *
SBMLWriter_writeSBMLToString (SBMLWriter_t *sw, const SBMLDocument_t *d);


/**
 * Predicate returning @c non-zero or @c zero depending on whether
 * libSBML is linked with zlib at compile time.
 *
 * @return @c non-zero if zlib is linked, @c zero otherwise.
 */
LIBSBML_EXTERN
int
SBMLWriter_hasZlib ();


/**
 * Predicate returning @c non-zero or @c zero depending on whether
 * libSBML is linked with bzip2 at compile time.
 *
 * @return @c non-zero if bzip2 is linked, @c zero otherwise.
 */
LIBSBML_EXTERN
int
SBMLWriter_hasBzip2 ();

#endif  /* !SWIG */


/**
 * Writes the given SBML document to filename.  This convenience function
 * is functionally equivalent to:
 *
 *   SBMLWriter_writeSBML(SBMLWriter_create(), d, filename);
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.
 */
LIBSBML_EXTERN
int
writeSBML (const SBMLDocument_t *d, const char *filename);


/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.  This convenience function is
 * functionally equivalent to:
 *
 *   SBMLWriter_writeSBMLToString(SBMLWriter_create(), d);
 *
 * @return the string on success and @c NULL if one of the underlying parser
 * components fail (rare).
 */
LIBSBML_EXTERN
char *
writeSBMLToString (const SBMLDocument_t *d);


LIBSBML_EXTERN
int
writeSBMLToFile (const SBMLDocument_t *d, const char *filename);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* SBMLWriter_h */
