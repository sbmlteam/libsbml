/**
 * \file    SBMLWriter.h
 * \brief   Writes an SBML Document to file or in-memory string
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef SBMLWriter_h
#define SBMLWriter_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <iosfwd>
#include <string>


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
   * Sets the name of this program, i.e. the one about to write out the
   * SBMLDocument.  If the program name and version are set
   * (setProgramVersion()), the following XML comment, intended for human
   * consumption, will be written at the beginning of the document:
   *
   *   <!-- Created by <program name> version <program version>
   *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
   */
  void setProgramName (const std::string& name);

  /**
   * Sets the version of this program, i.e. the one about to write out the
   * SBMLDocument.  If the program version and name are set
   * (setProgramName()), the following XML comment, intended for human
   * consumption, will be written at the beginning of the document:
   *
   *   <!-- Created by <program name> version <program version>
   *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
   */
  void setProgramVersion (const std::string& version);

  /**
   * Writes the given SBML document to filename.
   *
   * @return true on success and false if the filename could not be opened
   * for writing.
   */
  bool write (const SBMLDocument* d, const std::string& filename);

  /**
   * Writes the given SBML document to the output stream.
   *
   * @return true on success and false if one of the underlying Xerces or
   * Expat components fail (rare).
   */
  bool write (const SBMLDocument* d, std::ostream& stream);

  /**
   * Writes the given SBML document to an in-memory string and returns a
   * pointer to it.  The string is owned by the caller and should be freed
   * (with free()) when no longer needed.
   *
   * @return the string on success and 0 if one of the underlying Xerces or
   * Expat components fail (rare).
   */
  char* writeToString (const SBMLDocument* d);


 protected:

  std::string mProgramName;
  std::string mProgramVersion;
};


#endif  /* __cplusplus */



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
 * Sets the name of this program, i.e. the one about to write out the
 * SBMLDocument.  If the program name and version are set
 * (setProgramVersion()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
void
SBMLWriter_setProgramName (SBMLWriter_t *sw, const char *name);

/**
 * Sets the version of this program, i.e. the one about to write out the
 * SBMLDocument.  If the program version and name are set
 * (setProgramName()), the following XML comment, intended for human
 * consumption, will be written at the beginning of the document:
 *
 *   <!-- Created by <program name> version <program version>
 *   on yyyy-MM-dd HH:mm with libsbml version <libsbml version>. -->
 */
LIBSBML_EXTERN
void
SBMLWriter_setProgramVersion (SBMLWriter_t *sw, const char *version);

/**
 * Writes the given SBML document to filename.
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.)
 */
LIBSBML_EXTERN
int
SBMLWriter_writeSBML ( SBMLWriter_t         *sw,
                       const SBMLDocument_t *d,
                       const char           *filename );

/**
 * Writes the given SBML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return the string on success and NULL if one of the underlying Xerces or
 * Expat components fail (rare).
 */
LIBSBML_EXTERN
char *
SBMLWriter_writeSBMLToString (SBMLWriter_t *sw, const SBMLDocument_t *d);


#endif  /* !SWIG */


/**
 * Writes the given SBML document to filename.  This convenience function
 * is functionally equivalent to:
 *
 *   SBMLWriter_writeSBML(SBMLWriter_create(), d, filename);
 *
 * @return non-zero on success and zero if the filename could not be opened
 * for writing.)
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
 * @return the string on success and NULL if one of the underlying Xerces
 * or Expat components fail (rare).
 */
LIBSBML_EXTERN
char *
writeSBMLToString (const SBMLDocument_t *d);


END_C_DECLS


#endif  /* SBMLWriter_h */
