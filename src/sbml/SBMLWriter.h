/**
 * \file    SBMLWriter.h
 * \brief   Writes an SBML Document to file or in-memory string
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#ifndef SBMLWriter_h
#define SBMLWriter_h


#include "common/extern.h"


#ifdef __cplusplus


#include <iosfwd>
#include <string>


class SBMLDocument;


class SBMLWriter
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
  LIBSBML_EXTERN
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
  LIBSBML_EXTERN
  void setProgramVersion (const std::string& version);

  /**
   * Writes the given SBML document to filename.
   *
   * @return true on success and false if the filename could not be opened
   * for writing.
   */
  LIBSBML_EXTERN
  bool write (const SBMLDocument& d, const std::string& filename);

  /**
   * Writes the given SBML document to the output stream.
   *
   * @return true on success and false if one of the underlying Xerces or
   * Expat components fail (rare).
   */
  LIBSBML_EXTERN
  bool write (const SBMLDocument& d, std::ostream& stream);

  /**
   * Writes the given SBML document to an in-memory string and returns a
   * pointer to it.  The string is owned by the caller and should be freed
   * (with free()) when no longer needed.
   *
   * @return the string on success and 0 if one of the underlying Xerces or
   * Expat components fail (rare).
   */
  LIBSBML_EXTERN
  char* writeToString (const SBMLDocument& d);


 protected:

  std::string mProgramName;
  std::string mProgramVersion;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new SBMLWriter and returns a pointer to it.
 *
 * By default the character encoding is UTF-8
 * (CHARACTER_ENCODING_UTF_8).
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
