/**
 * \file    SBMLDocument.h
 * \brief   Top-level container for all things SBML
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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


#ifndef SBMLDocument_h
#define SBMLDocument_h


#include "common/extern.h"


#ifdef __cplusplus


#include <iosfwd>

#include "SBase.h"
#include "util/List.h"


class Model;
class ParseMessage;
class SBMLVisitor;


class SBMLDocument: public SBase
{
public:

  /**
   * Creates a new SBMLDocument.  The SBML level defaults to 2 and version
   * defaults to 1.
   */
  LIBSBML_EXTERN
  SBMLDocument (unsigned int level = 2, unsigned int version = 1);

  /**
   * Destroys this SBMLDocument.
   */
  LIBSBML_EXTERN
  virtual ~SBMLDocument ();


  /**
   * Accepts the given SBMLVisitor.
   */
  LIBSBML_EXTERN
  void accept (SBMLVisitor& v) const;

  /**
   * Creates a new Model (optionally with its Id attribute set) inside this
   * SBMLDocument and returns it.  This covenience method is equivalent to:
   *
   *   setModel( Model() );
   */
  LIBSBML_EXTERN
  Model& createModel (const std::string& sid = "");

  /**
   * @return the level of this SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getLevel () const;

  /**
   * @return the version of this SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getVersion () const;

  /**
   * @return the Model associated with this SBMLDocument.
   */
  LIBSBML_EXTERN
  Model* getModel ();

  /**
   * @return the nth warning encountered during the parse of this
   * SBMLDocument or NULL if n > getNumWarnings() - 1.
   */
  LIBSBML_EXTERN
  ParseMessage* getWarning (unsigned int n);

  /**
   * @return the nth error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumErrors() - 1.
   */
  LIBSBML_EXTERN
  ParseMessage* getError (unsigned int n);

  /**
   * @return the nth fatal error encountered during the parse of this
   * SBMLDocument or NULL if n > getNumFatals() - 1.
   */
  LIBSBML_EXTERN
  ParseMessage* getFatal (unsigned int n);

  /**
   * @return the number of warnings encountered during the parse of this
   * SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getNumWarnings () const;

  /**
   * @return the number of errors encountered during the parse of this
   * SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getNumErrors () const;

  /**
   * @return the number of fatal errors encountered during the parse of
   * this SBMLDocument.
   */
  LIBSBML_EXTERN
  unsigned int getNumFatals () const;

  /**
   * Prints all warnings encountered during the parse of this SBMLDocument
   * to the given stream.  If no warnings have occurred, i.e.
   * getNumWarnings() == 0, no output will be sent to stream. The format of
   * the output is:
   *
   *   N Warning(s):
   *     line: (id) message
   */
  LIBSBML_EXTERN
  void printWarnings (std::ostream& stream);

  /**
   * Prints all errors encountered during the parse of this SBMLDocument to
   * the given stream.  If no errors have occurred, i.e.  getNumErrors() ==
   * 0, no output will be sent to stream. The format of the output is:
   *
   *   N Error(s):
   *     line: (id) message
   */
  LIBSBML_EXTERN
  void printErrors (std::ostream& stream);

  /**
   * Prints all fatals encountered during the parse of this SBMLDocument to
   * the given stream.  If no fatals have occurred, i.e.  getNumFatals() ==
   * 0, no output will be sent to stream. The format of the output is:
   *
   *   N Fatal(s):
   *     line: (id) message
   */
  LIBSBML_EXTERN
  void printFatals (std::ostream& stream);

  /**
   * Sets the level of this SBMLDocument to the given level number.  Valid
   * levels are currently 1 and 2.
   */
  LIBSBML_EXTERN
  void setLevel (unsigned int level);

  /**
   * Sets the version of this SBMLDocument to the given version number.
   * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
   */
  LIBSBML_EXTERN
  void setVersion (unsigned int version);

  /**
   * Sets the Model of this SBMLDocument to the given Model.
   * Any previously defined model is unset and freed.
   */
  LIBSBML_EXTERN
  void setModel (Model* m);

  /**
   * Performs a set of semantic consistency checks on the document.  Query
   * the results by calling getWarning(), getNumError(),and getNumFatal().
   *
   * @return the number of failed checks (errors) encountered.
   */
  LIBSBML_EXTERN
  unsigned int checkConsistency ();

  /**
   * @deprecated use checkConsistency() instead.
   */
  LIBSBML_EXTERN
  unsigned int validate ();


protected:

  unsigned int level;
  unsigned int version;

  List error;
  List fatal;
  List warning;

  Model* model;


  friend class SBMLFormatter;
  friend class SBMLHandler;
  friend class SBMLReader;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include <stdio.h>
#include "common/sbmlfwd.h"


/**
 * Creates a new SBMLDocument and returns a pointer to it.
 *
 * The SBML level defaults to 2 and version defaults to 1.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_create (void);

/**
 * Creates a new SBMLDocument with the given level and version.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_createWith (unsigned int level, unsigned int version);

/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   d->model = Model_create();
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d);

/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  The name field of this Model is set to a copy of sid.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModelWith (SBMLDocument_t *d, const char *sid);

/**
 * Frees the given SBMLDocument.
 */
LIBSBML_EXTERN
void
SBMLDocument_free (SBMLDocument_t *d);


/**
 * @return the level of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getLevel (const SBMLDocument_t *d);

/**
 * @return the version of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getVersion (const SBMLDocument_t *d);

/**
 * @return the nth warning encountered during the parse of this
 * SBMLDocument or NULL if n > getNumWarnings() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getWarning (SBMLDocument_t *d, unsigned int n);

/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n);

/**
 * @return the nth fatal error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumFatals() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getFatal (SBMLDocument_t *d, unsigned int n);

/**
 * @return the Model associated with this SBMLDocument.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_getModel (SBMLDocument_t *d);

/**
 * @return the number of warnings encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumWarnings (const SBMLDocument_t *d);

/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumErrors (const SBMLDocument_t *d);

/**
 * @return the number of fatal errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumFatals (const SBMLDocument_t *d);

/**
 * Prints all warnings encountered during the parse of this SBMLDocument to
 * the given stream.  If no warnings have occurred, i.e.
 * SBMLDocument_getNumWarnings(d) == 0, no output will be sent to
 * stream. The format of the output is:
 *
 *   N Warning(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument_printWarnings (SBMLDocument_t *d, FILE *stream);

/**
 * Prints all errors encountered during the parse of this SBMLDocument to
 * the given stream.  If no errors have occurred, i.e.
 * SBMLDocument_getNumErrors(d) == 0, no output will be sent to stream. The
 * format of the output is:
 *
 *   N Error(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument_printErrors (SBMLDocument_t *d, FILE *stream);

/**
 * Prints all fatals encountered during the parse of this SBMLDocument to
 * the given stream.  If no fatals have occurred, i.e.
 * SBMLDocument_getNumFatals(d) == 0, no output will be sent to stream. The
 * format of the output is:
 *
 *   N Fatal(s):
 *     line: (id) message
 */
LIBSBML_EXTERN
void
SBMLDocument_printFatals (SBMLDocument_t *d, FILE *stream);

/**
 * Sets the level of this SBMLDocument to the given level number.  Valid
 * levels are currently 1 and 2.
 */
LIBSBML_EXTERN
void
SBMLDocument_setLevel (SBMLDocument_t *d, unsigned int level);

/**
 * Sets the version of this SBMLDocument to the given version number.
 * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
 */
LIBSBML_EXTERN
void
SBMLDocument_setVersion (SBMLDocument_t *d, unsigned int version);

/**
 * Sets the Model of this SBMLDocument to the given Model.
 * Any previously defined model is unset and freed.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, Model_t *m);

/**
 * Performs a set of semantic consistency checks on the document.  Query
 * the results by calling getWarning(), getNumError(),and getNumFatal().
 *
 * @return the number of failed checks (errors) encountered.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_checkConsistency (SBMLDocument_t *d);

/**
 * @deprecated use SBMLDocument_checkConsistency() instead.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_validate (SBMLDocument_t *d);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* SBMLDocument_h */
