/**
 * Filename    : SBMLDocument.h
 * Description : Top-level container for all things SBML
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-10-14
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef SBMLDocument_h
#define SBMLDocument_h


#include "common.h"
#include "List.h"
#include "ParseMessage.h"
#include "SBase.h"
#include "Model.h"


BEGIN_C_DECLS


/**
 * The SBMLDocument
 */
typedef struct
{
  SBASE_FIELDS;

  unsigned int level;
  unsigned int version;

  List_t *error;
  List_t *fatal;
  List_t *warning;

  Model_t *model;
} SBMLDocument_t;


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
SBMLDocument_getModel (const SBMLDocument_t *d);

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
 *   %d Warning(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience function to aid in debugging.  For example:
 * SBMLDocument_printWarnings(d, stdout).
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
 *   %d Error(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience function to aid in debugging.  For example:
 * SBMLDocument_printErrors(d, stdout).
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
 *   %d Fatal(s):
 *     Line %d, Col %d: %s
 *     ...
 *
 * This is a convenience function to aid in debugging.  For example:
 * SBMLDocument_printFatals(d, stdout).
 */
LIBSBML_EXTERN
void
SBMLDocument_printFatals (SBMLDocument_t *d, FILE *stream);


/**
 * Sets the Model of this SBMLDocument to the given Model.
 * Any previously defined model is unset and freed.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, Model_t *m);


END_C_DECLS


#endif  /** SBMLDocument_h **/
