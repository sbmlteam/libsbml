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


#include "List.h"
#include "Model.h"


#ifdef __cplusplus
extern "C" {
#endif


/**
 * SBMLDocuments contain three Lists of ParseMessages, one for each class
 * of messages that could be triggered during an XML parse: Warnings,
 * Errors and Fatal Errors.
 *
 * Each ParseMessage contains the message itself and the line and column
 * numbers of the XML entity that triggered the message.  If line or column
 * information is unavailable, -1 is used.
 */
typedef struct
{
  char *message;
  int  line;
  int  column;
} ParseMessage_t;


/**
 * The SBMLDocument
 */
typedef struct
{
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
 * The SBML document level and version are both 1.
 */
SBMLDocument_t *
SBMLDocument_create (void);

/**
 * Creates a new SBMLDocument with the given level and version.
 */
SBMLDocument_t *
SBMLDocument_createWith (unsigned int level, unsigned int version);

/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   d->model = Model_create();
 */
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d);

/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  The name field of this Model is set to a copy of sname.
 */
Model_t *
SBMLDocument_createModelWith (SBMLDocument_t *d, const char *sname);

/**
 * Frees the given SBMLDocument.
 */
void
SBMLDocument_free (SBMLDocument_t *d);

/**
 * @return the nth warning encountered during the parse of this
 * SBMLDocument or NULL if n > getNumWarnings() - 1.
 */
ParseMessage_t *
SBMLDocument_getWarning (SBMLDocument_t *d, unsigned int n);

/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
ParseMessage_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n);

/**
 * @return the nth fatal error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumFatals() - 1.
 */
ParseMessage_t *
SBMLDocument_getFatal (SBMLDocument_t *d, unsigned int n);

/**
 * @return the number of warnings encountered during the parse of this
 * SBMLDocument.
 */
unsigned int
SBMLDocument_getNumWarnings (SBMLDocument_t *d);

/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
unsigned int
SBMLDocument_getNumErrors (SBMLDocument_t *d);

/**
 * @return the number of fatal errors encountered during the parse of this
 * SBMLDocument.
 */
unsigned int
SBMLDocument_getNumFatals (SBMLDocument_t *d);


#ifdef __cplusplus
}
#endif


#endif  /** SBMLDocument_h **/
