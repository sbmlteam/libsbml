/**
 * Filename    : SBMLDocument.c
 * Description : Top-level container for all things SBML
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/SBMLConvert.h"
#include "sbml/SBMLDocument.h"


/**
 * Creates a new SBMLDocument and returns a pointer to it.
 *
 * The SBML level defaults to 2 and version defaults to 1.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_create (void)
{
  SBMLDocument_t *d;


  d = (SBMLDocument_t *) safe_calloc(1, sizeof(SBMLDocument_t));

  SBase_init((SBase_t *) d, SBML_DOCUMENT);

  d->level   = 2;
  d->version = 1;

  d->warning = List_create();
  d->error   = List_create();
  d->fatal   = List_create();

  return d;
}


/**
 * Creates a new SBMLDocument with the given level and version.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLDocument_createWith (unsigned int level, unsigned int version)
{
  SBMLDocument_t *d = SBMLDocument_create();


  d->level   = level;
  d->version = version;

  return d;
}


/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   d->model = Model_create();
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModel (SBMLDocument_t *d)
{
  d->model = Model_create();


  return d->model;
}


/**
 * Creates a new Model inside this SBMLDocument and returns a pointer to
 * it.  The name field of this Model is set to a copy of sid.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_createModelWith (SBMLDocument_t *d, const char *sid)
{
  d->model = Model_createWith(sid);


  return d->model;
}


/**
 * Frees the given SBMLDocument.
 */
LIBSBML_EXTERN
void
SBMLDocument_free (SBMLDocument_t *d)
{
  if (d == NULL) return;

  List_freeItems( d->warning, ParseMessage_free, ParseMessage_t );
  List_freeItems( d->error  , ParseMessage_free, ParseMessage_t );
  List_freeItems( d->fatal  , ParseMessage_free, ParseMessage_t );

  List_free( d->warning );
  List_free( d->error   );
  List_free( d->fatal   );

  Model_free(d->model);

  safe_free(d->notes);
  safe_free(d->annotation);
  safe_free(d);
}


/**
 * @return the level of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getLevel (const SBMLDocument_t *d)
{
  return d->level;
}


/**
 * @return the version of this SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getVersion (const SBMLDocument_t *d)
{
  return d->version;
}


/**
 * @return the Model associated with this SBMLDocument.
 */
LIBSBML_EXTERN
Model_t *
SBMLDocument_getModel (const SBMLDocument_t *d)
{
  return d->model;
}


/**
 * @return the nth warning encountered during the parse of this
 * SBMLDocument or NULL if n > getNumWarnings() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getWarning (SBMLDocument_t *d, unsigned int n)
{
  return (ParseMessage_t *) List_get(d->warning, n);
}


/**
 * @return the nth error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumErrors() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getError (SBMLDocument_t *d, unsigned int n)
{
  return (ParseMessage_t *) List_get(d->error, n);
}


/**
 * @return the nth fatal error encountered during the parse of this
 * SBMLDocument or NULL if n > getNumFatals() - 1.
 */
LIBSBML_EXTERN
ParseMessage_t *
SBMLDocument_getFatal (SBMLDocument_t *d, unsigned int n)
{
  return (ParseMessage_t *) List_get(d->fatal, n);
}


/**
 * @return the number of warnings encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumWarnings (const SBMLDocument_t *d)
{
  return List_size(d->warning);
}


/**
 * @return the number of errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumErrors (const SBMLDocument_t *d)
{
  return List_size(d->error);
}


/**
 * @return the number of fatal errors encountered during the parse of this
 * SBMLDocument.
 */
LIBSBML_EXTERN
unsigned int
SBMLDocument_getNumFatals (const SBMLDocument_t *d)
{
  return List_size(d->fatal);
}


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
SBMLDocument_printWarnings (SBMLDocument_t *d, FILE *stream)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  if ((size = SBMLDocument_getNumWarnings(d)) > 0)
  {
    printf("%d Warning(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getWarning(d, n);
      printf("  Line %d, Col %d: %s\n", pm->line, pm->column, pm->message);
    }

    printf("\n");
  }
}


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
SBMLDocument_printErrors (SBMLDocument_t *d, FILE *stream)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  if ((size = SBMLDocument_getNumErrors(d)) > 0)
  {
    printf("%d Error(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getError(d, n);
      printf("  Line %d, Col %d: %s\n", pm->line, pm->column, pm->message);
    }

    printf("\n");
  }
}


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
SBMLDocument_printFatals (SBMLDocument_t *d, FILE *stream)
{
  unsigned int   n, size;
  ParseMessage_t *pm;


  if ((size = SBMLDocument_getNumFatals(d)) > 0)
  {
    printf("%d Fatal(s):\n", size);

    for (n = 0; n < size; n++)
    {
      pm = SBMLDocument_getFatal(d, n);
      printf("  Line %d, Col %d: %s\n", pm->line, pm->column, pm->message);
    }

    printf("\n");
  }
}


/**
 * Sets the level of this SBMLDocument to the given level number.  Valid
 * levels are currently 1 and 2.
 */
LIBSBML_EXTERN
void
SBMLDocument_setLevel (SBMLDocument_t *d, unsigned int level)
{
  if (d->level == 1 && level == 2)
  {
    SBML_convertToL2( (SBase_t *) d );
  }
  else
  {
    d->level = level;

    if (d->level == 2)
    {
      d->version = 1;
    }
  }
}


/**
 * Sets the version of this SBMLDocument to the given version number.
 * Valid versions are currently 1 and 2 for SBML L1 and 1 for SBML L2.
 */
LIBSBML_EXTERN
void
SBMLDocument_setVersion (SBMLDocument_t *d, unsigned int version)
{
  d->version = version;
}


/**
 * Sets the Model of this SBMLDocument to the given Model.
 * Any previously defined model is unset and freed.
 */
LIBSBML_EXTERN
void
SBMLDocument_setModel (SBMLDocument_t *d, Model_t *m)
{
  if (d->model == m) return;


  if (d->model != NULL)
  {
    Model_free(d->model);
  }

  d->model = m;
}
