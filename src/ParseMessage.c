/**
 * Filename    : ParseMessage.c
 * Description : Stores error message encountered during an SBML parse
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-04-16
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#include "sbml/common.h"
#include "sbml/ParseMessage.h"


/**
 * Creates a new ParseMessage and returns a pointer to it.
 */
ParseMessage_t *
ParseMessage_create (void)
{
  return (ParseMessage_t *) safe_calloc(1, sizeof(ParseMessage_t));
}



/**
 * Creates a new ParseMessage reporting that message occurred at the given
 * line and column.
 */
ParseMessage_t *
ParseMessage_createWith ( const char   *message,
                          unsigned int  line,
                          unsigned int  column )
{
  ParseMessage_t *pm = ParseMessage_create();


  pm->message = safe_strdup(message);
  pm->line    = line;
  pm->column  = column;

  return pm;
}


/**
 * Frees the given ParseMessage.
 */
void
ParseMessage_free (ParseMessage_t *pm)
{
  if (pm == NULL) return;


  safe_free(pm->message);
  safe_free(pm);
}


/**
 * @return the message text of this ParseMessage.
 */
LIBSBML_EXTERN
const char *
ParseMessage_getMessage (ParseMessage_t *pm)
{
  return pm->message;
}


/**
 * @return the line number where this ParseMessage ocurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getLine (ParseMessage_t *pm)
{
  return pm->line;
}


/**
 * @return the column number where this ParseMessage occurred.
 */
LIBSBML_EXTERN
unsigned int
ParseMessage_getColumn (ParseMessage_t *pm)
{
  return pm->column;
}
