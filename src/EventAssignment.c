/**
 * Filename    : EventAssignment.c
 * Description : SBML EventAssignment
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-03
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


#include "sbml/EventAssignment.h"


/**
 * Creates a new EventAssignment and returns a pointer to it.
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_create (void)
{
  EventAssignment_t *ea;


  ea = (EventAssignment_t *) safe_calloc(1, sizeof(EventAssignment_t));
  SBase_init((SBase_t *) ea, SBML_EVENT_ASSIGNMENT);

  return ea;
}


/**
 * Creates a new EventAssignment with the given variable and math and
 * returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ea = EventAssignment_create();
 *   EventAssignment_setId(ea, variable);
 *   EventAssignment_setMath(ea, math);
 */
LIBSBML_EXTERN
EventAssignment_t *
EventAssignment_createWith (const char *variable, ASTNode_t* math)
{
  EventAssignment_t *ea = EventAssignment_create();


  EventAssignment_setVariable( ea, variable );
  EventAssignment_setMath    ( ea, math     );

  return ea;
}


/**
 * Frees the given EventAssignment.
 */
LIBSBML_EXTERN
void
EventAssignment_free (EventAssignment_t *ea)
{
  if (ea == NULL) return;


  SBase_clear((SBase_t *) ea);

  safe_free(ea->variable);
  ASTNode_free(ea->math);

  safe_free(ea);
}


/**
 * @return the variable of this EventAssignment.
 */
LIBSBML_EXTERN
const char *
EventAssignment_getVariable (const EventAssignment_t *ea)
{
  return ea->variable;
}


/**
 * @return the math of this EventAssignment.
 */
LIBSBML_EXTERN
const ASTNode_t *
EventAssignment_getMath (const EventAssignment_t *ea)
{
  return ea->math;
}


/**
 * @return 1 if the variable of this EventAssignment has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetVariable (const EventAssignment_t *ea)
{
  return (ea->variable != NULL);
}


/**
 * @return 1 if the math of this EventAssignment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
EventAssignment_isSetMath (const EventAssignment_t *ea)
{
  return (ea->math != NULL);
}


/**
 * Sets the id of this EventAssignment to a copy of sid.
 */
LIBSBML_EXTERN
void
EventAssignment_setVariable (EventAssignment_t *ea, const char *sid)
{
  if (ea->variable == sid) return;


  if (ea->variable != NULL)
  {
    safe_free(ea->variable);
  }

  ea->variable = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the math of this EventAssignment to the given ASTNode.
 *
 * The node <b>is not copied</b> and this EventAssignment <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * EventAssignment_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
EventAssignment_setMath (EventAssignment_t *ea, ASTNode_t *math)
{
  if (ea->math == math) return;


  if (ea->math != NULL)
  {
    ASTNode_free(ea->math);
  }

  ea->math = math;
}
