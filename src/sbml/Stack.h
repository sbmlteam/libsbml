/**
 * Filename    : Stack.h
 * Description : Generic (void *) Stack for C structs on the heap
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-10-21
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


#ifndef Stack_h
#define Stack_h


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  long  sp;
  long  capacity;
  void  **stack;
} Stack_t;


/**
 * Creates a new List and returns a pointer to it.
 */
Stack_t *
Stack_create (int capacity);


/**
 * Free the given Stack.
 *
 * This function does not free individual Stack items.  It frees only the
 * Stack_t structure.
 */
void
Stack_free (Stack_t *s);


/**
 * Pushes item onto the top of the Stack.
 */
void
Stack_push (Stack_t *s, void *item);

/**
 * @return (and removes) the top item on the Stack.
 */
void *
Stack_pop (Stack_t *s);

/**
 * @return (but does not remove) the top item on the Stack.
 */
void *
Stack_peek (Stack_t *s);

/**
 * @return (but does not remove) the nth item from the top of the Stack,
 * starting at zero, i.e. Stack_peekAt(0) is equivalent to Stack_peek().
 * If n is out of range (n < 0 or n >= Stack_size(s)) returns NULL.
 */
void *
Stack_peekAt (Stack_t *s, int n);

/**
 * @return the number of items currently on the Stack.
 */
int
Stack_size (Stack_t *s);

/**
 * @return the number of items the Stack is capable of holding before it
 * will (automatically) double its storage capacity.
 */
int
Stack_capacity (Stack_t *s);


#ifdef __cplusplus
}
#endif


#endif  /** Stack_h **/
