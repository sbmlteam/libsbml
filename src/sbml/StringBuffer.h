/**
 * Filename    : SringBuffer.h
 * Description : A growable buffer for creating character strings.
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-06-21
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


#ifndef StringBuffer_h
#define StringBuffer_h


#include "common.h"


BEGIN_C_DECLS


typedef struct
{
  unsigned long length;
  unsigned long capacity;

  char *buffer;
} StringBuffer_t;


/**
 * Creates a new StringBuffer and returns a pointer to it.
 */
LIBSBML_EXTERN
StringBuffer_t *
StringBuffer_create (unsigned long capacity);

/**
 * Frees the given StringBuffer.
 */
LIBSBML_EXTERN
void
StringBuffer_free (StringBuffer_t *sb);

/**
 * Resets (empties) this StringBuffer.  The current capacity remains
 * unchanged.
 */
LIBSBML_EXTERN
void
StringBuffer_reset (StringBuffer_t *sb);

/**
 * Appends the given string to this  StringBuffer.
 */
LIBSBML_EXTERN
void
StringBuffer_append (StringBuffer_t *sb, const char *s);

/**
 * Appends the given character to this StringBuffer.
 */
LIBSBML_EXTERN
void
StringBuffer_appendChar (StringBuffer_t *sb, char c);

/**
 * Appends a string representation of the given number to this StringBuffer
 * The function snprintf is used to do the conversion and currently n = 16;
 * i.e. the number will be truncated after 16 characters, regardless of the
 * buffer size.
 *
 * The format argument should be a printf conversion specifier, e.g. "%d",
 * "%f", "%g", etc.
 */
LIBSBML_EXTERN
void
StringBuffer_appendNumber (StringBuffer_t *sb, const char *format, ...);

/**
 * Appends a string representation of the given integer to this
 * StringBuffer.
 *
 * This function is equivalent to:
 *
 *   StringBuffer_appendNumber(sb, "%d", i);
 */
LIBSBML_EXTERN
void
StringBuffer_appendInt (StringBuffer_t *sb, long i);

/**
 * Appends a string representation of the given integer to this
 * StringBuffer.
 *
 * This function is equivalent to:
 *
 *   StringBuffer_appendNumber(sb, "%g", r);
 */
LIBSBML_EXTERN
void
StringBuffer_appendReal (StringBuffer_t *sb, double r);

/**
 * Doubles the capacity of this StringBuffer (if nescessary) until it can
 * hold at least n additional characters.
 *
 * Use this function only if you want fine-grained control of the
 * StringBuffer.  By default, the StringBuffer will automatically double
 * its capacity (as many times as needed) to accomodate an append
 * operation.
 */
LIBSBML_EXTERN
void
StringBuffer_ensureCapacity (StringBuffer_t *sb, unsigned long n);

/**
 * Grow the capacity of this StringBuffer by n characters.
 *
 * Use this function only if you want fine-grained control of the
 * StringBuffer.  By default, the StringBuffer will automatically double
 * its capacity (as many times as needed) to accomodate an append
 * operation.
 */
LIBSBML_EXTERN
void
StringBuffer_grow (StringBuffer_t *sb, unsigned long n);

/**
 * @return the underlying buffer contained in this StringBuffer.
 *
 * The buffer is not owned by the caller and should not be modified or
 * deleted.  The caller may take ownership of the buffer by freeing the
 * StringBuffer directly, e.g.:
 *
 *   char *buffer = StringBuffer_getBuffer(sb);
 *   safe_free(sb);
 *
 * This is more direct and efficient than:
 *
 *   char *buffer = StringBuffer_toString(sb);
 *   StringBuffer_free(sb);
 *
 * which creates a copy of the buffer and then destroys the original.
 */
LIBSBML_EXTERN
char *
StringBuffer_getBuffer (const StringBuffer_t *sb);

/**
 * @return the number of characters currently in this StringBuffer.
 */
LIBSBML_EXTERN
unsigned long
StringBuffer_length (const StringBuffer_t *sb);

/**
 * @return the number of characters this StringBuffer is capable of holding
 * before it will automatically double its storage capacity.
 */
LIBSBML_EXTERN
unsigned long
StringBuffer_capacity (const StringBuffer_t *sb);

/**
 * @return a copy of the string contained in this StringBuffer.
 *
 * The caller owns the copy and is responsible for freeing it.
 */
LIBSBML_EXTERN
char *
StringBuffer_toString (const StringBuffer_t *sb);


END_C_DECLS


#endif  /** StringBuffer_h **/
