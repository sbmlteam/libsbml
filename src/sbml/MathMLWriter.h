/**
 * \file    MathMLWriter.h
 * \brief   Writes a MathML Document to file or in-memory string
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


#ifndef MathMLWriter_h
#define MathMLWriter_h


#include "extern.h"
#include "sbmlfwd.h"


#ifndef SWIG


#ifdef __cplusplus


#include <iosfwd>


/**
 * Writes the given MathML document to an ostream.
 *
 * @return 1 on success and 0 on failure
 */
LIBSBML_EXTERN
int
writeMathMLToStream (MathMLDocument_t *d, std::ostream & o);


#endif  /* __cplusplus */


BEGIN_C_DECLS


/**
 * Writes the given MathML document to filename.
 *
 * @return 1 on success and 0 on failure (e.g., if filename could not be
 * opened for writing or the MathMLWriter character encoding is invalid).*
 */
LIBSBML_EXTERN
int
writeMathML (MathMLDocument_t *d, const char *filename);


/**
 * Writes the given MathML document to an in-memory string and returns a
 * pointer to it.  The string is owned by the caller and should be freed
 * (with free()) when no longer needed.
 *
 * @return NULL on failure
 */
LIBSBML_EXTERN
char *
writeMathMLToString (MathMLDocument_t *d);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* MathMLWriter_h */
