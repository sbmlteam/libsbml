/**
 * \file    MathMLDocument.h
 * \brief   Top-level container for all things MathML
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


#ifndef MathMLDocument_h
#define MathMLDocument_h


#include "extern.h"


#ifdef __cplusplus


class ASTNode;


class MathMLDocument
{
public:

  /**
   * Creates a new MathMLDocument.
   */
  LIBSBML_EXTERN
  MathMLDocument ();

  /**
   * Destroys this MathMLDocument.
   */
  LIBSBML_EXTERN
  virtual ~MathMLDocument ();


  /**
   * @return the an abstract syntax tree (AST) representation of the math
   * in this MathMLDocument.
   */
  LIBSBML_EXTERN
  const ASTNode* getMath () const;

  /**
   * @return true if the math of this MathMLDocument has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetMath () const;

  /**
   * Sets the math of this MathMLDocument to the given ASTNode.
   *
   * The node <b>is not copied</b> and this MathMLDocument <b>takes
   * ownership</b> of it; i.e. subsequent calls to this function or a call
   * to MathMLDocument_free() will free the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setMath (ASTNode* math);


protected:

  ASTNode* math;


  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "sbmlfwd.h"


/**
 * Creates a new MathMLDocument and returns a pointer to it.
 */
LIBSBML_EXTERN
MathMLDocument_t *
MathMLDocument_create (void);

/**
 * Frees the given MathMLDocument.
 */
LIBSBML_EXTERN
void
MathMLDocument_free (MathMLDocument_t *d);


/**
 * @return the an abstract syntax tree (AST) representation of the math in
 * this MathMLDocument.
 */
LIBSBML_EXTERN
const ASTNode_t *
MathMLDocument_getMath (const MathMLDocument_t *d);

/**
 * @return 1 if the math of this MathMLDocument has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
MathMLDocument_isSetMath (const MathMLDocument_t *d);

/**
 * Sets the math of this MathMLDocument to the given ASTNode.
 *
 * The node <b>is not copied</b> and this MathMLDocument <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * MathMLDocument_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
MathMLDocument_setMath (MathMLDocument_t *d, ASTNode_t *math);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* MathMLDocument_h */
