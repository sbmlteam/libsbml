/**
 * \file    AlgebraicRule.h
 * \brief   SBML AlgebraicRule
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


#ifndef AlgebraicRule_h
#define AlgebraicRule_h


#include "common/extern.h"


#ifdef __cplusplus


#include "Rule.h"


class SBMLVisitor;


class AlgebraicRule : public Rule
{
public:

  /**
   * Creates a new AlgebraicRule, optionally with its formula attribute
   * set.
   */
  LIBSBML_EXTERN
  AlgebraicRule (const std::string& formula = "");

  /**
   * Creates a new AlgebraicRule with its math attribute set.
   */
  LIBSBML_EXTERN
  AlgebraicRule (ASTNode* math);

  /**
   * Destroys this AlgebraicRule.
   */
  LIBSBML_EXTERN
  virtual ~AlgebraicRule ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  LIBSBML_EXTERN
  virtual bool accept (SBMLVisitor& v) const;


protected:

  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new AlgebraicRule and returns a pointer to it.
 */
LIBSBML_EXTERN
AlgebraicRule_t *
AlgebraicRule_create (void);

/**
 * Creates a new AlgebraicRule with the given formula and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   AlgebraicRule_t *ar = AlgebraicRule_create();
 *   Rule_setFormula((Rule_t *) ar, formula);
 */
LIBSBML_EXTERN
AlgebraicRule_t *
AlgebraicRule_createWith (const char *formula);

/**
 * Creates a new AlgebraicRule with the given math and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   AlgebraicRule_t *ar = AlgebraicRule_create();
 *   Rule_setMath((Rule_t *) ar, math);
 *
 * The node <b>is not copied</b> and this AlgebraicRule <b>takes
 * ownership</b> of it; i.e. subsequent calls to this function or a call to
 * AlgebraicRule_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
AlgebraicRule_t *
AlgebraicRule_createWithMath (ASTNode_t *math);

/**
 * Frees the given AlgebraicRule.
 */
LIBSBML_EXTERN
void
AlgebraicRule_free (AlgebraicRule_t *ar);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* AlgebraicRule_h */
