/**
 * \file    CompartmentVolumeRule.h
 * \brief   SBML CompartmentVolumeRule
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


#ifndef CompartmentVolumeRule_h
#define CompartmentVolumeRule_h


#include "extern.h"
#include "RuleType.h"


#ifdef __cplusplus


#include <string>
#include "AssignmentRule.h"


class SBMLVisitor;


class CompartmentVolumeRule : public AssignmentRule
{
public:

  /**
   * Creates a new CompartmentVolumeRule.
   */
  LIBSBML_EXTERN
  CompartmentVolumeRule ();

  /**
   * Creates a new CompartmentVolumeRule with its compartment, formula and
   * (optionally) type attributes set.
   */
  LIBSBML_EXTERN
  CompartmentVolumeRule
  (
      const std::string&  compartment
    , const std::string&  formula 
    , RuleType_t          type = RULE_TYPE_SCALAR
  );

  /**
   * Destroys this CompartmentVolumeRule.
   */
  LIBSBML_EXTERN
  virtual ~CompartmentVolumeRule ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next Rule
   * (if available).
   */
  LIBSBML_EXTERN
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return the compartment of this CompartmentVolumeRule.
   */
  LIBSBML_EXTERN
  const std::string& getCompartment () const;

  /**
   * @return true if the compartment of this CompartmentVolumeRule has been
   * set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetCompartment () const;

  /**
   * Sets the compartment of this CompartmentVolumeRule to a copy of sname.
   */
  LIBSBML_EXTERN
  void setCompartment (const std::string& sname);


protected:

  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "sbmlfwd.h"


/**
 * Creates a new CompartmentVolumeRule and returns a pointer to it.
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
CompartmentVolumeRule_create (void);

/**
 * Creates a new CompartmentVolumeRule with the given formula, type and
 * compartment and returns a pointer to it.  This convenience function is
 * functionally equivalent to:
 *
 *   CompartmentVolumeRule_t *cvr = CompartmentVolumeRule_create();
 *   Rule_setFormula((Rule_t *) cvr, formula);
 *   AssignmentRule_setType((AssignmentRule_t *) cvr, type);
 *   ...;
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
CompartmentVolumeRule_createWith ( const char *formula,
                                   RuleType_t type,
                                   const char *compartment );

/**
 * Frees the given CompartmentVolumeRule.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule_free (CompartmentVolumeRule_t *cvr);


/**
 * @return the compartment of this CompartmentVolumeRule.
 */
LIBSBML_EXTERN
const char *
CompartmentVolumeRule_getCompartment (const CompartmentVolumeRule_t *cvr);

/**
 * @return 1 if the compartment of this CompartmentVolumeRule has been set,
 * 0 otherwise.
 */
LIBSBML_EXTERN
int
CompartmentVolumeRule_isSetCompartment (const CompartmentVolumeRule_t *cvr);

/**
 * Sets the compartment of this CompartmentVolumeRule to a copy of sname.
 */
LIBSBML_EXTERN
void
CompartmentVolumeRule_setCompartment ( CompartmentVolumeRule_t *cvr,
                                       const char *sname );


END_C_DECLS


#endif  /* !SWIG */
#endif  /* CompartmentVolumeRule_h */
