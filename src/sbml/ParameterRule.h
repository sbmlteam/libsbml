/**
 * \file    ParameterRule.h
 * \brief   SBML ParameterRule
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


#ifndef ParameterRule_h
#define ParameterRule_h


#include "extern.h"
#include "RuleType.h"


#ifdef __cplusplus


#include <string>
#include "AssignmentRule.h"


class SBMLVisitor;


class ParameterRule : public AssignmentRule
{
public:

  /**
   * Creates a new ParameterRule.
   */
  LIBSBML_EXTERN
  ParameterRule ();

  /**
   * Creates a new ParameterRule with its name, formula and (optionally)
   * type attributes set.
   */
  LIBSBML_EXTERN
  ParameterRule
  (
      const std::string&  name
    , const std::string&  formula 
    , RuleType_t          type = RULE_TYPE_SCALAR
  );

  /**
   * Destroys this ParameterRule.
   */
  LIBSBML_EXTERN
  virtual ~ParameterRule ();


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
   * @return the (Parameter) name for this ParameterRule.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the units for this ParameterRule.
   */
  LIBSBML_EXTERN
  const std::string& getUnits () const;

  /**
   * @return true if the (Parameter) name for this ParameterRule has been
   * set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if the units for this ParameterRule has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetUnits () const;

  /**
   * Sets the (Parameter) name for this ParameterRule to a copy of sname.
   */
  LIBSBML_EXTERN
  void setName (const std::string& sname);

  /**
   * Sets the units for this ParameterRule to a copy of sname.
   */
  LIBSBML_EXTERN
  void setUnits (const std::string& sname);

  /**
   * Unsets the units for this ParameterRule.
   */
  LIBSBML_EXTERN
  void unsetUnits ();


protected:

  std::string units;

  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "sbmlfwd.h"


/**
 * Creates a new ParameterRule and returns a pointer to it.
 */
LIBSBML_EXTERN
ParameterRule_t *
ParameterRule_create (void);

/**
 * Creates a new ParameterRule with the given formula, type, and name and
 * and returns a pointer to it.  This convenience function is functionally
 * equivalent to:
 *
 *   ParameterRule_t *pr = ParameterRule_create();
 *   Rule_setFormula((Rule_t *) pr, formula); scr->type = type; ...;
 */
LIBSBML_EXTERN
ParameterRule_t *
ParameterRule_createWith ( const char *formula,
                           RuleType_t type,
                           const char *name );

/**
 * Frees the given ParameterRule.
 */
LIBSBML_EXTERN
void
ParameterRule_free (ParameterRule_t *pr);


/**
 * @return the (Parameter) name for this ParameterRule.
 */
LIBSBML_EXTERN
const char *
ParameterRule_getName (const ParameterRule_t *pr);

/**
 * @return the units for this ParameterRule.
 */
LIBSBML_EXTERN
const char *
ParameterRule_getUnits (const ParameterRule_t *pr);


/**
 * @return 1 if the (Parameter) name for this ParameterRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
ParameterRule_isSetName (const ParameterRule_t *pr);

/**
 * @return 1 if the units for this ParameterRule has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
ParameterRule_isSetUnits (const ParameterRule_t *pr);


/**
 * Sets the (Parameter) name for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule_setName (ParameterRule_t *pr, const char *sname);

/**
 * Sets the units for this ParameterRule to a copy of sname.
 */
LIBSBML_EXTERN
void
ParameterRule_setUnits (ParameterRule_t *pr, const char *sname);


/**
 * Unsets the units for this ParameterRule.  This is equivalent to:
 * safe_free(pr->units); pr->units = NULL;
 */
LIBSBML_EXTERN
void
ParameterRule_unsetUnits (ParameterRule_t *pr);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* ParameterRule_h */
