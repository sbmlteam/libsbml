/**
 * \file    KineticLaw.h
 * \brief   SBML KineticLaw
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


#ifndef KineticLaw_h
#define KineticLaw_h


#include "common/extern.h"


#ifdef __cplusplus


#include <string>
#include "SBase.h"
#include "ListOf.h"


class ASTNode;
class Parameter;
class SBMLVisitor;


class KineticLaw : public SBase
{
public:

  /**
   * Creates a new KineticLaw, optionally with its formula, timeUnits
   * and/or substanceUnits set.
   */
  LIBSBML_EXTERN
  KineticLaw (   const std::string& formula        = ""
               , const std::string& timeUnits      = ""
               , const std::string& substanceUnits = "" );

  /**
   * Destroys this KineticLaw.
   */
  LIBSBML_EXTERN
  virtual ~KineticLaw ();


  /**
   * Accepts the given SBMLVisitor.
   */
  LIBSBML_EXTERN
  void accept (SBMLVisitor& v) const;

  /**
   * @return the formula of this KineticLaw.
   */
  LIBSBML_EXTERN
  const std::string& getFormula () const;

  /**
   * @return the math of this KineticLaw.
   */
  LIBSBML_EXTERN
  const ASTNode* getMath () const;

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  LIBSBML_EXTERN
  ListOf& getListOfParameters ();

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfParameters () const;

  /**
   * @return the timeUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  const std::string& getTimeUnits () const;

  /**
   * @return the substanceUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  const std::string& getSubstanceUnits () const;

  /**
   * @return true if the formula (or equivalently the math) of this
   * KineticLaw has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetFormula () const;

  /**
   * @return true if the math (or equivalently the formula) of this
   * KineticLaw has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetMath () const;

  /**
   * @return true if the timeUnits of this KineticLaw has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetTimeUnits () const;

  /**
   * @return true if the substanceUnits of this KineticLaw has been set,
   * false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetSubstanceUnits () const;

  /**
   * Sets the formula of this KineticLaw to a copy of string.
   */
  LIBSBML_EXTERN
  void setFormula (const std::string& formula);

  /**
   * Sets the math of this KineticLaw to the given ASTNode.
   *
   * The node <b>is not copied</b> and this KineticLaw <b>takes
   * ownership</b> of it; i.e. subsequent calls to this method or deleting
   * this KineticLaw will delete the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setMath (ASTNode* math);

  /**
   * This method is no longer necessary.  LibSBML now keeps formula strings
   * and math ASTs synchronized automatically.  The method is kept around
   * for backward compatibility (and is used internally).
   */
  LIBSBML_EXTERN
  void setFormulaFromMath () const;

  /**
   * This method is no longer necessary.  LibSBML now keeps formula strings
   * and math ASTs synchronized automatically.  The method is kept around
   * for backward compatibility (and is used internally).
   */
  LIBSBML_EXTERN
  void setMathFromFormula () const;

  /**
   * Sets the timeUnits of this KineticLaw to a copy of sname.
   */
  LIBSBML_EXTERN
  void setTimeUnits (const std::string& sname);

  /**
   * Sets the substanceUnits of this KineticLaw to a copy of sname.
   */
  LIBSBML_EXTERN
  void setSubstanceUnits (const std::string& sname);

  /**
   * Adds the given Parameter to this KineticLaw.
   */
  LIBSBML_EXTERN
  void addParameter (Parameter& p);

  /**
   * @return the nth Parameter of this KineticLaw.
   */
  LIBSBML_EXTERN
  Parameter* getParameter (unsigned int n) const;

  /**
   * @return the Parameter in this kineticLaw with the given id or NULL if no
   * such Parameter exists.
   */
  LIBSBML_EXTERN
  Parameter* getParameter (const std::string& sid) const;

  /**
   * @return the number of Parameters in this KineticLaw.
   */
  LIBSBML_EXTERN
  unsigned int getNumParameters () const;

  /**
   * Unsets the timeUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  void unsetTimeUnits ();

  /**
   * Unsets the substanceUnits of this KineticLaw.
   */
  LIBSBML_EXTERN
  void unsetSubstanceUnits ();


protected:

  mutable std::string formula;
  mutable ASTNode*    math;

  ListOf      parameter;
  std::string timeUnits;
  std::string substanceUnits;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new KineticLaw and returns a pointer to it.
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_create (void);

/**
 * Creates a new KineticLaw with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   KineticLaw_t *kl = KineticLaw_create();
 *   KineticLaw_setFormula(kl, formula);
 *   KineticLaw_setTimeUnits(kl, timeUnits);
 *   ...;
 */
LIBSBML_EXTERN
KineticLaw_t *
KineticLaw_createWith ( const char *formula,
                        const char *timeUnits,
                        const char *substanceUnits );

/**
 * Frees the given KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_free (KineticLaw_t *kl);


/**
 * @return the formula of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getFormula (const KineticLaw_t *kl);

/**
 * @return the math of this KineticLaw.
 */
LIBSBML_EXTERN
const ASTNode_t *
KineticLaw_getMath (const KineticLaw_t *kl);

/**
 * @return the list of Parameters for this KineticLaw.
 */
LIBSBML_EXTERN
ListOf_t *
KineticLaw_getListOfParameters (KineticLaw_t *kl);

/**
 * @return the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getTimeUnits (const KineticLaw_t *kl);

/**
 * @return the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
const char *
KineticLaw_getSubstanceUnits (const KineticLaw_t *kl);


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl);

/**
 * @return true if the math (or equivalently the formula) of this
 * KineticLaw has been set, false otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl);

/**
 * @return 1 if the timeUnits of this KineticLaw has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl);

/**
 * @return 1 if the substanceUnits of this KineticLaw has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl);


/**
 * Sets the formula of this KineticLaw to a copy of string.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *string);

/**
 * Sets the math of this KineticLaw to the given ASTNode.
 *
 * The node <b>is not copied</b> and this KineticLaw <b>takes ownership</b>
 * of it; i.e. subsequent calls to this function or a call to
 * KineticLaw_free() will free the ASTNode (and any child nodes).
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, ASTNode_t *math);


/**
 * This function is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The function is kept around
 * for backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
KineticLaw_setFormulaFromMath (const KineticLaw_t *kl);

/**
 * This function is no longer necessary.  LibSBML now keeps formula strings
 * and math ASTs synchronized automatically.  The function is kept around
 * for backward compatibility (and is used internally).
 */
LIBSBML_EXTERN
void
KineticLaw_setMathFromFormula (const KineticLaw_t *kl);


/**
 * Sets the timeUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sname);

/**
 * Sets the substanceUnits of this KineticLaw to a copy of sname.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sname);


/**
 * Adds the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, Parameter_t *p);

/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (const KineticLaw_t *kl, unsigned int n);

/**
 * @return the Parameter in this kineticLaw with the given id or NULL if no such
 * Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (const KineticLaw_t *kl, const char *sid);

/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl);


/**
 * Unsets the timeUnits of this KineticLaw.  This is equivalent to:
 * safe_free(kl->timeUnits); kl->timeUnits = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl);

/**
 * Unsets the substanceUnits of this KineticLaw.  This is equivalent to:
 * safe_free(kl->substanceUnits); kl->substanceUnits = NULL;
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* KineticLaw_h */
