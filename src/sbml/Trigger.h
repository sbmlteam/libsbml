/**
 * \file    Trigger.h
 * \brief   SBML Trigger
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef Trigger_h
#define Trigger_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN Trigger : public SBase
{
public:

  /**
   * Creates a new Trigger, optionally with its formula set.
   */
  Trigger (   const std::string& formula        = "");

  /**
   * Copies this Trigger.
   */
  Trigger (const Trigger& rhs);

  /**
   * Destroys this Trigger.
   */
  virtual ~Trigger ();


  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Trigger.
   */
  virtual SBase* clone () const;


  /**
   * @return the formula of this Trigger.
   */
  const std::string& getFormula () const;

  /**
   * @return the math of this Trigger.
   */
  const ASTNode* getMath () const;


  /**
   * @return the sboTerm of this Trigger as an integer.  If not set,
   * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
   * sboTerm to a zero-padded, seven digit string.
   */
  int getSBOTerm () const;


  /**
   * @return true if the formula (or equivalently the math) of this
   * Trigger has been set, false otherwise.
   */
  bool isSetFormula () const;

  /**
   * @return true if the math (or equivalently the formula) of this
   * Trigger has been set, false otherwise.
   */
  bool isSetMath () const;

  /**
   * @return true if the sboTerm of this Trigger has been set, false
   * otherwise.
   */
  bool isSetSBOTerm () const;


  /**
   * Sets the formula of this Trigger to a copy of formula.
   */
  void setFormula (const std::string& formula);

  /**
   * Sets the math of this Trigger to a copy of the given ASTNode.
   */
  void setMath (const ASTNode* math);


  /**
   * Sets the sboTerm field of this Trigger to value.
   */
  void setSBOTerm (int sboTerm);


  /**
   * Unsets the sboTerm of this Trigger.
   */
  void unsetSBOTerm ();


  /**
   * Sets the parent SBMLDocument of this SBML object.
   */
  virtual void setSBMLDocument (SBMLDocument* d);


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;


protected:

  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
//  virtual SBase* createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream) const;


  mutable std::string  mFormula;
  mutable ASTNode*     mMath;

  int               mSBOTerm;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new Trigger and returns a pointer to it.
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_create (void);

/**
 * Creates a new Trigger with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   Trigger_t *t = Trigger_create();
 *   Trigger_setFormula(kl, formula);
 *   Trigger_setTimeUnits(kl, timeUnits);
 *   ...;
 */
LIBSBML_EXTERN
Trigger_t *
Trigger_createWith ( const char *formula);

/**
 * Frees the given Trigger.
 */
LIBSBML_EXTERN
void
Trigger_free (Trigger_t *t);

/**
 * @return a (deep) copy of this Trigger.
 */
LIBSBML_EXTERN
SBase_t *
Trigger_clone (const Trigger_t *t);


/**
 * @return the formula of this Trigger.
 */
LIBSBML_EXTERN
const char *
Trigger_getFormula (const Trigger_t *t);

/**
 * @return the math of this Trigger.
 */
LIBSBML_EXTERN
const ASTNode_t *
Trigger_getMath (const Trigger_t *t);

/**
 * @return the sboTerm of this Trigger as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the sboTerm
 * to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
Trigger_getSBOTerm (const Trigger_t *t);


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this Trigger has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetFormula (const Trigger_t *t);

/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this Trigger has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetMath (const Trigger_t *t);

/**
 * @return true (non-zero) if the substanceUnits of this Trigger has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Trigger_isSetSBOTerm (const Trigger_t *t);


/**
 * Sets the formula of this Trigger to a copy of formula.
 */
LIBSBML_EXTERN
void
Trigger_setFormula (Trigger_t *t, const char *formula);

/**
 * Sets the math of this Trigger to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Trigger_setMath (Trigger_t *t, const ASTNode_t *math);


/**
 * Sets the sboTerm field of this Trigger to value.
 */
LIBSBML_EXTERN
void
Trigger_setSBOTerm (Trigger_t *t, int sboTerm);


/**
 * Unsets the sboTerm of this Trigger.
 */
LIBSBML_EXTERN
void
Trigger_unsetSBOTerm (Trigger_t *t);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Trigger_h */
