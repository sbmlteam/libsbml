/**
 * \file    Delay.h
 * \brief   SBML Delay
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


#ifndef Delay_h
#define Delay_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>


class ASTNode;
class SBMLVisitor;


class LIBSBML_EXTERN Delay : public SBase
{
public:

  /**
   * Creates a new Delay, optionally with its formula set.
   */
  Delay (   const std::string& formula        = "");

  /**
   * Copies this Delay.
   */
  Delay (const Delay& rhs);

  /**
   * Destroys this Delay.
   */
  virtual ~Delay ();


  /**
   * Assignment operator
   */
  Delay& operator=(const Delay& orig);

  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Delay.
   */
  virtual SBase* clone () const;


  /**
   * @return the formula of this Delay.
   */
  const std::string& getFormula () const;

  /**
   * @return the math of this Delay.
   */
  const ASTNode* getMath () const;


  /**
   * @return true if the formula (or equivalently the math) of this
   * Delay has been set, false otherwise.
   */
  bool isSetFormula () const;

  /**
   * @return true if the math (or equivalently the formula) of this
   * Delay has been set, false otherwise.
   */
  bool isSetMath () const;

  /**
   * Sets the formula of this Delay to a copy of formula.
   */
  void setFormula (const std::string& formula);

  /**
   * Sets the math of this Delay to a copy of the given ASTNode.
   */
  void setMath (const ASTNode* math);


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

};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


/**
 * Creates a new Delay and returns a pointer to it.
 */
LIBSBML_EXTERN
Delay_t *
Delay_create (void);

/**
 * Creates a new Delay with the given formula, timeUnits and
 * substanceUnits and returns a pointer to it.  This convenience function
 * is functionally equivalent to:
 *
 *   Delay_t *d = Delay_create();
 *   Delay_setFormula(kl, formula);
 *   Delay_setTimeUnits(kl, timeUnits);
 *   ...;
 */
LIBSBML_EXTERN
Delay_t *
Delay_createWith ( const char *formula);

/**
 * Frees the given Delay.
 */
LIBSBML_EXTERN
void
Delay_free (Delay_t *d);

/**
 * @return a (deep) copy of this Delay.
 */
LIBSBML_EXTERN
SBase_t *
Delay_clone (const Delay_t *d);


/**
 * @return the formula of this Delay.
 */
LIBSBML_EXTERN
const char *
Delay_getFormula (const Delay_t *d);

/**
 * @return the math of this Delay.
 */
LIBSBML_EXTERN
const ASTNode_t *
Delay_getMath (const Delay_t *d);

/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this Delay has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetFormula (const Delay_t *d);

/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this Delay has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Delay_isSetMath (const Delay_t *d);

/**
 * Sets the formula of this Delay to a copy of formula.
 */
LIBSBML_EXTERN
void
Delay_setFormula (Delay_t *d, const char *formula);

/**
 * Sets the math of this Delay to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
Delay_setMath (Delay_t *d, const ASTNode_t *math);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Delay_h */
