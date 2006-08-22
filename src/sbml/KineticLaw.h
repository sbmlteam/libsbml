/**
 * \file    KineticLaw.h
 * \brief   SBML KineticLaw
 * \author  Ben Bornstein
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


#ifndef KineticLaw_h
#define KineticLaw_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/Parameter.h>


class ASTNode;
class Parameter;
class SBMLVisitor;


class LIBSBML_EXTERN KineticLaw : public SBase
{
public:

  /**
   * Creates a new KineticLaw, optionally with its formula, timeUnits
   * and/or substanceUnits set.
   */
  KineticLaw (   const std::string& formula        = ""
               , const std::string& timeUnits      = ""
               , const std::string& substanceUnits = "" );

  /**
   * Copies this KineticLaw.
   */
  KineticLaw (const KineticLaw& rhs);

  /**
   * Destroys this KineticLaw.
   */
  virtual ~KineticLaw ();


  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this KineticLaw.
   */
  virtual SBase* clone () const;


  /**
   * @return the formula of this KineticLaw.
   */
  const std::string& getFormula () const;

  /**
   * @return the math of this KineticLaw.
   */
  const ASTNode* getMath () const;

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  const ListOfParameters* getListOfParameters () const;

  /**
   * @return the list of Parameters for this KineticLaw.
   */
  ListOfParameters* getListOfParameters ();


  /**
   * @return the timeUnits of this KineticLaw.
   */
  const std::string& getTimeUnits () const;

  /**
   * @return the substanceUnits of this KineticLaw.
   */
  const std::string& getSubstanceUnits () const;

  /**
   * @return the sboTerm of this KineticLaw as an integer.  If not set,
   * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
   * sboTerm to a zero-padded, seven digit string.
   */
  int getSBOTerm () const;


  /**
   * @return true if the formula (or equivalently the math) of this
   * KineticLaw has been set, false otherwise.
   */
  bool isSetFormula () const;

  /**
   * @return true if the math (or equivalently the formula) of this
   * KineticLaw has been set, false otherwise.
   */
  bool isSetMath () const;

  /**
   * @return true if the timeUnits of this KineticLaw has been set, false
   * otherwise.
   */
  bool isSetTimeUnits () const;

  /**
   * @return true if the substanceUnits of this KineticLaw has been set,
   * false otherwise.
   */
  bool isSetSubstanceUnits () const;

  /**
   * @return true if the sboTerm of this KineticLaw has been set, false
   * otherwise.
   */
  bool isSetSBOTerm () const;


  /**
   * Sets the formula of this KineticLaw to a copy of formula.
   */
  void setFormula (const std::string& formula);

  /**
   * Sets the math of this KineticLaw to a copy of the given ASTNode.
   */
  void setMath (const ASTNode* math);

  /**
   * Sets the timeUnits of this KineticLaw to a copy of sid.
   */
  void setTimeUnits (const std::string& sid);

  /**
   * Sets the substanceUnits of this KineticLaw to a copy of sid.
   */
  void setSubstanceUnits (const std::string& sid);

  /**
   * Sets the sboTerm field of this KineticLaw to value.
   */
  void setSBOTerm (int sboTerm);


  /**
   * Adds a copy of the given Parameter to this KineticLaw.
   */
  void addParameter (const Parameter* p);

  /**
   * Creates a new Parameter, adds it to this KineticLaw's list of
   * parameters and returns it.
   */
  Parameter* createParameter ();


  /**
   * @return the nth Parameter of this KineticLaw.
   */
  const Parameter* getParameter (unsigned int n) const;

  /**
   * @return the nth Parameter of this KineticLaw.
   */
  Parameter* getParameter (unsigned int n);

  /**
   * @return the Parameter in this kineticLaw with the given id or NULL if
   * no such Parameter exists.
   */
  const Parameter* getParameter (const std::string& sid) const;

  /**
   * @return the Parameter in this kineticLaw with the given id or NULL if
   * no such Parameter exists.
   */
  Parameter* getParameter (const std::string& sid);

  /**
   * @return the number of Parameters in this KineticLaw.
   */
  unsigned int getNumParameters () const;


  /**
   * Unsets the timeUnits of this KineticLaw.
   */
  void unsetTimeUnits ();

  /**
   * Unsets the substanceUnits of this KineticLaw.
   */
  void unsetSubstanceUnits ();

  /**
   * Unsets the sboTerm of this KineticLaw.
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
   * returns expected position of Kinetic Law in the reaction
   */
  virtual int getElementPosition(unsigned int) const;

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
  virtual SBase* createObject (XMLInputStream& stream);

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

  ListOfParameters  mParameters;
  std::string       mTimeUnits;
  std::string       mSubstanceUnits;
  int               mSBOTerm;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


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
 * @return a (deep) copy of this KineticLaw.
 */
LIBSBML_EXTERN
SBase_t *
KineticLaw_clone (const KineticLaw_t *kl);


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
 * @return the sboTerm of this KineticLaw as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the sboTerm
 * to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
KineticLaw_getSBOTerm (const KineticLaw_t *kl);


/**
 * @return true (non-zero) if the formula (or equivalently the math) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetFormula (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the math (or equivalently the formula) of
 * this KineticLaw has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetMath (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the timeUnits of this KineticLaw has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetTimeUnits (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the substanceUnits of this KineticLaw has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSubstanceUnits (const KineticLaw_t *kl);

/**
 * @return true (non-zero) if the substanceUnits of this KineticLaw has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
KineticLaw_isSetSBOTerm (const KineticLaw_t *kl);


/**
 * Sets the formula of this KineticLaw to a copy of formula.
 */
LIBSBML_EXTERN
void
KineticLaw_setFormula (KineticLaw_t *kl, const char *formula);

/**
 * Sets the math of this KineticLaw to a copy of the given ASTNode.
 */
LIBSBML_EXTERN
void
KineticLaw_setMath (KineticLaw_t *kl, const ASTNode_t *math);


/**
 * Sets the timeUnits of this KineticLaw to a copy of sid.
 */
LIBSBML_EXTERN
void
KineticLaw_setTimeUnits (KineticLaw_t *kl, const char *sid);

/**
 * Sets the substanceUnits of this KineticLaw to a copy of sid.
 */
LIBSBML_EXTERN
void
KineticLaw_setSubstanceUnits (KineticLaw_t *kl, const char *sid);

/**
 * Sets the sboTerm field of this KineticLaw to value.
 */
LIBSBML_EXTERN
void
KineticLaw_setSBOTerm (KineticLaw_t *kl, int sboTerm);


/**
 * Adds a copy of the given Parameter to this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_addParameter (KineticLaw_t *kl, const Parameter_t *p);

/**
 * Creates a new Parameter, adds it to this KineticLaw's list of
 * parameters and returns it.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_createParameter (KineticLaw_t *kl);


/**
 * @return the nth Parameter of this KineticLaw.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameter (KineticLaw_t *kl, unsigned int n);

/**
 * @return the Parameter in this kineticLaw with the given id or NULL if no
 * such Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
KineticLaw_getParameterById (KineticLaw_t *kl, const char *sid);

/**
 * @return the number of Parameters in this KineticLaw.
 */
LIBSBML_EXTERN
unsigned int
KineticLaw_getNumParameters (const KineticLaw_t *kl);


/**
 * Unsets the timeUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetTimeUnits (KineticLaw_t *kl);

/**
 * Unsets the substanceUnits of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSubstanceUnits (KineticLaw_t *kl);

/**
 * Unsets the sboTerm of this KineticLaw.
 */
LIBSBML_EXTERN
void
KineticLaw_unsetSBOTerm (KineticLaw_t *kl);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* KineticLaw_h */
