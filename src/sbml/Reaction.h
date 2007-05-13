/**
 * @file    Reaction.h
 * @brief   SBML Reaction
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef Reaction_h
#define Reaction_h


#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/SpeciesReference.h>

class KineticLaw;
class SBMLVisitor;


class LIBSBML_EXTERN Reaction : public SBase
{
public:

  /**
   * Creates a new Reaction, optionally with its id, KineticLaw, and
   * reversible attributes set.
   */
  Reaction (  const std::string&  sid        = ""
            , const KineticLaw*   kl         = 0
            , bool                reversible = true );

  /**
   * Destroys this Reaction.
   */
  virtual ~Reaction ();


  /**
   * Copy constructor. Creates a copy of this Reaction.
   */
  Reaction (const Reaction& orig);

  /**
   * Assignment operator
   */
  Reaction& operator=(const Reaction& rhs);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * Reaction (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Reaction.
   */
  virtual SBase* clone () const;

  /**
   * Initializes the fields of this Reaction to their defaults:
   *
   *   - reversible = true
   *   - fast       = false  (L1 only)
   */
  void initDefaults ();


  /**
   * @return the KineticLaw of this Reaction.
   */
  const KineticLaw* getKineticLaw () const;

  /**
   * @return the KineticLaw of this Reaction.
   */
  KineticLaw* getKineticLaw ();

  /**
   * @return the reversible status of this Reaction.
   */
  bool getReversible () const;

  /**
   * @return the fast status of this Reaction.
   */
  bool getFast () const;

  /**
   * @return true if the KineticLaw of this Reaction has been set, false
   * otherwise.
   */
  bool isSetKineticLaw () const;

  /**
   * @return true if the fast status of this Reaction has been set, false
   * otherwise.
   *
   * In L1, fast is optional with a default of false, which means it is
   * effectively always set.  In L2, however, fast is optional with no
   * default value, so it may or may not be set to a specific value.
   */
  bool isSetFast () const;

  /**
   * Sets the KineticLaw of this Reaction to a copy of the given
   * KineticLaw.
   */
  void setKineticLaw (const KineticLaw* kl);

  /**
   * Sets the reversible status of this Reaction to value.
   */
  void setReversible (bool value);

  /**
   * Sets the fast status of this Reaction to value.
   */
  void setFast (bool value);

  /**
   * Unsets the KineticLaw of this Reaction.
   */
  void unsetKineticLaw ();

  /**
   * Unsets the fast status of this Reaction.
   *
   * In L1, fast is optional with a default of false, which means it is
   * effectively always set.  In L2, however, fast is optional with no
   * default value, so it may or may not be set to a specific value.
   */
  void unsetFast ();

  /**
   * Adds a copy of the given reactant (SpeciesReference) to this Reaction.
   */
  void addReactant (const SpeciesReference* sr);

  /**
   * Adds a copy of the given product (SpeciesReference) to this Reaction.
   */
  void addProduct (const SpeciesReference* sr);

  /**
   * Adds a copy of the given modifier (ModifierSpeciesReference) to this
   * Reaction.
   */
  void addModifier (const ModifierSpeciesReference* msr);


  /**
   * Creates a new SpeciesReference, adds it to this Reaction's list of
   * reactants and returns it.
   */
  SpeciesReference* createReactant ();

  /**
   * Creates a new SpeciesReference, adds it to this Reaction's list of
   * products and returns it.
   */
  SpeciesReference* createProduct ();

  /**
   * Creates a new ModifierSpeciesReference, adds it to this Reaction's
   * list of modifiers and returns it.
   */
  ModifierSpeciesReference* createModifier ();

  /**
   * Creates a new KineticLaw for this Reaction and returns it.  If this
   * Reaction had a previous KineticLaw, it will be destroyed.
   */
  KineticLaw* createKineticLaw ();

  /**
   * @return the list of Reactants for this Reaction.
   */
  const ListOfSpeciesReferences* getListOfReactants () const;

  /**
   * @return the list of Reactants for this Reaction.
   */
  ListOfSpeciesReferences* getListOfReactants ();

  /**
   * @return the list of Products for this Reaction.
   */
  const ListOfSpeciesReferences* getListOfProducts () const;

  /**
   * @return the list of Products for this Reaction.
   */
  ListOfSpeciesReferences* getListOfProducts ();

  /**
   * @return the list of Modifiers for this Reaction.
   */
  const ListOfSpeciesReferences* getListOfModifiers () const;

  /**
   * @return the list of Modifiers for this Reaction.
   */
  ListOfSpeciesReferences* getListOfModifiers ();


  /**
   * @return the nth reactant (SpeciesReference) of this Reaction.
   */
  const SpeciesReference* getReactant (unsigned int n) const;

  /**
   * @return the nth reactant (SpeciesReference) of this Reaction.
   */
  SpeciesReference* getReactant (unsigned int n);

  /**
   * @return the reactant (SpeciesReference) in this Reaction with the
   * given species or NULL if no such reactant exists.
   */
  const SpeciesReference* getReactant (const std::string& species) const;

  /**
   * @return the reactant (SpeciesReference) in this Reaction with the
   * given species or NULL if no such reactant exists.
   */
  SpeciesReference* getReactant (const std::string& species);


  /**
   * @return the nth product (SpeciesReference) of this Reaction.
   */
  const SpeciesReference* getProduct (unsigned int n) const;

  /**
   * @return the nth product (SpeciesReference) of this Reaction.
   */
  SpeciesReference* getProduct (unsigned int n);

  /**
   * @return the product (SpeciesReference) in this Reaction with the given
   * species or NULL if no such product exists.
   */
  const SpeciesReference* getProduct (const std::string& species) const;

  /**
   * @return the product (SpeciesReference) in this Reaction with the given
   * species or NULL if no such product exists.
   */
  SpeciesReference* getProduct (const std::string& species);


  /**
   * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
   */
  const ModifierSpeciesReference* getModifier (unsigned int n) const;

  /**
   * @return the nth modifier (ModifierSpeciesReference) of this Reaction.
   */
  ModifierSpeciesReference* getModifier (unsigned int n);

  /**
   * @return the modifier (ModifierSpeciesReference) in this Reaction with
   * the given species or NULL if no such modifier exists.
   */
  const ModifierSpeciesReference*
  getModifier (const std::string& species) const;

  /**
   * @return the modifier (ModifierSpeciesReference) in this Reaction with
   * the given species or NULL if no such modifier exists.
   */
  ModifierSpeciesReference* getModifier (const std::string& species);


  /**
   * @return the number of reactants (SpeciesReferences) in this Reaction.
   */
  unsigned int getNumReactants () const;

  /**
   * @return the number of products (SpeciesReferences) in this Reaction.
   */
  unsigned int getNumProducts () const;

  /**
   * @return the number of modifiers (ModifierSpeciesReferences) in this
   * Reaction.
   */
  unsigned int getNumModifiers () const;


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
   * @return the name of this element ie "reaction".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream) const;

  /** @endcond doxygen-libsbml-internal */


protected:
  /** @cond doxygen-libsbml-internal */

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


  ListOfSpeciesReferences  mReactants;
  ListOfSpeciesReferences  mProducts;
  ListOfSpeciesReferences  mModifiers;

  KineticLaw* mKineticLaw;
  bool        mReversible;
  bool        mFast;

  bool mIsSetFast;

  /** @endcond doxygen-libsbml-internal */
};



class LIBSBML_EXTERN ListOfReactions : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfReactions.
   */
  virtual SBase* clone () const;

  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @return the SBMLTypeCode_t of this object or SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const { return SBML_LIST_OF; };


  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
 * @return the name of this element ie "listOfReactions".
   */
  virtual const std::string& getElementName () const;


  /** @cond doxygen-libsbml-internal */

  /**
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;

  /** @endcond doxygen-libsbml-internal */


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS

/*-----------------------------------------------------------------------------
 * See the .cpp file for the documentation of the following functions.
 *---------------------------------------------------------------------------*/


LIBSBML_EXTERN
Reaction_t *
Reaction_create (void);


LIBSBML_EXTERN
Reaction_t *
Reaction_createWith ( const char   *sid,
                      KineticLaw_t *kl,
                      int          reversible,
                      int          fast );


LIBSBML_EXTERN
void
Reaction_free (Reaction_t *r);


LIBSBML_EXTERN
Reaction_t *
Reaction_clone (const Reaction_t *r);


LIBSBML_EXTERN
void
Reaction_initDefaults (Reaction_t *r);


LIBSBML_EXTERN
const char *
Reaction_getId (const Reaction_t *r);


LIBSBML_EXTERN
const char *
Reaction_getName (const Reaction_t *r);


LIBSBML_EXTERN
KineticLaw_t *
Reaction_getKineticLaw (Reaction_t *r);


LIBSBML_EXTERN
int
Reaction_getReversible (const Reaction_t *r);


LIBSBML_EXTERN
int
Reaction_getFast (const Reaction_t *r);


LIBSBML_EXTERN
int
Reaction_isSetId (const Reaction_t *r);


LIBSBML_EXTERN
int
Reaction_isSetName (const Reaction_t *r);


LIBSBML_EXTERN
int
Reaction_isSetKineticLaw (const Reaction_t *r);


LIBSBML_EXTERN
int
Reaction_isSetFast (const Reaction_t *r);


LIBSBML_EXTERN
void
Reaction_setId (Reaction_t *r, const char *sid);


LIBSBML_EXTERN
void
Reaction_setName (Reaction_t *r, const char *name);


LIBSBML_EXTERN
void
Reaction_setKineticLaw (Reaction_t *r, const KineticLaw_t *kl);


LIBSBML_EXTERN
void
Reaction_setReversible (Reaction_t *r, int value);


LIBSBML_EXTERN
void
Reaction_setFast (Reaction_t *r, int value);


LIBSBML_EXTERN
void
Reaction_unsetName (Reaction_t *r);


LIBSBML_EXTERN
void
Reaction_unsetKineticLaw (Reaction_t *r);


LIBSBML_EXTERN
void
Reaction_unsetFast (Reaction_t *r);


LIBSBML_EXTERN
void
Reaction_addReactant (Reaction_t *r, const SpeciesReference_t *sr);


LIBSBML_EXTERN
void
Reaction_addProduct (Reaction_t *r, const SpeciesReference_t *sr);


LIBSBML_EXTERN
void
Reaction_addModifier (Reaction_t *r, const SpeciesReference_t *msr);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createReactant (Reaction_t *r);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createProduct (Reaction_t *r);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_createModifier (Reaction_t *r);


LIBSBML_EXTERN
KineticLaw_t *
Reaction_createKineticLaw (Reaction_t *r);


LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfReactants (Reaction_t *r);


LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfProducts (Reaction_t *r);


LIBSBML_EXTERN
ListOf_t *
Reaction_getListOfModifiers (Reaction_t *r);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactant (Reaction_t *r, unsigned int n);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getReactantBySpecies (Reaction_t *r, const char *species);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProduct (Reaction_t *r, unsigned int n);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getProductBySpecies (Reaction_t *r, const char *species);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getModifier (Reaction_t *r, unsigned int n);


LIBSBML_EXTERN
SpeciesReference_t *
Reaction_getModifierBySpecies (Reaction_t *r, const char *species);


LIBSBML_EXTERN
unsigned int
Reaction_getNumReactants (const Reaction_t *r);


LIBSBML_EXTERN
unsigned int
Reaction_getNumProducts (const Reaction_t *r);


LIBSBML_EXTERN
unsigned int
Reaction_getNumModifiers (const Reaction_t *r);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Reaction_h */
