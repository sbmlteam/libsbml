/**
 * \file    Species.h
 * \brief   SBML Species
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


#ifndef Species_h
#define Species_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class SBMLVisitor;


class LIBSBML_EXTERN Species : public SBase
{
public:

  /**
   * Creates a new Species, optionally with its id and name attributes set.
   */
  Species (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this Species.
   */
  virtual ~Species ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * Species (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Species.
   */
  virtual SBase* clone () const;

  /**
   * Initializes the fields of this Species to their defaults:
   *
   *   - boundaryCondition = false
   *   - constant          = false  (L2 only)
   */
  void initDefaults ();


  /**
   * @return the speciesType of this Species.
   */
  const std::string& getSpeciesType () const;

  /**
   * @return the compartment of this Species.
   */
  const std::string& getCompartment () const;

  /**
   * @return the initialAmount of this Species.
   */
  double getInitialAmount () const;

  /**
   * @return the initialConcentration of this Species.
   */
  double getInitialConcentration () const;

  /**
   * @return the substanceUnits of this Species.
   */
  const std::string& getSubstanceUnits () const;

  /**
   * @return the spatialSizeUnits of this Species.
   */
  const std::string& getSpatialSizeUnits () const;

  /**
   * @return the units of this Species (L1 only).
   */
  const std::string& getUnits () const;

  /**
   * @return true if this Species hasOnlySubstanceUnits, false otherwise.
   */
  bool getHasOnlySubstanceUnits () const;

  /**
   * @return the boundaryCondition of this Species.
   */
  bool getBoundaryCondition () const;

  /**
   * @return the charge of this Species.
   */
  int getCharge () const;

  /**
   * @return true if this Species is constant, false otherwise.
   */
  bool getConstant () const;


  /**
   * @return true if the speciesType of this Species has been set, false
   * otherwise.
   */
  bool isSetSpeciesType () const;

  /**
   * @return true if the compartment of this Species has been set, false
   * otherwise.
   */
  bool isSetCompartment () const;

  /**
   * @return true if the initialAmount of this Species has been set, false
   * otherwise.
   *
   * In SBML L1, a Species initialAmount is required and therefore
   * <b>should always be set</b>.  In L2, initialAmount is optional and as
   * such may or may not be set.
   */
  bool isSetInitialAmount () const;

  /**
   * @return true if the initialConcentration of this Species has been set,
   * false otherwise.
   */
  bool isSetInitialConcentration () const;

  /**
   * @return true if the substanceUnits of this Species has been set, false
   * otherwise.
   */
  bool isSetSubstanceUnits () const;

  /**
   * @return true if the spatialSizeUnits of this Species has been set,
   * false otherwise.
   */
  bool isSetSpatialSizeUnits () const;

  /**
   * @return true if the units of this Species has been set, false
   * otherwise (L1 only).
   */
  bool isSetUnits () const;

  /**
   * @return true if the charge of this Species has been set, false
   * otherwise.
   */
  bool isSetCharge () const;


  /**
   * Sets the speciesType field of this Species to a copy of sid.
   */
  void setSpeciesType (const std::string& sid);

  /**
   * Sets the compartment of this Species to a copy of sid.
   */
  void setCompartment (const std::string& sid);

  /**
   * Sets the initialAmount of this Species to value and marks the field as
   * set.  This method also unsets the initialConentration field.
   */
  void setInitialAmount (double value);

  /**
   * Sets the initialConcentration of this Species to value and marks the
   * field as set.  This method also unsets the initialAmount field.
   */
  void setInitialConcentration (double value);

  /**
   * Sets the substanceUnits of this Species to a copy of sid.
   */
  void setSubstanceUnits (const std::string& sid);

  /**
   * Sets the spatialSizeUnits of this Species to a copy of sid.
   */
  void setSpatialSizeUnits (const std::string& sid);

  /**
   * Sets the units of this Species to a copy of sname (L1 only).
   */
  void setUnits (const std::string& sname);

  /**
   * Sets the hasOnlySubstanceUnits field of this Species to value.
   */
  void setHasOnlySubstanceUnits (bool value);

  /**
   * Sets the boundaryCondition of this Species to value.
   */
  void setBoundaryCondition (bool value);

  /**
   * Sets the charge of this Species to value and marks the field as set.
   */
  void setCharge (int value);

  /**
   * Sets the constant field of this Species to value.
   */
  void setConstant (bool value);


  /**
   * Unsets the speciesType of this Species.
   */
  void unsetSpeciesType ();

  /**
   * Marks the initialAmount of this Species as unset.
   */
  void unsetInitialAmount ();

  /**
   * Unsets the initialConcentration of this Species.
   */
  void unsetInitialConcentration ();

  /**
   * Unsets the substanceUnits of this Species.
   */
  void unsetSubstanceUnits ();

  /**
   * Unsets the spatialSizeUnits of this Species.
   */
  void unsetSpatialSizeUnits ();

  /**
   * Unsets the units of this Species (L1 only).
   */
  void unsetUnits ();

  /**
   * Unsets the charge of this Species.
   */
  void unsetCharge ();


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


protected:

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


  std::string  mSpeciesType;
  std::string  mCompartment;

  double  mInitialAmount;
  double  mInitialConcentration;

  std::string  mSubstanceUnits;
  std::string  mSpatialSizeUnits;

  bool  mHasOnlySubstanceUnits;
  bool  mBoundaryCondition;
  int   mCharge;
  bool  mConstant;

  bool  mIsSetInitialAmount;
  bool  mIsSetInitialConcentration;
  bool  mIsSetCharge;
};



class LIBSBML_EXTERN ListOfSpecies : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfSpecies.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * returns expected position of ListOfSpecies in the model
   */
  virtual int getElementPosition() const;

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


/**
 * Creates a new Species and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Species_create (void);


/**
 * Frees the given Species.
 */
LIBSBML_EXTERN
void
Species_free (Species_t *s);

/**
 * @return a (deep) copy of this Species.
 */
LIBSBML_EXTERN
Species_t *
Species_clone (const Species_t *s);

/**
 * Initializes the fields of this Species to their defaults:
 *
 *   - boundaryCondition = 0  (false)
 *   - constant          = 0  (false)  (L2 only)
 */
LIBSBML_EXTERN
void
Species_initDefaults (Species_t *s);


/**
 * @return the id of this Species
 */
LIBSBML_EXTERN
const char *
Species_getId (const Species_t *s);

/**
 * @return the name of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getName (const Species_t *s);

/**
 * @return the speciesType of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSpeciesType (const Species_t *s);

/**
 * @return the compartment of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getCompartment (const Species_t *s);

/**
 * @return the initialAmount of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialAmount (const Species_t *s);

/**
 * @return the initialConcentration of this Species.
 */
LIBSBML_EXTERN
double
Species_getInitialConcentration (const Species_t *s);

/**
 * @return the substanceUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSubstanceUnits (const Species_t *s);

/**
 * @return the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
const char *
Species_getSpatialSizeUnits (const Species_t *s);

/**
 * @return the units of this Species (L1 only).
 */
LIBSBML_EXTERN
const char *
Species_getUnits (const Species_t *s);

/**
 * @return true (non-zero) if this Species hasOnlySubstanceUnits, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getHasOnlySubstanceUnits (const Species_t *s);

/**
 * @return the boundaryCondition of this Species.
 */
LIBSBML_EXTERN
int
Species_getBoundaryCondition (const Species_t *s);

/**
 * @return the charge of this Species.
 */
LIBSBML_EXTERN
int
Species_getCharge (const Species_t *s);

/**
 * @return true (non-zero) if this Species is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Species_getConstant (const Species_t *s);


/**
 * @return true (non-zero) if the id of this Species has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetId (const Species_t *s);

/**
 * @return true (non-zero) if the name of this Species has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetName (const Species_t *s);

/**
 * @return true (non-zero) if the speciesType of this Species has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpeciesType (const Species_t *s);

/**
 * @return true (non-zero) if the compartment of this Species has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCompartment (const Species_t *s);

/**
 * @return true (non-zero) if the initialAmount of this Species has been
 * set, false (0) otherwise.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
int
Species_isSetInitialAmount (const Species_t *s);

/**
 * @return true (non-zero) if the initialConcentration of this Species has
 * been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetInitialConcentration (const Species_t *s);

/**
 * @return true (non-zero) if the substanceUnits of this Species has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSubstanceUnits (const Species_t *s);

/**
 * @return true (non-zero) if the spatialSizeUnits of this Species has been
 * set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetSpatialSizeUnits (const Species_t *s);

/**
 * @return true (non-zero) if the units of this Species has been set, false
 * (0) otherwise (L1 only).
 */
LIBSBML_EXTERN
int
Species_isSetUnits (const Species_t *s);

/**
 * @return true (non-zero) if the charge of this Species has been set,
 * false (0) otherwise.
 */
LIBSBML_EXTERN
int
Species_isSetCharge (const Species_t *s);


/**
 * Sets the id of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setId (Species_t *s, const char *sid);

/**
 * Sets the name of this Species to a copy of string.
 */
LIBSBML_EXTERN
void
Species_setName (Species_t *s, const char *string);

/**
 * Sets the speciesType of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSpeciesType (Species_t *s, const char *sid);

/**
 * Sets the compartment of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setCompartment (Species_t *s, const char *sid);

/**
 * Sets the initialAmount of this Species to value and marks the field as
 * set.  This method also unsets the initialConentration field.
 */
LIBSBML_EXTERN
void
Species_setInitialAmount (Species_t *s, double value);

/**
 * Sets the initialConcentration of this Species to value and marks the
 * field as set.  This method also unsets the initialAmount field.
 */
LIBSBML_EXTERN
void
Species_setInitialConcentration (Species_t *s, double value);

/**
 * Sets the substanceUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSubstanceUnits (Species_t *s, const char *sid);

/**
 * Sets the spatialSizeUnits of this Species to a copy of sid.
 */
LIBSBML_EXTERN
void
Species_setSpatialSizeUnits (Species_t *s, const char *sid);

/**
 * Sets the units of this Species to a copy of sname (L1 only).
 */
LIBSBML_EXTERN
void
Species_setUnits (Species_t *s, const char *sname);

/**
 * Sets the hasOnlySubstanceUnits field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setHasOnlySubstanceUnits (Species_t *s, int value);

/**
 * Sets the boundaryCondition of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setBoundaryCondition (Species_t *s, int value);

/**
 * Sets the charge of this Species to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Species_setCharge (Species_t *s, int value);

/**
 * Sets the constant field of this Species to value (boolean).
 */
LIBSBML_EXTERN
void
Species_setConstant (Species_t *s, int value);


/**
 * Unsets the name of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetName (Species_t *s);

/**
 * Unsets the speciesType of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetSpeciesType (Species_t *s);

/**
 * Unsets the initialAmount of this Species.
 *
 * In SBML L1, a Species initialAmount is required and therefore <b>should
 * always be set</b>.  In L2, initialAmount is optional and as such may or
 * may not be set.
 */
LIBSBML_EXTERN
void
Species_unsetInitialAmount (Species_t *s);

/**
 * Unsets the initialConcentration of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetInitialConcentration (Species_t *s);

/**
 * Unsets the substanceUnits of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetSubstanceUnits (Species_t *s);

/**
 * Unsets the spatialSizeUnits of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetSpatialSizeUnits (Species_t *s);

/**
 * Unsets the units of this Species (L1 only).
 */
LIBSBML_EXTERN
void
Species_unsetUnits (Species_t *s);

/**
 * Unsets the charge of this Species.
 */
LIBSBML_EXTERN
void
Species_unsetCharge (Species_t *s);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* Species_h */
