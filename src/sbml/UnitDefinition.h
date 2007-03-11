/**
 * \file    UnitDefinition.h
 * \brief   SBML UnitDefinition
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


#ifndef UnitDefinition_h
#define UnitDefinition_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/Unit.h>


class SBMLVisitor;


class LIBSBML_EXTERN UnitDefinition : public SBase
{
public:

  /**
   * Creates a new UnitDefinition, optionally with its id and name
   * attributes set.
   */
  UnitDefinition (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this UnitDefinition.
   */
  virtual ~UnitDefinition ();


  /**
  * Copy constructor.
  */
  UnitDefinition(const UnitDefinition& orig);


  /**
   * Assignment operator
   */
  UnitDefinition& operator=(const UnitDefinition& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * UnitDefinition (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this UnitDefinition.
   */
  virtual SBase* clone () const;


  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * area, i.e. square metres with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  bool isVariantOfArea () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * length, i.e. metres with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  bool isVariantOfLength () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * substance, i.e. moles or items with only abritrary variations in
   * scale, multiplier, or offset values, false otherwise.
   */
  bool isVariantOfSubstance () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * time, i.e. seconds with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  bool isVariantOfTime () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * volume, i.e. litre or cubic metre with only abritrary variations in
   * scale, multiplier, or offset values, false otherwise.
   */
  bool isVariantOfVolume () const;

  /**
   * @return true if this UnitDefinition is a variant of dimensionless
   * i.e. dimensionless with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  bool isVariantOfDimensionless () const;

  /**
   * @return true if this UnitDefinition is a variant of mass i.e. gram or
   * kilogram with only abritrary variations in scale, multiplier, or
   * offset values, false otherwise.
   */
  bool isVariantOfMass () const;


  /**
   * Adds a copy of the given Unit to this UnitDefinition.
   */
  void addUnit (const Unit* u);

  /**
   * Creates a new Unit, adds it to this UnitDefinition's list of units and
   * returns it.
   */
  Unit* createUnit ();


  /**
   * @return the list of Units for this UnitDefinition.
   */
  const ListOfUnits* getListOfUnits () const;

  /**
   * @return the list of Units for this UnitDefinition.
   */
  ListOfUnits* getListOfUnits ();


  /**
   * @return the nth Unit of this UnitDefinition
   */
  Unit* getUnit (unsigned int n);

  /**
   * @return the nth Unit of this UnitDefinition
   */
  const Unit* getUnit (unsigned int n) const;


  /**
   * @return the number of Units in this UnitDefinition.
   */
  unsigned int getNumUnits () const;


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


  ListOfUnits mUnits;
};



class LIBSBML_EXTERN ListOfUnitDefinitions : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfUnitDefinitions.
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
   * @return the ordinal position of the element with respect to its
   * siblings or -1 (default) to indicate the position is not significant.
   */
  virtual int getElementPosition () const;


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
 * Creates a new UnitDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (void);

/**
 * Creates a new UnitDefinition with the given id and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setId(UnitDefinition_create(), sid);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWith (const char *sid);

/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setName(UnitDefinition_create(), name);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithName (const char *name);

/**
 * Frees the given UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud);

/**
 * @return a (deep) copy of this UnitDefinition.
 */
LIBSBML_EXTERN
UnitDefinition_t*
UnitDefinition_clone (const UnitDefinition_t *ud);


/**
 * @return the id of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud);

/**
 * @return the name of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud);


/**
 * @return non-zero if the id of this UnitDefinition has been set, zero
 * otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud);

/**
 * @return non-zero if the name of this UnitDefinition has been set, zero
 * otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud);


/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * area, i.e. square metres with only abritrary variations in scale,
 * multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfArea (const UnitDefinition_t *ud);

/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * length, i.e. metres with only abritrary variations in scale, multiplier,
 * or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfLength (const UnitDefinition_t *ud);

/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * substance, i.e. moles or items with only abritrary variations in scale,
 * multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfSubstance (const UnitDefinition_t *ud);

/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * time, i.e. seconds with only abritrary variations in scale, multiplier,
 * or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfTime (const UnitDefinition_t *ud);

/**
 * @return non-zero if this UnitDefinition is a variant of the builtin type
 * volume, i.e. litre or cubic metre with only abritrary variations in
 * scale, multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfVolume (const UnitDefinition_t *ud);

/**
 * @return non-zero if this UnitDefinition is a variant of mass
 * i.e. gram or kilogram with only abritrary variations in
 * scale, multiplier, or offset values, zero otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isVariantOfMass (const UnitDefinition_t *ud);

/**
 * Sets the id of this UnitDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *sid);

/**
 * Sets the name of this UnitDefinition to a copy of string.
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *name);


/**
 * Unsets the name of this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_unsetName (UnitDefinition_t *ud);


/**
 * Adds a copy of the given Unit to this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, const Unit_t *u);

/**
 * Creates a new Unit, adds it to this UnitDefinition's list of units and
 * returns it.
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_createUnit (UnitDefinition_t *ud);


/**
 * @return the list of Units for this UnitDefinition.
 */
LIBSBML_EXTERN
ListOf_t *
UnitDefinition_getListOfUnits (UnitDefinition_t *ud);

/**
 * @return the nth Unit of this UnitDefinition.
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (UnitDefinition_t *ud, unsigned int n);

/**
 * @return the number of Units in this UnitDefinition.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* UnitDefinition_h */
