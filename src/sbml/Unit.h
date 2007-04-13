/**
 * \file    Unit.h
 * \brief   SBML Unit
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


#ifndef Unit_h
#define Unit_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/UnitKind.h>


#ifdef __cplusplus


#include <string>

#include <sbml/SBase.h>
#include <sbml/ListOf.h>


class SBMLVisitor;


class LIBSBML_EXTERN Unit : public SBase
{
public:

  /**
   * Creates a new Unit, optionally with its kind, exponent, scale
   * and multiplier attributes set.
   */
  Unit (   UnitKind_t  kind       = UNIT_KIND_INVALID
         , int         exponent   = 1
         , int         scale      = 0
         , double      multiplier = 1.0 );

  /**
   * Creates a new Unit, optionally with its kind (via string), exponent,
   * scale and multiplier attributes set.
   */
  Unit (   const std::string&  kind
         , int                 exponent   = 1
         , int                 scale      = 0
         , double              multiplier = 1.0 );

  /**
   * Destroys the given Unit.
   */
  virtual ~Unit ();

  /**
  * Copy constructor. Creates a copy of this Unit.
  */
  Unit(const Unit& orig);


  /**
   * Assignment operator.
   */
  Unit& operator=(const Unit& orig);

  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the UnitDefinition's
   * next Unit (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Unit.
   */
  virtual SBase* clone () const;

  /**
   * Initializes the fields of this Unit to their defaults:
   *
   *   - exponent   = 1
   *   - scale      = 0
   *   - multiplier = 1.0
   */
  void initDefaults ();

  /**
   * @return the kind of this Unit.
   */
  UnitKind_t getKind () const;

  /**
   * @return the exponent of this Unit.
   */
  int getExponent () const;

  /**
   * @return the scale of this Unit.
   */
  int getScale () const;

  /**
   * @return the multiplier of this Unit.
   */
  double getMultiplier () const;

  /**
   * @return the offset of this Unit.
   */
  double getOffset () const;


  /**
   * @return true if the kind of this Unit is 'ampere', false otherwise.
   */
  bool isAmpere () const;

  /**
   * @return true if the kind of this Unit is 'becquerel', false otherwise.
   */
  bool isBecquerel () const;

  /**
   * @return true if the kind of this Unit is 'candela', false otherwise.
   */
  bool isCandela () const;

  /**
   * @return true if the kind of this Unit is 'Celsius', false otherwise.
   */
  bool isCelsius () const;

  /**
   * @return true if the kind of this Unit is 'coulomb', false otherwise.
   */
  bool isCoulomb () const;

  /**
   * @return true if the kind of this Unit is 'dimensionless', false
   * otherwise.
   */
  bool isDimensionless () const;

  /**
   * @return true if the kind of this Unit is 'farad', false otherwise.
   */
  bool isFarad () const;

  /**
   * @return true if the kind of this Unit is 'gram', false otherwise.
   */
  bool isGram () const;

  /**
   * @return true if the kind of this Unit is 'gray', false otherwise.
   */
  bool isGray () const;

  /**
   * @return true if the kind of this Unit is 'henry', false otherwise.
   */
  bool isHenry () const;

  /**
   * @return true if the kind of this Unit is 'hertz', false otherwise.
   */
  bool isHertz () const;

  /**
   * @return true if the kind of this Unit is 'item', false otherwise.
   */
  bool isItem () const;

  /**
   * @return true if the kind of this Unit is 'joule', false otherwise.
   */
  bool isJoule () const;

  /**
   * @return true if the kind of this Unit is 'katal', false otherwise.
   */
  bool isKatal () const;

  /**
   * @return true if the kind of this Unit is 'kelvin', false otherwise.
   */
  bool isKelvin () const;

  /**
   * @return true if the kind of this Unit is 'kilogram', false otherwise.
   */
  bool isKilogram () const;

  /**
   * @return true if the kind of this Unit is 'litre' or 'liter', false
   * otherwise.
   */
  bool isLitre () const;

  /**
   * @return true if the kind of this Unit is 'lumen', false otherwise.
   */
  bool isLumen () const;

  /**
   * @return true if the kind of this Unit is 'lux', false otherwise.
   */
  bool isLux () const;

  /**
   * @return true if the kind of this Unit is 'metre' or 'meter', false
   * otherwise.
   */
  bool isMetre () const;

  /**
   * @return true if the kind of this Unit is 'mole', false otherwise.
   */
  bool isMole () const;

  /**
   * @return true if the kind of this Unit is 'newton', false otherwise.
   */
  bool isNewton () const;

  /**
   * @return true if the kind of this Unit is 'ohm', false otherwise.
   */
  bool isOhm () const;

  /**
   * @return true if the kind of this Unit is 'pascal', false otherwise.
   */
  bool isPascal () const;

  /**
   * @return true if the kind of this Unit is 'radian', false otherwise.
   */
  bool isRadian () const;

  /**
   * @return true if the kind of this Unit is 'second', false otherwise.
   */
  bool isSecond () const;

  /**
   * @return true if the kind of this Unit is 'siemens', false otherwise.
   */
  bool isSiemens () const;

  /**
   * @return true if the kind of this Unit is 'sievert', false otherwise.
   */
  bool isSievert () const;

  /**
   * @return true if the kind of this Unit is 'steradian', false otherwise.
   */
  bool isSteradian () const;

  /**
   * @return true if the kind of this Unit is 'tesla', false otherwise.
   */
  bool isTesla () const;

  /**
   * @return true if the kind of this Unit is 'volt', false otherwise.
   */
  bool isVolt () const;

  /**
   * @return true if the kind of this Unit is 'watt', false otherwise.
   */
  bool isWatt () const;

  /**
   * @return true if the kind of this Unit is 'weber', false otherwise.
   */
  bool isWeber () const;


  /**
   * @return true if the kind of this Unit has been set, false otherwise.
   */
  bool isSetKind () const;

  /**
   * Sets the kind of this Unit to the given UnitKind.
   */
  void setKind (UnitKind_t kind);

  /**
   * Sets the exponent of this Unit to the given value.
   */
  void setExponent (int value);

  /**
   * Sets the scale of this Unit to the given value.
   */
  void setScale (int value);

  /**
   * Sets the multiplier of this Unit to the given value.
   */
  void setMultiplier (double value);

  /**
   * Sets the offset of this Unit to the given value.
   */
  void setOffset (double value);


  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * @return the name of this element ie "unit".
   */
  virtual const std::string& getElementName () const;


  /**
   * @return true if name is one of the five SBML builtin Unit names
   * ('substance', 'volume', 'area', 'length' or 'time'), false otherwise.
   */
  static bool isBuiltIn (const std::string& name);

  /**
   * @return true if name is a valid UnitKind.
   */
  static bool isUnitKind (const std::string& name);


protected:
  /**
   * Subclasses should override this method to read (and store) XHTML,
   * MathML, etc. directly from the XMLInputStream.
   *
   * @return true if the subclass read from the stream, false otherwise.
   */
  virtual bool readOtherXML (XMLInputStream& stream);

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


  UnitKind_t  mKind;
  int         mExponent;
  int         mScale;
  double      mMultiplier;
  double      mOffset;  
};



class LIBSBML_EXTERN ListOfUnits : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfUnits.
   */
  virtual SBase* clone () const;

  /**
   * @return the SBMLTypeCode_t of SBML objects contained in this ListOf or
   * SBML_UNKNOWN (default).
   */
  virtual SBMLTypeCode_t getItemTypeCode () const;

  /**
 * @return the name of this element ie "listOfUnits".
   */
  virtual const std::string& getElementName () const;


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
 * Creates a new Unit and returns a pointer to it.
 */
LIBSBML_EXTERN
Unit_t *
Unit_create (void);

/**
 * Creates a new Unit with the given kind, exponent and scale and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Unit_t *u = Unit_create();
 *   Unit_setKind(kind); Unit_setExponent(exponent); ...;
 */
LIBSBML_EXTERN
Unit_t *
Unit_createWith (UnitKind_t kind, int exponent, int scale);

/**
 * Frees the given Unit.
 */
LIBSBML_EXTERN
void
Unit_free (Unit_t *u);

/**
 * Initializes the fields of this Unit to their defaults:
 *
 *   - exponent   = 1
 *   - scale      = 0
 *   - multiplier = 1.0
 */
LIBSBML_EXTERN
void
Unit_initDefaults (Unit_t *u);


/**
 * @return the kind of this Unit.
 */
LIBSBML_EXTERN
UnitKind_t
Unit_getKind (const Unit_t *u);

/**
 * @return the exponent of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getExponent (const Unit_t *u);

/**
 * @return the scale of this Unit.
 */
LIBSBML_EXTERN
int
Unit_getScale (const Unit_t *u);

/**
 * @return the multiplier of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getMultiplier (const Unit_t *u);

/**
 * @return the offset of this Unit.
 */
LIBSBML_EXTERN
double
Unit_getOffset (const Unit_t *u);


/**
 * @return true (non-zero) if the kind of this Unit is 'ampere', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isAmpere (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'becquerel', false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBecquerel (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'candela', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCandela (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'Celsius', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCelsius (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'coulomb', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isCoulomb (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'dimensionless', false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isDimensionless (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'farad', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isFarad (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'gram', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGram (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'gray', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isGray (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'henry', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHenry (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'hertz', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isHertz (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'item', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isItem (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'joule', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isJoule (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'katal', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKatal (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'kelvin', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKelvin (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'kilogram', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isKilogram (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'litre' or 'liter', false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLitre (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'lumen', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLumen (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'lux', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isLux (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'metre' or 'meter', false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMetre (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'mole', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isMole (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'newton', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isNewton (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'ohm', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isOhm (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'pascal', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isPascal (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'radian', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isRadian (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'second', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSecond (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'siemens', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSiemens (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'sievert', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSievert (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'steradian', false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSteradian (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'tesla', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isTesla (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'volt', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isVolt (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'watt', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWatt (const Unit_t *u);

/**
 * @return true (non-zero) if the kind of this Unit is 'weber', false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isWeber (const Unit_t *u);


/**
 * @return true (non-zero) if the kind of this Unit has been set, false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isSetKind (const Unit_t *u);


/**
 * Sets the kind of this Unit to the given UnitKind.
 */
LIBSBML_EXTERN
void
Unit_setKind (Unit_t *u, UnitKind_t kind);

/**
 * Sets the exponent of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setExponent (Unit_t *u, int value);

/**
 * Sets the scale of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setScale (Unit_t *u, int value);

/**
 * Sets the multiplier of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setMultiplier (Unit_t *u, double value);

/**
 * Sets the offset of this Unit to the given value.
 */
LIBSBML_EXTERN
void
Unit_setOffset (Unit_t *u, double value);

/**
 * @return true (non-zero) if name is one of the five SBML builtin Unit names
 * ('substance', 'volume', 'area', 'length' or 'time'), false (0) otherwise.
 */
LIBSBML_EXTERN
int
Unit_isBuiltIn (const char *name);


END_C_DECLS


#endif  /* !SWIG  */
#endif  /* Unit_h */
