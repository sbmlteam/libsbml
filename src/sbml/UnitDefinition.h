/**
 * \file    UnitDefinition.h
 * \brief   SBML UnitDefinition
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


#ifndef UnitDefinition_h
#define UnitDefinition_h


#include "extern.h"


#ifdef __cplusplus


#include <string>

#include "SBase.h"
#include "ListOf.h"


class Unit;
class SBMLVisitor;


class UnitDefinition : public SBase
{
public:

  /**
   * Creates a new UnitDefinition, optionally with its id and name
   * attributes set.
   */
  LIBSBML_EXTERN
  UnitDefinition (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this UnitDefinition.
   */
  LIBSBML_EXTERN
  virtual ~UnitDefinition ();


  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * UnitDefinition (if available).
   */
  LIBSBML_EXTERN
  bool accept (SBMLVisitor& v) const;

  /**
   * @return the id of this UnitDefinition.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this UnitDefinition.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return true if the id of this UnitDefinition has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this UnitDefinition has been set, false
   * otherwise.
   *
   * In SBML L1, a UnitDefinition name is required and therefore <b>should
   * always be set</b>.  In L2, name is optional and as such may or may not
   * be set.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * area, i.e. square metres with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  LIBSBML_EXTERN
  bool isVariantOfArea () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * length, i.e. metres with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  LIBSBML_EXTERN
  bool isVariantOfLength () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * substance, i.e. moles or items with only abritrary variations in
   * scale, multiplier, or offset values, false otherwise.
   */
  LIBSBML_EXTERN
  bool isVariantOfSubstance () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * time, i.e. seconds with only abritrary variations in scale,
   * multiplier, or offset values, false otherwise.
   */
  LIBSBML_EXTERN
  bool isVariantOfTime () const;

  /**
   * @return true if this UnitDefinition is a variant of the builtin type
   * volume, i.e. litre or cubic metre with only abritrary variations in
   * scale, multiplier, or offset values, false otherwise.
   */
  LIBSBML_EXTERN
  bool isVariantOfVolume () const;

  /**
   * Moves the id field of this UnitDefinition to its name field (iff name
   * is not already set).  This method is used for converting from L2 to
   * L1.
   */
  LIBSBML_EXTERN
  void moveIdToName ();

  /**
   * Moves the name field of this UnitDefinition to its id field (iff id is
   * not already set).  This method is used for converting from L1 to L2.
   */
  LIBSBML_EXTERN
  void moveNameToId ();

  /**
   * Sets the id of this UnitDefinition to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this UnitDefinition to a copy of string (SName in
   * L1).
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Unsets the name of this UnitDefinition.
   *
   * In SBML L1, a UnitDefinition name is required and therefore <b>should
   * always be set</b>.  In L2, name is optional and as such may or may not
   * be set.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Adds the given Unit to this UnitDefinition.
   */
  LIBSBML_EXTERN
  void addUnit (Unit& u);

  /**
   * @return the list of Units for this UnitDefinition.
   */
  LIBSBML_EXTERN
  ListOf& getListOfUnits ();

  /**
   * @return the list of Units for this UnitDefinition.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfUnits () const;


  /**
   * @return the nth Unit of this UnitDefinition
   */
  LIBSBML_EXTERN
  Unit* getUnit (unsigned int n) const;

  /**
   * @return the number of Units in this UnitDefinition.
   */
  LIBSBML_EXTERN
  unsigned int getNumUnits () const;


protected:

  std::string id;
  std::string name;
  ListOf      unit;


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "sbmlfwd.h"


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
 *   UnitDefinition_setName(UnitDefinition_create(), string);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithName (const char *string);

/**
 * Frees the given UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud);


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
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
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
 * Moves the id field of this UnitDefinition to its name field (iff name is
 * not already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
UnitDefinition_moveIdToName (UnitDefinition_t *ud);

/**
 * Moves the name field of this UnitDefinition to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
UnitDefinition_moveNameToId (UnitDefinition_t *ud);


/**
 * Sets the id of this UnitDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *sid);

/**
 * Sets the name of this UnitDefinition to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *string);


/**
 * Unsets the name of this UnitDefinition.  This is equivalent to:
 * safe_free(ud->name); ud->name = NULL;
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
void
UnitDefinition_unsetName (UnitDefinition_t *ud);


/**
 * Adds the given Unit to this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, Unit_t *u);

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
UnitDefinition_getUnit (const UnitDefinition_t *ud, unsigned int n);

/**
 * @return the number of Units in this UnitDefinition.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud);


/**
 * The UnitDefinitionIdCmp function compares the string sid to ud->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than ud->id.
 * Returns -1 if either sid or ud->id is NULL.
 */
LIBSBML_EXTERN
int
UnitDefinitionIdCmp (const char *sid, const UnitDefinition_t *ud);


END_C_DECLS


#endif  /* !SWIG */
#endif  /* UnitDefinition_h */
