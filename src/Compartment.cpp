/**
 * Filename    : Compartment.cpp
 * Description : SBML Compartment
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-13
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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


#include "sbml/util.h"
#include "sbml/Compartment.h"
#include "sbml/Compartment.hpp"


/**
 * Creates a new Compartment, optionally with its id attribute set.
 */
LIBSBML_EXTERN
Compartment::Compartment (const std::string& id) : SBase(), id(id)
{
  init(SBML_COMPARTMENT);

  isSet.size   = 0;
  isSet.volume = 0;

  initDefaults();
}


/**
 * Destroys this Compartment.
 */
LIBSBML_EXTERN
Compartment::~Compartment ()
{
}


/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume            = 1.0          (L1 only)
 *   - spatialDimensions = 3            (L2 only)
 *   - constant          = 1    (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Compartment::initDefaults ()
{
  /**
   * Calling Compartment_setVolume() will set c->isSet.size which is L2 not
   * L1.
   */
  size         = 1.0;
  isSet.volume = 1;

  setSpatialDimensions(3);
  setConstant(1);
}


/**
 * @return the id of this Compartment.
 */
LIBSBML_EXTERN
const std::string&
Compartment::getId () const
{
  return id;
}


/**
 * @return the name of this Compartment.
 */
LIBSBML_EXTERN
const std::string&
Compartment::getName () const
{
  return name;
}


/**
 * @return the spatialDimensions of this Compartment.
 */
LIBSBML_EXTERN
unsigned int
Compartment::getSpatialDimensions () const
{
  return spatialDimensions;
}


/**
 * @return the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment::getSize () const
{
  return size;
}


/**
 * @return the volume (size in L2) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment::getVolume () const
{
  return getSize();
}


/**
 * @return the units of this Compartment.
 */
LIBSBML_EXTERN
const std::string&
Compartment::getUnits () const
{
  return units;
}


/**
 * @return the outside of this Compartment.
 */
LIBSBML_EXTERN
const std::string&
Compartment::getOutside () const
{
  return outside;
}


/**
 * @return true if this Compartment is constant, false otherwise.
 */
LIBSBML_EXTERN
bool
Compartment::getConstant () const
{
  return constant;
}


/**
 * @return true if the id of this Compartment has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Compartment::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this Compartment has been set, false
 * otherwise.
 *
 * In SBML L1, a Compartment name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
bool
Compartment::isSetName () const
{
  return ! name.empty();
}


/**
 * @return true if the size (volume in L1) of this Compartment has been
 * set, false otherwise.
 */
LIBSBML_EXTERN
bool
Compartment::isSetSize () const
{
  return isSet.size;
}


/**
 * @return true if the volume (size in L2) of this Compartment has been
 * set, false otherwise.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
LIBSBML_EXTERN
bool
Compartment::isSetVolume () const
{
  return isSet.volume;
}


/**
 * @return true if the units of this Compartment has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Compartment::isSetUnits () const
{
  return ! units.empty();
}


/**
 * @return true if the outside of this Compartment has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Compartment::isSetOutside () const
{
  return ! outside.empty();
}


/**
 * Sets the id of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this Compartment to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Compartment::setName (const std::string& string)
{
  name = string;
}


/**
 * Sets the spatialDimensions of this Compartment to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
LIBSBML_EXTERN
void
Compartment::setSpatialDimensions (unsigned int value)
{
  if (value >= 0 && value <= 3)
  {
    spatialDimensions = value;
  }
}


/**
 * Sets the size (volume in L1) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment::setSize (double value)
{
  size         = value;
  isSet.size   = 1;
  isSet.volume = 1;
}


/**
 * Sets the volume (size in L2) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment::setVolume (double value)
{
  setSize(value);
}


/**
 * Sets the units of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment::setUnits (const std::string& sid)
{
  units = sid;
}


/**
 * Sets the outside of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment::setOutside (const std::string& sid)
{
  outside = sid;
}


/**
 * Sets the constant field of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment::setConstant (bool value)
{
  constant = value;
}


/**
 * Unsets the name of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment::unsetName ()
{
  name.erase();
}


/**
 * Unsets the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment::unsetSize ()
{
  size         = util_NaN();
  isSet.size   = 0;
  isSet.volume = 0;
}


/**
 * Unsets the volume (size in L2) of this Compartment.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume is optional with no default
 * value and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Compartment::unsetVolume ()
{
  unsetSize();
}


/**
 * Unsets the units of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment::unsetUnits ()
{
  units.erase();
}


/**
 * Unsets the outside of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment::unsetOutside ()
{
  outside.erase();
}




/**
 * Creates a new Compartment and returns a pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_create (void)
{
  return new(std::nothrow) Compartment;
}


/**
 * Creates a new Compartment with the  given id, size (volume in L1), units
 * and outside and  returns a pointer to it.   This convenience function is
 * functionally equivalent to:
 *
 *   Compartment_t *c = Compartment_create();
 *   Compartment_setId(c, sid); Compartment_setSize(c, size); ... ;
 */
LIBSBML_EXTERN
Compartment_t *
Compartment_createWith ( const char *sid  , double     size,
                         const char *units, const char *outside )
{
  Compartment* c = new(std::nothrow) Compartment;


  if (c != 0)
  {
    c->setId     ( sid     ? sid     : "" );
    c->setUnits  ( units   ? units   : "" );
    c->setOutside( outside ? outside : "" );

    c->setSize(size);
  }

  return c;
}


/**
 * Frees the given Compartment.
 */
LIBSBML_EXTERN
void
Compartment_free (Compartment_t *c)
{
  delete static_cast<Compartment*>(c);
}


/**
 * Initializes the fields of this Compartment to their defaults:
 *
 *   - volume            = 1.0          (L1 only)
 *   - spatialDimensions = 3            (L2 only)
 *   - constant          = 1    (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Compartment_initDefaults (Compartment_t *c)
{
  static_cast<Compartment*>(c)->initDefaults();
}


/**
 * @return the id of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getId (const Compartment_t *c)
{
  const Compartment* x = static_cast<const Compartment*>(c);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getName (const Compartment_t *c)
{
  const Compartment* x = static_cast<const Compartment*>(c);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the spatialDimensions of this Compartment.
 */
LIBSBML_EXTERN
unsigned int
Compartment_getSpatialDimensions (const Compartment_t *c)
{
  return static_cast<const Compartment*>(c)->getSpatialDimensions();
}


/**
 * @return the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getSize (const Compartment_t *c)
{
  return static_cast<const Compartment*>(c)->getSize();
}


/**
 * @return the volume (size in L2) of this Compartment.
 */
LIBSBML_EXTERN
double
Compartment_getVolume (const Compartment_t *c)
{
  return static_cast<const Compartment*>(c)->getVolume();
}


/**
 * @return the units of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getUnits (const Compartment_t *c)
{
  const Compartment* x = static_cast<const Compartment*>(c);


  return x->isSetUnits() ? x->getUnits().c_str() : NULL;
}


/**
 * @return the outside of this Compartment.
 */
LIBSBML_EXTERN
const char *
Compartment_getOutside (const Compartment_t *c)
{
  const Compartment* x = static_cast<const Compartment*>(c);


  return x->isSetOutside() ? x->getOutside().c_str() : NULL;
}


/**
 * @return true (non-zero) if this Compartment is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Compartment_getConstant (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->getConstant();
}


/**
 * @return 1 if the id of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetId (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->isSetId();
}


/**
 * @return 1 if the name of this Compartment has been set, 0 otherwise.
 *
 * In SBML L1, a Compartment name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
int
Compartment_isSetName (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->isSetName();
}


/**
 * @return 1 if the size (volume in L1) of this Compartment has been set, 0
 * otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetSize (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->isSetSize();
}


/**
 * @return 1 if the volume (size in L2) of this Compartment has been set, 0
 * otherwise.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume (size) is optional with no
 * default value and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Compartment_isSetVolume (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->isSetVolume();
}


/**
 * @return 1 if the units of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetUnits (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->isSetUnits();
}


/**
 * @return 1 if the outside of this Compartment has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Compartment_isSetOutside (const Compartment_t *c)
{
  return (int) static_cast<const Compartment*>(c)->isSetOutside();
}


/**
 * Sets the id of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setId (Compartment_t *c, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Compartment*>(c)->setId("");
  }
  else
  {
    static_cast<Compartment*>(c)->setId(sid);
  }
}


/**
 * Sets the name of this Compartment to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Compartment_setName (Compartment_t *c, const char *string)
{
  if (string == NULL)
  {
    static_cast<Compartment*>(c)->unsetName();
  }
  else
  {
    static_cast<Compartment*>(c)->setName(string);
  }
}


/**
 * Sets the spatialDimensions of this Compartment to value.
 *
 * If value is not one of [0, 1, 2, 3] the function will have no effect
 * (i.e. spatialDimensions will not be set).
 */
LIBSBML_EXTERN
void
Compartment_setSpatialDimensions (Compartment_t *c, unsigned int value)
{
  static_cast<Compartment*>(c)->setSpatialDimensions(value);
}


/**
 * Sets the size (volume in L1) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setSize (Compartment_t *c, double value)
{
  static_cast<Compartment*>(c)->setSize(value);
}


/**
 * Sets the volume (size in L2) of this Compartment to value.
 */
LIBSBML_EXTERN
void
Compartment_setVolume (Compartment_t *c, double value)
{
  static_cast<Compartment*>(c)->setVolume(value);
}


/**
 * Sets the units of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setUnits (Compartment_t *c, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Compartment*>(c)->unsetUnits();
  }
  else
  {
    static_cast<Compartment*>(c)->setUnits(sid);
  }
}


/**
 * Sets the outside of this Compartment to a copy of sid.
 */
LIBSBML_EXTERN
void
Compartment_setOutside (Compartment_t *c, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Compartment*>(c)->unsetOutside();
  }
  else
  {
    static_cast<Compartment*>(c)->setOutside(sid);
  }
}


/**
 * Sets the constant field of this Compartment to value (boolean).
 */
LIBSBML_EXTERN
void
Compartment_setConstant (Compartment_t *c, int value)
{
  static_cast<Compartment*>(c)->setConstant((bool) value);
}


/**
 * Unsets the name of this Compartment.  This is equivalent to:
 * safe_free(c->name); c->name = NULL;
 */
LIBSBML_EXTERN
void
Compartment_unsetName (Compartment_t *c)
{
  static_cast<Compartment*>(c)->unsetName();
}


/**
 * Unsets the size (volume in L1) of this Compartment.
 */
LIBSBML_EXTERN
void
Compartment_unsetSize (Compartment_t *c)
{
  static_cast<Compartment*>(c)->unsetSize();
}


/**
 * Unsets the volume (size in L2) of this Compartment.
 *
 * In SBML L1, a Compartment volume has a default value (1.0) and therefore
 * <b>should always be set</b>.  In L2, volume is optional with no default
 * value and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Compartment_unsetVolume (Compartment_t *c)
{
  static_cast<Compartment*>(c)->unsetVolume();
}


/**
 * Unsets the units of this Compartment.  This is equivalent to:
 * safe_free(c->units); c->units = NULL;
 */
LIBSBML_EXTERN
void
Compartment_unsetUnits (Compartment_t *c)
{
  static_cast<Compartment*>(c)->unsetUnits();
}


/**
 * Unsets the outside of this Compartment.  This is equivalent to:
 * safe_free(c->outside); c->outside = NULL;
 */
LIBSBML_EXTERN
void
Compartment_unsetOutside (Compartment_t *c)
{
  static_cast<Compartment*>(c)->unsetOutside();
}


/**
 * The CompartmentIdCmp function compares the string sid to c->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than c->id.
 * Returns -1 if either sid or c->id is NULL.
 */
LIBSBML_EXTERN
int
CompartmentIdCmp (const char *sid, const Compartment_t *c)
{
  int result = -1;


  if (sid != NULL && Compartment_isSetId(c))
  {
    result = strcmp(sid, Compartment_getId(c));
  }

  return result;
}
