/**
 * \file    Parameter.cpp
 * \brief   SBML Parameter
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


#include "util/util.h"
#include "SBMLVisitor.h"

#include "Parameter.h"


/**
 * Creates a new Parameter, optionally with its id attribute set.
 */
LIBSBML_EXTERN
Parameter::Parameter (const std::string& id) :
    SBase   ()
  , id      ( id   )
  , value   ( 0.0  )
  , constant( true )
{
  init(SBML_PARAMETER);
  isSet.value = 0;
}


/**
 * Creates a new Parameter, with its id and value attributes set and
 * optionally its units and constant attributes.
 */
LIBSBML_EXTERN
Parameter::Parameter (   const std::string&  id
                       , double              value
                       , const std::string&  units
                       , bool                constant ) :
    SBase   ()
  , id      ( id       )
  , value   ( value    )
  , units   ( units    )
  , constant( constant )
{
  init(SBML_PARAMETER);
  isSet.value = 1;
}


/**
 * Destroys this Parameter.
 */
LIBSBML_EXTERN
Parameter::~Parameter ()
{
}


/**
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the parent Model's or
 * KineticLaw's next Parameter (if available).
 */
LIBSBML_EXTERN
bool
Parameter::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = true  (L2 only)
 */
LIBSBML_EXTERN
void
Parameter::initDefaults ()
{
  setConstant(true);
}


/**
 * @return the id of this Parameter
 */
LIBSBML_EXTERN
const std::string&
Parameter::getId () const
{
  return id;
}


/**
 * @return the name of this Parameter.
 */
LIBSBML_EXTERN
const std::string&
Parameter::getName () const
{
  return name;
}


/**
 * @return the value of this Parameter.
 */
LIBSBML_EXTERN
double
Parameter::getValue () const
{
  return value;
}


/**
 * @return the units of this Parameter.
 */
LIBSBML_EXTERN
const std::string&
Parameter::getUnits () const
{
  return units;
}


/**
 * @return true if this Parameter is constant, false otherwise.
 */
LIBSBML_EXTERN
bool
Parameter::getConstant () const
{
  return constant;
}


/**
 * @return true if the id of this Parameter has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Parameter::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this Parameter has been set, false otherwise.
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
bool
Parameter::isSetName () const
{
  return ! name.empty();
}


/**
 * @return true if the value of this Parameter has been set, false
 * otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
bool
Parameter::isSetValue () const
{
  return (bool) isSet.value;
}


/**
 * @return true if the units of this Parameter has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
Parameter::isSetUnits () const
{
  return ! units.empty();
}


/**
 * Moves the id field of this Parameter to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Parameter::moveIdToName ()
{
  if ( isSetName() ) return;

  setName( getId() );
  setId  ( "" );
}


/**
 * Moves the name field of this Parameter to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Parameter::moveNameToId ()
{
  if ( isSetId() ) return;

  setId  ( getName() );
  setName( "" );
}


/**
 * Sets the id of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this Parameter to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Parameter::setName (const std::string& string)
{
  name = string;
}


/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter::setValue (double value)
{
  this->value       = value;
  this->isSet.value = 1;
}


/**
 * Sets the units of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter::setUnits (const std::string& sid)
{
  units = sid;
}


/**
 * Sets the constant field of this Parameter to value.
 */
LIBSBML_EXTERN
void
Parameter::setConstant (bool value)
{
  constant = value;
}


/**
 * Unsets the name of this Parameter.
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter::unsetName ()
{
  name.erase();
}


/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter::unsetValue ()
{
  value       = util_NaN();
  isSet.value = 0;
}


/**
 * Unsets the units of this Parameter.
 */
LIBSBML_EXTERN
void
Parameter::unsetUnits ()
{
  units.erase();
}




/**
 * Creates a new Parameter and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_create (void)
{
  return new(std::nothrow) Parameter;
}


/**
 * Creates a new Parameter with the given id, value and units and returns a
 * pointer to it.  This convenience function is functionally equivalent to:
 *
 *   Parameter_t *p = Parameter_create();
 *   Parameter_setName(p, id); Parameter_setValue(p, value); ... ;
 */
LIBSBML_EXTERN
Parameter_t *
Parameter_createWith (const char *sid, double value, const char *units)
{
  return
    new(std::nothrow) Parameter(sid ? sid : "", value, units ? units : "");
}


/**
 * Frees the given Parameter.
 */
LIBSBML_EXTERN
void
Parameter_free (Parameter_t *p)
{
  delete static_cast<Parameter*>(p);
}


/**
 * Initializes the fields of this Parameter to their defaults:
 *
 *   - constant = 1  (true)  (L2 only)
 */
LIBSBML_EXTERN
void
Parameter_initDefaults (Parameter_t *p)
{
  static_cast<Parameter*>(p)->initDefaults();
}


/**
 * @return the id of this Parameter
 */
LIBSBML_EXTERN
const char *
Parameter_getId (const Parameter_t *p)
{
  const Parameter* x = static_cast<const Parameter*>(p);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getName (const Parameter_t *p)
{
  const Parameter* x = static_cast<const Parameter*>(p);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return the value of this Parameter.
 */
LIBSBML_EXTERN
double
Parameter_getValue (const Parameter_t *p)
{
  return static_cast<const Parameter*>(p)->getValue();
}


/**
 * @return the units of this Parameter.
 */
LIBSBML_EXTERN
const char *
Parameter_getUnits (const Parameter_t *p)
{
  const Parameter* x = static_cast<const Parameter*>(p);


  return x->isSetUnits() ? x->getUnits().c_str() : NULL;
}


/**
 * @return true (non-zero) if this Parameter is constant, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Parameter_getConstant (const Parameter_t *p)
{
  return static_cast<const Parameter*>(p)->getConstant();
}


/**
 * @return 1 if the id of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetId (const Parameter_t *p)
{
  return (int) static_cast<const Parameter*>(p)->isSetId();
}


/**
 * @return 1 if the name of this Parameter has been set, 0 otherwise.
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetName (const Parameter_t *p)
{
  return (int) static_cast<const Parameter*>(p)->isSetName();
}


/**
 * @return 1 if the value of this Parameter has been set, 0 otherwise.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
int
Parameter_isSetValue (const Parameter_t *p)
{
  return (int) static_cast<const Parameter*>(p)->isSetValue();
}


/**
 * @return 1 if the units of this Parameter has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Parameter_isSetUnits (const Parameter_t *p)
{
  return (int) static_cast<const Parameter*>(p)->isSetUnits();
}


/**
 * Moves the id field of this Parameter to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Parameter_moveIdToName (Parameter_t *p)
{
  static_cast<Parameter*>(p)->moveIdToName();
}


/**
 * Moves the name field of this Parameter to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Parameter_moveNameToId (Parameter_t *p)
{
  static_cast<Parameter*>(p)->moveNameToId();
}


/**
 * Sets the id of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setId (Parameter_t *p, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Parameter*>(p)->setId("");
  }
  else
  {
    static_cast<Parameter*>(p)->setId(sid);
  }
}


/**
 * Sets the name of this Parameter to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Parameter_setName (Parameter_t *p, const char *string)
{
  if (string == NULL)
  {
    static_cast<Parameter*>(p)->unsetName();
  }
  else
  {
    static_cast<Parameter*>(p)->setName(string);
  }
}


/**
 * Sets the value of this Parameter to value and marks the field as set.
 */
LIBSBML_EXTERN
void
Parameter_setValue (Parameter_t *p, double value)
{
  static_cast<Parameter*>(p)->setValue(value);
}


/**
 * Sets the units field of this Parameter to a copy of sid.
 */
LIBSBML_EXTERN
void
Parameter_setUnits (Parameter_t *p, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Parameter*>(p)->unsetUnits();
  }
  else
  {
    static_cast<Parameter*>(p)->setUnits(sid);
  }
}


/**
 * Sets the constant field of this Parameter to value (boolean).
 */
LIBSBML_EXTERN
void
Parameter_setConstant (Parameter_t *p, int value)
{
  static_cast<Parameter*>(p)->setConstant((bool) value);
}


/**
 * Unsets the name of this Parameter.  This is equivalent to:
 * safe_free(s->name); s->name = NULL;
 *
 * In SBML L1, a Parameter name is required and therefore <b>should always be
 * set</b>.  In L2, name is optional and as such may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetName (Parameter_t *p)
{
  static_cast<Parameter*>(p)->unsetName();
}


/**
 * Unsets the value of this Parameter.
 *
 * In SBML L1v1, a Parameter value is required and therefore <b>should
 * always be set</b>.  In L1v2 and beyond, a value is optional and as such
 * may or may not be set.
 */
LIBSBML_EXTERN
void
Parameter_unsetValue (Parameter_t *p)
{
  static_cast<Parameter*>(p)->unsetValue();
}


/**
 * Unsets the units of this Parameter.  This is equivalent to:
 * safe_free(p->units); p->units = NULL;
 */
LIBSBML_EXTERN
void
Parameter_unsetUnits (Parameter_t *p)
{
  static_cast<Parameter*>(p)->unsetUnits();
}


/**
 * The ParameterIdCmp function compares the string sid to p->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than p->id.
 * Returns -1 if either sid or p->id is NULL.
 */
LIBSBML_EXTERN
int
ParameterIdCmp (const char *sid, const Parameter_t *p)
{
  int result = -1;


  if (sid != NULL && Parameter_isSetId(p))
  {
    result = strcmp(sid, Parameter_getId(p));
  }

  return result;
}
