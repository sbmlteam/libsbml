/**
 * Filename    : UnitDefinition.cpp
 * Description : SBML UnitDefinition
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-22
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
#include "sbml/UnitDefinition.h"
#include "sbml/UnitDefinition.hpp"


/**
 * Creates a new UnitDefinition, optionally with its id and name
 * attributes set.
 */
LIBSBML_EXTERN
UnitDefinition::UnitDefinition (   const std::string& sid
                                 , const std::string& string ) :
     SBase ()
   , id    (sid)
   , name  (string)
{
  init(SBML_UNIT_DEFINITION);
}


/**
 * Destroys this UnitDefinition.
 */
LIBSBML_EXTERN
UnitDefinition::~UnitDefinition ()
{
}


/**
 * @return the id of this UnitDefinition.
 */
LIBSBML_EXTERN
const std::string&
UnitDefinition::getId () const
{
  return id;
}


/**
 * @return the name of this UnitDefinition.
 */
LIBSBML_EXTERN
const std::string&
UnitDefinition::getName () const
{
  return name;
}


/**
 * @return true if the id of this UnitDefinition has been set, false
 * otherwise.
 */
LIBSBML_EXTERN
bool
UnitDefinition::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this UnitDefinition has been set, false
 * otherwise.
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
bool
UnitDefinition::isSetName () const
{
  return ! name.empty();
}


/**
 * Moves the id field of this UnitDefinition to its name field (iff name is
 * not already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
UnitDefinition::moveIdToName ()
{
  if ( isSetName() ) return;

  setName( getId() );
  setId  ( "" );
}


/**
 * Moves the name field of this UnitDefinition to its id field (iff id is
 * not already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
UnitDefinition::moveNameToId ()
{
  if ( isSetId() ) return;

  setId  ( getName() );
  setName( "" );
}


/**
 * Sets the id of this UnitDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
UnitDefinition::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this UnitDefinition to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
UnitDefinition::setName (const std::string& string)
{
  name = string;
}


/**
 * Unsets the name of this UnitDefinition.
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
void
UnitDefinition::unsetName ()
{
  name.erase();
}


/**
 * Adds the given Unit to this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition::addUnit (Unit& u)
{
  unit.append(&u);
}


/**
 * @return the list of Units for this UnitDefinition.
 */
LIBSBML_EXTERN
ListOf&
UnitDefinition::getListOfUnits ()
{
  return unit;
}


/**
 * @return the nth Unit of this UnitDefinition
 */
LIBSBML_EXTERN
Unit*
UnitDefinition::getUnit (unsigned int n) const
{
  return static_cast<Unit*>( unit.get(n) );
}


/**
 * @return the number of Units in this UnitDefinition.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition::getNumUnits () const
{
  return unit.getNumItems();
}




/**
 * Creates a new UnitDefinition and returns a pointer to it.
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_create (void)
{
  return new(std::nothrow) UnitDefinition;
}


/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setId(UnitDefinition_create(), sid);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWith (const char *sid)
{
  return new(std::nothrow) UnitDefinition(sid ? sid : "");
}


/**
 * Creates a new UnitDefinition with the given name and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   UnitDefinition_setName(UnitDefinition_create(), string);
 */
LIBSBML_EXTERN
UnitDefinition_t *
UnitDefinition_createWithName (const char *string)
{
  return new(std::nothrow) UnitDefinition("", string ? string : "");
}


/**
 * Frees the given UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_free (UnitDefinition_t *ud)
{
  delete static_cast<UnitDefinition*>(ud);
}


/**
 * @return the id of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getId (const UnitDefinition_t *ud)
{
  const UnitDefinition* x = static_cast<const UnitDefinition*>(ud);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this UnitDefinition.
 */
LIBSBML_EXTERN
const char *
UnitDefinition_getName (const UnitDefinition_t *ud)
{
  const UnitDefinition* x = static_cast<const UnitDefinition*>(ud);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return 1 if the id of this UnitDefinition has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetId (const UnitDefinition_t *ud)
{
  return static_cast<const UnitDefinition*>(ud)->isSetId();
}


/**
 * @return 1 if the name of this UnitDefinition has been set, 0 otherwise.
 *
 * In SBML L1, a UnitDefinition name is required and therefore <b>should
 * always be set</b>.  In L2, name is optional and as such may or may not
 * be set.
 */
LIBSBML_EXTERN
int
UnitDefinition_isSetName (const UnitDefinition_t *ud)
{
  return static_cast<const UnitDefinition*>(ud)->isSetName();
}


/**
 * Sets the id of this UnitDefinition to a copy of sid.
 */
LIBSBML_EXTERN
void
UnitDefinition_setId (UnitDefinition_t *ud, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<UnitDefinition*>(ud)->setId("");
  }
  else
  {
    static_cast<UnitDefinition*>(ud)->setId(sid);
  }
}


/**
 * Sets the name of this UnitDefinition to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
UnitDefinition_setName (UnitDefinition_t *ud, const char *string)
{
  if (string == NULL)
  {
    static_cast<UnitDefinition*>(ud)->unsetName();
  }
  else
  {
    static_cast<UnitDefinition*>(ud)->setName(string);
  }
}


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
UnitDefinition_unsetName (UnitDefinition_t *ud)
{
  static_cast<UnitDefinition*>(ud)->unsetName();
}


/**
 * Adds the given Unit to this UnitDefinition.
 */
LIBSBML_EXTERN
void
UnitDefinition_addUnit (UnitDefinition_t *ud, Unit_t *u)
{
  if (u != NULL)
  {
    static_cast<UnitDefinition*>(ud)->addUnit( * static_cast<Unit*>(u) );
  }
}


/**
 * @return the list of Units for this UnitDefinition.
 */
LIBSBML_EXTERN
ListOf_t *
UnitDefinition_getListOfUnits (UnitDefinition_t *ud)
{
  return (ListOf_t *) &
  static_cast<UnitDefinition*>(ud)->getListOfUnits();
}


/**
 * @return the nth Unit of this UnitDefinition
 */
LIBSBML_EXTERN
Unit_t *
UnitDefinition_getUnit (const UnitDefinition_t *ud, unsigned int n)
{
  return static_cast<const UnitDefinition*>(ud)->getUnit(n);
}


/**
 * @return the number of Units in this UnitDefinition.
 */
LIBSBML_EXTERN
unsigned int
UnitDefinition_getNumUnits (const UnitDefinition_t *ud)
{
  return static_cast<const UnitDefinition*>(ud)->getNumUnits();
}


/**
 * The UnitDefinitionIdCmp function compares the string sid to ud->id.
 *
 * @returns an integer less than, equal to, or greater than zero if sid is
 * found to be, respectively, less than, to match or be greater than ud->id.
 * Returns -1 if either sid or ud->id is NULL.
 */
LIBSBML_EXTERN
int
UnitDefinitionIdCmp (const char *sid, const UnitDefinition_t *ud)
{
  int result = -1;


  if (sid != NULL && UnitDefinition_isSetId(ud))
  {
    result = strcmp(sid, UnitDefinition_getId(ud));
  }

  return result;
}
