/**
 *
 * @file    FormulaUnitsData.cpp
 * @brief   Class for storing information relating to units of a formula
 * @author  SBML Team <sbml-team@caltech.edu>
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
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/Model.h>
#include <sbml/units/FormulaUnitsData.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/***********************************************************
* FormulaUnitsData class
*/
FormulaUnitsData::FormulaUnitsData()
{
  mContainsUndeclaredUnits = false;
  mCanIgnoreUndeclaredUnits = true;
  mUnitDefinition = new UnitDefinition();
  mPerTimeUnitDefinition = new UnitDefinition();
  mEventTimeUnitDefinition = new UnitDefinition();
}

FormulaUnitsData::FormulaUnitsData(const FormulaUnitsData& orig)
{
  mContainsUndeclaredUnits = 
                          orig.mContainsUndeclaredUnits;
  mCanIgnoreUndeclaredUnits = orig.mCanIgnoreUndeclaredUnits;
  if (orig.mUnitDefinition) 
  {
    mUnitDefinition = static_cast <UnitDefinition*> 
                                      (orig.mUnitDefinition->clone());
  }
  else
  {
    mUnitDefinition = NULL;
  }
  if (orig.mPerTimeUnitDefinition)
  {
    mPerTimeUnitDefinition = static_cast <UnitDefinition*> 
                                (orig.mPerTimeUnitDefinition->clone());
  }
  else
  {
    mPerTimeUnitDefinition = NULL;
  }
  if (orig.mEventTimeUnitDefinition)
  {
    mEventTimeUnitDefinition = static_cast <UnitDefinition*> 
                               (orig.mEventTimeUnitDefinition->clone());
  }
  else
  {
    mEventTimeUnitDefinition = NULL;
  }
}

FormulaUnitsData::~FormulaUnitsData()
{
  if (mUnitDefinition)              delete mUnitDefinition;
  if (mPerTimeUnitDefinition)       delete mPerTimeUnitDefinition;
  if (mEventTimeUnitDefinition)     delete mEventTimeUnitDefinition;
}

SBase*
FormulaUnitsData::clone() const
{
  return new FormulaUnitsData(*this);
}


/**
 * Get the unitReferenceId of this FormulaUnitsData.
 * 
 * @return the value of the unitReferenceId of this 
 * FormulaUnitsData as a string.
 */
const string& 
FormulaUnitsData::getUnitReferenceId() 
{ 
  return mUnitReferenceId; 
}

/**
 * Get the unitReferenceId of this FormulaUnitsData.
 * 
 * @return the value of the unitReferenceId of this 
 * FormulaUnitsData as a string.
 */
const string& 
FormulaUnitsData::getUnitReferenceId() const 
{ 
  return mUnitReferenceId; 
}


SBMLTypeCode_t 
FormulaUnitsData::getComponentTypecode() 
{ 
  return mTypeOfElement; 
}

const SBMLTypeCode_t 
FormulaUnitsData::getComponentTypecode() const 
{ 
  return mTypeOfElement; 
}

/**
  * Predicate returning @c true or @c false depending on whether this
  * FormulaUnitsData includes parameters/numbers with undeclared units.
  * 
  * @return @c true if the FormulaUnitsData includes parameters/numbers 
  * with undeclared units, @c false otherwise.
  */
bool 
FormulaUnitsData::getContainsUndeclaredUnits() 
{ 
  return mContainsUndeclaredUnits; 
}

/**
  * Predicate returning @c true or @c false depending on whether this
  * FormulaUnitsData includes parameters/numbers with undeclared units.
  * 
  * @return @c true if the FormulaUnitsData includes parameters/numbers 
  * with undeclared units, @c false otherwise.
  */
const bool 
FormulaUnitsData::getContainsUndeclaredUnits() const
{ 
  return mContainsUndeclaredUnits; 
}

/**
  * @return @c true if the parameters/numbers 
  * with undeclared units can be ignored, @c false otherwise.
  */
bool 
FormulaUnitsData::getCanIgnoreUndeclaredUnits() 
{ 
  return mCanIgnoreUndeclaredUnits; 
}

/**
  * @return @c true if the parameters/numbers 
  * with undeclared units can be ignored, @c false otherwise.
  */
const bool 
FormulaUnitsData::getCanIgnoreUndeclaredUnits() const 
{ 
  return mCanIgnoreUndeclaredUnits; 
}

/**
  * Get the unit definition for this FormulaUnitsData.
  * 
  * @return the UnitDefinition object of this FormulaUnitsData.
  *
  * @note the UnitDefinition object is constructed to represent
  * the units associated with the component used to populate 
  * this FormulaUnitsData object.
  */
UnitDefinition * 
FormulaUnitsData::getUnitDefinition() 
{ 
  return mUnitDefinition; 
}

/**
  * Get the unit definition for this FormulaUnitsData.
  * 
  * @return the UnitDefinition object of this FormulaUnitsData.
  *
  * @note the UnitDefinition object is constructed to represent
  * the units associated with the component used to populate 
  * this FormulaUnitsData object.
  */
const UnitDefinition * 
FormulaUnitsData::getUnitDefinition() const 
{ 
  return mUnitDefinition; 
}

/**
  * Get the 'perTime' unit definition for this FormulaUnitsData.
  * 
  * @return the 'perTime' UnitDefinition object of this FormulaUnitsData.
  *
  * @note the perTime UnitDefinition object is constructed to represent
  * the units associated with the component used to populate 
  * this FormulaUnitsData object divided by the time units for the model.
  */
UnitDefinition * 
FormulaUnitsData::getPerTimeUnitDefinition() 
{ 
  return mPerTimeUnitDefinition; 
}

/**
  * Get the 'perTime' unit definition for this FormulaUnitsData.
  * 
  * @return the 'perTime' UnitDefinition object of this FormulaUnitsData.
  *
  * @note the perTime UnitDefinition object is constructed to represent
  * the units associated with the component used to populate 
  * this FormulaUnitsData object divided by the time units for the model.
  */
const UnitDefinition * 
FormulaUnitsData::getPerTimeUnitDefinition() const 
{ 
  return mPerTimeUnitDefinition; 
}

/**
  * Get the 'EventTime' unit definition for this FormulaUnitsData.
  * 
  * @return the 'EventTime' UnitDefinition object of this FormulaUnitsData.
  *
  * @note the EventTime UnitDefinition object is constructed to represent
  * the time units associated with the Event used to populate 
  * this FormulaUnitsData object.
  */
UnitDefinition * 
FormulaUnitsData::getEventTimeUnitDefinition() 
{ 
  return mEventTimeUnitDefinition; 
}

/**
  * Get the 'EventTime' unit definition for this FormulaUnitsData.
  * 
  * @return the 'EventTime' UnitDefinition object of this FormulaUnitsData.
  *
  * @note the EventTime UnitDefinition object is constructed to represent
  * the time units associated with the Event used to populate 
  * this FormulaUnitsData object.
  */
const UnitDefinition * 
FormulaUnitsData::getEventTimeUnitDefinition() const 
{ 
  return mEventTimeUnitDefinition; 
}

/**
  * Sets the unitReferenceId attribute of this FormulaUnitsData.
  *
  * @param unitReferenceId the identifier of the object defined
  * elsewhere in this Model for which this FormulaUnitsData contains
  * unit information.
  */
void 
FormulaUnitsData::setUnitReferenceId(const std::string& unitReferenceId) 
{ 
  mUnitReferenceId = unitReferenceId; 
}

/**
  * Sets the SBMLTypecode of this FormulaUnitsData.
  * 
  * @param typecode the SBMLTypeCode_t of the object defined
  * elsewhere in this Model for which this FormulaUnitsData contains
  * unit information.
  */
void 
FormulaUnitsData::setComponentTypecode(SBMLTypeCode_t typecode) 
{ 
  mTypeOfElement = typecode; 
}


/**
  * Sets the value of the "containsUndeclaredUnits" flag for this 
  * FormulaUnitsData.
  * 
  * @parameter flag boolean value indicating whether the FormulaUnitsData 
  * includes parameters/numbers with undeclared units.
  */
void 
FormulaUnitsData::setContainsParametersWithUndeclaredUnits(bool flag)
{ 
  mContainsUndeclaredUnits = flag; 
}

/**
  * Sets the value of the "canIgnoreUndeclaredUnits" flag for this 
  * FormulaUnitsData.
  * 
  * @parameter flag boolean value indicating whether parameters/numbers 
  * with undeclared units can be ignored.
  */
void 
FormulaUnitsData::setCanIgnoreUndeclaredUnits(bool flag)
{ 
  mCanIgnoreUndeclaredUnits = flag; 
}

/**
  * Set the unit definition for this FormulaUnitsData.
  * 
  * @parameter ud the UnitDefinition object constructed to represent
  * the units associated with the component used to populate 
  * this FormulaUnitsData object.
  */
void 
FormulaUnitsData::setUnitDefinition(UnitDefinition * ud) 
{ 
  if(ud == mUnitDefinition) return;
  
  delete mUnitDefinition; 
  mUnitDefinition = ud; 
}

/**
  * Set the 'perTime' unit definition for this FormulaUnitsData.
  * 
  * @parameter ud the UnitDefinition object constructed to represent
  * the units associated with the component used to populate 
  * this FormulaUnitsData object divided by the time units for the model.
  */
void 
FormulaUnitsData::setPerTimeUnitDefinition(UnitDefinition * ud) 
{ 
  if(ud == mPerTimeUnitDefinition) return;
  
  delete mPerTimeUnitDefinition;
  mPerTimeUnitDefinition = ud; 
}

/**
  * Set the 'EventTime' unit definition for this FormulaUnitsData.
  * 
  * @parameter ud the UnitDefinition object constructed to represent
  * the time units associated with the Event used to populate 
  * this FormulaUnitsData object.
  */
void 
FormulaUnitsData::setEventTimeUnitDefinition(UnitDefinition * ud) 
{ 
  if(ud == mEventTimeUnitDefinition) return;
  
  delete mEventTimeUnitDefinition;
  mEventTimeUnitDefinition = ud; 
}

/** @cond doxygen-libsbml-internal */
SBMLTypeCode_t
FormulaUnitsData::getTypeCode () const
{
  return SBML_FORMULA_UNITS_DATA;
}


const string&
FormulaUnitsData::getElementName() const
{
  static const string name = "formulaUnits";
  return name;
}


bool 
FormulaUnitsData::accept (SBMLVisitor& v) const
{
  return true;
}
/** @endcond doxygen-libsbml-internal */


