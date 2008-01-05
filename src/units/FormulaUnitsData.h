/**
 * @cond doxygen-libsbml-internal
 *
 * @file    FormulaUnitsData.h
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

#ifndef FormulaUnitsData_h
#define FormulaUnitsData_h


#ifdef __cplusplus
#include <sbml/SBase.h>

#include <sbml/util/List.h>
#include <sbml/ListOf.h>
#include <sbml/UnitDefinition.h>
#include <sbml/Model.h>
#include <sbml/SBMLTypeCodes.h>

#include <sbml/math/FormulaFormatter.h>

#include <sbml/units/UnitFormulaFormatter.h>

class SBMLVisitor;

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


class LIBSBML_EXTERN FormulaUnitsData : public SBase
{
public:
   
  /**
   * Creates a new FormulaUnitsData.
   */
  FormulaUnitsData();

  /**
   * Copy constructor; creates a copy of this FormulaUnitsData.
   */
  FormulaUnitsData(const FormulaUnitsData& orig);

  /**
   * Destroys this FormulaUnitsData.
   */
  virtual ~FormulaUnitsData();
 
  /** @cond doxygen-libsbml-internal */
  /**
   * Accepts the given SBMLVisitor for this instance of Constraint.
   *
   * @param v the SBMLVisitor instance to be used.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether the Visitor would like to visit the next FormulaUnitsData in the
   * list of FormulaUnitsData within which this FormulaUnitsData is embedded 
   */
  virtual bool accept (SBMLVisitor& v) const;
  /** @endcond doxygen-libsbml-internal */

  /**
   * Creates and returns a deep copy of this FormulaUnitsData.
   * 
   * @return a (deep) copy of this FormulaUnitsData.
   */
  virtual SBase* clone () const;
  
  
  /**
   * Get the unitReferenceId of this FormulaUnitsData.
   * 
   * @return the value of the unitReferenceId 
   * Compartment as a string.
   */
  /**
   * returns the id associated with the units data
   * if this is a species/compartment/parameter it will be the id 
   * if it is aa assignment/rate rule it will be the variable
   */
  const string& getUnitReferenceId() { return mUnitReferenceId; };
  const string& getUnitReferenceId() const { return mUnitReferenceId; };
  /**
   * returns the SBMLTypeCode of the component used
   * to create the FormulaUnitsData object
   */
  SBMLTypeCode_t getTypecode() { return mTypeOfElement; };
  const SBMLTypeCode_t getTypecode() const { return mTypeOfElement; };

  /**
   * returns the ContainsParametersWithUndeclaredUnits flag
   */
  unsigned int getContainsParametersWithUndeclaredUnits() 
                                { return mContainsParametersWithUndeclaredUnits; };
  const unsigned int getContainsParametersWithUndeclaredUnits() const
                                { return mContainsParametersWithUndeclaredUnits; };

  /**
   * returns the CanIgnoreUndeclaredUnits flag
   */
  unsigned int getCanIgnoreUndeclaredUnits() { return mCanIgnoreUndeclaredUnits; };
  const unsigned int getCanIgnoreUndeclaredUnits() const { return mCanIgnoreUndeclaredUnits; };

  /**
  * returns the UnitDefinition relating to the FormulaUnitsData object
  */
  UnitDefinition * getUnitDefinition() { return mUnitDefinition; };
  const UnitDefinition * getUnitDefinition() const { return mUnitDefinition; };

  /**
  * returns the PerTimeUnitDefinition relating to the FormulaUnitsData object
  */
  UnitDefinition * getPerTimeUnitDefinition() { return mPerTimeUnitDefinition; };
  const UnitDefinition * getPerTimeUnitDefinition() const { return mPerTimeUnitDefinition; };

  /**
  * returns the EventTimeUnitDefinition relating to the FormulaUnitsData object
  */
  UnitDefinition * getEventTimeUnitDefinition() { return mEventTimeUnitDefinition; };
  const UnitDefinition * getEventTimeUnitDefinition() const { return mEventTimeUnitDefinition; };

  /**
  * returns the L1SpeciesConcUnitDefinition relating to the FormulaUnitsData object
  */
  UnitDefinition * getL1SpeciesConcUnitDefinition() { return mL1SpeciesConcUnitDefinition; };
  const UnitDefinition * getL1SpeciesConcUnitDefinition() const { return mL1SpeciesConcUnitDefinition; };

  /**
  * returns the L1SpeciesConcPerTimeUnitDefinition relating to the FormulaUnitsData object
  */
  UnitDefinition * getL1SpeciesConcPerTimeUnitDefinition() { return mL1SpeciesConcPerTimeUnitDefinition; };
  const UnitDefinition * getL1SpeciesConcPerTimeUnitDefinition() const { return mL1SpeciesConcPerTimeUnitDefinition; };

  /**
   * sets the id of the FormulaUnitsData
   *
   */
  void setUnitReferenceId(const std::string& unitReferenceId) { mUnitReferenceId = unitReferenceId; };
    
  /**
   * sets the SBMLTypeCode of the FormulaUnitsData object
   */
  void setTypecode(SBMLTypeCode_t typecode) { mTypeOfElement = typecode; };

  /**
  * sets the ContainsParametersWithUndeclaredUnits flag
  */
  void setContainsParametersWithUndeclaredUnits(unsigned int flag)
                                  { mContainsParametersWithUndeclaredUnits = flag; };

  /**
  * sets the CanIgnoreUndeclaredUnits flag
  */
  void setCanIgnoreUndeclaredUnits(unsigned int flag)
                                                { mCanIgnoreUndeclaredUnits = flag; };

  /**
  * sets the UnitDefinition relating to the FormulaUnitsData object
  */
  void setUnitDefinition(UnitDefinition * ud) 
  { 
    if(ud == mUnitDefinition) return;
    delete mUnitDefinition; 
    mUnitDefinition = ud; 
  };

  /**
  * sets the PerTimeUnitDefinition relating to the FormulaUnitsData object
  */
  void setPerTimeUnitDefinition(UnitDefinition * ud) 
  { 
    if(ud == mPerTimeUnitDefinition) return;
    delete mPerTimeUnitDefinition;
    mPerTimeUnitDefinition = ud; 
  };

  /**
  * sets the EventTimeUnitDefinition relating to the FormulaUnitsData object
  */
  void setEventTimeUnitDefinition(UnitDefinition * ud) 
  { 
    if(ud == mEventTimeUnitDefinition) return;
    delete mEventTimeUnitDefinition;
    mEventTimeUnitDefinition = ud; 
  };

  /**
  * sets the EventTimeUnitDefinition relating to the FormulaUnitsData object
  */
  void setL1SpeciesConcUnitDefinition(UnitDefinition * ud) 
  { 
    if(ud == mL1SpeciesConcUnitDefinition) return;
    delete mL1SpeciesConcUnitDefinition;
    mL1SpeciesConcUnitDefinition = ud; 
  };

  /**
  * sets the EventTimeUnitDefinition relating to the FormulaUnitsData object
  */
  void setL1SpeciesConcPerTimeUnitDefinition(UnitDefinition * ud) 
  { 
    if(ud == mL1SpeciesConcPerTimeUnitDefinition) return;
    delete mL1SpeciesConcPerTimeUnitDefinition;
    mL1SpeciesConcPerTimeUnitDefinition = ud; 
  };

  /**
   * Returns the libSBML type code for this %SBML object.
   * 
   * @return the SBMLTypeCode_t of this object or SBML_UNKNOWN (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;


  /**
   * @return the name of this element ie "".
   
   */
  virtual const std::string& getElementName () const;

protected:

    std::string mUnitReferenceId;

    unsigned int mContainsParametersWithUndeclaredUnits;
    unsigned int mCanIgnoreUndeclaredUnits;

    SBMLTypeCode_t mTypeOfElement;

    UnitDefinition * mUnitDefinition;
    UnitDefinition * mPerTimeUnitDefinition;
    UnitDefinition * mEventTimeUnitDefinition;
    UnitDefinition * mL1SpeciesConcUnitDefinition;
    UnitDefinition * mL1SpeciesConcPerTimeUnitDefinition;

};


#endif /* __cplusplus */




#endif


/** @endcond doxygen-libsbml-internal */
