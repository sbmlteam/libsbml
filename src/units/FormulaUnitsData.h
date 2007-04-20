/**
 * @file    FormulaUnitsData.h
 * @brief   Class for storing information relating to units of a formula
 * @author  SBML Team <sbml-team@caltech.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
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

#include "UnitFormulaFormatter.h"

class SBMLVisitor;

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


class LIBSBML_EXTERN FormulaUnitsData : public SBase
{
public:
   
  /** 
   * constructor
   */
  FormulaUnitsData();

  FormulaUnitsData(const FormulaUnitsData&);

  /** 
   * destructor
   */
  virtual ~FormulaUnitsData();
 
  /**
   * Accepts the given SBMLVisitor.
   *
   * @return the result of calling <code>v.visit()</code>, which indicates
   * whether or not the Visitor would like to visit the Model's next
   * Compartment (if available).
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Compartment.
   */
  virtual SBase* clone () const;
  /**
   * returns the id associated with the units data
   * if this is a species/compartment/parameter it will be the id 
   * if it is aa assignment/rate rule it will be the variable
   */
  const string& getId() { return mId; };
  const string& getId() const { return mId; };
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
   * sets the id of the FormulaUnitsData
   *
   */
  void setId(const std::string& id) { mId = id; };
    
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
  void setUnitDefinition(UnitDefinition * ud) { mUnitDefinition = ud; };

  /**
  * sets the PerTimeUnitDefinition relating to the FormulaUnitsData object
  */
  void setPerTimeUnitDefinition(UnitDefinition * ud) { mPerTimeUnitDefinition = ud; };

  /**
  * sets the EventTimeUnitDefinition relating to the FormulaUnitsData object
  */
  void setEventTimeUnitDefinition(UnitDefinition * ud) { mEventTimeUnitDefinition = ud; };

  /**
   * @return the name of this element ie "".
   
   */
  virtual const std::string& getElementName () const;

protected:

    std::string mId;

    unsigned int mContainsParametersWithUndeclaredUnits;
    unsigned int mCanIgnoreUndeclaredUnits;

    SBMLTypeCode_t mTypeOfElement;

    UnitDefinition * mUnitDefinition;
    UnitDefinition * mPerTimeUnitDefinition;
    UnitDefinition * mEventTimeUnitDefinition;

};

class LIBSBML_EXTERN ListFormulaUnitsData : public ListOf
{
public:

  /**
   * @return a (deep) copy of this ListOfCompartments.
   */
  virtual SBase* clone () const;


};


#endif /* __cplusplus */




#endif
