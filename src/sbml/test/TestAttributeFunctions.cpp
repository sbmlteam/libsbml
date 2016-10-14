/**
 * \file    TestAttributeFunctions.cpp
 * \brief   Attribute function tests
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBase.h>
#include <sbml/Model.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE


/*
 * We create a lot of strings in this file, for testing, and we don't 
 * do what this warning tries to help with, so we shut it up just
 * for this file.
 */
#ifdef __GNUC__ 
#pragma GCC diagnostic ignored "-Wwrite-strings"
#endif



BEGIN_C_DECLS

static SBase *S;


void
AttributeTest_setup (void)
{
  S = new(std::nothrow) Model(2, 4);

  if (S == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

}


void
AttributeTest_teardown (void)
{
  delete S;
}


START_TEST (test_Attributes_MetaId)
{
  const std::string& metaid = "x12345";
  std::string value;
  std::string other_value;
  int result;

  result = S->setAttribute("metaid", metaid);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S->getMetaId() == metaid);

  fail_unless(S->isSetMetaId() == true);
  fail_unless(S->isSetAttribute("metaid") == true);

  result = S->getAttribute("metaid", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == metaid);

  other_value = S->getAttribute<std::string>("metaid");

  fail_unless(other_value == metaid);

  result = S->unsetAttribute("metaid");

  fail_unless(S->isSetMetaId() == false);
  fail_unless(S->isSetAttribute("metaid") == false);

  result = S->getAttribute("metaid", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == "");

}
END_TEST


START_TEST (test_Attributes_Id)
{
  const std::string& id = "x12345";
  std::string value;
  std::string other_value;
  int result;

  result = S->setAttribute("id", id);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S->getIdAttribute() == id);

  fail_unless(S->isSetIdAttribute() == true);
  fail_unless(S->isSetAttribute("id") == true);

  result = S->getAttribute("id", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == id);

  other_value = S->getAttribute<std::string>("id");

  fail_unless(other_value == id);

  result = S->unsetAttribute("id");

  fail_unless(S->isSetIdAttribute() == false);
  fail_unless(S->isSetAttribute("id") == false);

  result = S->getAttribute("id", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == "");
}
END_TEST


START_TEST (test_Attributes_SBOTerm)
{
  int sboTerm = 5;
  int value;
  int other_value;
  int result;

  result = S->setAttribute("sboTerm", sboTerm);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(S->getSBOTerm() == sboTerm);

  fail_unless(S->isSetSBOTerm() == true);
  fail_unless(S->isSetAttribute("sboTerm") == true);

  result = S->getAttribute("sboTerm", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == sboTerm);

  other_value = S->getAttribute<int>("sboTerm");

  fail_unless(other_value == sboTerm);

  result = S->unsetAttribute("sboTerm");

  fail_unless(S->isSetSBOTerm() == false);
  fail_unless(S->isSetAttribute("sboTerm") == false);

  result = S->getAttribute("sboTerm", value);

  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value ==-1);
}
END_TEST


START_TEST (test_Attributes_AssignmentRule_variable)
{
  AssignmentRule *obj = new AssignmentRule(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("variable", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getVariable() == initialValue);
  fail_unless(obj->isSetVariable() == true);
  fail_unless(obj->isSetAttribute("variable") == true);

  result = obj->getAttribute("variable", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("variable");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("variable");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetVariable() == false);
  fail_unless(obj->isSetAttribute("variable") == false);

  result = obj->getAttribute("variable", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_Compartment_constant)
{
  Compartment *obj = new Compartment(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("constant", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getConstant() == initialValue);
  fail_unless(obj->isSetConstant() == true);
  fail_unless(obj->isSetAttribute("constant") == true);

  result = obj->getAttribute("constant", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<bool>("constant");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("constant");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetConstant() == false);
  fail_unless(obj->isSetAttribute("constant") == false);

  result = obj->getAttribute("constant", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST


START_TEST (test_Attributes_Compartment_size)
{
  Compartment *obj = new Compartment(3,1);
  double initialValue = 3.6;
  double value;
  double otherValue;
  int result;

  result = obj->setAttribute("size", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(obj->getSize(), initialValue));
  fail_unless(obj->isSetSize() == true);
  fail_unless(obj->isSetAttribute("size") == true);

  result = obj->getAttribute("size", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(value, initialValue));

  otherValue = static_cast<SBase*>(obj)->getAttribute<double>("size");
  fail_unless(util_isEqual(otherValue, initialValue));

  result = obj->unsetAttribute("size");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetSize() == false);
  fail_unless(obj->isSetAttribute("size") == false);

  result = obj->getAttribute("size", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isNaN(value));
}
END_TEST


START_TEST (test_Attributes_Compartment_spatialDimensions)
{
  Compartment *obj = new Compartment(3,1);
  unsigned int initialValue = 2;
  unsigned int value;
  unsigned int otherValue;
  int result;

  result = obj->setAttribute("spatialDimensions", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getSpatialDimensions() == initialValue);
  fail_unless(obj->isSetSpatialDimensions() == true);
  fail_unless(obj->isSetAttribute("spatialDimensions") == true);

  result = obj->getAttribute("spatialDimensions", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<unsigned
    int>("spatialDimensions");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("spatialDimensions");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetSpatialDimensions() == false);
  fail_unless(obj->isSetAttribute("spatialDimensions") == false);

  result = obj->getAttribute("spatialDimensions", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == 0);
}
END_TEST


START_TEST (test_Attributes_Compartment_units)
{
  Compartment *obj = new Compartment(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("units", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getUnits() == initialValue);
  fail_unless(obj->isSetUnits() == true);
  fail_unless(obj->isSetAttribute("units") == true);

  result = obj->getAttribute("units", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("units");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("units");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetUnits() == false);
  fail_unless(obj->isSetAttribute("units") == false);

  result = obj->getAttribute("units", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_Event_useValuesFromTriggerTime)
{
  Event *obj = new Event(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("useValuesFromTriggerTime", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getUseValuesFromTriggerTime() == initialValue);
  fail_unless(obj->isSetUseValuesFromTriggerTime() == true);
  fail_unless(obj->isSetAttribute("useValuesFromTriggerTime") == true);

  result = obj->getAttribute("useValuesFromTriggerTime", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<bool>("useValuesFromTriggerTime");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("useValuesFromTriggerTime");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetUseValuesFromTriggerTime() == false);
  fail_unless(obj->isSetAttribute("useValuesFromTriggerTime") == false);

  result = obj->getAttribute("useValuesFromTriggerTime", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST


START_TEST (test_Attributes_Event_timeUnits)
{
  Event *obj = new Event(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("timeUnits", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getTimeUnits() == initialValue);
  fail_unless(obj->isSetTimeUnits() == true);
  fail_unless(obj->isSetAttribute("timeUnits") == true);

  result = obj->getAttribute("timeUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<std::string>("timeUnits");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("timeUnits");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetTimeUnits() == false);
  fail_unless(obj->isSetAttribute("timeUnits") == false);

  result = obj->getAttribute("timeUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_EventAssignment_variable)
{
  EventAssignment *obj = new EventAssignment(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("variable", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getVariable() == initialValue);
  fail_unless(obj->isSetVariable() == true);
  fail_unless(obj->isSetAttribute("variable") == true);

  result = obj->getAttribute("variable", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("variable");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("variable");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetVariable() == false);
  fail_unless(obj->isSetAttribute("variable") == false);

  result = obj->getAttribute("variable", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_InitialAssignment_symbol)
{
  InitialAssignment *obj = new InitialAssignment(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("symbol", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getSymbol() == initialValue);
  fail_unless(obj->isSetSymbol() == true);
  fail_unless(obj->isSetAttribute("symbol") == true);

  result = obj->getAttribute("symbol", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("symbol");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("symbol");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetSymbol() == false);
  fail_unless(obj->isSetAttribute("symbol") == false);

  result = obj->getAttribute("symbol", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_KineticLaw_timeUnits)
{
  KineticLaw *obj = new KineticLaw(2,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("timeUnits", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getTimeUnits() == initialValue);
  fail_unless(obj->isSetTimeUnits() == true);
  fail_unless(obj->isSetAttribute("timeUnits") == true);

  result = obj->getAttribute("timeUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<std::string>("timeUnits");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("timeUnits");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetTimeUnits() == false);
  fail_unless(obj->isSetAttribute("timeUnits") == false);

  result = obj->getAttribute("timeUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_LocalParameter_value)
{
  LocalParameter *obj = new LocalParameter(3,1);
  double initialValue = 3.6;
  double value;
  double otherValue;
  int result;

  result = obj->setAttribute("value", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(obj->getValue(), initialValue));
  fail_unless(obj->isSetValue() == true);
  fail_unless(obj->isSetAttribute("value") == true);

  result = obj->getAttribute("value", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(value, initialValue));

  otherValue = static_cast<SBase*>(obj)->getAttribute<double>("value");
  fail_unless(util_isEqual(otherValue, initialValue));

  result = obj->unsetAttribute("value");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetValue() == false);
  fail_unless(obj->isSetAttribute("value") == false);

  result = obj->getAttribute("value", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isNaN(value));
}
END_TEST


START_TEST (test_Attributes_LocalParameter_units)
{
  LocalParameter *obj = new LocalParameter(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("units", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getUnits() == initialValue);
  fail_unless(obj->isSetUnits() == true);
  fail_unless(obj->isSetAttribute("units") == true);

  result = obj->getAttribute("units", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("units");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("units");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetUnits() == false);
  fail_unless(obj->isSetAttribute("units") == false);

  result = obj->getAttribute("units", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST




START_TEST (test_Attributes_Model_substanceUnits)
{
  Model *obj = new Model(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("substanceUnits", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getSubstanceUnits() == initialValue);
  fail_unless(obj->isSetSubstanceUnits() == true);
  fail_unless(obj->isSetAttribute("substanceUnits") == true);

  result = obj->getAttribute("substanceUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<std::string>("substanceUnits");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("substanceUnits");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetSubstanceUnits() == false);
  fail_unless(obj->isSetAttribute("substanceUnits") == false);

  result = obj->getAttribute("substanceUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_Parameter_constant)
{
  Parameter *obj = new Parameter(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("constant", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getConstant() == initialValue);
  fail_unless(obj->isSetConstant() == true);
  fail_unless(obj->isSetAttribute("constant") == true);

  result = obj->getAttribute("constant", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<bool>("constant");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("constant");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetConstant() == false);
  fail_unless(obj->isSetAttribute("constant") == false);

  result = obj->getAttribute("constant", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST


START_TEST (test_Attributes_Parameter_value)
{
  Parameter *obj = new Parameter(3,1);
  double initialValue = 3.6;
  double value;
  double otherValue;
  int result;

  result = obj->setAttribute("value", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(obj->getValue(), initialValue));
  fail_unless(obj->isSetValue() == true);
  fail_unless(obj->isSetAttribute("value") == true);

  result = obj->getAttribute("value", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(value, initialValue));

  otherValue = static_cast<SBase*>(obj)->getAttribute<double>("value");
  fail_unless(util_isEqual(otherValue, initialValue));

  result = obj->unsetAttribute("value");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetValue() == false);
  fail_unless(obj->isSetAttribute("value") == false);

  result = obj->getAttribute("value", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isNaN(value));
}
END_TEST


START_TEST (test_Attributes_Parameter_units)
{
  Parameter *obj = new Parameter(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("units", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getUnits() == initialValue);
  fail_unless(obj->isSetUnits() == true);
  fail_unless(obj->isSetAttribute("units") == true);

  result = obj->getAttribute("units", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("units");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("units");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetUnits() == false);
  fail_unless(obj->isSetAttribute("units") == false);

  result = obj->getAttribute("units", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST


START_TEST (test_Attributes_RateRule_variable)
{
  RateRule *obj = new RateRule(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("variable", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getVariable() == initialValue);
  fail_unless(obj->isSetVariable() == true);
  fail_unless(obj->isSetAttribute("variable") == true);

  result = obj->getAttribute("variable", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("variable");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("variable");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetVariable() == false);
  fail_unless(obj->isSetAttribute("variable") == false);

  result = obj->getAttribute("variable", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST



START_TEST (test_Attributes_Reaction_fast)
{
  Reaction *obj = new Reaction(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("fast", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getFast() == initialValue);
  fail_unless(obj->isSetFast() == true);
  fail_unless(obj->isSetAttribute("fast") == true);

  result = obj->getAttribute("fast", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<bool>("fast");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("fast");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetFast() == false);
  fail_unless(obj->isSetAttribute("fast") == false);

  result = obj->getAttribute("fast", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST


START_TEST (test_Attributes_Reaction_compartment)
{
  Reaction *obj = new Reaction(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("compartment", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getCompartment() == initialValue);
  fail_unless(obj->isSetCompartment() == true);
  fail_unless(obj->isSetAttribute("compartment") == true);

  result = obj->getAttribute("compartment", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<std::string>("compartment");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("compartment");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetCompartment() == false);
  fail_unless(obj->isSetAttribute("compartment") == false);

  result = obj->getAttribute("compartment", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST



START_TEST (test_Attributes_SimpleSpeciesReference_species)
{
  SpeciesReference *obj = new SpeciesReference(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("species", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getSpecies() == initialValue);
  fail_unless(obj->isSetSpecies() == true);
  fail_unless(obj->isSetAttribute("species") == true);

  result = obj->getAttribute("species", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<std::string>("species");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("species");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetSpecies() == false);
  fail_unless(obj->isSetAttribute("species") == false);

  result = obj->getAttribute("species", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST




START_TEST (test_Attributes_Species_hasOnlySubstanceUnits)
{
  Species *obj = new Species(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("hasOnlySubstanceUnits", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getHasOnlySubstanceUnits() == initialValue);
  fail_unless(obj->isSetHasOnlySubstanceUnits() == true);
  fail_unless(obj->isSetAttribute("hasOnlySubstanceUnits") == true);

  result = obj->getAttribute("hasOnlySubstanceUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<bool>("hasOnlySubstanceUnits");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("hasOnlySubstanceUnits");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetHasOnlySubstanceUnits() == false);
  fail_unless(obj->isSetAttribute("hasOnlySubstanceUnits") == false);

  result = obj->getAttribute("hasOnlySubstanceUnits", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST


START_TEST (test_Attributes_Species_charge)
{
  Species *obj = new Species(2,1);
  int initialValue = 2;
  int value;
  int otherValue;
  int result;

  result = obj->setAttribute("charge", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getCharge() == initialValue);
  fail_unless(obj->isSetCharge() == true);
  fail_unless(obj->isSetAttribute("charge") == true);

  result = obj->getAttribute("charge", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<int>("charge");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("charge");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetCharge() == false);
  fail_unless(obj->isSetAttribute("charge") == false);

  result = obj->getAttribute("charge", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == 0);
}
END_TEST


START_TEST (test_Attributes_Species_initialAmount)
{
  Species *obj = new Species(3,1);
  double initialValue = 3.6;
  double value;
  double otherValue;
  int result;

  result = obj->setAttribute("initialAmount", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(obj->getInitialAmount(), initialValue));
  fail_unless(obj->isSetInitialAmount() == true);
  fail_unless(obj->isSetAttribute("initialAmount") == true);

  result = obj->getAttribute("initialAmount", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(value, initialValue));

  otherValue = static_cast<SBase*>(obj)->getAttribute<double>("initialAmount");
  fail_unless(util_isEqual(otherValue, initialValue));

  result = obj->unsetAttribute("initialAmount");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetInitialAmount() == false);
  fail_unless(obj->isSetAttribute("initialAmount") == false);

  result = obj->getAttribute("initialAmount", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isNaN(value));
}
END_TEST


START_TEST (test_Attributes_Species_compartment)
{
  Species *obj = new Species(3,1);
  std::string initialValue = "string";
  std::string value;
  std::string otherValue;
  int result;

  result = obj->setAttribute("compartment", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getCompartment() == initialValue);
  fail_unless(obj->isSetCompartment() == true);
  fail_unless(obj->isSetAttribute("compartment") == true);

  result = obj->getAttribute("compartment", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue =
    static_cast<SBase*>(obj)->getAttribute<std::string>("compartment");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("compartment");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetCompartment() == false);
  fail_unless(obj->isSetAttribute("compartment") == false);

  result = obj->getAttribute("compartment", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value.empty());
}
END_TEST



START_TEST (test_Attributes_SpeciesReference_constant)
{
  SpeciesReference *obj = new SpeciesReference(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("constant", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getConstant() == initialValue);
  fail_unless(obj->isSetConstant() == true);
  fail_unless(obj->isSetAttribute("constant") == true);

  result = obj->getAttribute("constant", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<bool>("constant");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("constant");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetConstant() == false);
  fail_unless(obj->isSetAttribute("constant") == false);

  result = obj->getAttribute("constant", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST


START_TEST (test_Attributes_SpeciesReference_stoichiometry)
{
  SpeciesReference *obj = new SpeciesReference(3,1);
  double initialValue = 3.6;
  double value;
  double otherValue;
  int result;

  result = obj->setAttribute("stoichiometry", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(obj->getStoichiometry(), initialValue));
  fail_unless(obj->isSetStoichiometry() == true);
  fail_unless(obj->isSetAttribute("stoichiometry") == true);

  result = obj->getAttribute("stoichiometry", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(value, initialValue));

  otherValue = static_cast<SBase*>(obj)->getAttribute<double>("stoichiometry");
  fail_unless(util_isEqual(otherValue, initialValue));

  result = obj->unsetAttribute("stoichiometry");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetStoichiometry() == false);
  fail_unless(obj->isSetAttribute("stoichiometry") == false);

  result = obj->getAttribute("stoichiometry", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isNaN(value));
}
END_TEST


START_TEST (test_Attributes_SpeciesReference_denominator)
{
  SpeciesReference *obj = new SpeciesReference(3,1);
  unsigned int initialValue = 2;
  unsigned int value;
  unsigned int otherValue;
  int result;

  result = obj->setAttribute("denominator", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getDenominator() == initialValue);
  fail_unless(obj->isSetAttribute("denominator") == true);

  result = obj->getAttribute("denominator", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<unsigned
    int>("denominator");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("denominator");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetAttribute("denominator") == true);

  result = obj->getAttribute("denominator", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == 1);
}
END_TEST



START_TEST (test_Attributes_Trigger_initialValue)
{
  Trigger *obj = new Trigger(3,1);
  bool initialValue = true;
  bool value;
  bool otherValue;
  int result;

  result = obj->setAttribute("initialValue", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getInitialValue() == initialValue);
  fail_unless(obj->isSetInitialValue() == true);
  fail_unless(obj->isSetAttribute("initialValue") == true);

  result = obj->getAttribute("initialValue", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<bool>("initialValue");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("initialValue");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetInitialValue() == false);
  fail_unless(obj->isSetAttribute("initialValue") == false);

  result = obj->getAttribute("initialValue", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == true);
}
END_TEST




START_TEST (test_Attributes_Unit_scale)
{
  Unit *obj = new Unit(3,1);
  int initialValue = 2;
  int value;
  int otherValue;
  int result;

  result = obj->setAttribute("scale", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->getScale() == initialValue);
  fail_unless(obj->isSetScale() == true);
  fail_unless(obj->isSetAttribute("scale") == true);

  result = obj->getAttribute("scale", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == initialValue);

  otherValue = static_cast<SBase*>(obj)->getAttribute<int>("scale");
  fail_unless(otherValue == initialValue);

  result = obj->unsetAttribute("scale");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetScale() == false);
  fail_unless(obj->isSetAttribute("scale") == false);

  result = obj->getAttribute("scale", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(value == SBML_INT_MAX);
}
END_TEST


START_TEST (test_Attributes_Unit_multiplier)
{
  Unit *obj = new Unit(3,1);
  double initialValue = 3.6;
  double value;
  double otherValue;
  int result;

  result = obj->setAttribute("multiplier", initialValue);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(obj->getMultiplier(), initialValue));
  fail_unless(obj->isSetMultiplier() == true);
  fail_unless(obj->isSetAttribute("multiplier") == true);

  result = obj->getAttribute("multiplier", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isEqual(value, initialValue));

  otherValue = static_cast<SBase*>(obj)->getAttribute<double>("multiplier");
  fail_unless(util_isEqual(otherValue, initialValue));

  result = obj->unsetAttribute("multiplier");
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(obj->isSetMultiplier() == false);
  fail_unless(obj->isSetAttribute("multiplier") == false);

  result = obj->getAttribute("multiplier", value);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);
  fail_unless(util_isNaN(value));
}
END_TEST








Suite *
create_suite_Attributes (void)
{
  Suite *suite = suite_create("Generic Attribute Functions");
  TCase *tcase = tcase_create("Generic Attribute Functions");


  tcase_add_checked_fixture(tcase, AttributeTest_setup, AttributeTest_teardown);

  tcase_add_test(tcase, test_Attributes_MetaId     );
  tcase_add_test(tcase, test_Attributes_Id     );
  tcase_add_test(tcase, test_Attributes_SBOTerm     );
  tcase_add_test(tcase, test_Attributes_AssignmentRule_variable);
  tcase_add_test(tcase, test_Attributes_Compartment_constant);
  tcase_add_test(tcase, test_Attributes_Compartment_size);
  tcase_add_test(tcase, test_Attributes_Compartment_spatialDimensions);
  tcase_add_test(tcase, test_Attributes_Compartment_units);
tcase_add_test(tcase, test_Attributes_Event_useValuesFromTriggerTime);
tcase_add_test(tcase, test_Attributes_Event_timeUnits);
tcase_add_test(tcase, test_Attributes_EventAssignment_variable);
tcase_add_test(tcase, test_Attributes_InitialAssignment_symbol);
tcase_add_test(tcase, test_Attributes_KineticLaw_timeUnits);
tcase_add_test(tcase, test_Attributes_LocalParameter_value);
tcase_add_test(tcase, test_Attributes_LocalParameter_units);
tcase_add_test(tcase, test_Attributes_Model_substanceUnits);
tcase_add_test(tcase, test_Attributes_Parameter_constant);
tcase_add_test(tcase, test_Attributes_Parameter_value);
tcase_add_test(tcase, test_Attributes_Parameter_units);
tcase_add_test(tcase, test_Attributes_RateRule_variable);
tcase_add_test(tcase, test_Attributes_Reaction_fast);
tcase_add_test(tcase, test_Attributes_Reaction_compartment);
tcase_add_test(tcase, test_Attributes_SimpleSpeciesReference_species);
tcase_add_test(tcase, test_Attributes_Species_hasOnlySubstanceUnits);
tcase_add_test(tcase, test_Attributes_Species_charge);
tcase_add_test(tcase, test_Attributes_Species_initialAmount);
tcase_add_test(tcase, test_Attributes_Species_compartment);
tcase_add_test(tcase, test_Attributes_SpeciesReference_constant);
tcase_add_test(tcase, test_Attributes_SpeciesReference_stoichiometry);
tcase_add_test(tcase, test_Attributes_SpeciesReference_denominator);
tcase_add_test(tcase, test_Attributes_Trigger_initialValue);
tcase_add_test(tcase, test_Attributes_Unit_scale);
tcase_add_test(tcase, test_Attributes_Unit_multiplier);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
