/**
 * @file    TestSBMLReactionConverter.cpp
 * @brief   Tests for reaction to raterule conversion 
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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

#include <sbml/SBase.h>
#include <sbml/SBMLTypes.h>

#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLReactionConverter.h>

#include <sbml/math/FormulaParser.h>

#include <string>
#include <iostream>
#include <check.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE
BEGIN_C_DECLS

static ConversionProperties rn_rule_props;
static SBMLReactionConverter* rn_rule_converter;


static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}

static bool
formulas_equal(const char* expected, ASTNode* actual)
{
	return equals(expected, SBML_formulaToL3String(actual));
}

extern char *TestDataDirectory;

void
Reaction_setup(void)
{
	rn_rule_props.addOption("SBML Reaction Converter", true);

    rn_rule_converter = new SBMLReactionConverter();
    rn_rule_converter->setProperties(&rn_rule_props);
}

void
Reaction_teardown(void)
{
    delete rn_rule_converter;
}
START_TEST(test_reactionconversion_01)
{
	std::string filename(TestDataDirectory);
	filename += "valid_01_rr.xml";
	std::string filename1(TestDataDirectory);
	filename1 += "valid_01_bio.xml";

	SBMLDocument* d_rule = readSBMLFromFile(filename.c_str());
	SBMLDocument* d_rn = readSBMLFromFile(filename1.c_str());
    
	rn_rule_converter->setDocument(d_rn);
	fail_unless(rn_rule_converter->convert() == LIBSBML_OPERATION_SUCCESS);

	std::string out = writeSBMLToStdString(d_rn);
	std::string expected = writeSBMLToStdString(d_rule);

	fail_unless(equals(expected.c_str(), out.c_str()));

	delete d_rn;
    delete d_rule;
}
END_TEST


Suite*
create_suite_TestSBMLReactionConverter(void)
{
	bool testing = true;
	Suite* suite = suite_create("SBMLReactionConverter");
	TCase* tcase = tcase_create("SBMLReactionConverter");
	tcase_add_checked_fixture(tcase, Reaction_setup,
		Reaction_teardown);

	if (testing)
	{
		tcase_add_test(tcase, test_reactionconversion_01);
	}
	else
	{
		tcase_add_test(tcase, test_reactionconversion_01);
	}
	suite_add_tcase(suite, tcase);

	return suite;
}
END_C_DECLS
