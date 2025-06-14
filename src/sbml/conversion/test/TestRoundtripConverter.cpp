/**
 * @file    TestSBMLRoundtripConverter.cpp
 * @brief   Tests for raterule to reaction round tripping
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
#include <sbml/conversion/SBMLRateRuleConverter.h>
#include <sbml/conversion/ExpressionAnalyser.h>
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
static ConversionProperties rule_rn_props;
static SBMLRateRuleConverter* rule_rn_converter;

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
Roundtrip_setup(void)
{
	rule_rn_props.addOption("inferReactions", true);

	rule_rn_converter = new SBMLRateRuleConverter();
	rule_rn_converter->setProperties(&rule_rn_props);

    rn_rule_props.addOption("SBML Reaction Converter", true);

    rn_rule_converter = new SBMLReactionConverter();
    rn_rule_converter->setProperties(&rn_rule_props);
}

void
Roundtrip_teardown(void)
{
    delete rule_rn_converter;
    delete rn_rule_converter;
}

bool test_rule_to_reaction_to_rule(const std::string& raterule_file, 
									const std::string& reaction_file)

{
	SBMLDocument* d_rule = readSBMLFromFile(raterule_file.c_str());
	SBMLDocument* d_rn = readSBMLFromFile(reaction_file.c_str());
	SBMLDocument* d = readSBMLFromFile(raterule_file.c_str());
	rule_rn_converter->setDocument(d);
	if (rule_rn_converter->convert() != LIBSBML_OPERATION_SUCCESS)
		return false;
	std::string out = writeSBMLToStdString(d);
	std::string expected = writeSBMLToStdString(d_rn);
	bool strings_equal = equals(expected.c_str(), out.c_str());
	if (!strings_equal)
	{
		cout << "rule_reaction_rule: rule->reaction failed" << endl;
		return false;
	}
    

    rn_rule_converter->setDocument(d);
    if (rn_rule_converter->convert() != LIBSBML_OPERATION_SUCCESS)
        return false;
    std::string out1 = writeSBMLToStdString(d);
    std::string expected1 = writeSBMLToStdString(d_rule);
    strings_equal = equals(expected.c_str(), out.c_str());
    if (!strings_equal)
    {
        cout << "rule_reaction_rule: reaction->rule failed" << endl;
        return false;
    }
    delete d;
	delete d_rn;
	delete d_rule;
    return true;
}

bool test_reaction_to_rule_to_reaction(const std::string& raterule_file,
									const std::string& reaction_file)

{
	SBMLDocument* d_rule = readSBMLFromFile(raterule_file.c_str());
	SBMLDocument* d_rn = readSBMLFromFile(reaction_file.c_str());	
	SBMLDocument* d = readSBMLFromFile(reaction_file.c_str());
	rn_rule_converter->setDocument(d);
	if (rn_rule_converter->convert() != LIBSBML_OPERATION_SUCCESS)
		return false;
	std::string out = writeSBMLToStdString(d);
	std::string expected = writeSBMLToStdString(d_rule);
	bool strings_equal = equals(expected.c_str(), out.c_str());
	if (!strings_equal)
	{
		cout << "reaction_rule_reaction: reaction->rule failed" << endl;
		return false;
	}


	rule_rn_converter->setDocument(d);
	if (rule_rn_converter->convert() != LIBSBML_OPERATION_SUCCESS)
		return false;
	std::string out1 = writeSBMLToStdString(d);
	std::string expected1 = writeSBMLToStdString(d_rn);
	strings_equal = equals(expected.c_str(), out.c_str());
	if (!strings_equal)
	{
		cout << "reaction_rule_reaction: reaction_rule->reaction failed" << endl;
		return false;
	}
	delete d_rn;
	delete d_rule;
	delete d;
	return true;
}

START_TEST(test_roundtrip_01)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_01_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_01_bio.xml";

    bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);
    
	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_01_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_01_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_01_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_02)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_02_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_02_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_02_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_02_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_02_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


START_TEST(test_roundtrip_03)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_03_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_03_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_03_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_03_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_03_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_04)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_04_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_04_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_04_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_04_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_04_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST  

START_TEST(test_roundtrip_05)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_05_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_05_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_05_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_05_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_05_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_06)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_06_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_06_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_06_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_06_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_06_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_07)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_07_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_07_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_07_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_07_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_07_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


START_TEST(test_roundtrip_08)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_08_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_08_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_08_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_08_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_08_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


START_TEST(test_roundtrip_09)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_09_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_09_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_09_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_09_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_09_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


START_TEST(test_roundtrip_010)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_010_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_010_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_010_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_010_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_010_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


START_TEST(test_roundtrip_011)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_011_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_011_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_011_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_011_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_011_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


START_TEST(test_roundtrip_012)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_012_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_012_bio.xml";

	bool result = test_rule_to_reaction_to_rule(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_roundtrip_012_reverse)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_012_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_012_bio.xml";

	bool result = test_reaction_to_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST


Suite*
create_suite_TestSBMLRoundtripConverter(void)
{
	bool testing = true;
	Suite* suite = suite_create("SBMLRoundtripConverter");
	TCase* tcase = tcase_create("SBMLRoundtripConverter");
	tcase_add_checked_fixture(tcase, Roundtrip_setup,
		Roundtrip_teardown);

	if (testing)
	{
		tcase_add_test(tcase, test_roundtrip_012_reverse);
	}
	else
	{
		tcase_add_test(tcase, test_roundtrip_01);
		tcase_add_test(tcase, test_roundtrip_01_reverse);
		tcase_add_test(tcase, test_roundtrip_02);
		tcase_add_test(tcase, test_roundtrip_02_reverse);
		tcase_add_test(tcase, test_roundtrip_03); // rule to reaction problem with stoichiometry
		tcase_add_test(tcase, test_roundtrip_03_reverse);
		tcase_add_test(tcase, test_roundtrip_04);
		tcase_add_test(tcase, test_roundtrip_04_reverse);
		tcase_add_test(tcase, test_roundtrip_05);
		tcase_add_test(tcase, test_roundtrip_05_reverse);
		tcase_add_test(tcase, test_roundtrip_06);// rule to reaction local parameters
		tcase_add_test(tcase, test_roundtrip_06_reverse);// he passes but I don't see how
		tcase_add_test(tcase, test_roundtrip_07);// problem with rule to reaction
		tcase_add_test(tcase, test_roundtrip_07_reverse);
		tcase_add_test(tcase, test_roundtrip_08);// rule to reaction doesn't distinguish between products and reactants
		tcase_add_test(tcase, test_roundtrip_08_reverse);
		tcase_add_test(tcase, test_roundtrip_09);// rule to reaction no reaction produced at all
		tcase_add_test(tcase, test_roundtrip_09_reverse);
		tcase_add_test(tcase, test_roundtrip_010);// it fails but doesn't output any data
		tcase_add_test(tcase, test_roundtrip_010_reverse); // crashes
		tcase_add_test(tcase, test_roundtrip_011);// rule to reaction a problem with reactants and products
		tcase_add_test(tcase, test_roundtrip_011_reverse);
		tcase_add_test(tcase, test_roundtrip_012);// rule to reaction creates more reactions
		tcase_add_test(tcase, test_roundtrip_012_reverse);
	}
	suite_add_tcase(suite, tcase);

	return suite;
}
END_C_DECLS
