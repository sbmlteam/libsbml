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
    if (!strcmp(expected, actual))
    {
        //printf("\nStrings equal:\n");
        return true;
    }
  //printf("\nStrings are not equal:\n");
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

bool test_reaction_to_rule(const std::string& raterule_file,
						   const std::string& reaction_file,
                           bool parametersOnly = false)
{

	SBMLDocument* d_rule = readSBMLFromFile(raterule_file.c_str());
	SBMLDocument* d_rn = readSBMLFromFile(reaction_file.c_str());	
	SBMLDocument* d = readSBMLFromFile(reaction_file.c_str());

    if (parametersOnly)
    {
        rn_rule_props.addOption("rateRuleVariablesShouldBeParameters", true);
        rn_rule_converter->setProperties(&rn_rule_props);
    }
    else if (rn_rule_props.getOption("rateRuleVariablesShouldBeParameters"))
    {
        // it was set to true for a previous test - set it back to false
        rn_rule_props.addOption("rateRuleVariablesShouldBeParameters", false);
        rn_rule_converter->setProperties(&rn_rule_props);
    }

	rn_rule_converter->setDocument(d);
    if (rn_rule_converter->convert() != LIBSBML_OPERATION_SUCCESS)
    {
        //cout << "reaction_rule: reaction->rule conversion failed" << endl;
        delete d_rn;
        delete d_rule;
        delete d;
        return false;
    }
	std::string out = writeSBMLToStdString(d);
	std::string expected = writeSBMLToStdString(d_rule);
	bool strings_equal = equals(expected.c_str(), out.c_str());
	if (!strings_equal)
	{
        std::string TEST_file(TestDataDirectory);
        TEST_file += "test_reaction_to_rule_out.xml";
        cout << "reaction_rule: reaction->rule failed" << endl;
        writeSBMLToFile(d, TEST_file.c_str());
        delete d_rn;
        delete d_rule;
        delete d;
		return false;
	}

	delete d_rn;
	delete d_rule;
	delete d;
	return true;
}

START_TEST(test_reaction_rule_01)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_01_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_01_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_02)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_02_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_02_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_03)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_03_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_03_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_04)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_04_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_04_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_05)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_05_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_05_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_06)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_06_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_06_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_07)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_07_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_07_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_08)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_08_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_08_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_09)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_09_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_09_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_010)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "invalid_010_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "invalid_010_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == false);
}
END_TEST

START_TEST(test_reaction_rule_011)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_011_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_011_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_012)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_012_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_012_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_013)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_013_rr_original.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_013_bio_original.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_014)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_014_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_014_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file, true);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_015)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_015_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_015_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_016)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_016_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_016_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_017)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_017_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_017_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_051)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_51_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_51_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_052)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_52_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_52_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_053)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_53_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_53_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_054)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_54_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_54_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST

START_TEST(test_reaction_rule_055)
{
    std::string raterule_file(TestDataDirectory);
    raterule_file += "valid_55_rr.xml";
    std::string reaction_file(TestDataDirectory);
    reaction_file += "valid_55_bio.xml";

    bool result = test_reaction_to_rule(raterule_file, reaction_file);

    fail_unless(result == true);
}
END_TEST



Suite*
create_suite_TestSBMLReactionConverter(void)
{
    bool testing = false;
	Suite* suite = suite_create("SBMLReactionConverter");
	TCase* tcase = tcase_create("SBMLReactionConverter");
	tcase_add_checked_fixture(tcase, Reaction_setup,
		Reaction_teardown);

    if (testing)
    {
        tcase_add_test(tcase, test_reaction_rule_03);
    }
    else
    {
     tcase_add_test(tcase, test_reaction_rule_01);
     tcase_add_test(tcase, test_reaction_rule_02);
     tcase_add_test(tcase, test_reaction_rule_03);
     tcase_add_test(tcase, test_reaction_rule_04);
     tcase_add_test(tcase, test_reaction_rule_05);
     tcase_add_test(tcase, test_reaction_rule_06);
     tcase_add_test(tcase, test_reaction_rule_07);
     tcase_add_test(tcase, test_reaction_rule_08);
     tcase_add_test(tcase, test_reaction_rule_09);
     tcase_add_test(tcase, test_reaction_rule_010);
     tcase_add_test(tcase, test_reaction_rule_011);
     tcase_add_test(tcase, test_reaction_rule_012);
     tcase_add_test(tcase, test_reaction_rule_013); 
     tcase_add_test(tcase, test_reaction_rule_014);
    //tcase_add_test(tcase, test_reaction_rule_015);
    //tcase_add_test(tcase, test_reaction_rule_016);
    //tcase_add_test(tcase, test_reaction_rule_017);
     tcase_add_test(tcase, test_reaction_rule_051);           
     tcase_add_test(tcase, test_reaction_rule_052); // crashes
    //tcase_add_test(tcase, test_reaction_rule_053); // crashes   
    //tcase_add_test(tcase, test_reaction_rule_054); // crashes
    //tcase_add_test(tcase, test_reaction_rule_055); // crashes    
     }  

	suite_add_tcase(suite, tcase);

	return suite;
}
END_C_DECLS
