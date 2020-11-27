/**
 * \file    TestAttributeNamespaces.c
 * \brief   AttributeNamespaces unit tests
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/packages/comp/validator/CompSBMLError.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_comp_AttributeNamespaces_good)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:name=\"Aname\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 0);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_submod_id_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompSubmodelAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <submodel> object must have the attributes 'comp:id' and 'comp:modelRef' because they are required, and may also have the optional attributes 'comp:name', 'comp:timeConversionFactor, and/or 'comp:extentConversionFactor'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <submodel> object.\nReference: L3V1 Comp V1 Section 3.5\n The <comp:submodel> element with the 'id' with value 'A' must use 'comp:id' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_submod_id_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel id=\"A\" comp:id=\"A2\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompSubmodelAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <submodel> object must have the attributes 'comp:id' and 'comp:modelRef' because they are required, and may also have the optional attributes 'comp:name', 'comp:timeConversionFactor, and/or 'comp:extentConversionFactor'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <submodel> object.\nReference: L3V1 Comp V1 Section 3.5\n The <comp:submodel> element with the 'id' with value 'A' and the 'comp:id' with value 'A2' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_submod_id_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel id=\"A\" comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompSubmodelAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <submodel> object must have the attributes 'comp:id' and 'comp:modelRef' because they are required, and may also have the optional attributes 'comp:name', 'comp:timeConversionFactor, and/or 'comp:extentConversionFactor'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <submodel> object.\nReference: L3V1 Comp V1 Section 3.5\n The <comp:submodel> element with the 'id' with value 'A' and the 'comp:id' with value 'A' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_submod_name_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" name=\"Aname\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompSubmodelAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <submodel> object must have the attributes 'comp:id' and 'comp:modelRef' because they are required, and may also have the optional attributes 'comp:name', 'comp:timeConversionFactor, and/or 'comp:extentConversionFactor'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <submodel> object.\nReference: L3V1 Comp V1 Section 3.5\n The <comp:submodel> element with the 'name' with value 'Aname' must use 'comp:name' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_submod_name_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" name=\"Aname\" comp:name=\"A2name\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompSubmodelAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <submodel> object must have the attributes 'comp:id' and 'comp:modelRef' because they are required, and may also have the optional attributes 'comp:name', 'comp:timeConversionFactor, and/or 'comp:extentConversionFactor'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <submodel> object.\nReference: L3V1 Comp V1 Section 3.5\n The <comp:submodel> element with the 'name' with value 'Aname' and the 'comp:name' with value 'A2name' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_submod_name_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" name=\"Aname\" comp:name=\"Aname\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompSubmodelAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <submodel> object must have the attributes 'comp:id' and 'comp:modelRef' because they are required, and may also have the optional attributes 'comp:name', 'comp:timeConversionFactor, and/or 'comp:extentConversionFactor'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <submodel> object.\nReference: L3V1 Comp V1 Section 3.5\n The <comp:submodel> element with the 'name' with value 'Aname' and the 'comp:name' with value 'Aname' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_port_id_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "      <comp:listOfPorts>"
    "        <comp:port comp:idRef=\"a\" id=\"a\"/>"
    "      </comp:listOfPorts>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompPortAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <port> object must have a value for the required attribute 'comp:id', and one, and only one, of the attributes 'comp:idRef', 'comp:unitRef', or 'comp:metaIdRef'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <port> object.\nReference: L3V1 Comp V1 Section 3.4.3\n The <comp:port> element with the 'id' with value 'a' must use 'comp:id' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_port_id_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "      <comp:listOfPorts>"
    "        <comp:port comp:idRef=\"a\" id=\"a\" comp:id=\"a2\"/>"
    "      </comp:listOfPorts>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompPortAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <port> object must have a value for the required attribute 'comp:id', and one, and only one, of the attributes 'comp:idRef', 'comp:unitRef', or 'comp:metaIdRef'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <port> object.\nReference: L3V1 Comp V1 Section 3.4.3\n The <comp:port> element with the 'id' with value 'a' and the 'comp:id' with value 'a2' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_port_id_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "      <comp:listOfPorts>"
    "        <comp:port comp:idRef=\"a\" id=\"a\" comp:id=\"a\"/>"
    "      </comp:listOfPorts>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompPortAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <port> object must have a value for the required attribute 'comp:id', and one, and only one, of the attributes 'comp:idRef', 'comp:unitRef', or 'comp:metaIdRef'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <port> object.\nReference: L3V1 Comp V1 Section 3.4.3\n The <comp:port> element with the 'id' with value 'a' and the 'comp:id' with value 'a' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_port_name_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "      <comp:listOfPorts>"
    "        <comp:port comp:idRef=\"a\" comp:id=\"a\" name=\"a\"/>"
    "      </comp:listOfPorts>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompPortAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <port> object must have a value for the required attribute 'comp:id', and one, and only one, of the attributes 'comp:idRef', 'comp:unitRef', or 'comp:metaIdRef'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <port> object.\nReference: L3V1 Comp V1 Section 3.4.3\n The <comp:port> element with the 'name' with value 'a' must use 'comp:name' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_port_name_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "      <comp:listOfPorts>"
    "        <comp:port comp:idRef=\"a\" comp:id=\"a\" name=\"a2\" comp:name=\"a\"/>"
    "      </comp:listOfPorts>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompPortAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <port> object must have a value for the required attribute 'comp:id', and one, and only one, of the attributes 'comp:idRef', 'comp:unitRef', or 'comp:metaIdRef'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <port> object.\nReference: L3V1 Comp V1 Section 3.4.3\n The <comp:port> element with the 'name' with value 'a2' and the 'comp:name' with value 'a' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_port_name_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "      <comp:listOfPorts>"
    "        <comp:port comp:idRef=\"a\" comp:id=\"a\" name=\"a\" comp:name=\"a\"/>"
    "      </comp:listOfPorts>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompPortAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <port> object must have a value for the required attribute 'comp:id', and one, and only one, of the attributes 'comp:idRef', 'comp:unitRef', or 'comp:metaIdRef'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <port> object.\nReference: L3V1 Comp V1 Section 3.4.3\n The <comp:port> element with the 'name' with value 'a' and the 'comp:name' with value 'a' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_deletion_id_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\">"
    "        <comp:listOfDeletions>"
    "          <comp:deletion id=\"del1\" comp:idRef=\"a\"/>"
    "        </comp:listOfDeletions>"
    "      </comp:submodel>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompDeletionAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <deletion> object must have a value for one, and only one, of the attributes 'comp:portRef', 'comp:idRef', 'comp:unitRef', and 'comp:metaIdRef'.  It may also have the optional attributes 'comp:id' and 'comp:name'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <deletion> object.\nReference: L3V1 Comp V1 Section 3.5.3\n The <comp:deletion> element with the 'id' with value 'del1' must use 'comp:id' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_deletion_id_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\">"
    "        <comp:listOfDeletions>"
    "          <comp:deletion id=\"del1b\" comp:id=\"del1\" comp:idRef=\"a\"/>"
    "        </comp:listOfDeletions>"
    "      </comp:submodel>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompDeletionAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <deletion> object must have a value for one, and only one, of the attributes 'comp:portRef', 'comp:idRef', 'comp:unitRef', and 'comp:metaIdRef'.  It may also have the optional attributes 'comp:id' and 'comp:name'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <deletion> object.\nReference: L3V1 Comp V1 Section 3.5.3\n The <comp:deletion> element with the 'id' with value 'del1b' and the 'comp:id' with value 'del1' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_deletion_id_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\">"
    "        <comp:listOfDeletions>"
    "          <comp:deletion id=\"del1\" comp:id=\"del1\" comp:idRef=\"a\"/>"
    "        </comp:listOfDeletions>"
    "      </comp:submodel>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompDeletionAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <deletion> object must have a value for one, and only one, of the attributes 'comp:portRef', 'comp:idRef', 'comp:unitRef', and 'comp:metaIdRef'.  It may also have the optional attributes 'comp:id' and 'comp:name'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <deletion> object.\nReference: L3V1 Comp V1 Section 3.5.3\n The <comp:deletion> element with the 'id' with value 'del1' and the 'comp:id' with value 'del1' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_deletion_name_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\">"
    "        <comp:listOfDeletions>"
    "          <comp:deletion name=\"del1\" comp:idRef=\"a\"/>"
    "        </comp:listOfDeletions>"
    "      </comp:submodel>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompDeletionAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <deletion> object must have a value for one, and only one, of the attributes 'comp:portRef', 'comp:idRef', 'comp:unitRef', and 'comp:metaIdRef'.  It may also have the optional attributes 'comp:id' and 'comp:name'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <deletion> object.\nReference: L3V1 Comp V1 Section 3.5.3\n The <comp:deletion> element with the 'name' with value 'del1' must use 'comp:name' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_deletion_name_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\">"
    "        <comp:listOfDeletions>"
    "          <comp:deletion name=\"del1b\" comp:name=\"del1\" comp:idRef=\"a\"/>"
    "        </comp:listOfDeletions>"
    "      </comp:submodel>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompDeletionAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <deletion> object must have a value for one, and only one, of the attributes 'comp:portRef', 'comp:idRef', 'comp:unitRef', and 'comp:metaIdRef'.  It may also have the optional attributes 'comp:id' and 'comp:name'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <deletion> object.\nReference: L3V1 Comp V1 Section 3.5.3\n The <comp:deletion> element with the 'name' with value 'del1b' and the 'comp:name' with value 'del1' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_deletion_name_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\">"
    "        <comp:listOfDeletions>"
    "          <comp:deletion name=\"del1\" comp:name=\"del1\" comp:idRef=\"a\"/>"
    "        </comp:listOfDeletions>"
    "      </comp:submodel>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompDeletionAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <deletion> object must have a value for one, and only one, of the attributes 'comp:portRef', 'comp:idRef', 'comp:unitRef', and 'comp:metaIdRef'.  It may also have the optional attributes 'comp:id' and 'comp:name'.  No other attributes from the Hierarchical Model Composition namespace are permitted on a <deletion> object.\nReference: L3V1 Comp V1 Section 3.5.3\n The <comp:deletion> element with the 'name' with value 'del1' and the 'comp:name' with value 'del1' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_extmod_id_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"EM1\" />"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfExternalModelDefinitions>"
    "    <comp:externalModelDefinition id=\"EM1\" comp:source=\"new_aggregate.xml\" />"
    "  </comp:listOfExternalModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompExtModDefAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "An <externalModelDefinition> object must have the attributes 'comp:id' and 'comp:source', and may have the optional attributes 'comp:name', 'comp:modelRef', and 'comp:md5'. No other attributes from the Hierarchical Model Composition namespace are permitted on an <externalModelDefinition> object.\nReference: L3V1 Comp V1 Section 3.3.2\n The <comp:externalModelDefinition> element with the 'id' with value 'EM1' must use 'comp:id' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_extmod_id_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"EM1\" />"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfExternalModelDefinitions>"
    "    <comp:externalModelDefinition id=\"EM1\" comp:id=\"EM1b\" comp:source=\"new_aggregate.xml\" />"
    "  </comp:listOfExternalModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompExtModDefAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "An <externalModelDefinition> object must have the attributes 'comp:id' and 'comp:source', and may have the optional attributes 'comp:name', 'comp:modelRef', and 'comp:md5'. No other attributes from the Hierarchical Model Composition namespace are permitted on an <externalModelDefinition> object.\nReference: L3V1 Comp V1 Section 3.3.2\n The <comp:externalModelDefinition> element with the 'id' with value 'EM1' and the 'comp:id' with value 'EM1b' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_extmod_id_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"EM1\" />"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfExternalModelDefinitions>"
    "    <comp:externalModelDefinition id=\"EM1\" comp:id=\"EM1\" comp:source=\"new_aggregate.xml\" />"
    "  </comp:listOfExternalModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompExtModDefAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "An <externalModelDefinition> object must have the attributes 'comp:id' and 'comp:source', and may have the optional attributes 'comp:name', 'comp:modelRef', and 'comp:md5'. No other attributes from the Hierarchical Model Composition namespace are permitted on an <externalModelDefinition> object.\nReference: L3V1 Comp V1 Section 3.3.2\n The <comp:externalModelDefinition> element with the 'id' with value 'EM1' and the 'comp:id' with value 'EM1' must only use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_extmod_name_coreNotComp)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"EM1\" />"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfExternalModelDefinitions>"
    "    <comp:externalModelDefinition comp:id=\"EM1\" name=\"EM1\" comp:source=\"new_aggregate.xml\" />"
    "  </comp:listOfExternalModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompExtModDefAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "An <externalModelDefinition> object must have the attributes 'comp:id' and 'comp:source', and may have the optional attributes 'comp:name', 'comp:modelRef', and 'comp:md5'. No other attributes from the Hierarchical Model Composition namespace are permitted on an <externalModelDefinition> object.\nReference: L3V1 Comp V1 Section 3.3.2\n The <comp:externalModelDefinition> element with the 'name' with value 'EM1' must use 'comp:name' instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_extmod_name_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"EM1\" />"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfExternalModelDefinitions>"
    "    <comp:externalModelDefinition name=\"EM1b\" comp:name=\"EM1\" comp:id=\"EM1\" comp:source=\"new_aggregate.xml\" />"
    "  </comp:listOfExternalModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompExtModDefAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "An <externalModelDefinition> object must have the attributes 'comp:id' and 'comp:source', and may have the optional attributes 'comp:name', 'comp:modelRef', and 'comp:md5'. No other attributes from the Hierarchical Model Composition namespace are permitted on an <externalModelDefinition> object.\nReference: L3V1 Comp V1 Section 3.3.2\n The <comp:externalModelDefinition> element with the 'name' with value 'EM1b' and the 'comp:name' with value 'EM1' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_extmod_name_coreAndComp_warning)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"EM1\" />"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfExternalModelDefinitions>"
    "    <comp:externalModelDefinition name=\"EM1\" comp:name=\"EM1\" comp:id=\"EM1\" comp:source=\"new_aggregate.xml\" />"
    "  </comp:listOfExternalModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompExtModDefAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_WARNING);
  fail_unless( !strcmp(error->getMessage().c_str(), "An <externalModelDefinition> object must have the attributes 'comp:id' and 'comp:source', and may have the optional attributes 'comp:name', 'comp:modelRef', and 'comp:md5'. No other attributes from the Hierarchical Model Composition namespace are permitted on an <externalModelDefinition> object.\nReference: L3V1 Comp V1 Section 3.3.2\n The <comp:externalModelDefinition> element with the 'name' with value 'EM1' and the 'comp:name' with value 'EM1' must only use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedElement_id_compNotCore)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:listOfReplacedElements>"
    "          <comp:replacedElement comp:id=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "        </comp:listOfReplacedElements>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedElementAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedElement> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef', 'comp:metaIdRef', or 'comp:deletion'. It may also have a value for the optional attribute 'comp:conversionFactor'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedElement> object.\nReference: L3V1 Comp V1 Section 3.6.2\n The <comp:replacedElement> element with the 'comp:id' with value 're1' must not use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedElement_id_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:listOfReplacedElements>"
    "          <comp:replacedElement comp:id=\"re1b\" id=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "        </comp:listOfReplacedElements>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedElementAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedElement> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef', 'comp:metaIdRef', or 'comp:deletion'. It may also have a value for the optional attribute 'comp:conversionFactor'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedElement> object.\nReference: L3V1 Comp V1 Section 3.6.2\n The <comp:replacedElement> element with the 'comp:id' with value 're1b' and the 'id' with value 're1' must not use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedElement_id_coreAndComp_error2)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:listOfReplacedElements>"
    "          <comp:replacedElement comp:id=\"re1\" id=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "        </comp:listOfReplacedElements>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedElementAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedElement> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef', 'comp:metaIdRef', or 'comp:deletion'. It may also have a value for the optional attribute 'comp:conversionFactor'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedElement> object.\nReference: L3V1 Comp V1 Section 3.6.2\n The <comp:replacedElement> element with the 'comp:id' with value 're1' and the 'id' with value 're1' must not use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedBy_id_compNotCore)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:replacedBy comp:id=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedByAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedBy> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef' or 'comp:metaIdRef'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedBy> object.\nReference: L3V1 Comp V1 Section 3.6.4\n The <comp:replacedBy> element with the 'comp:id' with value 're1' must not use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedBy_id_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:replacedBy comp:id=\"re1b\" id=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedByAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedBy> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef' or 'comp:metaIdRef'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedBy> object.\nReference: L3V1 Comp V1 Section 3.6.4\n The <comp:replacedBy> element with the 'comp:id' with value 're1b' and the 'id' with value 're1' must not use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedBy_id_coreAndComp_error2)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:replacedBy comp:id=\"re1\" id=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedByAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedBy> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef' or 'comp:metaIdRef'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedBy> object.\nReference: L3V1 Comp V1 Section 3.6.4\n The <comp:replacedBy> element with the 'comp:id' with value 're1' and the 'id' with value 're1' must not use the 'comp:id' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedBy_name_compNotCore)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:replacedBy comp:name=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedByAllowedAttributes);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedBy> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef' or 'comp:metaIdRef'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedBy> object.\nReference: L3V1 Comp V1 Section 3.6.4\n The <comp:replacedBy> element with the 'comp:name' with value 're1' must not use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedBy_name_coreAndComp_error)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:replacedBy comp:name=\"re1\" name=\"re1b\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedByAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedBy> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef' or 'comp:metaIdRef'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedBy> object.\nReference: L3V1 Comp V1 Section 3.6.4\n The <comp:replacedBy> element with the 'comp:name' with value 're1' and the 'name' with value 're1b' must not use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_replacedBy_name_coreAndComp_error2)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <listOfParameters>"
    "      <parameter id=\"a\" value=\"3\" constant=\"true\">"
    "        <comp:replacedBy comp:name=\"re1\" name=\"re1\" comp:idRef=\"a\" comp:submodelRef=\"S1\"/>"
    "      </parameter>"
    "    </listOfParameters>"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"S1\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == CompReplacedByAllowedAttributes);
  fail_unless(error->getSeverity() == LIBSBML_SEV_ERROR);
  fail_unless( !strcmp(error->getMessage().c_str(), "A <replacedBy> object must have a value for the required attribute 'comp:submodelRef', and a value for one, and only one, of the following attributes: 'comp:portRef', 'comp:idRef', 'comp:unitRef' or 'comp:metaIdRef'. No other attributes from the HierarchicalModel Composition namespace are permitted on a <replacedBy> object.\nReference: L3V1 Comp V1 Section 3.6.4\n The <comp:replacedBy> element with the 'comp:name' with value 're1' and the 'name' with value 're1' must not use the 'comp:name' attribute.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_moddef_id_compNotCore)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model id=\"bar\" name=\"bar\">"
    "    <comp:listOfSubmodels>"
    "      <comp:submodel comp:id=\"A\" comp:modelRef=\"foo\"/>"
    "    </comp:listOfSubmodels>"
    "  </model>"
    "  <comp:listOfModelDefinitions>"
    "    <comp:modelDefinition comp:id=\"foo\" name=\"foo\">"
    "      <listOfParameters>"
    "        <parameter id=\"a\" value=\"3\" constant=\"true\"/>"
    "      </listOfParameters>"
    "    </comp:modelDefinition>"
    "  </comp:listOfModelDefinitions>"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  fail_unless(errors->getNumErrors() == 1);
  const SBMLError* error = errors->getError(0);
  fail_unless(error->getErrorId() == AllowedAttributesOnModel);
  fail_unless( !strcmp(error->getMessage().c_str(), "A Model object may only have the following attributes, all of which are optional: 'metaid', 'sboTerm', 'id', 'name', 'substanceUnits', 'timeUnits', 'volumeUnits', 'areaUnits', 'lengthUnits', 'extentUnits' and 'conversionFactor'. No other attributes from the SBML Level 3 Core namespace are permitted on a Model object.\nReference: L3V2 Section 4.2\n The <comp:modelDefinition> element with the 'comp:id' with value 'foo' may not use a 'comp:id': the id attribute from core must be used instead.\n"), NULL);
  delete doc;
}
END_TEST


START_TEST (test_comp_AttributeNamespaces_model_extraCompAtt)
{
  string input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<sbml xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" level=\"3\" version=\"2\" comp:required=\"true\">"
    "  <model comp:substanceUnits=\"foo\" />"
    "</sbml>";
  SBMLDocument* doc = readSBMLFromString(input.c_str());
  SBMLErrorLog* errors = doc->getErrorLog();
  //fail_unless(errors->getNumErrors() == 1);

}
END_TEST


Suite *
create_suite_TestComp_AttributeNamespaces (void)
{
  Suite *suite = suite_create("SBMLCompIdNamspace");
  TCase *tcase = tcase_create("SBMLCompIdNamspace");


  tcase_add_test(tcase, test_comp_AttributeNamespaces_good);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_submod_id_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_submod_id_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_submod_id_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_submod_name_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_submod_name_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_submod_name_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_port_id_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_port_id_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_port_id_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_port_name_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_port_name_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_port_name_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_deletion_id_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_deletion_id_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_deletion_id_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_deletion_name_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_deletion_name_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_deletion_name_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_extmod_id_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_extmod_id_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_extmod_id_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_extmod_name_coreNotComp);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_extmod_name_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_extmod_name_coreAndComp_warning);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedElement_id_compNotCore);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedElement_id_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedElement_id_coreAndComp_error2);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedBy_id_compNotCore);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedBy_id_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedBy_id_coreAndComp_error2);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedBy_name_compNotCore);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedBy_name_coreAndComp_error);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_replacedBy_name_coreAndComp_error2);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_moddef_id_compNotCore);
  tcase_add_test(tcase, test_comp_AttributeNamespaces_model_extraCompAtt);

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS

