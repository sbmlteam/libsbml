/**
 * Filename    : TestValidator.c
 * Description : Validator unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-03-25
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <check.h>

#include "sbml/common.h"
#include "sbml/Validator.h"
#include "sbml/SBMLReader.h"


#define XML_HEADER    "<?xml version='1.0' encoding='UTF-8'?>\n"
#define SBML_HEADER   "<sbml level='1' version='1'> <model name='testModel'>\n"
#define SBML_HEADER2  "<sbml level='2' version='1'> <model name='testModel'>\n"
#define SBML_FOOTER   "</model> </sbml>"

/**
 * Wraps the string s in the appropriate XML or SBML boilerplate.
 */
#define wrapXML(s)    XML_HEADER s
#define wrapSBML(s)   XML_HEADER SBML_HEADER  s SBML_FOOTER
#define wrapSBML2(s)  XML_HEADER SBML_HEADER2 s SBML_FOOTER


static SBMLDocument_t *D;


START_TEST (test_Validator_create)
{
  Validator_t *v = Validator_create();


  fail_unless( Validator_getNumRules(v) == 0, NULL );

  Validator_free(v);
}
END_TEST


START_TEST (test_Validator_free_NULL)
{
  Validator_free(NULL);
}
END_TEST


START_TEST (test_Validator_compartment_size_dimensions)
{
  const char *s = wrapSBML
  (
    "<listOfCompartments>"
    "  <compartment name='c' spatialDimensions='0' size='1'>"
    "</listOfCompartments>"
  );


  D = readSBMLFromString(s);

  SBMLDocument_validate(D);

  fail_unless(SBMLDocument_getNumErrors(D) == 1, NULL);

  SBMLDocument_free(D);
}
END_TEST


START_TEST (test_Validator_KineticLaw_substanceUnits)
{
  const char *s = wrapSBML
  (
    "<reaction name='J1'>"
    "  <kineticLaw formula='k1*X0' substanceUnits='foo'/>"
    "</reaction>"
  );

  
  D = readSBMLFromString(s);

  SBMLDocument_validate(D);

  fail_unless(SBMLDocument_getNumErrors(D) == 1, NULL);

  SBMLDocument_free(D);
}
END_TEST


START_TEST (test_Validator_multipleValidationRulesOnSameEntityType)
{
  const char *s = wrapSBML
  (
    "<listOfUnitDefinitions>"
    "   <unitDefinition id=\"litre\">"  /* error: litre is illegal */
    "       <listOfUnits>"
    "           <unit kind=\"volume\" exponent=\"-1\"/>"
    "       </listOfUnits>"
    "   </unitDefinition>"
    "  <unitDefinition id=\"substance\">"  /* error: 'substance' non-unique */
    "      <listOfUnits>"
    "          <unit kind=\"mole\" scale=\"-3\"/>"
    "      </listOfUnits>"
    "  </unitDefinition>"
    "  <unitDefinition id=\"substance\">"
    "      <listOfUnits>"
    "          <unit kind=\"mole\" scale=\"-3\"/>"
    "      </listOfUnits>"
    "  </unitDefinition>"
    "</listOfUnitDefinitions>"
  );

  D = readSBMLFromString(s);

  SBMLDocument_validate(D);

  fail_unless(SBMLDocument_getNumErrors(D) == 2, NULL);

  SBMLDocument_free(D);
}
END_TEST


Suite *
create_suite_Validator (void) 
{ 
  Suite *suite = suite_create("Validator");
  TCase *tcase = tcase_create("Validator");
 

  tcase_add_test(tcase, test_Validator_create                      );
  tcase_add_test(tcase, test_Validator_free_NULL                   );
  tcase_add_test(tcase, test_Validator_compartment_size_dimensions );
  tcase_add_test(tcase, test_Validator_KineticLaw_substanceUnits   );
  tcase_add_test(tcase, test_Validator_multipleValidationRulesOnSameEntityType);

  suite_add_tcase(suite, tcase);

  return suite;
}
