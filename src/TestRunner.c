/**
 * Filename    : TestRunner.c
 * Description : Runs all unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-14
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


/**
 * Test suite creation function prototypes.
 *
 * These functions are needed only for calls in main() below.  Therefore a
 * separate header file is not necessary and only adds a maintenance burden
 * to keep the two files synchronized.
 */
BEGIN_C_DECLS

Suite *create_suite_AlgebraicRule            (void);
Suite *create_suite_AssignmentRule           (void);
Suite *create_suite_ASTNode                  (void);
Suite *create_suite_Compartment              (void);
Suite *create_suite_CompartmentVolumeRule    (void);
Suite *create_suite_Event                    (void);
Suite *create_suite_EventAssignment          (void);
Suite *create_suite_FormulaFormatter         (void);
Suite *create_suite_FormulaParser            (void);
Suite *create_suite_FormulaTokenizer         (void);
Suite *create_suite_FunctionDefinition       (void);
Suite *create_suite_KineticLaw               (void);
Suite *create_suite_List                     (void);
Suite *create_suite_ListOf                   (void);
Suite *create_suite_MathMLDocument           (void);
Suite *create_suite_MathMLFormatter          (void);
Suite *create_suite_MathMLHandler            (void);
Suite *create_suite_MathMLWriter             (void);
Suite *create_suite_Model                    (void);
Suite *create_suite_ModifierSpeciesReference (void);
Suite *create_suite_Parameter                (void);
Suite *create_suite_ParameterRule            (void);
Suite *create_suite_ParseMessage             (void);
Suite *create_suite_RateRule                 (void);
Suite *create_suite_Reaction                 (void);
Suite *create_suite_Rule                     (void);
Suite *create_suite_RuleType                 (void);
Suite *create_suite_SAX2AttributesMock       (void);
Suite *create_suite_SBase                    (void);
Suite *create_suite_SBMLConvert              (void);
Suite *create_suite_SBMLDocument             (void);
Suite *create_suite_SBMLFormatter            (void);
Suite *create_suite_SBMLHandler              (void);
Suite *create_suite_SBMLReader               (void);
Suite *create_suite_SBMLWriter               (void);
Suite *create_suite_SimpleSpeciesReference   (void);
Suite *create_suite_Species                  (void);
Suite *create_suite_SpeciesConcentrationRule (void);
Suite *create_suite_SpeciesReference         (void);
Suite *create_suite_Stack                    (void);
Suite *create_suite_StringBuffer             (void);
Suite *create_suite_StringMap                (void);
Suite *create_suite_Unit                     (void);
Suite *create_suite_UnitDefinition           (void);
Suite *create_suite_UnitKind                 (void);
Suite *create_suite_Validator                (void);
Suite *create_suite_XMLStringFormatter       (void);
Suite *create_suite_XMLUtil                  (void);

Suite *create_suite_memory (void);
Suite *create_suite_util   (void);

Suite *create_suite_TestReadFromFile1 (void);
Suite *create_suite_TestReadFromFile2 (void);
Suite *create_suite_TestReadFromFile3 (void);
Suite *create_suite_TestReadFromFile4 (void);

END_C_DECLS


/**
 * Global.
 *
 * Declared extern in TestReadFromFileN suites.
 */
char *TestDataDirectory;


/**
 * Sets TestDataDirectory for the the TestReadFromFileN suites.
 *
 * For Automake's distcheck target to work properly, TestDataDirectory must
 * begin with the value of the environment variable SRCDIR.
 */
void
setTestDataDirectory (void)
{
  char *srcdir = getenv("srcdir");
  int  length  = (srcdir == NULL) ? 0 : strlen(srcdir);


  /**
   * strlen("/test-data/") = 11 + 1 (for NULL) = 12
   */
  TestDataDirectory = (char *) safe_calloc( length + 12, sizeof(char) );

  if (srcdir != NULL)
  {
    strcpy(TestDataDirectory, srcdir);
  }

  strcat(TestDataDirectory, "/test-data/");
}


int
main (void) 
{ 
  int num_failed;


  SRunner *runner = srunner_create( create_suite_memory() );

  srunner_add_suite( runner, create_suite_util                     () );
  srunner_add_suite( runner, create_suite_AlgebraicRule            () );
  srunner_add_suite( runner, create_suite_AssignmentRule           () );
  srunner_add_suite( runner, create_suite_ASTNode                  () );
  srunner_add_suite( runner, create_suite_Compartment              () );
  srunner_add_suite( runner, create_suite_CompartmentVolumeRule    () );
  srunner_add_suite( runner, create_suite_Event                    () );
  srunner_add_suite( runner, create_suite_EventAssignment          () );
  srunner_add_suite( runner, create_suite_FormulaFormatter         () );
  srunner_add_suite( runner, create_suite_FormulaParser            () );
  srunner_add_suite( runner, create_suite_FormulaTokenizer         () );
  srunner_add_suite( runner, create_suite_FunctionDefinition       () );
  srunner_add_suite( runner, create_suite_KineticLaw               () );
  srunner_add_suite( runner, create_suite_List                     () );
  srunner_add_suite( runner, create_suite_ListOf                   () );
  srunner_add_suite( runner, create_suite_MathMLDocument           () );
  srunner_add_suite( runner, create_suite_MathMLFormatter          () );
  srunner_add_suite( runner, create_suite_MathMLHandler            () );
  srunner_add_suite( runner, create_suite_MathMLWriter             () );
  srunner_add_suite( runner, create_suite_Model                    () );
  srunner_add_suite( runner, create_suite_ModifierSpeciesReference () );
  srunner_add_suite( runner, create_suite_Parameter                () );
  srunner_add_suite( runner, create_suite_ParameterRule            () );
  srunner_add_suite( runner, create_suite_ParseMessage             () );
  srunner_add_suite( runner, create_suite_RateRule                 () );
  srunner_add_suite( runner, create_suite_Reaction                 () );
  srunner_add_suite( runner, create_suite_Rule                     () );
  srunner_add_suite( runner, create_suite_RuleType                 () );
  srunner_add_suite( runner, create_suite_SAX2AttributesMock       () );
  srunner_add_suite( runner, create_suite_SBase                    () );
  srunner_add_suite( runner, create_suite_SBMLConvert              () );
  srunner_add_suite( runner, create_suite_SBMLDocument             () );
  srunner_add_suite( runner, create_suite_SBMLFormatter            () );
  srunner_add_suite( runner, create_suite_SBMLHandler              () );
  srunner_add_suite( runner, create_suite_SBMLReader               () );
  srunner_add_suite( runner, create_suite_SBMLWriter               () );
  srunner_add_suite( runner, create_suite_SimpleSpeciesReference   () );
  srunner_add_suite( runner, create_suite_Species                  () );
  srunner_add_suite( runner, create_suite_SpeciesConcentrationRule () );
  srunner_add_suite( runner, create_suite_SpeciesReference         () );
  srunner_add_suite( runner, create_suite_Stack                    () );
  srunner_add_suite( runner, create_suite_StringBuffer             () );
  srunner_add_suite( runner, create_suite_StringMap                () );
  srunner_add_suite( runner, create_suite_Unit                     () );
  srunner_add_suite( runner, create_suite_UnitDefinition           () );
  srunner_add_suite( runner, create_suite_UnitKind                 () );
  srunner_add_suite( runner, create_suite_Validator                () );
  srunner_add_suite( runner, create_suite_XMLStringFormatter       () );
  srunner_add_suite( runner, create_suite_XMLUtil                  () );

  srunner_add_suite( runner, create_suite_TestReadFromFile1() );
  srunner_add_suite( runner, create_suite_TestReadFromFile2() );
  srunner_add_suite( runner, create_suite_TestReadFromFile3() );
  srunner_add_suite( runner, create_suite_TestReadFromFile4() );

  setTestDataDirectory();

#ifdef TRACE_MEMORY
  srunner_set_fork_status(runner, CK_NOFORK);
#endif

  srunner_run_all(runner, CK_NORMAL);
  num_failed = srunner_ntests_failed(runner);

#ifdef TRACE_MEMORY

  if (MemTrace_getNumLeaks() > 0)
  {
    MemTrace_printLeaks(stdout);
  }

  MemTrace_printStatistics(stdout);

#endif

  srunner_free(runner);
  safe_free(TestDataDirectory);

  return num_failed;
}
