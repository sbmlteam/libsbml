//
// Filename    : TestRunner.c
// Description : This file contains the code that runs all tests 
// Organization: University of Heidelberg
// Created     : 2008-07-02
//
// Copyright 2008 University of Heidelberg
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 2.1 of the License, or
// any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
// MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
// documentation provided hereunder is on an "as is" basis, and the
// University of Heidelberg have no obligations to
// provide maintenance, support, updates, enhancements or modifications.
// In no event shall the University of Heidelberg be
// liable to any party for direct, indirect, special, incidental or
// consequential damages, including lost profits, arising out of the use of
// this software and its documentation, even if the University of 
// Heidelberg have been advised of the possibility of such
// damage.  See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// The original code contained here was initially developed by:
//
//     Ralph Gauges
//     BIOQUANT/BQ0018
//     Im Neuenheimer Feld 267
//     69120 Heidelberg
//     Germany
//
//     mailto:ralph.gauges@bioquant.uni-heidelberg.de
//
// Contributor(s):



#include <string.h>
#include <stdlib.h>

#include <sbml/common/extern.h>
#include <sbml/util/memory.h>

#include <check.h>

#ifdef LIBSBML_USE_VLD
  #include <vld.h>
#endif



/*
 * Test suite creation function prototypes.
 *
 * These functions are needed only for calls in main() below.  Therefore a
 * separate header file is not necessary and only adds a maintenance burden
 * to keep the two files synchronized.
 */
LIBSBML_CPP_NAMESPACE_USE
BEGIN_C_DECLS


/*
 * Global.
 *
 * Declared extern in TestReadFromFileN suites.
 */
char *TestDataDirectory;



Suite *create_suite_RenderExtension         (void);
Suite *create_suite_RelAbsVector            (void);
Suite *create_suite_ColorDefinition         (void);
Suite *create_suite_GradientStop            (void);
Suite *create_suite_RenderPoint             (void);
Suite *create_suite_RenderCubicBezier       (void);
Suite *create_suite_GradientBase            (void);
Suite *create_suite_LinearGradient          (void);
Suite *create_suite_RadialGradient          (void);
Suite *create_suite_Transformation          (void);
Suite *create_suite_Transformation2D        (void);
Suite *create_suite_Image                   (void);
Suite *create_suite_GraphicalPrimitive1D    (void);
Suite *create_suite_RenderCurve             (void);
Suite *create_suite_Text                    (void);
Suite *create_suite_GraphicalPrimitive2D    (void);
Suite *create_suite_Rectangle               (void);
Suite *create_suite_Ellipse                 (void);
Suite *create_suite_Polygon                 (void);
Suite *create_suite_Group                   (void);
Suite *create_suite_LineEnding              (void);
Suite *create_suite_Style                   (void);
Suite *create_suite_GlobalStyle             (void);
Suite *create_suite_LocalStyle              (void);
Suite *create_suite_RenderInformationBase   (void);
Suite *create_suite_GlobalRenderInformation (void);
Suite *create_suite_LocalRenderInformation  (void);
Suite *create_suite_RenderReading           (void);
Suite *create_suite_RenderWriting           (void);


/*
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


  /*
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
  //SRunner *runner = srunner_create(create_suite_RenderCurve());


  SRunner *runner = srunner_create( create_suite_RenderExtension  () );
  srunner_add_suite( runner, create_suite_RelAbsVector            () );
  srunner_add_suite( runner, create_suite_ColorDefinition         () );
  srunner_add_suite( runner, create_suite_GradientStop            () );
  srunner_add_suite( runner, create_suite_RenderPoint             () );
  srunner_add_suite( runner, create_suite_RenderCubicBezier       () );
  srunner_add_suite( runner, create_suite_GradientBase            () );
  srunner_add_suite( runner, create_suite_LinearGradient          () );
  srunner_add_suite( runner, create_suite_RadialGradient          () );
  srunner_add_suite( runner, create_suite_Transformation          () );
  srunner_add_suite( runner, create_suite_Transformation2D        () );
  srunner_add_suite( runner, create_suite_Image                   () );
  srunner_add_suite( runner, create_suite_GraphicalPrimitive1D    () );
  srunner_add_suite( runner, create_suite_RenderCurve             () );
  srunner_add_suite( runner, create_suite_Text                    () );
  srunner_add_suite( runner, create_suite_GraphicalPrimitive2D    () );
  srunner_add_suite( runner, create_suite_Rectangle               () );
  srunner_add_suite( runner, create_suite_Ellipse                 () );
  srunner_add_suite( runner, create_suite_Polygon                 () );
  srunner_add_suite( runner, create_suite_Group                   () );
  srunner_add_suite( runner, create_suite_LineEnding              () );
  srunner_add_suite( runner, create_suite_Style                   () );
  srunner_add_suite( runner, create_suite_GlobalStyle             () );
  srunner_add_suite( runner, create_suite_LocalStyle              () );
  srunner_add_suite( runner, create_suite_RenderInformationBase   () );
  srunner_add_suite( runner, create_suite_GlobalRenderInformation () );
  srunner_add_suite( runner, create_suite_LocalRenderInformation  () );
  srunner_add_suite( runner, create_suite_RenderReading           () );
  srunner_add_suite( runner, create_suite_RenderWriting           () );

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

  free(TestDataDirectory);

  return num_failed;
}


END_C_DECLS

