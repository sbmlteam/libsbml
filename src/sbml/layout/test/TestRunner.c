/**
 * Filename    : TestRunner.c
 * Description : TestRunner that runs all the test suites.
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2005-05-03
 *
 * Copyright 2005 European Media Laboratories Research gGmbH
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
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */


#include <string.h>
#include <stdlib.h>

#include "common/extern.h"
#include "util/memory.h"

#include <check.h>

/**
 * Test suite creation function prototypes.
 *
 * These functions are needed only for calls in main() below.  Therefore a
 * separate header file is not necessary and only adds a maintenance burden
 * to keep the two files synchronized.
 */
BEGIN_C_DECLS

Suite *create_suite_Point                 (void);
Suite *create_suite_Dimensions            (void);
Suite *create_suite_BoundingBox           (void);
Suite *create_suite_LineSegment           (void);
Suite *create_suite_CubicBezier           (void);
Suite *create_suite_Curve                 (void);
Suite *create_suite_GraphicalObject       (void);
Suite *create_suite_CompartmentGlyph      (void);
Suite *create_suite_SpeciesGlyph          (void);
Suite *create_suite_ReactionGlyph         (void);
Suite *create_suite_SpeciesReferenceGlyph (void);
Suite *create_suite_TextGlyph             (void);
Suite *create_suite_Layout                (void);
Suite *create_suite_LayoutCreation        (void);
Suite *create_suite_LayoutFormatter       (void);
Suite *create_suite_SBMLHandler           (void);
Suite *create_suite_LayoutWriting         (void);
Suite *create_suite_Misc                  (void);

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

  setTestDataDirectory();

  SRunner *runner = srunner_create( create_suite_Point() );

  srunner_add_suite( runner, create_suite_Dimensions            () );
  srunner_add_suite( runner, create_suite_BoundingBox           () );
  srunner_add_suite( runner, create_suite_LineSegment           () );
  srunner_add_suite( runner, create_suite_CubicBezier           () );
  srunner_add_suite( runner, create_suite_Curve                 () );
  srunner_add_suite( runner, create_suite_GraphicalObject       () );
  srunner_add_suite( runner, create_suite_CompartmentGlyph      () );
  srunner_add_suite( runner, create_suite_SpeciesGlyph          () );
  srunner_add_suite( runner, create_suite_ReactionGlyph         () );
  srunner_add_suite( runner, create_suite_SpeciesReferenceGlyph () );
  srunner_add_suite( runner, create_suite_TextGlyph             () );
  srunner_add_suite( runner, create_suite_Layout                () );
  srunner_add_suite( runner, create_suite_LayoutCreation        () );
  srunner_add_suite( runner, create_suite_LayoutFormatter       () );
  srunner_add_suite( runner, create_suite_SBMLHandler           () );
  srunner_add_suite( runner, create_suite_LayoutWriting         () );
 /* srunner_add_suite( runner, create_suite_Misc                  () ); */


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

  return num_failed;
}
