/**
 * Filename    : TestLayout.cpp
 * Description : Unit tests for Layout
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

#include <check.h>

#include <common/common.h>
#include <common/extern.h>

#include "Layout.h"

BEGIN_C_DECLS

static Layout * L;

void
LayoutTest_setup (void)
{
    L = new(std::nothrow )Layout();

    if (L == NULL)
    {
        fail("new(std::nothrow) Layout() returned a NULL pointer.");
    }

}

void 
LayoutTest_teardown (void)
{
    delete L;
}

START_TEST ( test_Layout_new )
{
}
END_TEST

START_TEST ( test_Layout_new_with_id_and_dimensions )
{
}
END_TEST

START_TEST ( test_Layout_setId )
{
}
END_TEST

START_TEST ( test_Layout_setDimensions )
{
}
END_TEST

START_TEST ( test_Layout_addCompartmentGlyph )
{
}
END_TEST

START_TEST ( test_Layout_addSpeciesGlyph )
{
}
END_TEST

START_TEST ( test_Layout_addReactionGlyph )
{
}
END_TEST

START_TEST ( test_Layout_addTextGlyph )
{
}
END_TEST

START_TEST ( test_Layout_addAdditionalGraphicalObject )
{
}
END_TEST

START_TEST ( test_Layout_getObjectWithId )
{
}
END_TEST

START_TEST ( test_Layout_getNumCompartmentGlyphs )
{
}
END_TEST

START_TEST ( test_Layout_getNumSpeciesGlyphs )
{
}
END_TEST


START_TEST ( test_Layout_getNumReactionGlyphs )
{
}
END_TEST


START_TEST ( test_Layout_getNumTextGlyphs )
{
}
END_TEST


START_TEST ( test_Layout_getNumAdditionalGraphicalObjects )
{
}
END_TEST

START_TEST ( test_Layout_createCompartmentGlyph )
{
}
END_TEST

START_TEST ( test_Layout_createSpeciesGlyph )
{
}
END_TEST


START_TEST ( test_Layout_createReactionGlyph )
{
}
END_TEST


START_TEST ( test_Layout_createTextGlyph )
{
}
END_TEST


START_TEST ( test_Layout_createAdditionalGraphicalObject )
{
}
END_TEST


START_TEST ( test_Layout_createSpeciesReferenceGlyph )
{
}
END_TEST


START_TEST ( test_Layout_createLineSegment )
{
}
END_TEST


START_TEST ( test_Layout_createCubicBezier )
{
}
END_TEST



Suite *
create_suite_Layout (void)
{
  Suite *suite = suite_create("Layout");
  TCase *tcase = tcase_create("Layout");

  tcase_add_checked_fixture( tcase,
                             LayoutTest_setup,
                             LayoutTest_teardown );

  tcase_add_test ( tcase , test_Layout_new                              );
  tcase_add_test ( tcase , test_Layout_new_with_id_and_dimensions       );
  tcase_add_test ( tcase , test_Layout_setId                            );
  tcase_add_test ( tcase , test_Layout_setDimensions                    );
  tcase_add_test ( tcase , test_Layout_addCompartmentGlyph              );
  tcase_add_test ( tcase , test_Layout_addSpeciesGlyph                  );
  tcase_add_test ( tcase , test_Layout_addReactionGlyph                 );
  tcase_add_test ( tcase , test_Layout_addTextGlyph                     );
  tcase_add_test ( tcase , test_Layout_addAdditionalGraphicalObject     );
  tcase_add_test ( tcase , test_Layout_getObjectWithId                  );
  tcase_add_test ( tcase , test_Layout_getNumCompartmentGlyphs          );
  tcase_add_test ( tcase , test_Layout_getNumSpeciesGlyphs              );
  tcase_add_test ( tcase , test_Layout_getNumReactionGlyphs             );
  tcase_add_test ( tcase , test_Layout_getNumTextGlyphs                 );
  tcase_add_test ( tcase , test_Layout_getNumAdditionalGraphicalObjects );
  tcase_add_test ( tcase , test_Layout_createCompartmentGlyph           );
  tcase_add_test ( tcase , test_Layout_createSpeciesGlyph               );
  tcase_add_test ( tcase , test_Layout_createReactionGlyph              );
  tcase_add_test ( tcase , test_Layout_createTextGlyph                  );
  tcase_add_test ( tcase , test_Layout_createAdditionalGraphicalObject  );
  tcase_add_test ( tcase , test_Layout_createSpeciesReferenceGlyph      );
  tcase_add_test ( tcase , test_Layout_createLineSegment                );
  tcase_add_test ( tcase , test_Layout_createCubicBezier                );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
