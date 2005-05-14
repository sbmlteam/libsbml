/**
 * Filename    : TestTextGlyph.cpp
 * Description : Unit tests for TextGlyph
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

#include "TextGlyph.h"

BEGIN_C_DECLS

static TextGlyph * TG;

void
TextGlyphTest_setup (void)
{
    TG = new(std::nothrow )TextGlyph();

    if (TG == NULL)
    {
        fail("new(std::nothrow) TextGlyph() returned a NULL pointer.");
    }

}

void 
TextGlyphTest_teardown (void)
{
    delete TG;
}


START_TEST ( test_TextGlyph_new )
{
}
END_TEST

START_TEST ( test_TextGlyph_new_with_text )
{
}
END_TEST

START_TEST ( test_TextGlyph_setText )
{
}
END_TEST

START_TEST ( test_TextGlyph_setGraphicalObjectId )
{
}
END_TEST

START_TEST ( test_TextGlyph_setOriginOfTextId )
{
}
END_TEST


Suite *
create_suite_TextGlyph (void)
{
  Suite *suite = suite_create("TextGlyph");
  TCase *tcase = tcase_create("TextGlyph");

  tcase_add_checked_fixture( tcase,
                             TextGlyphTest_setup,
                             TextGlyphTest_teardown );


  tcase_add_test(tcase , test_TextGlyph_new                  );
  tcase_add_test(tcase , test_TextGlyph_new_with_text        );
  tcase_add_test(tcase , test_TextGlyph_setText              );
  tcase_add_test(tcase , test_TextGlyph_setGraphicalObjectId );
  tcase_add_test(tcase , test_TextGlyph_setOriginOfTextId    );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
