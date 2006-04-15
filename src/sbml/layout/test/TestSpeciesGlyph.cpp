/**
 * Filename    : TestSpeciesGlyph.cpp
 * Description : Unit tests for SpeciesGlyph
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

#include <string>
#include <check.h>

#include <common/common.h>
#include <common/extern.h>

#include "SpeciesGlyph.h"

BEGIN_C_DECLS


static SpeciesGlyph * SG;

void
SpeciesGlyphTest_setup (void)
{
    SG = new(std::nothrow )SpeciesGlyph();

    if (SG == NULL)
    {
        fail("new(std::nothrow) SpeciesGlyph() returned a NULL pointer.");
    }

}

void 
SpeciesGlyphTest_teardown (void)
{
    delete SG;
}

START_TEST ( test_SpeciesGlyph_new )
{
    fail_unless( SG->getTypeCode()    == SBML_LAYOUT_SPECIESGLYPH );
    fail_unless( SG->getMetaId()      == "" );
//    fail_unless( SG->getNotes()       == "" );
//    fail_unless( SG->getAnnotation()  == "" );
    fail_unless( SG->getId()          == "" );
    fail_unless( !SG->isSetId());
    fail_unless( !SG->isSetSpeciesId());
}
END_TEST

START_TEST ( test_SpeciesGlyph_new_with_id_and_speciesid)
{
    
    std::string id="TestSpeciesGlyph";
    std::string speciesId="TestSpecies";
    SpeciesGlyph* sg=new SpeciesGlyph(id,speciesId);
    fail_unless(sg->isSetSpeciesId());
    fail_unless(sg->getSpeciesId()==speciesId);
    delete sg;
}
END_TEST

START_TEST ( test_SpeciesGlyph_setSpeciesId )
{
    std::string speciesId="TestSpeciesGlyph";
    SG->setSpeciesId(speciesId);
    fail_unless(SG->isSetSpeciesId());
    fail_unless(SG->getSpeciesId()==speciesId);
    speciesId="";
    SG->setSpeciesId(speciesId);
    fail_unless(!SG->isSetSpeciesId());
}
END_TEST



Suite *
create_suite_SpeciesGlyph (void)
{
  Suite *suite = suite_create("SpeciesGlyph");
  TCase *tcase = tcase_create("SpeciesGlyph");

  tcase_add_checked_fixture( tcase,
                             SpeciesGlyphTest_setup,
                             SpeciesGlyphTest_teardown );

  tcase_add_test( tcase, test_SpeciesGlyph_new                       );
  tcase_add_test( tcase, test_SpeciesGlyph_new_with_id_and_speciesid );
  tcase_add_test( tcase, test_SpeciesGlyph_setSpeciesId              );
    
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
