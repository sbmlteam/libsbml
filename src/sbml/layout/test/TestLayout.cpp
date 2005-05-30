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
#include "GraphicalObject.h"
#include "CompartmentGlyph.h"
#include "SpeciesGlyph.h"
#include "ReactionGlyph.h"
#include "TextGlyph.h"

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
    fail_unless( L->getTypeCode()    == SBML_LAYOUT_LAYOUT );
    fail_unless( L->getMetaId()      == "" );
    fail_unless( L->getNotes()       == "" );
    fail_unless( L->getAnnotation()  == "" );
    fail_unless( L->getId()          == "" );
    fail_unless( !L->isSetId());
    Dimensions dim=L->getDimensions();
    fail_unless (dim.getWidth()  == 0.0 );
    fail_unless (dim.getHeight() == 0.0 );
    fail_unless (dim.getDepth()  == 0.0 );

    fail_unless ( L->getNumCompartmentGlyphs()         == 0 );
    fail_unless ( L->getNumSpeciesGlyphs()             == 0 );
    fail_unless ( L->getNumReactionGlyphs()            == 0 );
    fail_unless ( L->getNumTextGlyphs()                == 0 );
    fail_unless ( L->getNumAdditionalGraphicalObjects() == 0 );
}
END_TEST

START_TEST ( test_Layout_new_with_id_and_dimensions )
{
    std::string id="TestLayoutId";
    Dimensions dimensions=Dimensions(-1.1,2.2,3.3);
    Layout* l=new Layout(id,dimensions);
    fail_unless( l->getTypeCode()    == SBML_LAYOUT_LAYOUT );
    fail_unless( l->getMetaId()      == "" );
    fail_unless( l->getNotes()       == "" );
    fail_unless( l->getAnnotation()  == "" );
    fail_unless( l->getId()          == id );
    fail_unless( l->isSetId());
    Dimensions dim=l->getDimensions();
    fail_unless (dim.getWidth()  == dimensions.getWidth() );
    fail_unless (dim.getHeight() == dimensions.getHeight() );
    fail_unless (dim.getDepth()  == dimensions.getDepth() );

    fail_unless ( l->getNumCompartmentGlyphs()         == 0 );
    fail_unless ( l->getNumSpeciesGlyphs()             == 0 );
    fail_unless ( l->getNumReactionGlyphs()            == 0 );
    fail_unless ( l->getNumTextGlyphs()                == 0 );
    fail_unless ( l->getNumAdditionalGraphicalObjects() == 0 );
    delete l;
}
END_TEST

START_TEST ( test_Layout_setId )
{
    std::string id="TestLayoutId";
    L->setId(id);
    fail_unless(L->isSetId());
    fail_unless(L->getId() == id);
}
END_TEST

START_TEST ( test_Layout_setDimensions )
{
    Dimensions dimensions=Dimensions(-1.1,2.2,-3.3);
    L->setDimensions(dimensions);
    Dimensions dim=L->getDimensions();
    fail_unless(dim.getWidth()  == dimensions.getWidth());
    fail_unless(dim.getHeight() == dimensions.getHeight());
    fail_unless(dim.getDepth()  == dimensions.getDepth());
}
END_TEST

START_TEST ( test_Layout_addCompartmentGlyph )
{
    CompartmentGlyph* cg=new CompartmentGlyph();
    L->addCompartmentGlyph(*cg);
    fail_unless ( L->getNumCompartmentGlyphs() == 1 );
    fail_unless ( L->getCompartmentGlyph(0)    == cg);
}
END_TEST

START_TEST ( test_Layout_addSpeciesGlyph )
{
    SpeciesGlyph* sg=new SpeciesGlyph();
    L->addSpeciesGlyph(*sg);
    fail_unless ( L->getNumSpeciesGlyphs() == 1 );
    fail_unless ( L->getSpeciesGlyph(0)    == sg);
}
END_TEST

START_TEST ( test_Layout_addReactionGlyph )
{
    ReactionGlyph* rg=new ReactionGlyph();
    L->addReactionGlyph(*rg);
    fail_unless ( L->getNumReactionGlyphs() == 1 );
    fail_unless ( L->getReactionGlyph(0)    == rg);
}
END_TEST

START_TEST ( test_Layout_addTextGlyph )
{
    TextGlyph* tg=new TextGlyph();
    L->addTextGlyph(*tg);
    fail_unless ( L->getNumTextGlyphs() == 1 );
    fail_unless ( L->getTextGlyph(0)    == tg);
}
END_TEST

START_TEST ( test_Layout_addAdditionalGraphicalObject )
{
    GraphicalObject* ago=new GraphicalObject();
    L->addAdditionalGraphicalObject(*ago);
    fail_unless ( L->getNumAdditionalGraphicalObjects() == 1 );
    fail_unless ( L->getAdditionalGraphicalObject(0)    == ago);
}
END_TEST

START_TEST ( test_Layout_getNumCompartmentGlyphs )
{
    std::string id1="TestCompartment_1";
    std::string id2="TestCompartment_2";
    std::string id3="TestCompartment_3";
    std::string id4="TestCompartment_4";
    std::string id5="TestCompartment_5";
    CompartmentGlyph* cg1=new CompartmentGlyph(id1); 
    CompartmentGlyph* cg2=new CompartmentGlyph(id2); 
    CompartmentGlyph* cg3=new CompartmentGlyph(id3); 
    CompartmentGlyph* cg4=new CompartmentGlyph(id4); 
    CompartmentGlyph* cg5=new CompartmentGlyph(id5); 
    L->addCompartmentGlyph(*cg1);
    L->addCompartmentGlyph(*cg2);
    L->addCompartmentGlyph(*cg3);
    L->addCompartmentGlyph(*cg4);
    L->addCompartmentGlyph(*cg5);
    fail_unless( L->getNumCompartmentGlyphs() == 5);
}
END_TEST

START_TEST ( test_Layout_getNumSpeciesGlyphs )
{
    std::string id1="TestSpecies_1";
    std::string id2="TestSpecies_2";
    std::string id3="TestSpecies_3";
    std::string id4="TestSpecies_4";
    std::string id5="TestSpecies_5";
    SpeciesGlyph* sg1=new SpeciesGlyph(id1); 
    SpeciesGlyph* sg2=new SpeciesGlyph(id2); 
    SpeciesGlyph* sg3=new SpeciesGlyph(id3); 
    SpeciesGlyph* sg4=new SpeciesGlyph(id4); 
    SpeciesGlyph* sg5=new SpeciesGlyph(id5); 
    L->addSpeciesGlyph(*sg1);
    L->addSpeciesGlyph(*sg2);
    L->addSpeciesGlyph(*sg3);
    L->addSpeciesGlyph(*sg4);
    L->addSpeciesGlyph(*sg5);
    fail_unless( L->getNumSpeciesGlyphs() == 5);
}
END_TEST


START_TEST ( test_Layout_getNumReactionGlyphs )
{
    std::string id1="TestReaction_1";
    std::string id2="TestReaction_2";
    std::string id3="TestReaction_3";
    std::string id4="TestReaction_4";
    std::string id5="TestReaction_5";
    ReactionGlyph* rg1=new ReactionGlyph(id1); 
    ReactionGlyph* rg2=new ReactionGlyph(id2); 
    ReactionGlyph* rg3=new ReactionGlyph(id3); 
    ReactionGlyph* rg4=new ReactionGlyph(id4); 
    ReactionGlyph* rg5=new ReactionGlyph(id5); 
    L->addReactionGlyph(*rg1);
    L->addReactionGlyph(*rg2);
    L->addReactionGlyph(*rg3);
    L->addReactionGlyph(*rg4);
    L->addReactionGlyph(*rg5);
    fail_unless( L->getNumReactionGlyphs() == 5);
}
END_TEST


START_TEST ( test_Layout_getNumTextGlyphs )
{
    std::string id1="TestText_1";
    std::string id2="TestText_2";
    std::string id3="TestText_3";
    std::string id4="TestText_4";
    std::string id5="TestText_5";
    TextGlyph* tg1=new TextGlyph(id1); 
    TextGlyph* tg2=new TextGlyph(id2); 
    TextGlyph* tg3=new TextGlyph(id3); 
    TextGlyph* tg4=new TextGlyph(id4); 
    TextGlyph* tg5=new TextGlyph(id5); 
    L->addTextGlyph(*tg1);
    L->addTextGlyph(*tg2);
    L->addTextGlyph(*tg3);
    L->addTextGlyph(*tg4);
    L->addTextGlyph(*tg5);
    fail_unless( L->getNumTextGlyphs() == 5);
}
END_TEST


START_TEST ( test_Layout_getNumAdditionalGraphicalObjects )
{
    std::string id1="TestGraphicalObject_1";
    std::string id2="TestGraphicalObject_2";
    std::string id3="TestGraphicalObject_3";
    std::string id4="TestGraphicalObject_4";
    std::string id5="TestGraphicalObject_5";
    GraphicalObject* go1=new GraphicalObject(id1); 
    GraphicalObject* go2=new GraphicalObject(id2); 
    GraphicalObject* go3=new GraphicalObject(id3); 
    GraphicalObject* go4=new GraphicalObject(id4); 
    GraphicalObject* go5=new GraphicalObject(id5); 
    L->addAdditionalGraphicalObject(*go1);
    L->addAdditionalGraphicalObject(*go2);
    L->addAdditionalGraphicalObject(*go3);
    L->addAdditionalGraphicalObject(*go4);
    L->addAdditionalGraphicalObject(*go5);
    fail_unless( L->getNumAdditionalGraphicalObjects() == 5);
}
END_TEST

START_TEST ( test_Layout_createCompartmentGlyph )
{
    L->createCompartmentGlyph();
    L->createCompartmentGlyph();
    L->createCompartmentGlyph();
    fail_unless ( L->getNumCompartmentGlyphs() == 3 );  
}
END_TEST

START_TEST ( test_Layout_createSpeciesGlyph )
{
    L->createSpeciesGlyph();
    L->createSpeciesGlyph();
    L->createSpeciesGlyph();
    fail_unless ( L->getNumSpeciesGlyphs() == 3 );  
}
END_TEST


START_TEST ( test_Layout_createReactionGlyph )
{
    L->createReactionGlyph();
    L->createReactionGlyph();
    L->createReactionGlyph();
    fail_unless ( L->getNumReactionGlyphs() == 3 );  
}
END_TEST


START_TEST ( test_Layout_createTextGlyph )
{
    L->createTextGlyph();
    L->createTextGlyph();
    L->createTextGlyph();
    fail_unless ( L->getNumTextGlyphs() == 3 );  
}
END_TEST


START_TEST ( test_Layout_createAdditionalGraphicalObject )
{
    L->createAdditionalGraphicalObject();
    L->createAdditionalGraphicalObject();
    L->createAdditionalGraphicalObject();
    fail_unless ( L->getNumAdditionalGraphicalObjects() == 3 );  
}
END_TEST


START_TEST ( test_Layout_createSpeciesReferenceGlyph )
{
    SpeciesReferenceGlyph* srg=L->createSpeciesReferenceGlyph();
    fail_unless(srg == NULL);
    L->createReactionGlyph();
    srg=L->createSpeciesReferenceGlyph();
    fail_unless(srg != NULL);
}
END_TEST


START_TEST ( test_Layout_createLineSegment )
{
    LineSegment* ls=L->createLineSegment();
    fail_unless(ls == NULL);
    L->createReactionGlyph();
    ls=L->createLineSegment();
    fail_unless(ls != NULL);
    L->createSpeciesReferenceGlyph();
    ls=L->createLineSegment();
    fail_unless ( ls != NULL );
    ReactionGlyph* rg=L->getReactionGlyph(0);
    fail_unless( rg->getCurve()->getNumCurveSegments() == 1);
    fail_unless( rg->getSpeciesReferenceGlyph(0)->getCurve()->getNumCurveSegments() == 1);
}
END_TEST


START_TEST ( test_Layout_createCubicBezier )
{
    CubicBezier* cb=L->createCubicBezier();
    fail_unless(cb == NULL);
    L->createReactionGlyph();
    cb=L->createCubicBezier();
    fail_unless(cb != NULL);
    L->createSpeciesReferenceGlyph();
    cb=L->createCubicBezier();
    fail_unless ( cb != NULL );
    ReactionGlyph* rg=L->getReactionGlyph(0);
    fail_unless( rg->getCurve()->getNumCurveSegments() == 1);
    fail_unless( rg->getSpeciesReferenceGlyph(0)->getCurve()->getNumCurveSegments() == 1);
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
  tcase_add_test ( tcase , test_Layout_createCompartmentGlyph           );
  tcase_add_test ( tcase , test_Layout_createSpeciesGlyph               );
  tcase_add_test ( tcase , test_Layout_createReactionGlyph              );
  tcase_add_test ( tcase , test_Layout_createTextGlyph                  );
  tcase_add_test ( tcase , test_Layout_createAdditionalGraphicalObject  );
  tcase_add_test ( tcase , test_Layout_createSpeciesReferenceGlyph      );
  tcase_add_test ( tcase , test_Layout_createLineSegment                );
  tcase_add_test ( tcase , test_Layout_createCubicBezier                );
  tcase_add_test ( tcase , test_Layout_getNumCompartmentGlyphs          );
  tcase_add_test ( tcase , test_Layout_getNumSpeciesGlyphs              );
  tcase_add_test ( tcase , test_Layout_getNumReactionGlyphs             );
  tcase_add_test ( tcase , test_Layout_getNumTextGlyphs                 );
  tcase_add_test ( tcase , test_Layout_getNumAdditionalGraphicalObjects );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
