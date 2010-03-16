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


#include <common/common.h>
#include <common/extern.h>

#include "Layout.h"
#include "GraphicalObject.h"
#include "CompartmentGlyph.h"
#include "SpeciesGlyph.h"
#include "ReactionGlyph.h"
#include "TextGlyph.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

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
//    fail_unless( L->getNotes()       == "" );
//    fail_unless( L->getAnnotation()  == "" );
    fail_unless( L->getId()          == "" );
    fail_unless( !L->isSetId());
    const Dimensions& dim=*(L->getDimensions());
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

START_TEST ( test_Layout_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   Layout *l=new Layout(level, version); 
   fail_unless( l->getTypeCode() == SBML_LAYOUT_LAYOUT );
   fail_unless( l->getMetaId()   == "" );

   fail_unless(l->getLevel() == level);
   fail_unless(l->getVersion() == version);

   fail_unless( l->isSetId() == false );
   const Dimensions& dim=*(L->getDimensions());
   fail_unless (dim.getWidth()  == 0.0 );
   fail_unless (dim.getHeight() == 0.0 );
   fail_unless (dim.getDepth()  == 0.0 );

   fail_unless ( l->getNumCompartmentGlyphs()         == 0 );
   fail_unless ( l->getNumSpeciesGlyphs()             == 0 );
   fail_unless ( l->getNumReactionGlyphs()            == 0 );
   fail_unless ( l->getNumTextGlyphs()                == 0 );
   fail_unless ( l->getNumAdditionalGraphicalObjects() == 0 );
   
   delete l;

   level=2;
   version=3;
   l=new Layout(level, version); 
   fail_unless( l->getTypeCode() == SBML_LAYOUT_LAYOUT );
   fail_unless( l->getMetaId()   == "" );

   fail_unless(l->getLevel() == level);
   fail_unless(l->getVersion() == version);

   fail_unless( l->isSetId() == false );
   const Dimensions& dim2=*(L->getDimensions());
   fail_unless (dim2.getWidth()  == 0.0 );
   fail_unless (dim2.getHeight() == 0.0 );
   fail_unless (dim2.getDepth()  == 0.0 );

   fail_unless ( l->getNumCompartmentGlyphs()         == 0 );
   fail_unless ( l->getNumSpeciesGlyphs()             == 0 );
   fail_unless ( l->getNumReactionGlyphs()            == 0 );
   fail_unless ( l->getNumTextGlyphs()                == 0 );
   fail_unless ( l->getNumAdditionalGraphicalObjects() == 0 );
   
   delete l;
}
END_TEST

START_TEST ( test_Layout_new_WithNamespace )
{
   SBMLNamespaces* ns = new SBMLNamespaces;
   Layout *l=new Layout(ns); 
   fail_unless( l->getTypeCode() == SBML_LAYOUT_LAYOUT );
   fail_unless( l->getMetaId()   == "" );

   fail_unless(l->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(l->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( l->isSetId() == false );
   const Dimensions& dim=*(L->getDimensions());
   fail_unless (dim.getWidth()  == 0.0 );
   fail_unless (dim.getHeight() == 0.0 );
   fail_unless (dim.getDepth()  == 0.0 );

   fail_unless ( l->getNumCompartmentGlyphs()         == 0 );
   fail_unless ( l->getNumSpeciesGlyphs()             == 0 );
   fail_unless ( l->getNumReactionGlyphs()            == 0 );
   fail_unless ( l->getNumTextGlyphs()                == 0 );
   fail_unless ( l->getNumAdditionalGraphicalObjects() == 0 );
   
   delete l;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   l=new Layout(ns); 
   fail_unless( l->getTypeCode() == SBML_LAYOUT_LAYOUT );
   fail_unless( l->getMetaId()   == "" );

   fail_unless(l->getLevel() == 2);
   fail_unless(l->getVersion() == 3);

   fail_unless( l->isSetId() == false );
   const Dimensions& dim2=*(L->getDimensions());
   fail_unless (dim2.getWidth()  == 0.0 );
   fail_unless (dim2.getHeight() == 0.0 );
   fail_unless (dim2.getDepth()  == 0.0 );

   fail_unless ( l->getNumCompartmentGlyphs()         == 0 );
   fail_unless ( l->getNumSpeciesGlyphs()             == 0 );
   fail_unless ( l->getNumReactionGlyphs()            == 0 );
   fail_unless ( l->getNumTextGlyphs()                == 0 );
   fail_unless ( l->getNumAdditionalGraphicalObjects() == 0 );
   
   delete l;
   delete ns;
}
END_TEST


START_TEST ( test_Layout_new_with_id_and_dimensions )
{
    std::string id="TestLayoutId";
    Dimensions dimensions=Dimensions(-1.1,2.2,3.3);
    Layout* l=new Layout(id,&dimensions);
    fail_unless( l->getTypeCode()    == SBML_LAYOUT_LAYOUT );
    fail_unless( l->getMetaId()      == "" );
//    fail_unless( l->getNotes()       == "" );
//    fail_unless( l->getAnnotation()  == "" );
    fail_unless( l->getId()          == id );
    fail_unless( l->isSetId());
    const Dimensions& dim=*(l->getDimensions());
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
    L->setDimensions(&dimensions);
    const Dimensions& dim=*(L->getDimensions());
    fail_unless(dim.getWidth()  == dimensions.getWidth());
    fail_unless(dim.getHeight() == dimensions.getHeight());
    fail_unless(dim.getDepth()  == dimensions.getDepth());
}
END_TEST

START_TEST ( test_Layout_addCompartmentGlyph )
{
  // we can only add valid objects to a layout
  // which means that the CompartmentGlyph needs to have an id
  CompartmentGlyph* cg=new CompartmentGlyph();
  cg->setId("CompartmentGlyph_1");
  L->addCompartmentGlyph(cg);
  fail_unless ( L->getNumCompartmentGlyphs() == 1 );
  delete cg;
}
END_TEST

START_TEST ( test_Layout_addSpeciesGlyph )
{
  // we can only add valid objects to a layout
  // which means that the SpeciesGlyph needs to have an id
  SpeciesGlyph* sg=new SpeciesGlyph();
  sg->setId("SpeciesGlyph_1");
  L->addSpeciesGlyph(sg);
  fail_unless ( L->getNumSpeciesGlyphs() == 1 );
  delete sg;
}
END_TEST

START_TEST ( test_Layout_addReactionGlyph )
{
  // we can only add valid objects to a layout
  // which means that the ReactionGlyph needs to have an id
  ReactionGlyph* rg=new ReactionGlyph();
  rg->setId("ReactionGlyph_1");
  L->addReactionGlyph(rg);
  fail_unless ( L->getNumReactionGlyphs() == 1 );

    delete rg;
}
END_TEST

START_TEST ( test_Layout_addTextGlyph )
{
  // we can only add valid objects to a layout
  // which means that the TextGlyph needs to have an id
  TextGlyph* tg=new TextGlyph();
  tg->setId("TextGlyph_1");
  L->addTextGlyph(tg);
  fail_unless ( L->getNumTextGlyphs() == 1 );
  delete tg;
}
END_TEST

START_TEST ( test_Layout_addAdditionalGraphicalObject )
{
  // we can only add valid objects to a layout
  // which means that the GraphicalObject needs to have an id
  GraphicalObject* ago=new GraphicalObject();
  ago->setId("GraphicalObject_1");
  L->addAdditionalGraphicalObject(ago);
  fail_unless ( L->getNumAdditionalGraphicalObjects() == 1 );
  delete ago;
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
    L->addCompartmentGlyph(cg1);
    L->addCompartmentGlyph(cg2);
    L->addCompartmentGlyph(cg3);
    L->addCompartmentGlyph(cg4);
    L->addCompartmentGlyph(cg5);
    fail_unless( L->getNumCompartmentGlyphs() == 5);
    delete cg1;
    delete cg2;
    delete cg3;
    delete cg4;
    delete cg5;
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
    L->addSpeciesGlyph(sg1);
    L->addSpeciesGlyph(sg2);
    L->addSpeciesGlyph(sg3);
    L->addSpeciesGlyph(sg4);
    L->addSpeciesGlyph(sg5);
    fail_unless( L->getNumSpeciesGlyphs() == 5);
    delete sg1;
    delete sg2;
    delete sg3;
    delete sg4;
    delete sg5;
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
    L->addReactionGlyph(rg1);
    L->addReactionGlyph(rg2);
    L->addReactionGlyph(rg3);
    L->addReactionGlyph(rg4);
    L->addReactionGlyph(rg5);
    fail_unless( L->getNumReactionGlyphs() == 5);
    delete rg1;
    delete rg2;
    delete rg3;
    delete rg4;
    delete rg5;
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
    L->addTextGlyph(tg1);
    L->addTextGlyph(tg2);
    L->addTextGlyph(tg3);
    L->addTextGlyph(tg4);
    L->addTextGlyph(tg5);
    fail_unless( L->getNumTextGlyphs() == 5);
    delete tg1;
    delete tg2;
    delete tg3;
    delete tg4;
    delete tg5;
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
    L->addAdditionalGraphicalObject(go1);
    L->addAdditionalGraphicalObject(go2);
    L->addAdditionalGraphicalObject(go3);
    L->addAdditionalGraphicalObject(go4);
    L->addAdditionalGraphicalObject(go5);
    fail_unless( L->getNumAdditionalGraphicalObjects() == 5);
    delete go1;
    delete go2;
    delete go3;
    delete go4;
    delete go5;
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



START_TEST ( test_Layout_copyConstructor )
{
    Layout* l1=new Layout();
    XMLNode* notes=new XMLNode();
    l1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    l1->setAnnotation(annotation);
    GraphicalObject* go=l1->createCompartmentGlyph();
    go->setId("go1");
    go=l1->createCompartmentGlyph();
    go->setId("go2");
    go=l1->createCompartmentGlyph();
    go->setId("go3");
    go=l1->createSpeciesGlyph();
    go->setId("go4");
    go=l1->createSpeciesGlyph();
    go->setId("go5");
    go=l1->createSpeciesGlyph();
    go->setId("go6");
    go=l1->createSpeciesGlyph();
    go->setId("go7");
    go=l1->createSpeciesGlyph();
    go->setId("go8");
    go=l1->createSpeciesGlyph();
    go->setId("go9");
    go=l1->createSpeciesGlyph();
    go->setId("go10");
    go=l1->createReactionGlyph();
    go->setId("go11");
    go=l1->createReactionGlyph();
    go->setId("go12");
    go=l1->createReactionGlyph();
    go->setId("go13");
    go=l1->createReactionGlyph();
    go->setId("go14");
    go=l1->createReactionGlyph();
    go->setId("go15");
    go=l1->createReactionGlyph();
    go->setId("go16");
    go=l1->createReactionGlyph();
    go->setId("go17");
    go=l1->createReactionGlyph();
    go->setId("go18");
    go=l1->createTextGlyph();
    go->setId("go19");
    go=l1->createTextGlyph();
    go->setId("go20");
    go=l1->createTextGlyph();
    go->setId("go21");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go22");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go23");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go24");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go25");
    Layout* l2=new Layout(*l1);
    delete l2;
    delete l1;
}
END_TEST

START_TEST ( test_Layout_assignmentOperator )
{
    Layout* l1=new Layout();
    XMLNode* notes=new XMLNode();
    l1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    l1->setAnnotation(annotation);
    GraphicalObject* go=l1->createCompartmentGlyph();
    go->setId("go1");
    go=l1->createCompartmentGlyph();
    go->setId("go2");
    go=l1->createCompartmentGlyph();
    go->setId("go3");
    go=l1->createSpeciesGlyph();
    go->setId("go4");
    go=l1->createSpeciesGlyph();
    go->setId("go5");
    go=l1->createSpeciesGlyph();
    go->setId("go6");
    go=l1->createSpeciesGlyph();
    go->setId("go7");
    go=l1->createSpeciesGlyph();
    go->setId("go8");
    go=l1->createSpeciesGlyph();
    go->setId("go9");
    go=l1->createSpeciesGlyph();
    go->setId("go10");
    go=l1->createReactionGlyph();
    go->setId("go11");
    go=l1->createReactionGlyph();
    go->setId("go12");
    go=l1->createReactionGlyph();
    go->setId("go13");
    go=l1->createReactionGlyph();
    go->setId("go14");
    go=l1->createReactionGlyph();
    go->setId("go15");
    go=l1->createReactionGlyph();
    go->setId("go16");
    go=l1->createReactionGlyph();
    go->setId("go17");
    go=l1->createReactionGlyph();
    go->setId("go18");
    go=l1->createTextGlyph();
    go->setId("go19");
    go=l1->createTextGlyph();
    go->setId("go20");
    go=l1->createTextGlyph();
    go->setId("go21");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go22");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go23");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go24");
    go=l1->createAdditionalGraphicalObject();
    go->setId("go25");
    Layout* l2=new Layout();
    (*l2)=(*l1);
    delete l2;
    delete l1;
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

  tcase_add_test ( tcase , test_Layout_new                               );
  tcase_add_test ( tcase , test_Layout_new_WithLevelVersionAndNamespaces );
  tcase_add_test ( tcase , test_Layout_new_WithNamespace                 );
  tcase_add_test ( tcase , test_Layout_new_with_id_and_dimensions        );
  tcase_add_test ( tcase , test_Layout_setId                             );
  tcase_add_test ( tcase , test_Layout_setDimensions                     );
  tcase_add_test ( tcase , test_Layout_addCompartmentGlyph               );
  tcase_add_test ( tcase , test_Layout_addSpeciesGlyph                   );
  tcase_add_test ( tcase , test_Layout_addReactionGlyph                  );
  tcase_add_test ( tcase , test_Layout_addTextGlyph                      );
  tcase_add_test ( tcase , test_Layout_addAdditionalGraphicalObject      );
  tcase_add_test ( tcase , test_Layout_createCompartmentGlyph            );
  tcase_add_test ( tcase , test_Layout_createSpeciesGlyph                );
  tcase_add_test ( tcase , test_Layout_createReactionGlyph               );
  tcase_add_test ( tcase , test_Layout_createTextGlyph                   );
  tcase_add_test ( tcase , test_Layout_createAdditionalGraphicalObject   );
  tcase_add_test ( tcase , test_Layout_createSpeciesReferenceGlyph       );
  tcase_add_test ( tcase , test_Layout_createLineSegment                 );
  tcase_add_test ( tcase , test_Layout_createCubicBezier                 );
  tcase_add_test ( tcase , test_Layout_getNumCompartmentGlyphs           );
  tcase_add_test ( tcase , test_Layout_getNumSpeciesGlyphs               );
  tcase_add_test ( tcase , test_Layout_getNumReactionGlyphs              );
  tcase_add_test ( tcase , test_Layout_getNumTextGlyphs                  );
  tcase_add_test ( tcase , test_Layout_getNumAdditionalGraphicalObjects  );
  tcase_add_test(  tcase , test_Layout_copyConstructor                   );
  tcase_add_test(  tcase , test_Layout_assignmentOperator                );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
