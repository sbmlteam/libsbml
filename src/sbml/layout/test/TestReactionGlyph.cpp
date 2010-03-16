/**
 * Filename    : TestReactionGlyph.cpp
 * Description : Unit tests for ReactionGlyph
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

#include <common/common.h>
#include <common/extern.h>

#include "ReactionGlyph.h"
#include "SpeciesReferenceGlyph.h"
#include "CubicBezier.h"
#include "Curve.h"
#include "Point.h"
#include "LineSegment.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static ReactionGlyph * RG;

void
ReactionGlyphTest_setup (void)
{
    RG = new(std::nothrow )ReactionGlyph();

    if (RG == NULL)
    {
        fail("new(std::nothrow) ReactionGlyph() returned a NULL pointer.");
    }

}

void 
ReactionGlyphTest_teardown (void)
{
    delete RG;
}

START_TEST ( test_ReactionGlyph_new )
{
    fail_unless( RG->getTypeCode()    == SBML_LAYOUT_REACTIONGLYPH );
    fail_unless( RG->getMetaId()      == "" );
//    fail_unless( RG->getNotes()       == "" );
//    fail_unless( RG->getAnnotation()  == "" );
    fail_unless( RG->getId()          == "" );
    fail_unless( !RG->isSetId());
    fail_unless( !RG->isSetReactionId());
    fail_unless( RG->getCurve() != NULL );
    fail_unless( !RG->isSetCurve() );
}
END_TEST

START_TEST ( test_ReactionGlyph_new_WithLevelVersionAndNamespaces )
{
    unsigned int level=1;
    unsigned int version=2;
    ReactionGlyph *rg=new ReactionGlyph(level, version); 
    fail_unless( rg->getTypeCode() == SBML_LAYOUT_REACTIONGLYPH );
    fail_unless( rg->getMetaId()   == "" );

    fail_unless(rg->getLevel() == level);
    fail_unless(rg->getVersion() == version);

    fail_unless( rg->isSetId() == false );
    fail_unless( !rg->isSetReactionId());
    fail_unless( rg->getCurve() != NULL );
    fail_unless( !rg->isSetCurve() );

    delete rg;

    level=2;
    version=3;
    rg=new ReactionGlyph(level, version); 
    fail_unless( rg->getTypeCode() == SBML_LAYOUT_REACTIONGLYPH );
    fail_unless( rg->getMetaId()   == "" );

    fail_unless(rg->getLevel() == level);
    fail_unless(rg->getVersion() == version);

    fail_unless( rg->isSetId() == false );
    fail_unless( !rg->isSetReactionId());
    fail_unless( rg->getCurve() != NULL );
    fail_unless( !rg->isSetCurve() );

    delete rg;
}
END_TEST

START_TEST ( test_ReactionGlyph_new_WithNamespace )
{
    SBMLNamespaces* ns= new SBMLNamespaces;
    ReactionGlyph *rg=new ReactionGlyph(ns); 
    fail_unless( rg->getTypeCode() == SBML_LAYOUT_REACTIONGLYPH );
    fail_unless( rg->getMetaId()   == "" );

    fail_unless(rg->getLevel() == SBML_DEFAULT_LEVEL);
    fail_unless(rg->getVersion() == SBML_DEFAULT_VERSION);

    fail_unless( rg->isSetId() == false );
    fail_unless( !rg->isSetReactionId());
    fail_unless( rg->getCurve() != NULL );
    fail_unless( !rg->isSetCurve() );

    delete rg;
    delete ns;

    ns = new SBMLNamespaces(2,3);
    rg=new ReactionGlyph(ns);
    fail_unless( rg->getTypeCode() == SBML_LAYOUT_REACTIONGLYPH );
    fail_unless( rg->getMetaId()   == "" );

    fail_unless(rg->getLevel() == 2);
    fail_unless(rg->getVersion() == 3);

    fail_unless( rg->isSetId() == false );
    fail_unless( !rg->isSetReactionId());
    fail_unless( rg->getCurve() != NULL );
    fail_unless( !rg->isSetCurve() );

    delete rg;
    delete ns;
}
END_TEST


START_TEST ( test_ReactionGlyph_new_with_reactionId )
{
    std::string id="TestReactionGlyph";
    std::string reactionId="TestReaction";
    ReactionGlyph* rg=new ReactionGlyph(id,reactionId);
    fail_unless( rg->getTypeCode()    == SBML_LAYOUT_REACTIONGLYPH );
    fail_unless( rg->getMetaId()      == "" );
//    fail_unless( rg->getNotes()       == "" );
//    fail_unless( rg->getAnnotation()  == "" );
    fail_unless( rg->isSetId());
    fail_unless( rg->getId()          == id );
    fail_unless( rg->isSetReactionId());
    fail_unless( rg->getReactionId()  == reactionId );
    fail_unless( rg->getCurve() != NULL );
    fail_unless( !rg->isSetCurve() );
    delete rg;
}
END_TEST

START_TEST ( test_ReactionGlyph_setReactionId )
{
    std::string id="TestReactionGlyph";
    RG->setId(id);
    fail_unless(RG->isSetId());
    fail_unless(RG->getId() == id);
    id="";
    RG->setId(id);
    fail_unless(!RG->isSetId());
}
END_TEST


START_TEST ( test_ReactionGlyph_addSpeciesReferenceGlyph )
{
    std::string srgId="TestSpeciesReferenceGlyph";
    SpeciesReferenceGlyph srg;
    srg.setId(srgId);
    RG->addSpeciesReferenceGlyph(&srg);
    fail_unless(RG->getNumSpeciesReferenceGlyphs() == 1);
    fail_unless(RG->getSpeciesReferenceGlyph(0)->getId() == srgId); 
    
}
END_TEST

START_TEST ( test_ReactionGlyph_getNumSpeciesReferenceGlyphs )
{
    fail_unless(RG->getNumSpeciesReferenceGlyphs() == 0);
    RG->createSpeciesReferenceGlyph();
    RG->createSpeciesReferenceGlyph();
    RG->createSpeciesReferenceGlyph();
    RG->createSpeciesReferenceGlyph();
    fail_unless(RG->getNumSpeciesReferenceGlyphs() == 4);
}
END_TEST

START_TEST ( test_ReactionGlyph_setCurve )
{
    Curve* c=new Curve();
    c->createLineSegment();
    RG->setCurve(c);
    fail_unless(RG->getCurve() != NULL);
    fail_unless(RG->isSetCurve());
    fail_unless(RG->getCurve()->getNumCurveSegments() == 1);
    delete c;
}
END_TEST

START_TEST ( test_ReactionGlyph_isSetCurve )
{
    fail_unless(!RG->isSetCurve());
    RG->createLineSegment();
    fail_unless(RG->isSetCurve());
}
END_TEST

START_TEST ( test_ReactionGlyph_setCurve_NULL )
{
    RG->setCurve(NULL);
    fail_unless(RG->getCurve() != NULL);
    fail_unless(!RG->isSetCurve());
}
END_TEST

START_TEST ( test_ReactionGlyph_createSpeciesReferenceGlyph )
{
    RG->createSpeciesReferenceGlyph();
    RG->createSpeciesReferenceGlyph();
    fail_unless(RG->getNumSpeciesReferenceGlyphs() == 2);
}
END_TEST

START_TEST ( test_ReactionGlyph_createLineSegment )
{
    RG->createLineSegment();
    RG->createLineSegment();
    fail_unless(RG->isSetCurve());
    fail_unless(RG->getCurve()->getNumCurveSegments() == 2);

    LineSegment* ls=RG->getCurve()->getCurveSegment(0);
    const Point* p=ls->getStart();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=ls->getEnd();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    ls=RG->getCurve()->getCurveSegment(1);
    p=ls->getStart();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=ls->getEnd();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
}
END_TEST

START_TEST ( test_ReactionGlyph_createCubicBezier )
{
    RG->createCubicBezier();
    RG->createCubicBezier();
    fail_unless(RG->isSetCurve());
    fail_unless(RG->getCurve()->getNumCurveSegments() == 2);
    fail_unless(RG->getCurve()->getCurveSegment(0)->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
    
    CubicBezier* cb=(CubicBezier*)RG->getCurve()->getCurveSegment(0);
    Point* p=cb->getStart();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=cb->getBasePoint1();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=cb->getBasePoint2();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=cb->getEnd();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    fail_unless(RG->getCurve()->getCurveSegment(1)->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
    cb=(CubicBezier*)RG->getCurve()->getCurveSegment(1);
    p=cb->getStart();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=cb->getBasePoint1();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=cb->getBasePoint2();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
    p=cb->getEnd();
    fail_unless(p->getXOffset() == 0.0);
    fail_unless(p->getYOffset() == 0.0);
    fail_unless(p->getZOffset() == 0.0);
}
END_TEST

START_TEST ( test_ReactionGlyph_copyConstructor )
{
    ReactionGlyph* rg1=new ReactionGlyph();
    XMLNode* notes=new XMLNode();
    rg1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    rg1->setAnnotation(annotation);
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createCubicBezier();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createCubicBezier();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createCubicBezier();
    SpeciesReferenceGlyph* srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg1");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg2");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg3");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg4");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg5");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg6");
    ReactionGlyph* rg2=new ReactionGlyph(*rg1);
    delete rg2;
    delete rg1;
}
END_TEST

START_TEST ( test_ReactionGlyph_assignmentOperator )
{
    ReactionGlyph* rg1=new ReactionGlyph();
    XMLNode* notes=new XMLNode();
    rg1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    rg1->setAnnotation(annotation);
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createCubicBezier();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createCubicBezier();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createLineSegment();
    rg1->getCurve()->createCubicBezier();
    SpeciesReferenceGlyph* srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg1");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg2");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg3");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg4");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg5");
    srg=rg1->createSpeciesReferenceGlyph();
    srg->setId("srg6");
     ReactionGlyph* rg2=new ReactionGlyph();
    (*rg2)=(*rg1);
    delete rg2;
    delete rg1;
}
END_TEST


Suite *
create_suite_ReactionGlyph (void)
{
  Suite *suite = suite_create("ReactionGlyph");
  TCase *tcase = tcase_create("ReactionGlyph");

  tcase_add_checked_fixture( tcase,
                             ReactionGlyphTest_setup,
                             ReactionGlyphTest_teardown );


  tcase_add_test (tcase , test_ReactionGlyph_new                              );
  tcase_add_test( tcase, test_ReactionGlyph_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_ReactionGlyph_new_WithNamespace                 );
  tcase_add_test (tcase , test_ReactionGlyph_new_with_reactionId              );
  tcase_add_test (tcase , test_ReactionGlyph_setReactionId                    );
  tcase_add_test (tcase , test_ReactionGlyph_addSpeciesReferenceGlyph         );
  tcase_add_test (tcase , test_ReactionGlyph_getNumSpeciesReferenceGlyphs     );
  tcase_add_test (tcase , test_ReactionGlyph_setCurve                         );
  tcase_add_test (tcase , test_ReactionGlyph_setCurve_NULL                    );
  tcase_add_test (tcase , test_ReactionGlyph_isSetCurve                       );
  tcase_add_test (tcase , test_ReactionGlyph_createSpeciesReferenceGlyph      );
  tcase_add_test (tcase , test_ReactionGlyph_createLineSegment                );
  tcase_add_test (tcase , test_ReactionGlyph_createCubicBezier                );
  tcase_add_test( tcase , test_ReactionGlyph_copyConstructor                  );
  tcase_add_test( tcase , test_ReactionGlyph_assignmentOperator               );

  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
