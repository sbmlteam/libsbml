/**
 * Filename    : TestGeneralGlyph.cpp
 * Description : Unit tests for GeneralGlyph
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
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
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

#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/packages/layout/sbml/GeneralGlyph.h>
#include <sbml/packages/layout/sbml/ReferenceGlyph.h>
#include <sbml/packages/layout/sbml/LineSegment.h>
#include <sbml/packages/layout/sbml/CubicBezier.h>
#include <sbml/packages/layout/sbml/Curve.h>
#include <sbml/packages/layout/sbml/Point.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GeneralGlyph * RG;
static LayoutPkgNamespaces* LN;

void
GeneralGlyphTest_setup (void)
{
  LN = new LayoutPkgNamespaces();
  RG = new (std::nothrow) GeneralGlyph(LN);
  
  if (RG == NULL)
  {
    fail("new(std::nothrow) GeneralGlyph() returned a NULL pointer.");
  }
  
}

void
GeneralGlyphTest_teardown (void)
{
  delete RG;
  delete LN;
}

START_TEST ( test_GeneralGlyph_new )
{
  fail_unless( RG->getTypeCode()    == SBML_LAYOUT_GENERALGLYPH );
  fail_unless( RG->getMetaId()      == "" );
  fail_unless( RG->getId()          == "" );
  fail_unless( !RG->isSetId());
  fail_unless( !RG->isSetReferenceId());
  fail_unless( RG->getCurve() != NULL );
  fail_unless( !RG->isSetCurve() );
}
END_TEST

START_TEST ( test_GeneralGlyph_new_with_ReferenceId )
{
  std::string id="TestGeneralGlyph";
  std::string ReferenceId="TestReaction";
  GeneralGlyph* rg=new GeneralGlyph(LN,id,ReferenceId);
  fail_unless( rg->getTypeCode()    == SBML_LAYOUT_GENERALGLYPH );
  fail_unless( rg->getMetaId()      == "" );
  fail_unless( rg->isSetId());
  fail_unless( rg->getId()          == id );
  fail_unless( rg->isSetReferenceId());
  fail_unless( rg->getReferenceId()  == ReferenceId );
  fail_unless( rg->getCurve() != NULL );
  fail_unless( !rg->isSetCurve() );
  delete rg;
}
END_TEST

START_TEST ( test_GeneralGlyph_setReferenceId )
{
  std::string id="TestGeneralGlyph";
  RG->setId(id);
  fail_unless(RG->isSetId());
  fail_unless(RG->getId() == id);
  id="";
  RG->setId(id);
  fail_unless(!RG->isSetId());
}
END_TEST


START_TEST ( test_GeneralGlyph_addReferenceGlyph )
{
  std::string srgId="TestReferenceGlyph";
  ReferenceGlyph srg;
  srg.setId(srgId);
  RG->addReferenceGlyph(&srg);
  fail_unless(RG->getNumReferenceGlyphs() == 1);
  fail_unless(RG->getReferenceGlyph(0)->getId() == srgId);
  
}
END_TEST

START_TEST ( test_GeneralGlyph_getNumReferenceGlyphs )
{
  fail_unless(RG->getNumReferenceGlyphs() == 0);
  RG->createReferenceGlyph();
  RG->createReferenceGlyph();
  RG->createReferenceGlyph();
  RG->createReferenceGlyph();
  fail_unless(RG->getNumReferenceGlyphs() == 4);
}
END_TEST

START_TEST ( test_GeneralGlyph_setCurve )
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

START_TEST ( test_GeneralGlyph_isSetCurve )
{
  fail_unless(!RG->isSetCurve());
  RG->createLineSegment();
  fail_unless(RG->isSetCurve());
}
END_TEST

START_TEST ( test_GeneralGlyph_setCurve_NULL )
{
  RG->setCurve(NULL);
  fail_unless(RG->getCurve() != NULL);
  fail_unless(!RG->isSetCurve());
}
END_TEST

START_TEST ( test_GeneralGlyph_createReferenceGlyph )
{
  RG->createReferenceGlyph();
  RG->createReferenceGlyph();
  fail_unless(RG->getNumReferenceGlyphs() == 2);
}
END_TEST

START_TEST ( test_GeneralGlyph_createLineSegment )
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

START_TEST ( test_GeneralGlyph_createCubicBezier )
{
  RG->createCubicBezier();
  RG->createCubicBezier();
  fail_unless(RG->isSetCurve());
	Curve * curve = RG->getCurve();
	fail_unless(curve != NULL);
  fail_unless(curve->getNumCurveSegments() == 2);
	LineSegment *segment = curve->getCurveSegment(0);
	fail_unless(segment != NULL);
	if (segment == NULL) return;
  fail_unless(segment->getTypeCode() == SBML_LAYOUT_CUBICBEZIER );
  CubicBezier* cb= static_cast<CubicBezier*> (segment);
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
  cb= static_cast<CubicBezier*> (RG->getCurve()->getCurveSegment(1));
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

START_TEST ( test_GeneralGlyph_copyConstructor )
{
  GeneralGlyph* rg1=new GeneralGlyph();
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
  ReferenceGlyph* srg=rg1->createReferenceGlyph();
  srg->setId("srg1");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg2");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg3");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg4");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg5");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg6");
  GeneralGlyph* rg2=new GeneralGlyph(*rg1);
  delete rg2;
  delete rg1;
}
END_TEST

START_TEST ( test_GeneralGlyph_assignmentOperator )
{
  GeneralGlyph* rg1=new GeneralGlyph();
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
  ReferenceGlyph* srg=rg1->createReferenceGlyph();
  srg->setId("srg1");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg2");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg3");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg4");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg5");
  srg=rg1->createReferenceGlyph();
  srg->setId("srg6");
  GeneralGlyph* rg2=new GeneralGlyph();
  (*rg2)=(*rg1);
  delete rg2;
  delete rg1;
}
END_TEST


Suite *
create_suite_GeneralGlyph (void)
{
  Suite *suite = suite_create("GeneralGlyph");
  TCase *tcase = tcase_create("GeneralGlyph");
  
  tcase_add_checked_fixture( tcase,
                            GeneralGlyphTest_setup,
                            GeneralGlyphTest_teardown );
  
  
  tcase_add_test (tcase , test_GeneralGlyph_new                          );
  tcase_add_test (tcase , test_GeneralGlyph_new_with_ReferenceId          );
  tcase_add_test (tcase , test_GeneralGlyph_setReferenceId                );
  tcase_add_test (tcase , test_GeneralGlyph_addReferenceGlyph     );
  tcase_add_test (tcase , test_GeneralGlyph_getNumReferenceGlyphs );
  tcase_add_test (tcase , test_GeneralGlyph_setCurve                     );
  tcase_add_test (tcase , test_GeneralGlyph_setCurve_NULL                );
  tcase_add_test (tcase , test_GeneralGlyph_isSetCurve                   );
  tcase_add_test (tcase , test_GeneralGlyph_createReferenceGlyph  );
  tcase_add_test (tcase , test_GeneralGlyph_createLineSegment            );
  tcase_add_test (tcase , test_GeneralGlyph_createCubicBezier            );
  tcase_add_test( tcase , test_GeneralGlyph_copyConstructor              );
  tcase_add_test( tcase , test_GeneralGlyph_assignmentOperator           );
  
  
  suite_add_tcase(suite, tcase);
  
  return suite;
}



END_C_DECLS
