/**
 * Filename    : TestReferenceGlyph.cpp
 * Description : Unit tests for ReferenceGlyph
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

#include <sbml/packages/layout/sbml/ReferenceGlyph.h>
#include <sbml/packages/layout/sbml/LineSegment.h>
#include <sbml/packages/layout/sbml/CubicBezier.h>
#include <sbml/packages/layout/sbml/Curve.h>
#include <sbml/packages/layout/sbml/Point.h>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static ReferenceGlyph * SRG;
static LayoutPkgNamespaces* LN;

void
ReferenceGlyphTest_setup (void)
{
  LN = new LayoutPkgNamespaces();
  SRG = new (std::nothrow) ReferenceGlyph(LN);
  
  if (SRG == NULL)
  {
    fail("new(std::nothrow) ReferenceGlyph() returned a NULL pointer.");
  }
  
}

void
ReferenceGlyphTest_teardown (void)
{
  delete SRG;
  delete LN;
}

START_TEST (test_ReferenceGlyph_new )
{
  fail_unless( SRG->getTypeCode()   == SBML_LAYOUT_REFERENCEGLYPH );
  fail_unless( SRG->getMetaId()     == "" );
  fail_unless( !SRG->isSetId() );
  fail_unless( !SRG->isSetReferenceId() );
  fail_unless( !SRG->isSetGlyphId() );
  fail_unless( !SRG->isSetRole() );
  fail_unless( SRG->getRole() == "" );
  fail_unless( SRG->getCurve() != NULL);
  fail_unless( !SRG->isSetCurve());
}
END_TEST

START_TEST (test_ReferenceGlyph_new_with_data)
{
  std::string sid="TestReferenceGlyph";
  std::string glyphId="TestGlyph";
  std::string referenceId="TestReference";
  ReferenceGlyph* srg=new ReferenceGlyph( LN, sid,
                                         glyphId,
                                         referenceId,
                                         "substrate"
                                         );
  
  fail_unless( srg->getTypeCode()   == SBML_LAYOUT_REFERENCEGLYPH );
  fail_unless( srg->getMetaId()     == "" );
  fail_unless( srg->isSetId() );
  fail_unless( srg->getId() == sid);
  fail_unless( srg->isSetReferenceId() );
  fail_unless( srg->getReferenceId() == referenceId);
  fail_unless( srg->isSetGlyphId() );
  fail_unless( srg->getGlyphId() == glyphId);
  fail_unless( srg->isSetRole());
  fail_unless( srg->getRole() == "substrate" );
  fail_unless( srg->getCurve() != NULL);
  fail_unless( !srg->isSetCurve());
  
  delete srg;
}
END_TEST

START_TEST (test_ReferenceGlyph_setGlyphId)
{
  std::string glyphId="TestGlyph";
  SRG->setGlyphId(glyphId);
  fail_unless(SRG->isSetGlyphId());
  fail_unless(SRG->getGlyphId() == glyphId);
  SRG->setGlyphId("");
  fail_unless(!SRG->isSetGlyphId());
}
END_TEST

START_TEST (test_ReferenceGlyph_setReferenceId)
{
  std::string referenceId="TestReference";
  SRG->setReferenceId(referenceId);
  fail_unless(SRG->isSetReferenceId());
  fail_unless(SRG->getReferenceId() == referenceId);
  SRG->setReferenceId("");
  fail_unless(!SRG->isSetReferenceId());
}
END_TEST

START_TEST (test_ReferenceGlyph_setRole)
{
  SRG->setRole("modifier");
  fail_unless(SRG->isSetRole());
  fail_unless(SRG->getRole() == "modifier");
}
END_TEST


START_TEST ( test_ReferenceGlyph_getRole )
{
  SRG->setRole("undefined");
  fail_unless(SRG->getRole() == "undefined");
  SRG->setRole("substrate");
  fail_unless(SRG->getRole() == "substrate");
}
END_TEST

START_TEST (test_ReferenceGlyph_setCurve)
{
  Curve* c=new Curve();
  LineSegment* ls=new LineSegment();
  c->addCurveSegment(ls);
  delete ls;
  ls=new LineSegment();
  c->addCurveSegment(ls);
  delete ls;
  SRG->setCurve(c);
  fail_unless(SRG->isSetCurve());
  fail_unless(SRG->getCurve()->getNumCurveSegments() == 2);
  delete c;
}
END_TEST

START_TEST (test_ReferenceGlyph_setCurve_NULL)
{
  SRG->setCurve(NULL);
  fail_unless(!SRG->isSetCurve());
  fail_unless(SRG->getCurve() != NULL);
}
END_TEST

START_TEST (test_ReferenceGlyph_createLineSegment)
{
  LineSegment* ls=SRG->createLineSegment();
  fail_unless(SRG->isSetCurve());
  Point* p=ls->getStart();
  fail_unless(p->getXOffset() == 0.0);
  fail_unless(p->getYOffset() == 0.0);
  fail_unless(p->getZOffset() == 0.0);
  p=ls->getEnd();
  fail_unless(p->getXOffset() == 0.0);
  fail_unless(p->getYOffset() == 0.0);
  fail_unless(p->getZOffset() == 0.0);
}
END_TEST

START_TEST (test_ReferenceGlyph_createCubicBezier)
{
  CubicBezier* cb=SRG->createCubicBezier();
  fail_unless(SRG->isSetCurve());
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
}
END_TEST

START_TEST ( test_ReferenceGlyph_copyConstructor )
{
  ReferenceGlyph* srg1=new ReferenceGlyph();
  XMLNode* notes=new XMLNode();
  srg1->setNotes(notes);
  XMLNode* annotation=new XMLNode();
  srg1->setAnnotation(annotation);
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createCubicBezier();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createCubicBezier();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createCubicBezier();
  ReferenceGlyph* srg2=new ReferenceGlyph(*srg1);
  delete srg2;
  delete srg1;
}
END_TEST

START_TEST ( test_ReferenceGlyph_assignmentOperator )
{
  ReferenceGlyph* srg1=new ReferenceGlyph();
  XMLNode* notes=new XMLNode();
  srg1->setNotes(notes);
  XMLNode* annotation=new XMLNode();
  srg1->setAnnotation(annotation);
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createCubicBezier();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createCubicBezier();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createLineSegment();
  srg1->getCurve()->createCubicBezier();
  ReferenceGlyph* srg2=new ReferenceGlyph();
  (*srg2)=(*srg1);
  delete srg2;
  delete srg1;
}
END_TEST



Suite *
create_suite_ReferenceGlyph (void)
{
  Suite *suite = suite_create("ReferenceGlyph");
  TCase *tcase = tcase_create("ReferenceGlyph");
  
  tcase_add_checked_fixture( tcase,
                            ReferenceGlyphTest_setup,
                            ReferenceGlyphTest_teardown );
  
  tcase_add_test( tcase, test_ReferenceGlyph_new                   );
  tcase_add_test( tcase, test_ReferenceGlyph_new_with_data         );
  tcase_add_test( tcase, test_ReferenceGlyph_setGlyphId     );
  tcase_add_test( tcase, test_ReferenceGlyph_setReferenceId );
  tcase_add_test( tcase, test_ReferenceGlyph_setRole               );
  tcase_add_test( tcase, test_ReferenceGlyph_getRole         );
  tcase_add_test( tcase, test_ReferenceGlyph_setCurve              );
  tcase_add_test( tcase, test_ReferenceGlyph_setCurve_NULL         );
  tcase_add_test( tcase, test_ReferenceGlyph_createLineSegment     );
  tcase_add_test( tcase, test_ReferenceGlyph_createCubicBezier     );
  tcase_add_test( tcase, test_ReferenceGlyph_copyConstructor       );
  tcase_add_test( tcase, test_ReferenceGlyph_assignmentOperator    );
  
  suite_add_tcase(suite, tcase);
  
  return suite;
}



END_C_DECLS
