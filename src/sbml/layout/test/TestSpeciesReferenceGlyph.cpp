/**
 * Filename    : TestSpeciesReferenceGlyph.cpp
 * Description : Unit tests for SpeciesReferenceGlyph
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

#include "SpeciesReferenceGlyph.h"
#include "Curve.h"
#include "LineSegment.h"
#include "CubicBezier.h"
#include "Point.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static SpeciesReferenceGlyph * SRG;

void
SpeciesReferenceGlyphTest_setup (void)
{
    SRG = new(std::nothrow )SpeciesReferenceGlyph();

    if (SRG == NULL)
    {
        fail("new(std::nothrow) SpeciesReferenceGlyph() returned a NULL pointer.");
    }

}

void 
SpeciesReferenceGlyphTest_teardown (void)
{
    delete SRG;
}

START_TEST (test_SpeciesReferenceGlyph_new )
{
   fail_unless( SRG->getTypeCode()   == SBML_LAYOUT_SPECIESREFERENCEGLYPH );
   fail_unless( SRG->getMetaId()     == "" );
//   fail_unless( SRG->getNotes()      == "" );
//   fail_unless( SRG->getAnnotation() == "" );

   fail_unless( !SRG->isSetId() );
   fail_unless( !SRG->isSetSpeciesReferenceId() );
   fail_unless( !SRG->isSetSpeciesGlyphId() );
   fail_unless( !SRG->isSetRole() );
   fail_unless( SRG->getRole() == SPECIES_ROLE_UNDEFINED );
   fail_unless( SRG->getCurve() != NULL);
   fail_unless( !SRG->isSetCurve());
 }
END_TEST

START_TEST ( test_SpeciesReferenceGlyph_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   SpeciesReferenceGlyph *srg=new SpeciesReferenceGlyph(level, version); 
   fail_unless( srg->getTypeCode() == SBML_LAYOUT_SPECIESREFERENCEGLYPH );
   fail_unless( srg->getMetaId()   == "" );

   fail_unless(srg->getLevel() == level);
   fail_unless(srg->getVersion() == version);

   fail_unless( srg->isSetId() == false );
   fail_unless( !srg->isSetSpeciesReferenceId() );
   fail_unless( !srg->isSetSpeciesGlyphId() );
   fail_unless( !srg->isSetRole() );
   fail_unless( srg->getRole() == SPECIES_ROLE_UNDEFINED );
   fail_unless( srg->getCurve() != NULL);
   fail_unless( !srg->isSetCurve());
   
   delete srg;

   level=2;
   version=3;
   srg=new SpeciesReferenceGlyph(level, version); 
   fail_unless( srg->getTypeCode() == SBML_LAYOUT_SPECIESREFERENCEGLYPH );
   fail_unless( srg->getMetaId()   == "" );

   fail_unless(srg->getLevel() == level);
   fail_unless(srg->getVersion() == version);

   fail_unless( srg->isSetId() == false );
   fail_unless( !srg->isSetSpeciesReferenceId() );
   fail_unless( !srg->isSetSpeciesGlyphId() );
   fail_unless( !srg->isSetRole() );
   fail_unless( srg->getRole() == SPECIES_ROLE_UNDEFINED );
   fail_unless( srg->getCurve() != NULL);
   fail_unless( !srg->isSetCurve());
   
   delete srg;
}
END_TEST

START_TEST ( test_SpeciesReferenceGlyph_new_WithNamespace )
{
   SBMLNamespaces* ns = new SBMLNamespaces;
   SpeciesReferenceGlyph *srg=new SpeciesReferenceGlyph(ns); 
   fail_unless( srg->getTypeCode() == SBML_LAYOUT_SPECIESREFERENCEGLYPH );
   fail_unless( srg->getMetaId()   == "" );

   fail_unless(srg->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(srg->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( srg->isSetId() == false );
   fail_unless( !srg->isSetSpeciesReferenceId() );
   fail_unless( !srg->isSetSpeciesGlyphId() );
   fail_unless( !srg->isSetRole() );
   fail_unless( srg->getRole() == SPECIES_ROLE_UNDEFINED );
   fail_unless( srg->getCurve() != NULL);
   fail_unless( !srg->isSetCurve());
   
   delete srg;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   srg=new SpeciesReferenceGlyph(ns);
   fail_unless( srg->getTypeCode() == SBML_LAYOUT_SPECIESREFERENCEGLYPH );
   fail_unless( srg->getMetaId()   == "" );

   fail_unless(srg->getLevel() == 2);
   fail_unless(srg->getVersion() == 3);

   fail_unless( srg->isSetId() == false );
   fail_unless( !srg->isSetSpeciesReferenceId() );
   fail_unless( !srg->isSetSpeciesGlyphId() );
   fail_unless( !srg->isSetRole() );
   fail_unless( srg->getRole() == SPECIES_ROLE_UNDEFINED );
   fail_unless( srg->getCurve() != NULL);
   fail_unless( !srg->isSetCurve());
   
   delete srg;
   delete ns;
}
END_TEST


START_TEST (test_SpeciesReferenceGlyph_new_with_data)
{
    std::string sid="TestSpeciesReferenceGlyph";
    std::string glyphId="TestSpeciesGlyph";
    std::string referenceId="TestSpeciesReference";
    SpeciesReferenceRole_t role=SPECIES_ROLE_SUBSTRATE;
    SpeciesReferenceGlyph* srg=new SpeciesReferenceGlyph( sid,
                                                          glyphId,
                                                          referenceId,
                                                          role
                                                        );

   fail_unless( srg->getTypeCode()   == SBML_LAYOUT_SPECIESREFERENCEGLYPH );
   fail_unless( srg->getMetaId()     == "" );
//   fail_unless( srg->getNotes()      == "" );
//   fail_unless( srg->getAnnotation() == "" );

   fail_unless( srg->isSetId() );
   fail_unless( srg->getId() == sid);
   fail_unless( srg->isSetSpeciesReferenceId() );
   fail_unless( srg->getSpeciesReferenceId() == referenceId);
   fail_unless( srg->isSetSpeciesGlyphId() );
   fail_unless( srg->getSpeciesGlyphId() == glyphId);
   fail_unless( srg->isSetRole());
   fail_unless( srg->getRole() == role );
   fail_unless( srg->getCurve() != NULL);
   fail_unless( !srg->isSetCurve());

   delete srg;
}
END_TEST

START_TEST (test_SpeciesReferenceGlyph_setSpeciesGlyphId)
{
    std::string glyphId="TestSpeciesGlyph";
    SRG->setSpeciesGlyphId(glyphId);
    fail_unless(SRG->isSetSpeciesGlyphId());
    fail_unless(SRG->getSpeciesGlyphId() == glyphId);
    SRG->setSpeciesGlyphId("");
    fail_unless(!SRG->isSetSpeciesGlyphId());
}
END_TEST

START_TEST (test_SpeciesReferenceGlyph_setSpeciesReferenceId)
{
    std::string referenceId="TestSpeciesReference";
    SRG->setSpeciesReferenceId(referenceId);
    fail_unless(SRG->isSetSpeciesReferenceId());
    fail_unless(SRG->getSpeciesReferenceId() == referenceId);
    SRG->setSpeciesReferenceId("");
    fail_unless(!SRG->isSetSpeciesReferenceId());
}
END_TEST

START_TEST (test_SpeciesReferenceGlyph_setRole)
{
    SpeciesReferenceRole_t role=SPECIES_ROLE_MODIFIER;
    SRG->setRole(role);
    fail_unless(SRG->isSetRole());
    fail_unless(SRG->getRole() == role);
}
END_TEST

START_TEST (test_SpeciesReferenceGlyph_setRole_by_string)
{
    SRG->setRole("undefined");
    fail_unless(SRG->getRole()==SPECIES_ROLE_UNDEFINED);
    SRG->setRole("substrate");
    fail_unless(SRG->getRole()==SPECIES_ROLE_SUBSTRATE);
    SRG->setRole("product");
    fail_unless(SRG->getRole()==SPECIES_ROLE_PRODUCT);
    SRG->setRole("sidesubstrate");
    fail_unless(SRG->getRole()==SPECIES_ROLE_SIDESUBSTRATE);
    SRG->setRole("sideproduct");
    fail_unless(SRG->getRole()==SPECIES_ROLE_SIDEPRODUCT);
    SRG->setRole("modifier");
    fail_unless(SRG->getRole()==SPECIES_ROLE_MODIFIER);
    SRG->setRole("activator");
    fail_unless(SRG->getRole()==SPECIES_ROLE_ACTIVATOR);
    SRG->setRole("inhibitor");
    fail_unless(SRG->getRole()==SPECIES_ROLE_INHIBITOR);
    SRG->setRole("test");
    fail_unless(SRG->getRole()==SPECIES_ROLE_UNDEFINED);
}
END_TEST

START_TEST ( test_SpeciesReferenceGlyph_getRoleString )
{
    SRG->setRole(SPECIES_ROLE_UNDEFINED);
    fail_unless(SRG->getRoleString() == "undefined");
    SRG->setRole(SPECIES_ROLE_SUBSTRATE);
    fail_unless(SRG->getRoleString() == "substrate");
    SRG->setRole(SPECIES_ROLE_PRODUCT);
    fail_unless(SRG->getRoleString() == "product");
    SRG->setRole(SPECIES_ROLE_SIDESUBSTRATE);
    fail_unless(SRG->getRoleString() == "sidesubstrate");
    SRG->setRole(SPECIES_ROLE_SIDEPRODUCT);
    fail_unless(SRG->getRoleString() == "sideproduct");
    SRG->setRole(SPECIES_ROLE_MODIFIER);
    fail_unless(SRG->getRoleString() == "modifier");
    SRG->setRole(SPECIES_ROLE_ACTIVATOR);
    fail_unless(SRG->getRoleString() == "activator");
    SRG->setRole(SPECIES_ROLE_INHIBITOR);
    fail_unless(SRG->getRoleString() == "inhibitor");
}
END_TEST

START_TEST (test_SpeciesReferenceGlyph_setCurve)
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

START_TEST (test_SpeciesReferenceGlyph_setCurve_NULL)
{
    SRG->setCurve(NULL);
    fail_unless(!SRG->isSetCurve());
    fail_unless(SRG->getCurve() != NULL);
}
END_TEST

START_TEST (test_SpeciesReferenceGlyph_createLineSegment)
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

START_TEST (test_SpeciesReferenceGlyph_createCubicBezier)
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

START_TEST ( test_SpeciesReferenceGlyph_copyConstructor )
{
    SpeciesReferenceGlyph* srg1=new SpeciesReferenceGlyph();
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
    SpeciesReferenceGlyph* srg2=new SpeciesReferenceGlyph(*srg1);
    delete srg2;
    delete srg1;
}
END_TEST

START_TEST ( test_SpeciesReferenceGlyph_assignmentOperator )
{
    SpeciesReferenceGlyph* srg1=new SpeciesReferenceGlyph();
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
     SpeciesReferenceGlyph* srg2=new SpeciesReferenceGlyph();
    (*srg2)=(*srg1);
    delete srg2;
    delete srg1;
}
END_TEST



Suite *
create_suite_SpeciesReferenceGlyph (void)
{
  Suite *suite = suite_create("SpeciesReferenceGlyph");
  TCase *tcase = tcase_create("SpeciesReferenceGlyph");

  tcase_add_checked_fixture( tcase,
                             SpeciesReferenceGlyphTest_setup,
                             SpeciesReferenceGlyphTest_teardown );

  tcase_add_test( tcase, test_SpeciesReferenceGlyph_new                               );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_new_WithNamespace                 );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_new_with_data                     );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_setSpeciesGlyphId                 );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_setSpeciesReferenceId             );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_setRole                           );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_getRoleString                     );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_setRole_by_string                 );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_setCurve                          );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_setCurve_NULL                     );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_createLineSegment                 );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_createCubicBezier                 );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_copyConstructor                   );
  tcase_add_test( tcase, test_SpeciesReferenceGlyph_assignmentOperator                );
  
  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
