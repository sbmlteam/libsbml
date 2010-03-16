/**
 * Filename    : TestCurve.cpp
 * Description : Unit tests for Curve
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

#include "Curve.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Curve_t * C;

void
CurveTest_setup (void)
{
    C = Curve_create();

    if (C == NULL)
    {
        fail("Curve_create(); returned a NULL pointer.");
    }

}

void 
CurveTest_teardown (void)
{
    Curve_free(C);
}

START_TEST (test_Curve_create)
{
    fail_unless( SBase_getTypeCode   ((SBase_t*) C) == SBML_LAYOUT_CURVE );
    fail_unless( SBase_getMetaId     ((SBase_t*) C) == NULL );
//    fail_unless( SBase_getNotes      ((SBase_t*) C) == NULL );
//    fail_unless( SBase_getAnnotation ((SBase_t*) C) == NULL );
   fail_unless(C->getNumCurveSegments() == 0);   
}
END_TEST

START_TEST ( test_Curve_new_WithLevelVersionAndNamespaces )
{
   unsigned int level=1;
   unsigned int version=2;
   Curve *c=new Curve(level, version); 
   fail_unless( c->getTypeCode() == SBML_LAYOUT_CURVE );
   fail_unless( c->getMetaId()   == "" );

   fail_unless(c->getLevel() == level);
   fail_unless(c->getVersion() == version);

   fail_unless( c->isSetId() == false );
   fail_unless( c->getNumCurveSegments() == 0);   
   
   delete c;

   level=2;
   version=3;
   c=new Curve(level, version); 
   fail_unless( c->getTypeCode() == SBML_LAYOUT_CURVE );
   fail_unless( c->getMetaId()   == "" );

   fail_unless(c->getLevel() == level);
   fail_unless(c->getVersion() == version);

   fail_unless( c->isSetId() == false );
   fail_unless( c->getNumCurveSegments() == 0);   
   
   delete c;
}
END_TEST

START_TEST ( test_Curve_new_WithNamespace )
{
   SBMLNamespaces* ns=new SBMLNamespaces;
   Curve *c=new Curve(ns); 
   fail_unless( c->getTypeCode() == SBML_LAYOUT_CURVE );
   fail_unless( c->getMetaId()   == "" );

   fail_unless(c->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(c->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( c->isSetId() == false );
   fail_unless( c->getNumCurveSegments() == 0);   
   
   delete c;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   c=new Curve(ns); 
   fail_unless( c->getTypeCode() == SBML_LAYOUT_CURVE );
   fail_unless( c->getMetaId()   == "" );

   fail_unless(c->getLevel() == 2);
   fail_unless(c->getVersion() == 3);

   fail_unless( c->isSetId() == false );
   fail_unless( c->getNumCurveSegments() == 0);   
   
   delete c;
   delete ns;
}
END_TEST

START_TEST (test_Curve_createFrom)
{
    Curve_t* c=Curve_createFrom(C);
    Curve_free(c);
}
END_TEST

START_TEST (test_Curve_createFrom_NULL)
{
    Curve_t* c=Curve_createFrom(NULL);
    Curve_free(c);

}
END_TEST

START_TEST (test_Curve_addCurveSegment)
{
}
END_TEST

START_TEST (test_Curve_addCurveSegment_NULL)
{
    
}
END_TEST

START_TEST (test_Curve_getNumCurveSegments)
{
    
}
END_TEST

START_TEST (test_Curve_getCurveSegment)
{
    Curve_createLineSegment(C);
    LineSegment_t* ls=Curve_getCurveSegment(C,Curve_getNumCurveSegments(C)-1);
    fail_unless(ls != NULL);
}
END_TEST

START_TEST (test_Curve_getListOfCurveSegments )
{
    Curve_createLineSegment(C);
    ListOf* l=Curve_getListOfCurveSegments(C);
    fail_unless(l != NULL);
    fail_unless(ListOf_size(l) == 1);
}
END_TEST

START_TEST (test_Curve_createLineSegment )
{
    unsigned int number=Curve_getNumCurveSegments(C);
    LineSegment_t* ls=Curve_createLineSegment(C);
    fail_unless(ls !=NULL);
    fail_unless(Curve_getNumCurveSegments(C)==number+1);
}
END_TEST

START_TEST (test_Curve_createCubicBezier )
{
    unsigned int number=Curve_getNumCurveSegments(C);
    CubicBezier_t* cb=Curve_createCubicBezier(C);
    fail_unless(cb !=NULL);
    fail_unless(Curve_getNumCurveSegments(C)==number+1);
}
END_TEST

START_TEST ( test_Curve_copyConstructor )
{
    Curve* c1=new Curve();
    XMLNode* notes=new XMLNode();
    c1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    c1->setAnnotation(annotation);
    c1->createLineSegment();
    c1->createLineSegment();
    c1->createCubicBezier();
    c1->createCubicBezier();
    Curve* c2=new Curve(*c1);
    delete c2;
    delete c1;
}
END_TEST

START_TEST ( test_Curve_assignmentOperator )
{
    Curve* c1=new Curve();
    XMLNode* notes=new XMLNode();
    c1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    c1->setAnnotation(annotation);
    c1->createLineSegment();
    c1->createLineSegment();
    c1->createCubicBezier();
    c1->createCubicBezier();
    Curve* c2=new Curve();
    (*c2)=(*c1);
    delete c2;
    delete c1;
}
END_TEST

Suite *
create_suite_Curve (void)
{
  Suite *suite = suite_create("Curve");
  TCase *tcase = tcase_create("Curve");


  tcase_add_checked_fixture( tcase,
                             CurveTest_setup,
                             CurveTest_teardown );

  tcase_add_test( tcase, test_Curve_create                            );
  tcase_add_test( tcase, test_Curve_new_WithLevelVersionAndNamespaces );
  tcase_add_test( tcase, test_Curve_new_WithNamespace                 );
  tcase_add_test( tcase, test_Curve_createFrom                        );
  tcase_add_test( tcase, test_Curve_createFrom_NULL                   );
  tcase_add_test( tcase, test_Curve_addCurveSegment                   );
  tcase_add_test( tcase, test_Curve_addCurveSegment_NULL              );
  tcase_add_test( tcase, test_Curve_getNumCurveSegments               );
  tcase_add_test( tcase, test_Curve_getCurveSegment                   );
  tcase_add_test( tcase, test_Curve_getListOfCurveSegments            );
  tcase_add_test( tcase, test_Curve_createLineSegment                 );
  tcase_add_test( tcase, test_Curve_createCubicBezier                 );
  tcase_add_test( tcase, test_Curve_copyConstructor                   );
  tcase_add_test( tcase, test_Curve_assignmentOperator                );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
