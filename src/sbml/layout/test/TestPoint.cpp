/**
 * Filename    : TestPoint.cpp
 * Description : Unit tests for Point
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

#include "Point.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Point * P;

void
PointTest_setup (void)
{
    P = new(std::nothrow )Point();

    if (P == NULL)
    {
        fail("new(std::nothrow) Point() returned a NULL pointer.");
    }

}

void 
PointTest_teardown (void)
{
    delete P;
}

START_TEST (test_Point_create)
{
    fail_unless( P->getTypeCode   () == SBML_LAYOUT_POINT );
    fail_unless( P->getMetaId     () == "" );
//    fail_unless( P->getNotes      () == "" );
//    fail_unless( P->getAnnotation () == "" );
    fail_unless( P->getXOffset() == 0.0 );
    fail_unless( P->getYOffset() == 0.0 );
    fail_unless( P->getZOffset() == 0.0 );
}
END_TEST


START_TEST ( test_Point_new_WithNamespace )
{
    SBMLNamespaces* ns=new SBMLNamespaces;
    Point *p=new Point(ns); 
    fail_unless( p->getTypeCode() == SBML_LAYOUT_POINT );
    fail_unless( p->getMetaId()   == "" );

    fail_unless(p->getLevel() == SBML_DEFAULT_LEVEL);
    fail_unless(p->getVersion() == SBML_DEFAULT_VERSION);

    fail_unless( p->isSetId() == false );
    fail_unless( p->getXOffset() == 0.0 );
    fail_unless( p->getYOffset() == 0.0 );
    fail_unless( p->getZOffset() == 0.0 );

    delete p;
    delete ns;

    ns = new SBMLNamespaces(2,3);
    p=new Point(ns); 
    fail_unless( p->getTypeCode() == SBML_LAYOUT_POINT );
    fail_unless( p->getMetaId()   == "" );

    fail_unless(p->getLevel() == 2);
    fail_unless(p->getVersion() == 3);

    fail_unless( p->isSetId() == false );
    fail_unless( p->getXOffset() == 0.0 );
    fail_unless( p->getYOffset() == 0.0 );
    fail_unless( p->getZOffset() == 0.0 );

    delete p;
    delete ns;
}
END_TEST


START_TEST (test_Point_createWithCoordinates)
{
    Point* p = new(std::nothrow) Point( 1.2 , 0.4 , 3.1415 );
    if (p == NULL)
    {
        fail("new(std::nothrow) Point(1.2,0.4,3.1415) returned a NULL pointer.");
    }

    fail_unless( p->getTypeCode   () == SBML_LAYOUT_POINT );
    fail_unless( p->getMetaId     () == "" );
//    fail_unless( p->getNotes      () == "" );
//    fail_unless( p->getAnnotation () == "" );
    fail_unless( p->getXOffset() == 1.2 );
    fail_unless( p->getYOffset() == 0.4 );
    fail_unless( p->getZOffset() == 3.1415 );

    delete p;
}
END_TEST

START_TEST ( test_Point_free_NULL)
{
    Point_free(NULL);
}
END_TEST

START_TEST ( test_Point_setOffsets)
{
    P->setOffsets(1.1 , -2.2 , 3.3);
   
    fail_unless( P->getXOffset() ==  1.1 );
    fail_unless( P->getYOffset() == -2.2 );
    fail_unless( P->getZOffset() ==  3.3 );

}
END_TEST

START_TEST ( test_Point_initDefaults)
{
    P->setOffsets(-1.1 , 2.2 , -3.3);
    P->initDefaults();

    fail_unless( P->getXOffset() == -1.1 );
    fail_unless( P->getYOffset() ==  2.2 );
    fail_unless( P->getZOffset() ==  0.0 );

}
END_TEST

START_TEST ( test_Point_setXOffset)
{
    P->setOffsets(1.1 , 2.2 , 3.3);
    P->setXOffset(8.8);

    fail_unless(P->getXOffset() == 8.8);
    fail_unless(P->getYOffset() == 2.2);
    fail_unless(P->getZOffset() == 3.3);

}
END_TEST

START_TEST ( test_Point_setYOffset)
{
    P->setOffsets(1.1 , 2.2 , 3.3);
    P->setYOffset(8.8);

    fail_unless(P->getXOffset() == 1.1);
    fail_unless(P->getYOffset() == 8.8);
    fail_unless(P->getZOffset() == 3.3);

}
END_TEST

START_TEST ( test_Point_setZOffset)
{
    P->setOffsets(1.1 , 2.2 , 3.3);
    P->setZOffset(8.8);

    fail_unless(P->getXOffset() == 1.1);
    fail_unless(P->getYOffset() == 2.2);
    fail_unless(P->getZOffset() == 8.8);

}
END_TEST

START_TEST ( test_Point_copyConstructor )
{
    Point* p1=new Point();
    XMLNode* notes=new XMLNode();
    p1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    p1->setAnnotation(annotation);
    Point* p2=new Point(*p1);
    delete p2;
    delete p1;
}
END_TEST

START_TEST ( test_Point_assignmentOperator )
{
    Point* p1=new Point();
    XMLNode* notes=new XMLNode();
    p1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    p1->setAnnotation(annotation);
    Point* p2=new Point();
    (*p2)=(*p1);
    delete p2;
    delete p1;
}
END_TEST



Suite *
create_suite_Point (void)
{
  Suite *suite = suite_create("Point");
  TCase *tcase = tcase_create("Point");


  tcase_add_checked_fixture( tcase,
                             PointTest_setup,
                             PointTest_teardown );

  tcase_add_test( tcase, test_Point_create                            );
  tcase_add_test( tcase, test_Point_new_WithNamespace                 );
  tcase_add_test( tcase, test_Point_createWithCoordinates             );
  tcase_add_test( tcase, test_Point_free_NULL                         );
  tcase_add_test( tcase, test_Point_setOffsets                        );
  tcase_add_test( tcase, test_Point_initDefaults                      );
  tcase_add_test( tcase, test_Point_setXOffset                        );
  tcase_add_test( tcase, test_Point_setYOffset                        );
  tcase_add_test( tcase, test_Point_setZOffset                        );  
  tcase_add_test( tcase, test_Point_copyConstructor                   );
  tcase_add_test( tcase, test_Point_assignmentOperator                );
  

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
