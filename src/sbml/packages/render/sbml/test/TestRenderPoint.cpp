//
// Filename    : TestRenderPoint.cpp
// Description : Tests for the RenderPoint class
// Organization: University of Heidelberg
// Created     : 2009-09-30
//
// Copyright 2008 University of Heidelberg
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 2.1 of the License, or
// any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
// MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
// documentation provided hereunder is on an "as is" basis, and the
// University of Heidelberg have no obligations to
// provide maintenance, support, updates, enhancements or modifications.
// In no event shall the University of Heidelberg be
// liable to any party for direct, indirect, special, incidental or
// consequential damages, including lost profits, arising out of the use of
// this software and its documentation, even if the University of 
// Heidelberg have been advised of the possibility of such
// damage.  See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// The original code contained here was initially developed by:
//
//     Ralph Gauges
//     BIOQUANT/BQ0018
//     Im Neuenheimer Feld 267
//     69120 Heidelberg
//     Germany
//
//     mailto:ralph.gauges@bioquant.uni-heidelberg.de
//
// Contributor(s):



#include <sbml/common/common.h>
#include <sbml/common/extern.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/packages/layout/sbml/test/utility.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <RenderPoint.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static RenderPoint *P;
static RenderPkgNamespaces *renderns;

void
RenderPointTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    P = new (std::nothrow) RenderPoint(renderns);

    if (P == NULL)
    {
        fail("new(std::nothrow)RenderPoint(renderns) returned a NULL pointer.");
    }

}

void 
RenderPointTest_teardown (void)
{
    delete P;
    delete renderns;
}

START_TEST (test_RenderPoint_setCoordinates )
{
    fail_unless(P->x().getAbsoluteValue() < 1e-9);
    fail_unless(P->x().getRelativeValue() < 1e-9);
    fail_unless(P->y().getAbsoluteValue() < 1e-9);
    fail_unless(P->y().getRelativeValue() < 1e-9);
    fail_unless(P->z().getAbsoluteValue() < 1e-9);
    fail_unless(P->z().getRelativeValue() < 1e-9);
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(fabs((P->x().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 30.0) / 30.0) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 40.0) / 40.0) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(fabs((P->z().getAbsoluteValue() - 60.0) / 60.0) < 1e-9);
    fail_unless(fabs((P->z().getRelativeValue() - 70.0) / 70.0) < 1e-9);
    P->setCoordinates(RelAbsVector(2.0,3.0),RelAbsVector(4.0,5.0));
    fail_unless(fabs((P->x().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 3.0) / 3.0) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    fail_unless(P->z().getAbsoluteValue() < 1e-9);
    fail_unless(P->z().getRelativeValue() < 1e-9);
    P->setX(RelAbsVector(3.2,2.3));
    fail_unless(fabs((P->x().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    fail_unless(P->z().getAbsoluteValue() < 1e-9);
    fail_unless(P->z().getRelativeValue() < 1e-9);
    P->setY(RelAbsVector(1.1,2.2));
    fail_unless(fabs((P->x().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(P->z().getAbsoluteValue() < 1e-9);
    fail_unless(P->z().getRelativeValue() < 1e-9);
    P->setZ(RelAbsVector(5.5,6.6));
    fail_unless(fabs((P->x().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((P->z().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((P->z().getRelativeValue() - 6.6) / 6.6) < 1e-9);
}
END_TEST 

START_TEST ( test_RenderPoint_compare )
{
    RenderPoint p(2,4);
    fail_unless(p == *P);
    p.setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    RenderPoint p2(2,4);
    p2.setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(p == p2);
    p2.setCoordinates(RelAbsVector(20.0,35.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(!(p == p2));
}
END_TEST

START_TEST ( test_RenderPoint_assignment )
{
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    RenderPoint p(2,4);
    p.setCoordinates(RelAbsVector(2.0,3.0),RelAbsVector(4.0,5.0));
    fail_unless(!(p == *P));
    p = *P;
    fail_unless(p == *P);
}
END_TEST

START_TEST ( test_RenderPoint_initDefaults )
{
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(fabs((P->x().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 30.0) / 30.0) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 40.0) / 40.0) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(fabs((P->z().getAbsoluteValue() - 60.0) / 60.0) < 1e-9);
    fail_unless(fabs((P->z().getRelativeValue() - 70.0) / 70.0) < 1e-9);
    P->initDefaults();
    fail_unless(fabs((P->x().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
    fail_unless(fabs((P->x().getRelativeValue() - 30.0) / 30.0) < 1e-9);
    fail_unless(fabs((P->y().getAbsoluteValue() - 40.0) / 40.0) < 1e-9);
    fail_unless(fabs((P->y().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(P->z().getAbsoluteValue() < 1e-9);
    fail_unless(P->z().getRelativeValue() < 1e-9);
}
END_TEST

START_TEST ( test_RenderPoint_hasRequiredAttributes )
{
    fail_unless( P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless( P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,std::numeric_limits<double>::quiet_NaN()),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(std::numeric_limits<double>::quiet_NaN(),50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(std::numeric_limits<double>::quiet_NaN(),70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,std::numeric_limits<double>::quiet_NaN()));
    fail_unless(! P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,std::numeric_limits<double>::quiet_NaN()),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setCoordinates(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless( P->hasRequiredAttributes() );
}
END_TEST

START_TEST ( test_RenderPoint_hasRequiredElements )
{
    // has required elements should awlays be true
    fail_unless(P->hasRequiredElements() );
}
END_TEST

START_TEST ( test_RenderPoint_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderPoint\" x=\"10\" y=\"20\"/>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  RenderPoint p(*pNode);
  fail_unless(fabs((p.x().getAbsoluteValue() - 10.0) / 10.0) < 1e-9 );
  fail_unless(p.x().getRelativeValue() < 1e-9);
  fail_unless(fabs((p.y().getAbsoluteValue() - 20.0) / 20.0) < 1e-9 );
  fail_unless(p.y().getRelativeValue() < 1e-9);
  fail_unless(p.z().getAbsoluteValue() < 1e-9);
  fail_unless(p.z().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderPoint\" x=\"30%\" y=\"50%\" z=\"3\"/>\n"
      ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  p = RenderPoint(*pNode);
  fail_unless(fabs((p.x().getRelativeValue() - 30.0) / 30.0) < 1e-9 );
  fail_unless(p.x().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.y().getRelativeValue() - 50.0) / 50.0) < 1e-9 );
  fail_unless(p.y().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.z().getAbsoluteValue() - 3.0) / 3.0) < 1e-9 );
  fail_unless(p.z().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;

}
END_TEST

START_TEST ( test_RenderPoint_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderPoint\" x=\"10\" y=\"20\"/>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  RenderPoint* pP = new RenderPoint(*pNode1);
  fail_unless(pP != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pP->toXML("element"));
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));

  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pP;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderPoint\" x=\"30%\" y=\"50%\" z=\"3\"/>\n"
      ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pP = new RenderPoint(*pNode1);
  fail_unless(pP != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pP->toXML("element"));
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pP;

}
END_TEST


Suite *
create_suite_RenderPoint (void)
{
  Suite *suite = suite_create("RenderPoint");
  TCase *tcase = tcase_create("RenderPoint");


  tcase_add_checked_fixture( tcase,
                             RenderPointTest_setup,
                             RenderPointTest_teardown );

  tcase_add_test( tcase, test_RenderPoint_setCoordinates        );
  tcase_add_test( tcase, test_RenderPoint_compare               );
  tcase_add_test( tcase, test_RenderPoint_assignment            );
  tcase_add_test( tcase, test_RenderPoint_initDefaults          );
  tcase_add_test( tcase, test_RenderPoint_hasRequiredAttributes );
  tcase_add_test( tcase, test_RenderPoint_hasRequiredElements   );
  tcase_add_test( tcase, test_RenderPoint_read                  );
  tcase_add_test( tcase, test_RenderPoint_write                 );


  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
