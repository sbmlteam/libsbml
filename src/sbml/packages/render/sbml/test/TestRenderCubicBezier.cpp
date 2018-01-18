//
// Filename    : TestRenderCubicBezier.cpp
// Description : Tests for the RenderCubicBezier class
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

#include <RenderCubicBezier.h>
#include <RelAbsVector.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static RenderCubicBezier *P;
static RenderPkgNamespaces *renderns;

void
RenderCubicBezierTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    P = new (std::nothrow) RenderCubicBezier(renderns);

    if (P == NULL)
    {
        fail("new(std::nothrow)RenderCubicBezier(renderns) returned a NULL pointer.");
    }

}

void 
RenderCubicBezierTest_teardown (void)
{
    delete P;
    delete renderns;
}

START_TEST (test_RenderCubicBezier_setBasePoint1 )
{
    fail_unless(P->basePoint1_X().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint1_X().getRelativeValue() < 1e-9);
    fail_unless(P->basePoint1_Y().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint1_Y().getRelativeValue() < 1e-9);
    fail_unless(P->basePoint1_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint1_Z().getRelativeValue() < 1e-9);
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(fabs((P->basePoint1_X().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_X().getRelativeValue() - 30.0) / 30.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getAbsoluteValue() - 40.0) / 40.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Z().getAbsoluteValue() - 60.0) / 60.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Z().getRelativeValue() - 70.0) / 70.0) < 1e-9);
    P->setBasePoint1(RelAbsVector(2.0,3.0),RelAbsVector(4.0,5.0));
    fail_unless(fabs((P->basePoint1_X().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_X().getRelativeValue() - 3.0) / 3.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    fail_unless(P->basePoint1_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint1_Z().getRelativeValue() < 1e-9);
    P->setBasePoint1_X(RelAbsVector(3.2,2.3));
    fail_unless(fabs((P->basePoint1_X().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->basePoint1_X().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    fail_unless(P->basePoint1_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint1_Z().getRelativeValue() < 1e-9);
    P->setBasePoint1_Y(RelAbsVector(1.1,2.2));
    fail_unless(fabs((P->basePoint1_X().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->basePoint1_X().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(P->basePoint1_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint1_Z().getRelativeValue() < 1e-9);
    P->setBasePoint1_Z(RelAbsVector(5.5,6.6));
    fail_unless(fabs((P->basePoint1_X().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->basePoint1_X().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((P->basePoint1_Y().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((P->basePoint1_Z().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((P->basePoint1_Z().getRelativeValue() - 6.6) / 6.6) < 1e-9);
}
END_TEST 

START_TEST (test_RenderCubicBezier_setBasePoint2 )
{
    fail_unless(P->basePoint2_X().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint2_X().getRelativeValue() < 1e-9);
    fail_unless(P->basePoint2_Y().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint2_Y().getRelativeValue() < 1e-9);
    fail_unless(P->basePoint2_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint2_Z().getRelativeValue() < 1e-9);
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(fabs((P->basePoint2_X().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_X().getRelativeValue() - 30.0) / 30.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getAbsoluteValue() - 40.0) / 40.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Z().getAbsoluteValue() - 60.0) / 60.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Z().getRelativeValue() - 70.0) / 70.0) < 1e-9);
    P->setBasePoint2(RelAbsVector(2.0,3.0),RelAbsVector(4.0,5.0));
    fail_unless(fabs((P->basePoint2_X().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_X().getRelativeValue() - 3.0) / 3.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    fail_unless(P->basePoint2_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint2_Z().getRelativeValue() < 1e-9);
    P->setBasePoint2_X(RelAbsVector(3.2,2.3));
    fail_unless(fabs((P->basePoint2_X().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->basePoint2_X().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    fail_unless(P->basePoint2_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint2_Z().getRelativeValue() < 1e-9);
    P->setBasePoint2_Y(RelAbsVector(1.1,2.2));
    fail_unless(fabs((P->basePoint2_X().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->basePoint2_X().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(P->basePoint2_Z().getAbsoluteValue() < 1e-9);
    fail_unless(P->basePoint2_Z().getRelativeValue() < 1e-9);
    P->setBasePoint2_Z(RelAbsVector(5.5,6.6));
    fail_unless(fabs((P->basePoint2_X().getAbsoluteValue() - 3.2) / 3.2) < 1e-9);
    fail_unless(fabs((P->basePoint2_X().getRelativeValue() - 2.3) / 2.3) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((P->basePoint2_Y().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((P->basePoint2_Z().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((P->basePoint2_Z().getRelativeValue() - 6.6) / 6.6) < 1e-9);
}
END_TEST 

START_TEST ( test_RenderCubicBezier_hasRequiredAttributes )
{
    fail_unless( P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless( P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(40.0,std::numeric_limits<double>::quiet_NaN()),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(std::numeric_limits<double>::quiet_NaN(),50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(std::numeric_limits<double>::quiet_NaN(),70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,std::numeric_limits<double>::quiet_NaN()));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,std::numeric_limits<double>::quiet_NaN()),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint1(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless( P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless( P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(40.0,std::numeric_limits<double>::quiet_NaN()),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(std::numeric_limits<double>::quiet_NaN(),50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(std::numeric_limits<double>::quiet_NaN(),70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,std::numeric_limits<double>::quiet_NaN()));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,std::numeric_limits<double>::quiet_NaN()),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless(! P->hasRequiredAttributes() );
    P->setBasePoint2(RelAbsVector(20.0,30.0),RelAbsVector(40.0,50.0),RelAbsVector(60.0,70.0));
    fail_unless( P->hasRequiredAttributes() );
}
END_TEST

START_TEST ( test_RenderCubicBezier_hasRequiredElements )
{
    // has required elements should awlays be true
    fail_unless(P->hasRequiredElements() );
}
END_TEST

START_TEST ( test_RenderCubicBezier_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderCubicBezier\" x=\"10\" y=\"20\"\n"
                  " basePoint1_x=\"15%\" basePoint1_y=\"25%\"\n"
                  " basePoint2_x=\"35%\" basePoint2_y=\"55\" />\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  RenderCubicBezier p(*pNode);
  fail_unless(fabs((p.x().getAbsoluteValue() - 10.0) / 10.0) < 1e-9 );
  fail_unless(p.x().getRelativeValue() < 1e-9);
  fail_unless(fabs((p.y().getAbsoluteValue() - 20.0) / 20.0) < 1e-9 );
  fail_unless(p.y().getRelativeValue() < 1e-9);
  fail_unless(p.z().getAbsoluteValue() < 1e-9);
  fail_unless(p.z().getRelativeValue() < 1e-9);
  // base point 1
  fail_unless(p.basePoint1_X().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint1_X().getRelativeValue() - 15.0) / 15.0) < 1e-9 );
  fail_unless(p.basePoint1_Y().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint1_Y().getRelativeValue() - 25.0) / 25.0) < 1e-9 );
  fail_unless(p.basePoint1_Z().getAbsoluteValue() < 1e-9);
  fail_unless(p.basePoint1_Z().getRelativeValue() < 1e-9);
  // base point 2
  fail_unless(p.basePoint2_X().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint2_X().getRelativeValue() - 35.0) / 35.0) < 1e-9 );
  fail_unless(fabs((p.basePoint2_Y().getAbsoluteValue() - 55.0) / 55.0) < 1e-9 );
  fail_unless(p.basePoint2_Y().getRelativeValue() < 1e-9);
  fail_unless(p.basePoint2_Z().getAbsoluteValue() < 1e-9);
  fail_unless(p.basePoint2_Z().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderPoint\" x=\"30%\" y=\"50%\" z=\"3\"\n"
      " basePoint1_x=\"15%\" basePoint1_y=\"25%\" basePoint1_z=\"23%\"\n"
      " basePoint2_x=\"35%\" basePoint2_y=\"55\" basePoint2_z=\"13\"/>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  p = RenderCubicBezier(*pNode);
  fail_unless(fabs((p.x().getRelativeValue() - 30.0) / 30.0) < 1e-9 );
  fail_unless(p.x().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.y().getRelativeValue() - 50.0) / 50.0) < 1e-9 );
  fail_unless(p.y().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.z().getAbsoluteValue() - 3.0) / 3.0) < 1e-9 );
  fail_unless(p.z().getRelativeValue() < 1e-9);
  // base point 1
  fail_unless(p.basePoint1_X().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint1_X().getRelativeValue() - 15.0) / 15.0) < 1e-9 );
  fail_unless(p.basePoint1_Y().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint1_Y().getRelativeValue() - 25.0) / 25.0) < 1e-9 );
  fail_unless(p.basePoint1_Z().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint1_Z().getRelativeValue() - 23.0) / 23.0) < 1e-9);
  // base point 2
  fail_unless(p.basePoint2_X().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((p.basePoint2_X().getRelativeValue() - 35.0) / 35.0) < 1e-9 );
  fail_unless(fabs((p.basePoint2_Y().getAbsoluteValue() - 55.0) / 55.0) < 1e-9 );
  fail_unless(p.basePoint2_Y().getRelativeValue() < 1e-9);
  fail_unless(fabs((p.basePoint2_Z().getAbsoluteValue() - 13.0) / 13.0) < 1e-9);
  fail_unless(p.basePoint2_Z().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_RenderCubicBezier_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderCubicBezier\" x=\"10\" y=\"20\"\n"
                  " basePoint1_x=\"15%\" basePoint1_y=\"25%\"\n"
                  " basePoint2_x=\"35%\" basePoint2_y=\"55\" />\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  RenderCubicBezier* pCB = new RenderCubicBezier(*pNode1);
  fail_unless(pCB != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pCB->toXML("element"));
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pCB;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<element xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"RenderCubicBezier\" x=\"30%\" y=\"50%\" z=\"3\"\n"
      " basePoint1_x=\"15%\" basePoint1_y=\"25%\" basePoint1_z=\"23%\"\n"
      " basePoint2_x=\"35%\" basePoint2_y=\"55\" basePoint2_z=\"13\"/>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pCB = new RenderCubicBezier(*pNode1);
  fail_unless(pCB != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pCB->toXML("element"));
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pCB;

}
END_TEST

Suite *
create_suite_RenderCubicBezier (void)
{
  Suite *suite = suite_create("RenderCubicBezier");
  TCase *tcase = tcase_create("RenderCubicBezier");


  tcase_add_checked_fixture( tcase,
                             RenderCubicBezierTest_setup,
                             RenderCubicBezierTest_teardown );

  tcase_add_test( tcase, test_RenderCubicBezier_setBasePoint1         );
  tcase_add_test( tcase, test_RenderCubicBezier_setBasePoint2         );
  tcase_add_test( tcase, test_RenderCubicBezier_hasRequiredAttributes );
  tcase_add_test( tcase, test_RenderCubicBezier_hasRequiredElements   );
  tcase_add_test( tcase, test_RenderCubicBezier_read                  );
  tcase_add_test( tcase, test_RenderCubicBezier_write                 );


  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
