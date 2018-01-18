//
// Filename    : TestLinearGradient.cpp
// Description : Tests for the LinearGradient class
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

#include <LinearGradient.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static LinearGradient *G;
static RenderPkgNamespaces *renderns;

void
LinearGradientTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    G = new (std::nothrow) LinearGradient(renderns);

    if (G == NULL)
    {
        fail("new(std::nothrow)LinearGradient(renderns) returned a NULL pointer.");
    }

}

void 
LinearGradientTest_teardown (void)
{
    delete G;
    delete renderns;
}

START_TEST (test_LinearGradient_setCoordinates )
{
    fail_unless(G->getXPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getXPoint1().getRelativeValue() < 1e-9);
    fail_unless(G->getYPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getYPoint1().getRelativeValue() < 1e-9);
    fail_unless(G->getZPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getZPoint1().getRelativeValue() < 1e-9);
    fail_unless(G->getXPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
    fail_unless(G->getYPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
    fail_unless(G->getZPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
    G->setCoordinates(RelAbsVector(1.1,2.2),RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6)
                     ,RelAbsVector(7.7,8.8),RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12)
                     );
    fail_unless(fabs((G->getXPoint1().getAbsoluteValue() -1.1) / 1.1) < 1e-9);
    fail_unless(fabs((G->getXPoint1().getRelativeValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getZPoint1().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getZPoint1().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getAbsoluteValue() -7.7) / 7.7) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() -8.8) / 8.8) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getAbsoluteValue() -9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() -10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getZPoint2().getAbsoluteValue() -11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getZPoint2().getRelativeValue() -12.12) / 12.12) < 1e-9);
    G->setCoordinates(RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6)
                     ,RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12)
                     );
    fail_unless(fabs((G->getXPoint1().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getXPoint1().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(G->getZPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getZPoint1().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getXPoint2().getAbsoluteValue() -9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() -10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getAbsoluteValue() -11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() -12.12) / 12.12) < 1e-9);
    fail_unless(G->getZPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
    G->setPoint1(RelAbsVector(1.1,2.2),RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6));
    fail_unless(fabs((G->getXPoint1().getAbsoluteValue() -1.1) / 1.1) < 1e-9);
    fail_unless(fabs((G->getXPoint1().getRelativeValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getZPoint1().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getZPoint1().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getAbsoluteValue() -9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() -10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getAbsoluteValue() -11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() -12.12) / 12.12) < 1e-9);
    fail_unless(G->getZPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
    G->setPoint1(RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6));
    fail_unless(fabs((G->getXPoint1().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getXPoint1().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(G->getZPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getZPoint1().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getXPoint2().getAbsoluteValue() -9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() -10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getAbsoluteValue() -11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() -12.12) / 12.12) < 1e-9);
    fail_unless(G->getZPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
    G->setPoint2(RelAbsVector(1.1,2.2),RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6));
    fail_unless(fabs((G->getXPoint1().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getXPoint1().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(G->getZPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getZPoint1().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getXPoint2().getAbsoluteValue() -1.1) / 1.1) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getZPoint2().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getZPoint2().getRelativeValue() -6.6) / 6.6) < 1e-9);
    G->setPoint2(RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12));
    fail_unless(fabs((G->getXPoint1().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getXPoint1().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getYPoint1().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(G->getZPoint1().getAbsoluteValue() < 1e-9);
    fail_unless(G->getZPoint1().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getXPoint2().getAbsoluteValue() -9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getXPoint2().getRelativeValue() -10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getAbsoluteValue() -11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getYPoint2().getRelativeValue() -12.12) / 12.12) < 1e-9);
    fail_unless(G->getZPoint2().getAbsoluteValue() < 1e-9);
    fail_unless(G->getZPoint2().getRelativeValue() < 1e-9);
}
END_TEST 

START_TEST ( test_LinearGradient_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<linearGradient id=\"gradient\">\n"
                  "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
                  "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
                  "</linearGradient>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  LinearGradient l(*pNode);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "gradient");
  fail_unless(l.getSpreadMethod() == GradientBase::PAD);
  fail_unless(l.getNumGradientStops() == 2);
  fail_unless(l.getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(l.getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(l.getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(l.getGradientStop(1)->getStopColor() == "#0000FF");
  // linear gradient attributes 
  fail_unless(l.getXPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(l.getXPoint1().getRelativeValue() < 1e-9);
  fail_unless(l.getYPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(l.getYPoint1().getRelativeValue() < 1e-9);
  fail_unless(l.getZPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(l.getZPoint1().getRelativeValue() < 1e-9);
  fail_unless(l.getXPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getXPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  fail_unless(l.getYPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getYPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  fail_unless(l.getZPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<linearGradient id=\"gradient\" spreadMethod=\"reflect\"\n"
      "                x1=\"3.0\" y1=\"4.0\" z1=\"5.0\"\n"
      "                x2=\"6.0\" y2=\"7.0\" z2=\"8.0\">\n"
      "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
      "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
      "</linearGradient>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  l = LinearGradient(*pNode);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "gradient");
  fail_unless(l.getSpreadMethod() == GradientBase::REFLECT);
  fail_unless(l.getNumGradientStops() == 2);
  fail_unless(l.getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(l.getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(l.getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((l.getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(l.getGradientStop(1)->getStopColor() == "#0000FF");
  // linear gradient attributes 
  fail_unless(fabs((l.getXPoint1().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
  fail_unless(l.getXPoint1().getRelativeValue() < 1e-9);
  fail_unless(fabs((l.getYPoint1().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
  fail_unless(l.getYPoint1().getRelativeValue() < 1e-9);
  fail_unless(fabs((l.getZPoint1().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(l.getZPoint1().getRelativeValue() < 1e-9);
  fail_unless(fabs((l.getXPoint2().getAbsoluteValue() - 6.0) / 6.0) < 1e-9);
  fail_unless(l.getXPoint2().getRelativeValue() < 1e-9);
  fail_unless(fabs((l.getYPoint2().getAbsoluteValue() - 7.0) / 7.0) < 1e-9);
  fail_unless(l.getYPoint2().getRelativeValue() < 1e-9);
  fail_unless(fabs((l.getZPoint2().getAbsoluteValue() - 8.0) / 8.0) < 1e-9);
  fail_unless(l.getZPoint2().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_LinearGradient_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<linearGradient id=\"gradient\">\n"
                  "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
                  "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
                  "</linearGradient>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  LinearGradient* pLG = new LinearGradient(*pNode1);
  fail_unless(pLG != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pLG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<linearGradient id=\"gradient\" spreadMethod=\"reflect\"\n"
      "                x1=\"3\" y1=\"4\" z1=\"5\"\n"
      "                x2=\"6\" y2=\"7\" z2=\"8\">\n"
      "  <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
      "  <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
      "</linearGradient>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pLG = new LinearGradient(*pNode1);
  fail_unless(pLG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pLG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<render:linearGradient xmlns:render='http://projects.eml.org/bcb/sbml/render/level2' render:id=\"gradient\" render:spreadMethod=\"reflect\"\n"
      "                render:x1=\"3\" render:y1=\"4\" render:z1=\"5\"\n"
      "                render:x2=\"6\" render:y2=\"7\" render:z2=\"8\">\n"
      "  <render:stop render:offset=\"20%\" render:stop-color=\"#ff0000\"/>\n"
      "  <render:stop render:offset=\"80%\" render:stop-color=\"#0000ff\"/>\n"
      "</render:linearGradient>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pLG = new LinearGradient(*pNode1);
  fail_unless(pLG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pLG->toXML());
  fail_unless(pNode1 != NULL);
  s = pNode2->toXMLString();
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLG;




}
END_TEST

Suite *
create_suite_LinearGradient (void)
{
  Suite *suite = suite_create("LinearGradient");
  TCase *tcase = tcase_create("LinearGradient");


  tcase_add_checked_fixture( tcase,
                             LinearGradientTest_setup,
                             LinearGradientTest_teardown );

  tcase_add_test( tcase, test_LinearGradient_setCoordinates );
  tcase_add_test( tcase, test_LinearGradient_read           );
  tcase_add_test( tcase, test_LinearGradient_write          );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
