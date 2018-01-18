//
// Filename    : TestLineEnding
// Description : Tests for the LineEnding class
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
#include <sbml/packages/render/common/RenderExtensionTypes.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static LineEnding *L;
static RenderPkgNamespaces *renderns;
static LayoutPkgNamespaces *layoutns;

void
LineEndingTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
  layoutns = new (std::nothrow) LayoutPkgNamespaces();
    L = new (std::nothrow) LineEnding(renderns);

    if (L == NULL)
    {
        fail("new(std::nothrow)LineEnding(renderns) returned a NULL pointer.");
    }

}

void 
LineEndingTest_teardown (void)
{
    delete L;
    delete renderns;
    delete layoutns;
}

START_TEST (test_LineEnding_id )
{
    fail_unless(!L->isSetId());
    fail_unless(L->getId() == "");
    L->setId("SimpleArrow");
    fail_unless(L->isSetId());
    fail_unless(L->getId() == "SimpleArrow");
    L->unsetId();
    fail_unless(!L->isSetId());
    fail_unless(L->getId() == "");
}
END_TEST 

START_TEST ( test_LineEnding_boundingBox )
{
    BoundingBox* pB=L->getBoundingBox();
    fail_unless( pB != NULL );
    fail_unless( pB->getPosition()->x() < 1e-9);
    fail_unless( pB->getPosition()->y() < 1e-9);
    fail_unless( pB->getDimensions()->getWidth() < 1e-9);
    fail_unless( pB->getDimensions()->getHeight() < 1e-9);
    pB=new BoundingBox(layoutns);
    fail_unless( pB != NULL );
    Point p(layoutns);
    p.setOffsets(4.0,5.0,6.0);
    Dimensions d(layoutns);
    d.setBounds(7.0,8.0);
    pB->setPosition(&p);
    pB->setDimensions(&d);
    L->setBoundingBox(pB);
    fail_unless( L->getBoundingBox() != pB );
    delete pB;
    fail_unless(fabs((L->getBoundingBox()->getPosition()->x() - 4.0) / 4.0) < 1e-9 );
    fail_unless(fabs((L->getBoundingBox()->getPosition()->y() - 5.0) / 5.0) < 1e-9 );
    fail_unless(fabs((L->getBoundingBox()->getPosition()->z() - 6.0) / 6.0) < 1e-9 );
    fail_unless(fabs((L->getBoundingBox()->getDimensions()->getWidth() - 7.0) / 7.0) < 1e-9 );
    fail_unless(fabs((L->getBoundingBox()->getDimensions()->getHeight() - 8.0) / 8.0) < 1e-9 );
    fail_unless(L->getBoundingBox()->getDimensions()->getDepth() < 1e-9);
}
END_TEST

START_TEST ( test_LineEnding_rotationalMapping )
{
    fail_unless( L->getIsEnabledRotationalMapping() == true );
    L->setEnableRotationalMapping(false);
    fail_unless( L->getIsEnabledRotationalMapping() == false );
    L->setEnableRotationalMapping(true);
    fail_unless( L->getIsEnabledRotationalMapping() == true );
}
END_TEST

START_TEST ( test_LineEnding_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<lineEnding id=\"ending\">\n"
                  "  <boundingBox>\n"
                  "    <position x=\"-10\" y=\"-8\"/>\n"
                  "    <dimensions width=\"10\" height=\"20\"/>\n"
                  "  </boundingBox>\n"
                  "  <g>\n"
                  "    <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
                  "    <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
                  "  </g>\n"
                  "</lineEnding>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  LineEnding l(*pNode, 4);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "ending");
  fail_unless(l.getIsEnabledRotationalMapping() == true );
  fail_unless(l.getBoundingBox()->getPosition()->x() == -10);
  fail_unless(l.getBoundingBox()->getPosition()->y() == -8);
  fail_unless(l.getBoundingBox()->getDimensions()->width() == 10);
  fail_unless(l.getBoundingBox()->getDimensions()->height() == 20);
  const RenderGroup* g=l.getGroup();
  fail_unless(g != NULL);
  fail_unless(!g->isSetMatrix());
  fail_unless(!g->isSetStroke());
  fail_unless(!g->isSetStrokeWidth());
  fail_unless(!g->isSetDashArray());
  fail_unless(!g->isSetFillColor());
  fail_unless(!g->isSetFillRule());
  fail_unless(!g->isSetFontFamily());
  fail_unless(g->getFontFamily() == "");
  fail_unless(!g->isSetFontSize());
  fail_unless(!g->isSetFontWeight());
  fail_unless(!g->isSetFontStyle());
  fail_unless(!g->isSetTextAnchor());
  fail_unless(!g->isSetVTextAnchor());
  fail_unless(!g->isSetStartHead());
  fail_unless(!g->isSetEndHead());
  fail_unless(g->getNumElements() == 2);
  const Rectangle* r=dynamic_cast<const Rectangle*>(g->getElement(0));
  fail_unless( r != NULL );
  if (r == NULL) return;
  fail_unless(!r->isSetMatrix());
  fail_unless(!r->isSetStroke());
  fail_unless(!r->isSetStrokeWidth());
  fail_unless(!r->isSetDashArray());
  fail_unless(!r->isSetFillColor());
  fail_unless(!r->isSetFillRule());
  fail_unless(fabs((r->getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r->getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r->getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r->getY().getRelativeValue()  < 1e-9);
  fail_unless(r->getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(r->getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r->getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r->getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r->getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r->getHeight().getRelativeValue()  < 1e-9);
  fail_unless(r->getRadiusX().getAbsoluteValue()  < 1e-9);
  fail_unless(r->getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(r->getRadiusY().getAbsoluteValue()  < 1e-9);
  fail_unless(r->getRadiusY().getRelativeValue()  < 1e-9);
  const Ellipse* e=dynamic_cast<const Ellipse*>(g->getElement(1));
  fail_unless( e != NULL );
  fail_unless(!e->isSetMatrix());
  fail_unless(!e->isSetStroke());
  fail_unless(!e->isSetStrokeWidth());
  fail_unless(!e->isSetDashArray());
  fail_unless(!e->isSetFillColor());
  fail_unless(!e->isSetFillRule());
  fail_unless(fabs((e->getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e->getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e->getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e->getCY().getRelativeValue()  < 1e-9);
  fail_unless(e->getCZ().getAbsoluteValue()  < 1e-9);
  fail_unless(e->getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e->getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e->getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e->getRY().getAbsoluteValue() - e->getRX().getAbsoluteValue()) / e->getRX().getAbsoluteValue()) < 1e-9);
  fail_unless(e->getRY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<lineEnding id=\"ending\" enableRotationalMapping=\"false\">\n"
      "  <boundingBox>\n"
      "    <position x=\"-10\" y=\"-8\"/>\n"
      "    <dimensions width=\"10\" height=\"20\"/>\n"
      "  </boundingBox>\n"
      "  <g>\n"
      "    <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "    <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "  </g>\n"
      "</lineEnding>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // required attributes
  l = LineEnding(*pNode, 4);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "ending");
  fail_unless(l.getIsEnabledRotationalMapping() == false );
  fail_unless(l.getBoundingBox()->getPosition()->x() == -10);
  fail_unless(l.getBoundingBox()->getPosition()->y() == -8);
  fail_unless(l.getBoundingBox()->getDimensions()->width() == 10);
  fail_unless(l.getBoundingBox()->getDimensions()->height() == 20);
  g=l.getGroup();
  fail_unless(g != NULL);
  fail_unless(!g->isSetMatrix());
  fail_unless(!g->isSetStroke());
  fail_unless(!g->isSetStrokeWidth());
  fail_unless(!g->isSetDashArray());
  fail_unless(!g->isSetFillColor());
  fail_unless(!g->isSetFillRule());
  fail_unless(!g->isSetFontFamily());
  fail_unless(g->getFontFamily() == "");
  fail_unless(!g->isSetFontSize());
  fail_unless(!g->isSetFontWeight());
  fail_unless(!g->isSetFontStyle());
  fail_unless(!g->isSetTextAnchor());
  fail_unless(!g->isSetVTextAnchor());
  fail_unless(!g->isSetStartHead());
  fail_unless(!g->isSetEndHead());
  fail_unless(g->getNumElements() == 2);
  r=dynamic_cast<const Rectangle*>(g->getElement(0));
  fail_unless( r != NULL );
  fail_unless(!r->isSetMatrix());
  fail_unless(!r->isSetStroke());
  fail_unless(!r->isSetStrokeWidth());
  fail_unless(!r->isSetDashArray());
  fail_unless(!r->isSetFillColor());
  fail_unless(!r->isSetFillRule());
  fail_unless(fabs((r->getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r->getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r->getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r->getY().getRelativeValue()  < 1e-9);
  fail_unless(r->getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(r->getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r->getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r->getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r->getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r->getHeight().getRelativeValue()  < 1e-9);
  fail_unless(r->getRadiusX().getAbsoluteValue()  < 1e-9);
  fail_unless(r->getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(r->getRadiusY().getAbsoluteValue()  < 1e-9);
  fail_unless(r->getRadiusY().getRelativeValue()  < 1e-9);
  e=dynamic_cast<const Ellipse*>(g->getElement(1));
  fail_unless( e != NULL );
  fail_unless(!e->isSetMatrix());
  fail_unless(!e->isSetStroke());
  fail_unless(!e->isSetStrokeWidth());
  fail_unless(!e->isSetDashArray());
  fail_unless(!e->isSetFillColor());
  fail_unless(!e->isSetFillRule());
  fail_unless(fabs((e->getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e->getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e->getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e->getCY().getRelativeValue()  < 1e-9);
  fail_unless(e->getCZ().getAbsoluteValue()  < 1e-9);
  fail_unless(e->getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e->getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e->getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e->getRY().getAbsoluteValue() - e->getRX().getAbsoluteValue()) / e->getRX().getAbsoluteValue()) < 1e-9);
  fail_unless(e->getRY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_LineEnding_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<lineEnding id=\"ending\">\n"
                  "  <boundingBox>\n"
                  "    <position x=\"-10\" y=\"-8\"/>\n"
                  "    <dimensions width=\"10\" height=\"20\"/>\n"
                  "  </boundingBox>\n"
                  "  <g>\n"
                  "    <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
                  "    <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
                  "  </g>\n"
                  "</lineEnding>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  LineEnding* pLE = new LineEnding(*pNode1, 4);
  fail_unless(pLE != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pLE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLE;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<lineEnding id=\"ending\" enableRotationalMapping=\"false\">\n"
      "  <boundingBox>\n"
      "    <position x=\"-10\" y=\"-8\"/>\n"
      "    <dimensions width=\"10\" height=\"20\"/>\n"
      "  </boundingBox>\n"
      "  <g>\n"
      "    <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "    <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "  </g>\n"
      "</lineEnding>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pLE = new LineEnding(*pNode1, 4);
  fail_unless(pLE != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pLE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLE;

}
END_TEST

Suite *
create_suite_LineEnding (void)
{
  Suite *suite = suite_create("LineEnding");
  TCase *tcase = tcase_create("LineEnding");


  tcase_add_checked_fixture( tcase,
                             LineEndingTest_setup,
                             LineEndingTest_teardown );

  tcase_add_test( tcase, test_LineEnding_id                );
  tcase_add_test( tcase, test_LineEnding_boundingBox       );
  tcase_add_test( tcase, test_LineEnding_rotationalMapping );
  tcase_add_test( tcase, test_LineEnding_read              );
  tcase_add_test( tcase, test_LineEnding_write             );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
