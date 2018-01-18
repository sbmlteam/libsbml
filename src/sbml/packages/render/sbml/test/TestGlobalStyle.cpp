//
// Filename    : TestGlobalStyle.cpp
// Description : Tests for the GlobalStyle class
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

#include <Style.h>
#include <GlobalStyle.h>
#include <Ellipse.h>
#include <Rectangle.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GlobalStyle *S;
static RenderPkgNamespaces *renderns;

void
GlobalStyleTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    S = new (std::nothrow) GlobalStyle(renderns);

    if (S == NULL)
    {
        fail("new(std::nothrow)GlobalStyle(renderns) returned a NULL pointer.");
    }
}

void 
GlobalStyleTest_teardown (void)
{
    delete S;
    delete renderns;
}

START_TEST ( test_GlobalStyle_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<style id=\"test_style\">\n"
                  "  <g>\n"
                  "    <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
                  "    <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
                  "  </g>\n"
                  "</style>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  GlobalStyle l(*pNode);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "test_style");
  fail_unless(l.getRoleList().empty());
  fail_unless(l.getTypeList().empty());
  const RenderGroup* g=l.getGroup();
  fail_unless(g != NULL);
  fail_unless(!g->isSetMatrix());
  fail_unless(!g->isSetStroke());
  fail_unless(g->isSetStrokeWidth());
  fail_unless(g->getStrokeWidth() < 1e-9);
  fail_unless(!g->isSetDashArray());
  fail_unless(!g->isSetFillColor());
  fail_unless(g->isSetFillRule());
  fail_unless(g->getFillRule() == GraphicalPrimitive2D::NONZERO);
  fail_unless(g->isSetFontFamily());
  fail_unless(g->getFontFamily() == "sans-serif");
  fail_unless(g->isSetFontSize());
  fail_unless(g->getFontSize().getAbsoluteValue() < 1e-9);
  fail_unless(g->getFontSize().getRelativeValue() < 1e-9);
  fail_unless(g->isSetFontWeight());
  fail_unless(g->getFontWeight() == Text::WEIGHT_NORMAL);
  fail_unless(g->isSetFontStyle());
  fail_unless(g->getFontStyle() == Text::STYLE_NORMAL);
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
      "<style id=\"test_style\"\n"
      "       roleList=\"role_3 role_4\" typeList=\"type_a type_b\">\n"
      "  <g>\n"
      "    <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "    <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "  </g>\n"
      "</style>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  l = GlobalStyle(*pNode);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "test_style");
  const std::set<std::string>& roleList=l.getRoleList();
  fail_unless(roleList.size() == 2);
  fail_unless(roleList.find("role_3") != roleList.end());
  fail_unless(roleList.find("role_4") != roleList.end());
  const std::set<std::string>& typeList=l.getTypeList();
  fail_unless(typeList.size() == 2);
  fail_unless(typeList.find("type_a") != typeList.end());
  fail_unless(typeList.find("type_b") != typeList.end());
  g=l.getGroup();
  fail_unless(g != NULL);
  fail_unless(!g->isSetMatrix());
  fail_unless(!g->isSetStroke());
  fail_unless(g->isSetStrokeWidth());
  fail_unless(g->getStrokeWidth() < 1e-9);
  fail_unless(!g->isSetDashArray());
  fail_unless(!g->isSetFillColor());
  fail_unless(g->isSetFillRule());
  fail_unless(g->getFillRule() == GraphicalPrimitive2D::NONZERO);
  fail_unless(g->isSetFontFamily());
  fail_unless(g->getFontFamily() == "sans-serif");
  fail_unless(g->isSetFontSize());
  fail_unless(g->getFontSize().getAbsoluteValue() < 1e-9);
  fail_unless(g->getFontSize().getRelativeValue() < 1e-9);
  fail_unless(g->isSetFontWeight());
  fail_unless(g->getFontWeight() == Text::WEIGHT_NORMAL);  
  fail_unless(g->isSetFontStyle());
  fail_unless(g->getFontStyle() == Text::STYLE_NORMAL);  
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

START_TEST ( test_GlobalStyle_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<style id=\"test_style\">\n"
                  "  <g stroke-width=\"0\" fill-rule=\"nonzero\"\n"
                  "     font-size=\"0\" font-family=\"sans-serif\"\n"
                  "     font-style=\"normal\" font-weight=\"normal\">\n"
                  "    <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
                  "    <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
                  "  </g>\n"
                  "</style>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  GlobalStyle* pGS = new GlobalStyle(*pNode1);
  fail_unless(pGS != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pGS->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pGS;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<style id=\"test_style\"\n"
      "       roleList=\"role_3 role_4\" typeList=\"type_a type_b\">\n"
      "  <g stroke-width=\"0\" fill-rule=\"nonzero\"\n"
      "     font-size=\"0\" font-family=\"sans-serif\"\n"
      "     font-style=\"normal\" font-weight=\"normal\">\n"
      "    <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "    <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "  </g>\n"
      "</style>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pGS = new GlobalStyle(*pNode1);
  fail_unless(pGS != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pGS->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pGS;

}
END_TEST

Suite *
create_suite_GlobalStyle (void)
{
  Suite *suite = suite_create("GlobalStyle");
  TCase *tcase = tcase_create("GlobalStyle");


  tcase_add_checked_fixture( tcase,
                             GlobalStyleTest_setup,
                             GlobalStyleTest_teardown );

  tcase_add_test( tcase, test_GlobalStyle_read   );
  tcase_add_test( tcase, test_GlobalStyle_write   );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
