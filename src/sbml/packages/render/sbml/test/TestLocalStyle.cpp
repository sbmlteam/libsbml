//
// Filename    : TestLocalStyle.cpp
// Description : Tests for the LocalStyle class
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
#include <LocalStyle.h>
#include <Ellipse.h>
#include <Rectangle.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static LocalStyle *S;
static RenderPkgNamespaces *renderns;

void
LocalStyleTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    S = new (std::nothrow) LocalStyle(renderns);

    if (S == NULL)
    {
        fail("new(std::nothrow)LocalStyle(renderns) returned a NULL pointer.");
    }
}

void 
LocalStyleTest_teardown (void)
{
    delete S;
    delete renderns;
}

START_TEST ( test_LocalStyle_rolelist )
{
    fail_unless ( S->getNumRoles() == 0 );
    S->addRole("ROLE_1");
    fail_unless ( S->getNumRoles() == 1 );
    S->addRole("ROLE_2");
    fail_unless ( S->getNumRoles() == 2 );
    S->addRole("ROLE_3");
    fail_unless ( S->getNumRoles() == 3 );
    S->addRole("ROLE_4");
    fail_unless ( S->getNumRoles() == 4 );
    S->removeRole("ROLE_3");
    fail_unless ( S->getNumRoles() == 3 );
    fail_unless( S->isInRoleList("ROLE_1"));
    fail_unless( S->isInRoleList("ROLE_2"));
    fail_unless( S->isInRoleList("ROLE_4"));
    std::set<std::string> s;
    s.insert("role_a");
    s.insert("role_b");
    s.insert("role_c");
    s.insert("role_d");
    S->setRoleList(s);
    fail_unless ( S->getNumRoles() == 4 );
    fail_unless( S->isInRoleList("role_a"));
    fail_unless( S->isInRoleList("role_b"));
    fail_unless( S->isInRoleList("role_c"));
    fail_unless( S->isInRoleList("role_d"));
}
END_TEST

START_TEST ( test_LocalStyle_idlist )
{
    fail_unless ( S->getNumIds() == 0 );
    S->addId("ID_1");
    fail_unless ( S->getNumIds() == 1 );
    S->addId("ID_2");
    fail_unless ( S->getNumIds() == 2 );
    S->addId("ID_3");
    fail_unless ( S->getNumIds() == 3 );
    S->addId("ID_4");
    fail_unless ( S->getNumIds() == 4 );
    S->removeId("ID_3");
    fail_unless ( S->getNumIds() == 3 );
    fail_unless( S->isInIdList("ID_1"));
    fail_unless( S->isInIdList("ID_2"));
    fail_unless( S->isInIdList("ID_4"));
    std::set<std::string> s;
    s.insert("id_a");
    s.insert("id_b");
    s.insert("id_c");
    s.insert("id_d");
    S->setIdList(s);
    fail_unless ( S->getNumIds() == 4 );
    fail_unless( S->isInIdList("id_a"));
    fail_unless( S->isInIdList("id_b"));
    fail_unless( S->isInIdList("id_c"));
    fail_unless( S->isInIdList("id_d"));
}
END_TEST

START_TEST ( test_LocalStyle_read )
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
  LocalStyle l(*pNode);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "test_style");
  fail_unless(l.getIdList().empty());
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
      "<style id=\"test_style\" idList=\"object_1 object_2\"\n"
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
  l = LocalStyle(*pNode);
  fail_unless(l.isSetId());
  fail_unless(l.getId() == "test_style");
  const std::set<std::string>& idList=l.getIdList();
  fail_unless(idList.size() == 2);
  fail_unless(idList.find("object_1") != idList.end());
  fail_unless(idList.find("object_2") != idList.end());
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

START_TEST ( test_LocalStyle_write )
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
  LocalStyle* pLS = new LocalStyle(*pNode1);
  fail_unless(pLS != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pLS->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLS;

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
  pLS = new LocalStyle(*pNode1);
  fail_unless(pLS != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pLS->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pLS;

}
END_TEST



Suite *
create_suite_LocalStyle (void)
{
  Suite *suite = suite_create("LocalStyle");
  TCase *tcase = tcase_create("LocalStyle");


  tcase_add_checked_fixture( tcase,
                             LocalStyleTest_setup,
                             LocalStyleTest_teardown );

  tcase_add_test( tcase, test_LocalStyle_rolelist );
  tcase_add_test( tcase, test_LocalStyle_idlist   );
  tcase_add_test( tcase, test_LocalStyle_read     );
  tcase_add_test( tcase, test_LocalStyle_write    );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
