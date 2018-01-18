//
// Filename    : TestLocalRenderInformation.cpp
// Description : Tests for the LocalRenderInformation class
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

#include <LocalRenderInformation.h>
#include <Ellipse.h>
#include <Rectangle.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static LocalRenderInformation *R;
static RenderPkgNamespaces *renderns;

void
LocalRenderInformationTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    R = new (std::nothrow) LocalRenderInformation(renderns);

    if (R == NULL)
    {
        fail("new(std::nothrow)LocalRenderInformation(renderns) returned a NULL pointer.");
    }

}

void 
LocalRenderInformationTest_teardown (void)
{
    delete R;
    delete renderns;
}

START_TEST ( test_LocalRenderInformation_createMethods )
{
    // create local style
    LocalStyle* pLS=R->createStyle("local_style_1");
    fail_unless ( pLS != NULL );
    fail_unless ( R->getNumStyles() == 1 );
    fail_unless( pLS == R->getStyle(0) );
}
END_TEST

START_TEST ( test_LocalRenderInformation_addMethods )
{
    fail_unless( R->getNumStyles() == 0 );
    // add local style 
    LocalStyle* pLS=new LocalStyle(renderns);
    fail_unless ( pLS != NULL );
    fail_unless ( R->addStyle( pLS ) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( R->getNumStyles() == 1 );
    fail_unless( pLS != R->getStyle(0));
    pLS->setId("some_style");
    fail_unless ( R->addStyle( pLS ) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( R->getNumStyles() == 2 );
    fail_unless( pLS != R->getStyle(1));
    delete pLS;
    pLS = new LocalStyle(2,1);
    fail_unless ( pLS != NULL );
    pLS->setId("some_other_style");
    fail_unless( R->addStyle( pLS ) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( R->getNumStyles() == 2 );
    delete pLS;
}
END_TEST

START_TEST( test_LocalRenderInformation_read )
{
   std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                   "<renderInformation id=\"test_render_info\" backgroundColor='#FFFFFFFF'>\n"
		   "  <listOfColorDefinitions>\n"
                   "    <colorDefinition id=\"red\" value=\"#C90000\" />\n"
                   "    <colorDefinition id=\"black\" value=\"#000000\" />\n"
                   "    <colorDefinition id=\"white\" value=\"#FFFFFF\" />\n"
		   "  </listOfColorDefinitions>\n"
		   "  <listOfGradientDefinitions>\n"
                   "    <linearGradient id=\"gradient_1\">\n"
                   "      <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
                   "      <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
                   "    </linearGradient>		   "
                   "    <radialGradient id=\"gradient_2\">\n"
                   "      <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
                   "      <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
                   "    </radialGradient>\n"
		   "  </listOfGradientDefinitions>\n"
		   "  <listOfLineEndings>\n"
                   "    <lineEnding id=\"ending_1\">\n"
                   "      <boundingBox>\n"
                   "        <position x=\"-10\" y=\"-8\"/>\n"
                   "        <dimensions width=\"10\" height=\"20\"/>\n"
                   "      </boundingBox>\n"
                   "      <g>\n"
                   "        <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
                   "      </g>\n"
                   "    </lineEnding>\n"
                   "    <lineEnding id=\"ending_2\">\n"
                   "      <boundingBox>\n"
                   "        <position x=\"-10\" y=\"-8\"/>\n"
                   "        <dimensions width=\"10\" height=\"20\"/>\n"
                   "      </boundingBox>\n"
                   "      <g>\n"
                   "        <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
                   "      </g>\n"
                   "    </lineEnding>\n"
		   "  </listOfLineEndings>\n"
		   "  <listOfStyles>\n"
                   "    <style id=\"test_style_1\" idList=\"object_1 object_2\"\n"
                   "           roleList=\"role_3 role_4\" typeList=\"type_a type_b\">\n"
                   "      <g>\n"
                   "        <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
                   "      </g>\n"
                   "    </style>\n"
                   "    <style id=\"test_style_2\" idList=\"object_5 object_6\">\n"
                   "      <g>\n"
                   "        <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
                   "      </g>\n"
                   "    </style>\n"
		   "  </listOfStyles>\n"
		   "</renderInformation>\n"
    ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  LocalRenderInformation lri(2,1);
  lri.parseXML(*pNode);
  fail_unless(lri.getNumColorDefinitions() == 3);
  const ColorDefinition* pCD = lri.getColorDefinition(0);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "red");
  fail_unless(pCD->getRed() == 201 );
  fail_unless(pCD->getGreen() == 0 );
  fail_unless(pCD->getBlue() == 0 );
  fail_unless(pCD->getAlpha() == 255 );
  pCD = lri.getColorDefinition(1);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "black");
  fail_unless(pCD->getRed() == 0 );
  fail_unless(pCD->getGreen() == 0 );
  fail_unless(pCD->getBlue() == 0 );
  fail_unless(pCD->getAlpha() == 255 );
  pCD = lri.getColorDefinition(2);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "white");
  fail_unless(pCD->getRed() == 255 );
  fail_unless(pCD->getGreen() == 255 );
  fail_unless(pCD->getBlue() == 255 );
  fail_unless(pCD->getAlpha() == 255 );
  // check gradients
  fail_unless(lri.getNumGradientDefinitions() == 2);
  const LinearGradient* lg=dynamic_cast<const LinearGradient*>(lri.getGradientDefinition(0));
  fail_unless( lg != NULL);
  if (lg == NULL) return;
  fail_unless(lg->isSetId());
  fail_unless(lg->getId() == "gradient_1");
  fail_unless(lg->getSpreadMethod() == GradientBase::PAD);
  fail_unless(lg->getNumGradientStops() == 2);
  fail_unless(lg->getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(lg->getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(lg->getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(lg->getGradientStop(1)->getStopColor() == "#0000FF");
  // linear gradient attributes 
  fail_unless(lg->getXPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(lg->getXPoint1().getRelativeValue() < 1e-9);
  fail_unless(lg->getYPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(lg->getYPoint1().getRelativeValue() < 1e-9);
  fail_unless(lg->getZPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(lg->getZPoint1().getRelativeValue() < 1e-9);
  fail_unless(lg->getXPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getXPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  fail_unless(lg->getYPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getYPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  fail_unless(lg->getZPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  const RadialGradient* rg=dynamic_cast<const RadialGradient*>(lri.getGradientDefinition(1));
  fail_unless(rg != NULL);
  fail_unless(rg->isSetId());
  fail_unless(rg->getId() == "gradient_2");
  fail_unless(rg->getSpreadMethod() == GradientBase::PAD);
  fail_unless(rg->getNumGradientStops() == 2);
  fail_unless(rg->getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(rg->getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(rg->getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(rg->getGradientStop(1)->getStopColor() == "#0000FF");
  // radial gradient attributes 
  fail_unless(rg->getCenterX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getCenterX().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(rg->getCenterY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getCenterY().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(rg->getCenterZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getCenterZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(rg->getFocalPointX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getFocalPointX().getRelativeValue() - rg->getCenterX().getRelativeValue()) / rg->getCenterX().getRelativeValue()) < 1e-9);
  fail_unless(rg->getFocalPointY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getFocalPointY().getRelativeValue() - rg->getCenterY().getRelativeValue()) / rg->getCenterY().getRelativeValue()) < 1e-9);
  fail_unless(rg->getFocalPointZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getFocalPointZ().getRelativeValue() - rg->getCenterZ().getRelativeValue()) / rg->getCenterZ().getRelativeValue()) < 1e-9);
  fail_unless(rg->getRadius().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getRadius().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  // line endings
  fail_unless(lri.getNumLineEndings() == 2);
  const LineEnding* le=lri.getLineEnding(0);
  fail_unless(le->isSetId());
  fail_unless(le->getId() == "ending_1");
  fail_unless(le->getIsEnabledRotationalMapping() == true );
  fail_unless(le->getBoundingBox()->getPosition()->x() == -10);
  fail_unless(le->getBoundingBox()->getPosition()->y() == -8);
  fail_unless(le->getBoundingBox()->getDimensions()->width() == 10);
  fail_unless(le->getBoundingBox()->getDimensions()->height() == 20);
  const RenderGroup* g=le->getGroup();
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
  fail_unless(g->getNumElements() == 1);
  const Rectangle* r=dynamic_cast<const Rectangle*>(g->getElement(0));
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
  le=lri.getLineEnding(1);
  fail_unless(le->isSetId());
  fail_unless(le->getId() == "ending_2");
  fail_unless(le->getIsEnabledRotationalMapping() == true );
  fail_unless(le->getBoundingBox()->getPosition()->x() == -10);
  fail_unless(le->getBoundingBox()->getPosition()->y() == -8);
  fail_unless(le->getBoundingBox()->getDimensions()->width() == 10);
  fail_unless(le->getBoundingBox()->getDimensions()->height() == 20);
  g=le->getGroup();
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
  fail_unless(g->getNumElements() == 1);
  const Ellipse* e = dynamic_cast<const Ellipse*>(g->getElement(0));
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
  // check styles
  fail_unless(lri.getNumStyles() == 2);
  const LocalStyle* ls=lri.getStyle(0);
  fail_unless(ls != NULL);
  fail_unless(ls->isSetId());
  fail_unless(ls->getId() == "test_style_1");
  const std::set<std::string>* idList=&ls->getIdList();
  fail_unless(idList->size() == 2);
  fail_unless(idList->find("object_1") != idList->end());
  fail_unless(idList->find("object_2") != idList->end());
  const std::set<std::string>* roleList=&ls->getRoleList();
  fail_unless(roleList->size() == 2);
  fail_unless(roleList->find("role_3") != roleList->end());
  fail_unless(roleList->find("role_4") != roleList->end());
  const std::set<std::string>* typeList=&ls->getTypeList();
  fail_unless(typeList->size() == 2);
  fail_unless(typeList->find("type_a") != typeList->end());
  fail_unless(typeList->find("type_b") != typeList->end());
  g=ls->getGroup();
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
  fail_unless(g->getNumElements() == 1);
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
  ls=lri.getStyle(1);
  fail_unless(ls != NULL);
  fail_unless(ls->isSetId());
  fail_unless(ls->getId() == "test_style_2");
  idList=&ls->getIdList();
  fail_unless(idList->size() == 2);
  fail_unless(idList->find("object_5") != idList->end());
  fail_unless(idList->find("object_6") != idList->end());
  fail_unless(ls->getRoleList().empty());
  fail_unless(ls->getTypeList().empty());
  g=ls->getGroup();
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
  fail_unless(g->getNumElements() == 1);
  e=dynamic_cast<const Ellipse*>(g->getElement(0));
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
  fail_unless(lri.isSetId());
  fail_unless(lri.getId() == "test_render_info");
  fail_unless(!lri.isSetName());
  fail_unless(lri.getName() == "");
  fail_unless(lri.getProgramName() == "");
  fail_unless(lri.getProgramVersion() == "");
  fail_unless(lri.getReferenceRenderInformationId() == "");
  fail_unless(lri.getBackgroundColor() == "#FFFFFFFF");

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<renderInformation id=\"test_render_info\" name=\"Some Render Info\"\n"
      "                   programName=\"SomeProgram\" programVersion=\"1.2.3\"\n"
      "                   backgroundColor=\"#FF5533\" referenceRenderInformation=\"test_render_info_2\">\n"
      "  <listOfColorDefinitions>\n"
      "    <colorDefinition id=\"red\" value=\"#C90000\" />\n"
      "    <colorDefinition id=\"black\" value=\"#000000\" />\n"
      "    <colorDefinition id=\"white\" value=\"#FFFFFF\" />\n"
      "  </listOfColorDefinitions>\n"
      "  <listOfGradientDefinitions>\n"
      "    <linearGradient id=\"gradient_1\">\n"
      "      <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
      "      <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
      "    </linearGradient>		   "
      "    <radialGradient id=\"gradient_2\">\n"
      "      <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
      "      <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
      "    </radialGradient>\n"
      "  </listOfGradientDefinitions>\n"
      "  <listOfLineEndings>\n"
      "    <lineEnding id=\"ending_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"-10\" y=\"-8\"/>\n"
      "        <dimensions width=\"10\" height=\"20\"/>\n"
      "      </boundingBox>\n"
      "      <g>\n"
      "        <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "      </g>\n"
      "    </lineEnding>\n"
      "    <lineEnding id=\"ending_2\">\n"
      "      <boundingBox>\n"
      "        <position x=\"-10\" y=\"-8\"/>\n"
      "        <dimensions width=\"10\" height=\"20\"/>\n"
      "      </boundingBox>\n"
      "      <g>\n"
      "        <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "      </g>\n"
      "    </lineEnding>\n"
      "  </listOfLineEndings>\n"
      "  <listOfStyles>\n"
      "    <style id=\"test_style_1\" idList=\"object_1 object_2\"\n"
      "           roleList=\"role_3 role_4\" typeList=\"type_a type_b\">\n"
      "      <g>\n"
      "        <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "      </g>\n"
      "    </style>\n"
      "    <style id=\"test_style_2\" idList=\"object_5 object_6\">\n"
      "      <g>\n"
      "        <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "      </g>\n"
      "    </style>\n"
      "  </listOfStyles>\n"
      "</renderInformation>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  lri = LocalRenderInformation (2,4);
  lri.parseXML(*pNode);
  fail_unless(lri.getNumColorDefinitions() == 3);
  pCD = lri.getColorDefinition(0);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "red");
  fail_unless(pCD->getRed() == 201 );
  fail_unless(pCD->getGreen() == 0 );
  fail_unless(pCD->getBlue() == 0 );
  fail_unless(pCD->getAlpha() == 255 );
  pCD = lri.getColorDefinition(1);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "black");
  fail_unless(pCD->getRed() == 0 );
  fail_unless(pCD->getGreen() == 0 );
  fail_unless(pCD->getBlue() == 0 );
  fail_unless(pCD->getAlpha() == 255 );
  pCD = lri.getColorDefinition(2);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "white");
  fail_unless(pCD->getRed() == 255 );
  fail_unless(pCD->getGreen() == 255 );
  fail_unless(pCD->getBlue() == 255 );
  fail_unless(pCD->getAlpha() == 255 );
  // check gradients
  fail_unless(lri.getNumGradientDefinitions() == 2);
  lg=dynamic_cast<const LinearGradient*>(lri.getGradientDefinition(0));
  fail_unless( lg != NULL);
  fail_unless(lg->isSetId());
  fail_unless(lg->getId() == "gradient_1");
  fail_unless(lg->getSpreadMethod() == GradientBase::PAD);
  fail_unless(lg->getNumGradientStops() == 2);
  fail_unless(lg->getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(lg->getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(lg->getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(lg->getGradientStop(1)->getStopColor() == "#0000FF");
  // linear gradient attributes 
  fail_unless(lg->getXPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(lg->getXPoint1().getRelativeValue() < 1e-9);
  fail_unless(lg->getYPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(lg->getYPoint1().getRelativeValue() < 1e-9);
  fail_unless(lg->getZPoint1().getAbsoluteValue() < 1e-9);
  fail_unless(lg->getZPoint1().getRelativeValue() < 1e-9);
  fail_unless(lg->getXPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getXPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  fail_unless(lg->getYPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getYPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  fail_unless(lg->getZPoint2().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((lg->getZPoint2().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  rg=dynamic_cast<const RadialGradient*>(lri.getGradientDefinition(1));
  fail_unless(rg != NULL);
  fail_unless(rg->isSetId());
  fail_unless(rg->getId() == "gradient_2");
  fail_unless(rg->getSpreadMethod() == GradientBase::PAD);
  fail_unless(rg->getNumGradientStops() == 2);
  fail_unless(rg->getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(rg->getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(rg->getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(rg->getGradientStop(1)->getStopColor() == "#0000FF");
  // radial gradient attributes 
  fail_unless(rg->getCenterX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getCenterX().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(rg->getCenterY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getCenterY().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(rg->getCenterZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getCenterZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(rg->getFocalPointX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getFocalPointX().getRelativeValue() - rg->getCenterX().getRelativeValue()) / rg->getCenterX().getRelativeValue()) < 1e-9);
  fail_unless(rg->getFocalPointY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getFocalPointY().getRelativeValue() - rg->getCenterY().getRelativeValue()) / rg->getCenterZ().getRelativeValue()) < 1e-9);
  fail_unless(rg->getFocalPointZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getFocalPointZ().getRelativeValue() - rg->getCenterY().getRelativeValue()) / rg->getCenterZ().getRelativeValue()) < 1e-9);
  fail_unless(rg->getRadius().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((rg->getRadius().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  // line endings
  fail_unless(lri.getNumLineEndings() == 2);
  le=lri.getLineEnding(0);
  fail_unless(le->isSetId());
  fail_unless(le->getId() == "ending_1");
  fail_unless(le->getIsEnabledRotationalMapping() == true );
  fail_unless(le->getBoundingBox()->getPosition()->x() == -10);
  fail_unless(le->getBoundingBox()->getPosition()->y() == -8);
  fail_unless(le->getBoundingBox()->getDimensions()->width() == 10);
  fail_unless(le->getBoundingBox()->getDimensions()->height() == 20);
  g=le->getGroup();
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
  fail_unless(g->getNumElements() == 1);
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
  le=lri.getLineEnding(1);
  fail_unless(le->isSetId());
  fail_unless(le->getId() == "ending_2");
  fail_unless(le->getIsEnabledRotationalMapping() == true );
  fail_unless(le->getBoundingBox()->getPosition()->x() == -10);
  fail_unless(le->getBoundingBox()->getPosition()->y() == -8);
  fail_unless(le->getBoundingBox()->getDimensions()->width() == 10);
  fail_unless(le->getBoundingBox()->getDimensions()->height() == 20);
  g=le->getGroup();
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
  fail_unless(g->getNumElements() == 1);
  e=dynamic_cast<const Ellipse*>(g->getElement(0));
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
  // check styles
  fail_unless(lri.getNumStyles() == 2);
  ls=lri.getStyle(0);
  fail_unless(ls != NULL);
  fail_unless(ls->isSetId());
  fail_unless(ls->getId() == "test_style_1");
  idList=&ls->getIdList();
  fail_unless(idList->size() == 2);
  fail_unless(idList->find("object_1") != idList->end());
  fail_unless(idList->find("object_2") != idList->end());
  roleList=&ls->getRoleList();
  fail_unless(roleList->size() == 2);
  fail_unless(roleList->find("role_3") != roleList->end());
  fail_unless(roleList->find("role_4") != roleList->end());
  typeList=&ls->getTypeList();
  fail_unless(typeList->size() == 2);
  fail_unless(typeList->find("type_a") != typeList->end());
  fail_unless(typeList->find("type_b") != typeList->end());
  g=ls->getGroup();
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
  fail_unless(g->getNumElements() == 1);
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
  ls=lri.getStyle(1);
  fail_unless(ls != NULL);
  fail_unless(ls->isSetId());
  fail_unless(ls->getId() == "test_style_2");
  idList=&ls->getIdList();
  fail_unless(idList->size() == 2);
  fail_unless(idList->find("object_5") != idList->end());
  fail_unless(idList->find("object_6") != idList->end());
  fail_unless(ls->getRoleList().empty());
  fail_unless(ls->getTypeList().empty());
  g=ls->getGroup();
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
  fail_unless(g->getNumElements() == 1);
  e=dynamic_cast<const Ellipse*>(g->getElement(0));
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
  fail_unless(lri.isSetId());
  fail_unless(lri.getId() == "test_render_info");
  fail_unless(lri.isSetName());
  fail_unless(lri.getName() == "Some Render Info");
  fail_unless(lri.getProgramName() == "SomeProgram");
  fail_unless(lri.getProgramVersion() == "1.2.3");
  fail_unless(lri.getReferenceRenderInformationId() == "test_render_info_2");
  fail_unless(lri.getBackgroundColor() == "#FF5533");

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_LocalRenderInformation_write )
{
   std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                   "<renderInformation id=\"test_render_info\" backgroundColor=\"#ffffffff\">\n"
		   "  <listOfColorDefinitions>\n"
                   "    <colorDefinition id=\"red\" value=\"#c90000\" />\n"
                   "    <colorDefinition id=\"black\" value=\"#000000\" />\n"
                   "    <colorDefinition id=\"white\" value=\"#ffffff\" />\n"
		   "  </listOfColorDefinitions>\n"
		   "  <listOfGradientDefinitions>\n"
                   "    <linearGradient id=\"gradient_1\">\n"
                   "      <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
                   "      <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
                   "    </linearGradient>		   "
                   "    <radialGradient id=\"gradient_2\">\n"
                   "      <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
                   "      <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
                   "    </radialGradient>\n"
		   "  </listOfGradientDefinitions>\n"
		   "  <listOfLineEndings>\n"
                   "    <lineEnding id=\"ending_1\">\n"
                   "      <boundingBox>\n"
                   "        <position x=\"-10\" y=\"-8\"/>\n"
                   "        <dimensions width=\"10\" height=\"20\"/>\n"
                   "      </boundingBox>\n"
                   "      <g>\n"
                   "        <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
                   "      </g>\n"
                   "    </lineEnding>\n"
                   "    <lineEnding id=\"ending_2\">\n"
                   "      <boundingBox>\n"
                   "        <position x=\"-10\" y=\"-8\"/>\n"
                   "        <dimensions width=\"10\" height=\"20\"/>\n"
                   "      </boundingBox>\n"
                   "      <g>\n"
                   "        <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
                   "      </g>\n"
                   "    </lineEnding>\n"
		   "  </listOfLineEndings>\n"
		   "  <listOfStyles>\n"
                   "    <style id=\"test_style_1\" idList=\"object_1 object_2\"\n"
                   "           roleList=\"role_3 role_4\" typeList=\"type_a type_b\">\n"
                   "      <g stroke-width=\"0\" fill-rule=\"nonzero\"\n"
                   "         font-size=\"0\" font-family=\"sans-serif\"\n"
                   "         font-style=\"normal\" font-weight=\"normal\">\n"
                   "        <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
                   "      </g>\n"
                   "    </style>\n"
                   "    <style id=\"test_style_2\" idList=\"object_5 object_6\">\n"
                   "      <g stroke-width=\"0\" fill-rule=\"nonzero\"\n"
                   "         font-size=\"0\" font-family=\"sans-serif\"\n"
                   "         font-style=\"normal\" font-weight=\"normal\">\n"
                   "        <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
                   "      </g>\n"
                   "    </style>\n"
		   "  </listOfStyles>\n"
		   "</renderInformation>\n"
    ;

   XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
   XMLNode* pNode1 = new XMLNode(*pStream);
   fail_unless(pNode1 != NULL);
   LocalRenderInformation* pLRI = new LocalRenderInformation(2,1);
   pLRI->parseXML(*pNode1);
   fail_unless(pLRI != NULL);
   // create the XMLNode from the object
   XMLNode* pNode2 = new XMLNode(pLRI->toXML());
   fail_unless(pNode1 != NULL);
   // compare the two nodes
   fail_unless(pNode1->equals(*pNode2, true));
   delete pNode1;
   delete pNode2;
   delete pStream;
   delete pLRI;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<renderInformation id=\"test_render_info\" name=\"Some Render Info\"\n"
      "                   programName=\"SomeProgram\" programVersion=\"1.2.3\"\n"
      "                   backgroundColor=\"#ff5533\" referenceRenderInformation=\"test_render_info_2\">\n"
      "  <listOfColorDefinitions>\n"
      "    <colorDefinition id=\"red\" value=\"#c90000\" />\n"
      "    <colorDefinition id=\"black\" value=\"#000000\" />\n"
      "    <colorDefinition id=\"white\" value=\"#ffffff\" />\n"
      "  </listOfColorDefinitions>\n"
      "  <listOfGradientDefinitions>\n"
      "    <linearGradient id=\"gradient_1\">\n"
      "      <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
      "      <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
      "    </linearGradient>		   "
      "    <radialGradient id=\"gradient_2\">\n"
      "      <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
      "      <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
      "    </radialGradient>\n"
      "  </listOfGradientDefinitions>\n"
      "  <listOfLineEndings>\n"
      "    <lineEnding id=\"ending_1\">\n"
      "      <boundingBox>\n"
      "        <position x=\"-10\" y=\"-8\"/>\n"
      "        <dimensions width=\"10\" height=\"20\"/>\n"
      "      </boundingBox>\n"
      "      <g>\n"
      "        <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "      </g>\n"
      "    </lineEnding>\n"
      "    <lineEnding id=\"ending_2\">\n"
      "      <boundingBox>\n"
      "        <position x=\"-10\" y=\"-8\"/>\n"
      "        <dimensions width=\"10\" height=\"20\"/>\n"
      "      </boundingBox>\n"
      "      <g>\n"
      "        <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "      </g>\n"
      "    </lineEnding>\n"
      "  </listOfLineEndings>\n"
      "  <listOfStyles>\n"
      "    <style id=\"test_style_1\" idList=\"object_1 object_2\"\n"
      "           roleList=\"role_3 role_4\" typeList=\"type_a type_b\">\n"
      "      <g stroke-width=\"0\" fill-rule=\"nonzero\"\n"
      "         font-size=\"0\" font-family=\"sans-serif\"\n"
      "         font-style=\"normal\" font-weight=\"normal\">\n"
      "        <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "      </g>\n"
      "    </style>\n"
      "    <style id=\"test_style_2\" idList=\"object_5 object_6\">\n"
      "      <g stroke-width=\"0\" fill-rule=\"nonzero\"\n"
      "         font-size=\"0\" font-family=\"sans-serif\"\n"
      "         font-style=\"normal\" font-weight=\"normal\">\n"
      "        <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "      </g>\n"
      "    </style>\n"
      "  </listOfStyles>\n"
      "</renderInformation>\n"
    ;

   pStream = new XMLInputStream(s.c_str(),false);
   pNode1 = new XMLNode(*pStream);
   fail_unless(pNode1 != NULL);
   pLRI = new LocalRenderInformation(2,3);
   pLRI->parseXML(*pNode1);
   fail_unless(pLRI != NULL);
   // create the XMLNode from the object
   pNode2 = new XMLNode(pLRI->toXML());
   fail_unless(pNode1 != NULL);
   // compare the two nodes
   fail_unless(pNode1->equals(*pNode2, true));
   delete pNode1;
   delete pNode2;
   delete pStream;
   delete pLRI;

}
END_TEST

Suite *
create_suite_LocalRenderInformation (void)
{
  Suite *suite = suite_create("LocalRenderInformation");
  TCase *tcase = tcase_create("LocalRenderInformation");


  tcase_add_checked_fixture( tcase,
                             LocalRenderInformationTest_setup,
                             LocalRenderInformationTest_teardown );

  tcase_add_test( tcase, test_LocalRenderInformation_createMethods              );
  tcase_add_test( tcase, test_LocalRenderInformation_addMethods                 );
  tcase_add_test( tcase, test_LocalRenderInformation_read                       );
  tcase_add_test( tcase, test_LocalRenderInformation_write                      );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
