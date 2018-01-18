//
// Filename    : TestGroup.cpp
// Description : Tests for the Group class
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

static RenderGroup *G;
static RenderPkgNamespaces *renderns;

void
GroupTest_setup (void)
{
    renderns = new (std::nothrow) RenderPkgNamespaces();
    G = new (std::nothrow) RenderGroup(renderns);

    if (G == NULL)
    {
        fail("new(std::nothrow)Group(renderns) returned a NULL pointer.");
    }

}

void 
GroupTest_teardown (void)
{
    delete G;
    delete renderns;
}

START_TEST ( test_Group_FontFamily )
{
    fail_unless(! G->isSetFontFamily() );
    fail_unless(G->getFontFamily() == "");
    G->setFontFamily("Helvetica");
    fail_unless( G->isSetFontFamily() );
    fail_unless(G->getFontFamily() == "Helvetica");
    G->setFontFamily("");
    fail_unless(! G->isSetFontFamily() );
    fail_unless(G->getFontFamily() == "");
}
END_TEST


START_TEST ( test_Group_FontSize )
{
    fail_unless(!G->isSetFontSize());
    G->setFontSize(RelAbsVector(18.0,0.0));
    fail_unless(G->isSetFontSize());
    fail_unless(G->getFontSize() == RelAbsVector(18.0,0.0));
    G->setFontSize(RelAbsVector(0.0,50.0));
    fail_unless(G->isSetFontSize());
    fail_unless(G->getFontSize() == RelAbsVector(0.0,50.0));
    G->setFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN()));
    fail_unless(!G->isSetFontSize());
    G->setFontSize(RelAbsVector(23.0,std::numeric_limits<double>::quiet_NaN()));
    fail_unless(!G->isSetFontSize());
    G->setFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),75.0));
    fail_unless(!G->isSetFontSize());
}
END_TEST

START_TEST( test_Group_FontWeight )
{
    fail_unless( !G->isSetFontWeight() );
    fail_unless( G->getFontWeight() == Text::WEIGHT_UNSET );
    G->setFontWeight(Text::WEIGHT_NORMAL);
    fail_unless(G->isSetFontWeight());
    fail_unless(G->getFontWeight() == Text::WEIGHT_NORMAL);
    G->setFontWeight(Text::WEIGHT_BOLD);
    fail_unless(G->isSetFontWeight());
    fail_unless(G->getFontWeight() == Text::WEIGHT_BOLD);
    G->setFontWeight(Text::WEIGHT_UNSET);
    fail_unless( !G->isSetFontWeight());
    fail_unless(G->getFontWeight() == Text::WEIGHT_UNSET);
}
END_TEST

START_TEST( test_Group_FontStyle )
{
    fail_unless( !G->isSetFontStyle() );
    fail_unless( G->getFontStyle() == Text::STYLE_UNSET );
    G->setFontStyle(Text::STYLE_NORMAL);
    fail_unless(G->isSetFontStyle());
    fail_unless(G->getFontStyle() == Text::STYLE_NORMAL);
    G->setFontStyle(Text::STYLE_ITALIC);
    fail_unless(G->isSetFontStyle());
    fail_unless(G->getFontStyle() == Text::STYLE_ITALIC);
    G->setFontStyle(Text::STYLE_UNSET);
    fail_unless( !G->isSetFontStyle());
    fail_unless(G->getFontStyle() == Text::STYLE_UNSET);
}
END_TEST

START_TEST( test_Group_GroupAnchor )
{
    fail_unless( !G->isSetTextAnchor() );
    fail_unless( G->getTextAnchor() == Text::ANCHOR_UNSET );
    G->setTextAnchor(Text::ANCHOR_START);
    fail_unless(G->isSetTextAnchor());
    fail_unless(G->getTextAnchor() == Text::ANCHOR_START);
    G->setTextAnchor(Text::ANCHOR_MIDDLE);
    fail_unless(G->isSetTextAnchor());
    fail_unless(G->getTextAnchor() == Text::ANCHOR_MIDDLE);
    G->setTextAnchor(Text::ANCHOR_END);
    fail_unless(G->isSetTextAnchor());
    fail_unless(G->getTextAnchor() == Text::ANCHOR_END);
    //G->setTextAnchor(Text::ANCHOR_BASELINE);
    //fail_unless( !G->isSetTextAnchor());
    //fail_unless(G->getTextAnchor() == Text::ANCHOR_UNSET);
    G->setTextAnchor(Text::ANCHOR_UNSET);
    fail_unless( !G->isSetTextAnchor());
    fail_unless(G->getTextAnchor() == Text::ANCHOR_UNSET);
}
END_TEST

START_TEST( test_Group_VGroupAnchor )
{
    fail_unless( !G->isSetVTextAnchor() );
    fail_unless( G->getVTextAnchor() == Text::ANCHOR_UNSET );
    G->setVTextAnchor(Text::ANCHOR_TOP);
    fail_unless(G->isSetVTextAnchor());
    fail_unless(G->getVTextAnchor() == Text::ANCHOR_TOP);
    G->setVTextAnchor(Text::ANCHOR_MIDDLE);
    fail_unless(G->isSetVTextAnchor());
    fail_unless(G->getVTextAnchor() == Text::ANCHOR_MIDDLE);
    G->setVTextAnchor(Text::ANCHOR_BOTTOM);
    fail_unless(G->isSetVTextAnchor());
    fail_unless(G->getVTextAnchor() == Text::ANCHOR_BOTTOM);
    //G->setVTextAnchor(Text::ANCHOR_BASELINE);
    //fail_unless(G->isSetVTextAnchor());
    //fail_unless(G->getVTextAnchor() == Text::ANCHOR_BASELINE);
    G->setVTextAnchor(Text::ANCHOR_UNSET);
    fail_unless( !G->isSetVTextAnchor());
    fail_unless(G->getVTextAnchor() == Text::ANCHOR_UNSET);
}
END_TEST

START_TEST ( test_Group_heads )
{
    fail_unless( ! G->isSetStartHead() );
    fail_unless( ! G->isSetEndHead() );
    fail_unless( G->getStartHead() == "" );
    fail_unless( G->getEndHead() == "" );
    G->setStartHead("SimpleArrow");
    fail_unless(G->isSetStartHead());
    fail_unless(G->getStartHead() == "SimpleArrow");
    G->setEndHead("SimpleCircle");
    fail_unless(G->isSetEndHead());
    fail_unless(G->getEndHead() == "SimpleCircle");
    G->setStartHead("none");
    fail_unless(! G->isSetStartHead());
    fail_unless( G->getStartHead() == "none");
    G->setEndHead("none");
    fail_unless(! G->isSetEndHead());
    fail_unless( G->getEndHead() == "none");
}
END_TEST

START_TEST ( test_Group_createMethods )
{
    fail_unless(G->getNumElements() == 0);
    Image* pImage=G->createImage();
    fail_unless(pImage != NULL);
    fail_unless(G->getNumElements() == 1);
    RenderGroup* pGroup=G->createGroup();
    fail_unless(pGroup != NULL);
    fail_unless(G->getNumElements() == 2);
    Rectangle* pRectangle=G->createRectangle();
    fail_unless(pRectangle != NULL);
    fail_unless(G->getNumElements() == 3);
    Ellipse* pEllipse=G->createEllipse();
    fail_unless(pEllipse != NULL);
    fail_unless(G->getNumElements() == 4);
    RenderCurve* pRenderCurve=G->createCurve();
    fail_unless(pRenderCurve != NULL);
    fail_unless(G->getNumElements() == 5);
    Polygon* pPolygon=G->createPolygon();
    fail_unless(pPolygon != NULL);
    fail_unless(G->getNumElements() == 6);
    Text* pText=G->createText();
    fail_unless(pText != NULL);
    fail_unless(G->getNumElements() == 7);
}
END_TEST

START_TEST ( test_Group_addChildElement )
{
    fail_unless(G->getNumElements() == 0);
    Image* pImage=new Image(renderns);
    fail_unless(pImage != NULL);
    fail_unless(G->addChildElement(pImage) == LIBSBML_INVALID_OBJECT);
    fail_unless(G->getNumElements() == 0);
    pImage->setImageReference("test.png");
    fail_unless(G->addChildElement(pImage) == LIBSBML_OPERATION_SUCCESS);
    delete pImage;
    fail_unless(G->getNumElements() == 1);
    pImage=new Image(2,1);
    pImage->setImageReference("test.png");
    fail_unless(G->addChildElement(pImage) == LIBSBML_LEVEL_MISMATCH);
    fail_unless(G->getNumElements() == 1);
    delete pImage;
    RenderGroup* pGroup=new RenderGroup(renderns);
    fail_unless(pGroup != NULL);
    G->addChildElement(pGroup);
    delete pGroup;
    fail_unless(G->getNumElements() == 2);
    Rectangle* pRectangle=new Rectangle(renderns);
    fail_unless(pRectangle != NULL);
    fail_unless(G->addChildElement(pRectangle) == LIBSBML_OPERATION_SUCCESS);
    delete pRectangle;
    fail_unless(G->getNumElements() == 3);
    Ellipse* pEllipse=new Ellipse(renderns);
    fail_unless(pEllipse != NULL);
    fail_unless(G->addChildElement(pEllipse) == LIBSBML_OPERATION_SUCCESS); 
    delete pEllipse;
    fail_unless(G->getNumElements() == 4);
    RenderCurve* pRenderCurve=new RenderCurve(renderns);
    fail_unless(pRenderCurve != NULL);
    G->addChildElement(pRenderCurve);
    delete pRenderCurve;
    fail_unless(G->getNumElements() == 5);
    Polygon* pPolygon=new Polygon(renderns);
    fail_unless(pPolygon != NULL);
    G->addChildElement(pPolygon);
    delete pPolygon;
    fail_unless(G->getNumElements() == 6);
    Text* pText=new Text(renderns);
    // empty text is valid !
    fail_unless(pText != NULL);
    fail_unless(G->addChildElement(pText) == LIBSBML_OPERATION_SUCCESS);
    fail_unless(G->getNumElements() == 7);
    delete pText;
    pText = new Text(renderns);
    pText->setText("My Text");
    fail_unless(G->addChildElement(pText) == LIBSBML_OPERATION_SUCCESS);
    delete pText;
    fail_unless(G->getNumElements() == 8);
}
END_TEST

START_TEST ( test_Group_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<g>\n"
                  "  <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
                  "  <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
                  "</g>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  RenderGroup g(*pNode);
  fail_unless(!g.isSetMatrix());
  fail_unless(!g.isSetStroke());
  fail_unless(!g.isSetStrokeWidth());
  fail_unless(!g.isSetDashArray());
  fail_unless(!g.isSetFillColor());
  fail_unless(!g.isSetFillRule());
  fail_unless(!g.isSetFontFamily());
  fail_unless(g.getFontFamily() == "");
  fail_unless(!g.isSetFontSize());
  fail_unless(!g.isSetFontWeight());
  fail_unless(!g.isSetFontStyle());
  fail_unless(!g.isSetTextAnchor());
  fail_unless(!g.isSetVTextAnchor());
  fail_unless(!g.isSetStartHead());
  fail_unless(!g.isSetEndHead());
  fail_unless(g.getNumElements() == 2);
  const Rectangle* r=dynamic_cast<const Rectangle*>(g.getElement(0));
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
  const Ellipse* e=dynamic_cast<const Ellipse*>(g.getElement(1));
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
      "<g font-family=\"Courier\" font-size=\"36.0\"\n"
      "   font-weight=\"bold\" font-style=\"italic\"\n"
      "   text-anchor=\"end\" vtext-anchor=\"bottom\"\n"
      "   startHead=\"start_arrow\" endHead=\"end_arrow\">\n"
      "  <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  g = RenderGroup(*pNode);
  fail_unless(!g.isSetMatrix());
  fail_unless(!g.isSetStroke());
  fail_unless(!g.isSetStrokeWidth());
  fail_unless(!g.isSetDashArray());
  fail_unless(!g.isSetFillColor());
  fail_unless(!g.isSetFillRule());
  fail_unless(g.isSetFontFamily());
  fail_unless(g.getFontFamily() == "Courier");
  fail_unless(g.isSetFontSize());
  fail_unless(fabs((g.getFontSize().getAbsoluteValue() - 36.0) / 36.0) < 1e-9);
  fail_unless(g.getFontSize().getRelativeValue()  < 1e-9);
  fail_unless(g.isSetFontWeight());
  fail_unless(g.getFontWeight() == Text::WEIGHT_BOLD);
  fail_unless(g.isSetFontStyle());
  fail_unless(g.getFontStyle() == Text::STYLE_ITALIC);
  fail_unless(g.isSetTextAnchor());
  fail_unless(g.getTextAnchor() == Text::ANCHOR_END);
  fail_unless(g.isSetVTextAnchor());
  fail_unless(g.getVTextAnchor() == Text::ANCHOR_BOTTOM);
  fail_unless(g.isSetStartHead());
  fail_unless(g.getStartHead() == "start_arrow");
  fail_unless(g.isSetEndHead());
  fail_unless(g.getEndHead() == "end_arrow");
  fail_unless(g.getNumElements() == 2);
  r=dynamic_cast<const Rectangle*>(g.getElement(0));
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
  e=dynamic_cast<const Ellipse*>(g.getElement(1));
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

  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\">\n"
      "  <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  g = RenderGroup(*pNode);
  fail_unless(g.isSetMatrix());
  const double* pMatrix=g.getMatrix2D();
  fail_unless(pMatrix != NULL);
  fail_unless(fabs((pMatrix[0] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(fabs((pMatrix[1] - 0.173648) / 0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[2] - -0.173648) / -0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[3] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(pMatrix[4] < 1e-9);
  fail_unless(pMatrix[5] < 1e-9);
  fail_unless(!g.isSetStroke());
  fail_unless(!g.isSetStrokeWidth());
  fail_unless(!g.isSetDashArray());
  fail_unless(!g.isSetFillColor());
  fail_unless(!g.isSetFillRule());
  fail_unless(!g.isSetFontFamily());
  fail_unless(g.getFontFamily() == "");
  fail_unless(!g.isSetFontSize());
  fail_unless(!g.isSetFontWeight());
  fail_unless(!g.isSetFontStyle());
  fail_unless(!g.isSetTextAnchor());
  fail_unless(!g.isSetVTextAnchor());
  fail_unless(!g.isSetStartHead());
  fail_unless(!g.isSetEndHead());
  fail_unless(g.getNumElements() == 2);
  r=dynamic_cast<const Rectangle*>(g.getElement(0));
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
  e=dynamic_cast<const Ellipse*>(g.getElement(1));
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

  // 1D attributes (stroke, stroke_width, stroke-dasharray  

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\">\n"
      "  <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  g = RenderGroup(*pNode);
  fail_unless(!g.isSetMatrix());
  fail_unless(g.isSetStroke());
  fail_unless(g.getStroke() == "#00FF00");
  fail_unless(g.isSetStrokeWidth());
  fail_unless(fabs((g.getStrokeWidth() - 3.0) / 3.0) < 1e-9);
  fail_unless(g.isSetDashArray());
  const std::vector<unsigned int>& array = g.getDashArray();
  fail_unless(array.size() == 2);
  fail_unless(array[0] == 32);
  fail_unless(array[1] == 20);
  fail_unless(!g.isSetFillColor());
  fail_unless(!g.isSetFillRule());
  fail_unless(!g.isSetFontFamily());
  fail_unless(g.getFontFamily() == "");
  fail_unless(!g.isSetFontSize());
  fail_unless(!g.isSetFontWeight());
  fail_unless(!g.isSetFontStyle());
  fail_unless(!g.isSetTextAnchor());
  fail_unless(!g.isSetVTextAnchor());
  fail_unless(!g.isSetStartHead());
  fail_unless(!g.isSetEndHead());
  fail_unless(g.getNumElements() == 2);
  r=dynamic_cast<const Rectangle*>(g.getElement(0));
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
  e=dynamic_cast<const Ellipse*>(g.getElement(1));
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
      "<g fill=\"#DDFF00\" fill-rule=\"evenodd\">\n"
      "  <rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  g = RenderGroup(*pNode);
  fail_unless(!g.isSetMatrix());
  fail_unless(!g.isSetStroke());
  fail_unless(!g.isSetStrokeWidth());
  fail_unless(!g.isSetDashArray());
  fail_unless(g.isSetFillColor());
  fail_unless(g.getFillColor() == "#DDFF00");
  fail_unless(g.isSetFillRule());
  fail_unless(g.getFillRule() == GraphicalPrimitive2D::EVENODD);
  fail_unless(!g.isSetFontFamily());
  fail_unless(g.getFontFamily() == "");
  fail_unless(!g.isSetFontSize());
  fail_unless(!g.isSetFontWeight());
  fail_unless(!g.isSetFontStyle());
  fail_unless(!g.isSetTextAnchor());
  fail_unless(!g.isSetVTextAnchor());
  fail_unless(!g.isSetStartHead());
  fail_unless(!g.isSetEndHead());
  fail_unless(g.getNumElements() == 2);
  r=dynamic_cast<const Rectangle*>(g.getElement(0));
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
  e=dynamic_cast<const Ellipse*>(g.getElement(1));
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
      "<g>\n"
      "</g>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // required attributes
  g = RenderGroup(*pNode);
  fail_unless(!g.isSetMatrix());
  fail_unless(!g.isSetStroke());
  fail_unless(!g.isSetStrokeWidth());
  fail_unless(!g.isSetDashArray());
  fail_unless(!g.isSetFillColor());
  fail_unless(!g.isSetFillRule());
  fail_unless(!g.isSetFontFamily());
  fail_unless(g.getFontFamily() == "");
  fail_unless(!g.isSetFontSize());
  fail_unless(!g.isSetFontWeight());
  fail_unless(!g.isSetFontStyle());
  fail_unless(!g.isSetTextAnchor());
  fail_unless(!g.isSetVTextAnchor());
  fail_unless(!g.isSetStartHead());
  fail_unless(!g.isSetEndHead());
  fail_unless(g.getNumElements() == 0);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_Group_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<g>\n"
                  "  <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
                  "  <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
                  "</g>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  RenderGroup* pG = new RenderGroup(*pNode1);
  fail_unless(pG != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g font-family=\"Courier\" font-size=\"36\"\n"
      "   font-weight=\"bold\" font-style=\"italic\"\n"
      "   text-anchor=\"end\" vtext-anchor=\"bottom\"\n"
      "   startHead=\"start_arrow\" endHead=\"end_arrow\">\n"
      "  <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pG = new RenderGroup(*pNode1);
  fail_unless(pG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\">\n"
      "  <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pG = new RenderGroup(*pNode1);
  fail_unless(pG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\">\n"
      "  <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pG = new RenderGroup(*pNode1);
  fail_unless(pG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g fill=\"#DDFF00\" fill-rule=\"evenodd\">\n"
      "  <rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
      "  <ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
      "</g>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pG = new RenderGroup(*pNode1);
  fail_unless(pG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<g>\n"
      "</g>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pG = new RenderGroup(*pNode1);
  fail_unless(pG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pG;

}
END_TEST
    
Suite *
create_suite_Group (void)
{
  Suite *suite = suite_create("Group");
  TCase *tcase = tcase_create("Group");


  tcase_add_checked_fixture( tcase,
                             GroupTest_setup,
                             GroupTest_teardown );

  tcase_add_test( tcase, test_Group_FontFamily         );
  tcase_add_test( tcase, test_Group_FontSize           );
  tcase_add_test( tcase, test_Group_FontWeight         );
  tcase_add_test( tcase, test_Group_FontStyle          );
  tcase_add_test( tcase, test_Group_GroupAnchor        );
  tcase_add_test( tcase, test_Group_VGroupAnchor       );
  tcase_add_test( tcase, test_Group_heads              );
  tcase_add_test( tcase, test_Group_addChildElement    );
  tcase_add_test( tcase, test_Group_createMethods      );
  tcase_add_test( tcase, test_Group_read               );
  tcase_add_test( tcase, test_Group_write              );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
