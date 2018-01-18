//
// Filename    : TestPolygon.cpp
// Description : Tests for the Polygon class
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

#include <Polygon.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Polygon *P;
static RenderPkgNamespaces *renderns;

void
PolygonTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    P = new (std::nothrow) Polygon(renderns);

    if (P == NULL)
    {
        fail("new(std::nothrow)Polygon(renderns) returned a NULL pointer.");
    }

}

void 
PolygonTest_teardown (void)
{
    delete P;
    delete renderns;
}

START_TEST (test_Polygon_createMethods )
{
    fail_unless( P->getNumElements() == 0);
    RenderPoint* pP=P->createPoint();
    fail_unless(pP != NULL);
    fail_unless(P->getNumElements() == 1);
    RenderCubicBezier* pC=P->createCubicBezier();
    fail_unless(pC != NULL);
    fail_unless(P->getNumElements() == 2);
    pP=NULL;
    pP=P->getElement(0);
    fail_unless(pP != NULL);
    fail_unless(dynamic_cast<RenderCubicBezier*>(pP) == NULL);
    pP=NULL;
    pP=P->getElement(1);
    fail_unless(pP != NULL);
    fail_unless(dynamic_cast<RenderCubicBezier*>(pP) != NULL);
}
END_TEST 


START_TEST ( test_Polygon_addElement )
{
    fail_unless(P->getNumElements() == 0);
    RenderPoint* pP=new RenderPoint(renderns);
    fail_unless(pP != NULL);
    fail_unless( P->addElement(pP) == LIBSBML_OPERATION_SUCCESS);
    fail_unless( P->getNumElements() == 1);
    fail_unless( P->getElement(0) != pP );
    fail_unless( dynamic_cast<RenderCubicBezier*>(P->getElement(0)) == NULL );
    delete pP;
    pP = new RenderPoint(2,1);
    fail_unless ( pP != NULL );
    fail_unless( P->addElement(pP) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( P->getNumElements() == 1);
    delete pP;
    RenderCubicBezier* pCB = new RenderCubicBezier( renderns );
    fail_unless( pCB != NULL);
    fail_unless( P->addElement(pCB) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( P->getNumElements() == 2 );
    fail_unless( P->getElement(1) != pCB );
    fail_unless( dynamic_cast<RenderCubicBezier*>(P->getElement(1)) != NULL );
    delete pCB;
    pCB = new RenderCubicBezier(2,1);
    fail_unless( pCB != NULL);
    fail_unless( P->addElement(pCB) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( P->getNumElements() == 2 );
    delete pCB;
}
END_TEST


START_TEST ( test_Polygon_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" >\n"
                  "  <listOfElements>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
                  "  </listOfElements>\n"
                  "</polygon>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // no attributes
  Polygon c(*pNode);
  fail_unless(c.getNumElements() == 4);
  fail_unless(!c.isSetMatrix());
  fail_unless(!c.isSetStroke());
  fail_unless(!c.isSetStrokeWidth());
  fail_unless(!c.isSetDashArray());
  fail_unless(!c.isSetFillColor());
  fail_unless(!c.isSetFillRule());

  delete pNode;
  delete pStream;

  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</polygon>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  c = Polygon(*pNode);
  fail_unless(c.getNumElements() == 4);
  fail_unless(c.isSetMatrix());
  const double* pMatrix=c.getMatrix2D();
  fail_unless(pMatrix != NULL);
  fail_unless(fabs((pMatrix[0] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(fabs((pMatrix[1] - 0.173648) / 0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[2] - -0.173648) / -0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[3] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(pMatrix[4] < 1e-9);
  fail_unless(pMatrix[5] < 1e-9);
  fail_unless(!c.isSetStroke());
  fail_unless(!c.isSetStrokeWidth());
  fail_unless(!c.isSetDashArray());
  fail_unless(!c.isSetFillColor());
  fail_unless(!c.isSetFillRule());
  delete pNode;
  delete pStream;

  // 1D attributes (stroke, stroke_width, stroke-dasharray  

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</polygon>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  c = Polygon(*pNode);
  fail_unless(c.getNumElements() == 4);
  fail_unless(!c.isSetMatrix());
  fail_unless(c.isSetStroke());
  fail_unless(c.getStroke() == "#00FF00");
  fail_unless(c.isSetStrokeWidth());
  fail_unless(fabs((c.getStrokeWidth() - 3.0) / 3.0) < 1e-9);
  fail_unless(c.isSetDashArray());
  const std::vector<unsigned int>& array = c.getDashArray();
  fail_unless(array.size() == 2);
  fail_unless(array[0] == 32);
  fail_unless(array[1] == 20);
  fail_unless(!c.isSetFillColor());
  fail_unless(!c.isSetFillRule());

  delete pNode;
  delete pStream;

  // 2d attributes (fill, fill-rule)

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" fill=\"#DDFF00\" fill-rule=\"evenodd\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</polygon>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  c = Polygon(*pNode);
  fail_unless(c.getNumElements() == 4);
  fail_unless(!c.isSetMatrix());
  fail_unless(!c.isSetStroke());
  fail_unless(!c.isSetStrokeWidth());
  fail_unless(!c.isSetDashArray());
  fail_unless(c.isSetFillColor());
  fail_unless(c.getFillColor() == "#DDFF00");
  fail_unless(c.isSetFillRule());
  fail_unless(c.getFillRule() == GraphicalPrimitive2D::EVENODD);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_Polygon_read_old_style )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
                  "  <listOfCurveSegments>\n"
                  "    <curveSegment xsi:type=\"LineSegment\">\n"
                  "      <start x=\"0\" y=\"0\" />\n"
                  "      <end x=\"10\" y=\"3\" />\n"
                  "    </curveSegment>\n"
                  "    <curveSegment xsi:type=\"LineSegment\">\n"
                  "      <start x=\"10\" y=\"3\" />\n"
                  "      <end x=\"0\" y=\"6\" />\n"
                  "    </curveSegment>\n"
                  "  </listOfCurveSegments>\n"
                  "</polygon>\n"
                ;
      
  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  Polygon c = Polygon(*pNode);
  fail_unless(!c.isSetMatrix());
  fail_unless(!c.isSetStroke());
  fail_unless(!c.isSetStrokeWidth());
  fail_unless(!c.isSetDashArray());
  fail_unless(!c.isSetFillColor());
  fail_unless(!c.isSetFillRule());
  fail_unless(c.getNumElements() == 3);
  const RenderPoint* pP=c.getElement(0);
  fail_unless( pP != NULL );
  if (pP == NULL) return;
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( pP->x().getAbsoluteValue() < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( pP->y().getAbsoluteValue() < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(1);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 10.0 ) / 10.0) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(2);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( pP->x().getAbsoluteValue() < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 6.0) / 6.0) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  delete pNode;
  delete pStream;
      
      
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "  <listOfCurveSegments>\n"
      "    <curveSegment xsi:type=\"CubicBezier\">\n"
      "      <start x=\"2.0\" y=\"7.71428\"/>\n"
      "      <basePoint1 x=\"10.21428\" y=\"12.0\"/>\n"
      "      <basePoint2 x=\"15.21428\" y=\"17.04464\"/>\n"
      "      <end x=\"24.05357\" y=\"15.83928\"/>\n"
      "    </curveSegment>\n"
      "    <curveSegment xsi:type=\"CubicBezier\">\n"
      "      <start x=\"24.05357\" y=\"15.83928\"/>\n"
      "      <basePoint1 x=\"32.89285\" y=\"14.63392\"/>\n"
      "      <basePoint2 x=\"45.57142\" y=\"7.17856\"/>\n"
      "      <end x=\"45.57142\" y=\"7.17856\"/>\n"
      "    </curveSegment>\n"
      "    <curveSegment xsi:type=\"LineSegment\">\n"
      "      <start x=\"45.57142\" y=\"7.17856\"/>\n"
      "      <end x=\"41.46427\" y=\"2.0\"/>\n"
      "    </curveSegment>\n"
      "    <curveSegment xsi:type=\"CubicBezier\">\n"
      "      <start x=\"41.46427\" y=\"2.0\"/>\n"
      "      <basePoint1 x=\"41.46427\" y=\"2.0\"/>\n"
      "      <basePoint2 x=\"31.9107\" y=\"9.14285\"/>\n"
      "      <end x=\"23.42856\" y=\"9.32142\"/>\n"
      "    </curveSegment>\n"
      "    <curveSegment xsi:type=\"CubicBezier\">\n"
      "      <start x=\"23.42856\" y=\"9.32142\"/>\n"
      "      <basePoint1 x=\"14.94642\" y=\"9.49999\"/>\n"
      "      <basePoint2 x=\"7.5357\" y=\"2.71428\"/>\n"
      "      <end x=\"7.5357\" y=\"2.71428\"/>\n"
      "    </curveSegment>\n"
      "    <curveSegment xsi:type=\"LineSegment\">\n"
      "      <start x=\"7.5357\" y=\"2.71428\"/>\n"
      "      <end x=\"2.0\" y=\"7.71428\"/>\n"
      "    </curveSegment>\n"
      "  </listOfCurveSegments>\n"
      "</polygon>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);
  
  c = Polygon(*pNode);
  fail_unless(!c.isSetMatrix());
  fail_unless(!c.isSetStroke());
  fail_unless(!c.isSetStrokeWidth());
  fail_unless(!c.isSetDashArray());
  fail_unless(!c.isSetFillColor());
  fail_unless(!c.isSetFillRule());
  fail_unless(c.getNumElements() == 7);
  pP=c.getElement(0);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 7.71428) / 7.71428) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(1);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_CUBICBEZIER );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) != NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 24.05357) / 24.05357) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 15.83928) / 15.83928) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(2);
  fail_unless( pP != NULL );
  fail_unless(pP->getTypeCode() == SBML_RENDER_CUBICBEZIER);
  fail_unless(dynamic_cast<const RenderCubicBezier*>(pP) != NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 45.57142) / 45.57142) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 7.17856) / 7.17856) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(3);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 41.46427) / 41.46427) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(4);
  fail_unless( pP != NULL );
  fail_unless(pP->getTypeCode() == SBML_RENDER_CUBICBEZIER);
  fail_unless(dynamic_cast<const RenderCubicBezier*>(pP) != NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 23.42856) / 23.42856) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 9.32142) / 9.32142) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(5);
  fail_unless( pP != NULL );
  fail_unless(pP->getTypeCode() == SBML_RENDER_CUBICBEZIER);
  fail_unless(dynamic_cast<const RenderCubicBezier*>(pP) != NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 7.5357) / 7.5357) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 2.71428) / 2.71428) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=c.getElement(6);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 7.71428) / 7.71428) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  delete pNode;
  delete pStream;
} 
END_TEST

START_TEST ( test_Polygon_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" >\n"
                  "  <listOfElements>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
                  "  </listOfElements>\n"
                  "</polygon>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  Polygon* pP = new Polygon(*pNode1);
  fail_unless(pP != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pP->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pP;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</polygon>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pP = new Polygon(*pNode1);
  fail_unless(pP != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pP->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pP;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</polygon>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pP = new Polygon(*pNode1);
  fail_unless(pP != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pP->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pP;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<polygon xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" fill=\"#DDFF00\" fill-rule=\"evenodd\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</polygon>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pP = new Polygon(*pNode1);
  fail_unless(pP != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pP->toXML());
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
create_suite_Polygon (void)
{
  Suite *suite = suite_create("Polygon");
  TCase *tcase = tcase_create("Polygon");


  tcase_add_checked_fixture( tcase,
                             PolygonTest_setup,
                             PolygonTest_teardown );

  tcase_add_test( tcase, test_Polygon_createMethods  );
  tcase_add_test( tcase, test_Polygon_addElement     );
  tcase_add_test( tcase, test_Polygon_read           );
  tcase_add_test( tcase, test_Polygon_read_old_style );
  tcase_add_test( tcase, test_Polygon_write          );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
