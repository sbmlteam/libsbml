//
// Filename    : TestRenderCurve.cpp
// Description : Tests for the RenderCurve class
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

static RenderCurve *C;
static RenderPkgNamespaces *renderns;

void
RenderCurveTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    C = new (std::nothrow) RenderCurve(renderns);

    if (C == NULL)
    {
        fail("new(std::nothrow)RenderCurve(renderns) returned a NULL pointer.");
    }

}

void 
RenderCurveTest_teardown (void)
{
    delete C;
    delete renderns;
}

START_TEST (test_RenderCurve_createMethods )
{
    fail_unless( C->getNumElements() == 0);
    RenderPoint* pP=C->createPoint();
    fail_unless(pP != NULL);
    fail_unless(C->getNumElements() == 1);
    RenderCubicBezier* pC=C->createCubicBezier();
    fail_unless(pC != NULL);
    fail_unless(C->getNumElements() == 2);
    pP=NULL;
    pP=C->getElement(0);
    fail_unless(pP != NULL);
    fail_unless(dynamic_cast<RenderCubicBezier*>(pP) == NULL);
    pP=NULL;
    pP=C->getElement(1);
    fail_unless(pP != NULL);
    fail_unless(dynamic_cast<RenderCubicBezier*>(pP) != NULL);
}
END_TEST 


START_TEST ( test_RenderCurve_addElement )
{
    fail_unless(C->getNumElements() == 0);
    RenderPoint* pP=new RenderPoint(renderns);
    fail_unless(pP != NULL);
    fail_unless( C->addElement(pP) == LIBSBML_OPERATION_SUCCESS);
    fail_unless( C->getNumElements() == 1);
    fail_unless( C->getElement(0) != pP );
    fail_unless( dynamic_cast<RenderCubicBezier*>(C->getElement(0)) == NULL );
    delete pP;
    pP = new RenderPoint(2,1);
    fail_unless ( pP != NULL );
    fail_unless( C->addElement(pP) == LIBSBML_LEVEL_MISMATCH);
    fail_unless( C->getNumElements() == 1);
    delete pP;
    RenderCubicBezier* pCB = new RenderCubicBezier( renderns );
    fail_unless( pCB != NULL);
    fail_unless( C->addElement(pCB) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( C->getNumElements() == 2 );
    fail_unless( C->getElement(1) != pCB );
    fail_unless( dynamic_cast<RenderCubicBezier*>(C->getElement(1)) != NULL );
    delete pCB;
    pCB = new RenderCubicBezier(2,1);
    fail_unless( pCB != NULL);
    fail_unless( C->addElement(pCB) == LIBSBML_LEVEL_MISMATCH);
    fail_unless( C->getNumElements() == 2 );
    delete pCB;
}
END_TEST

START_TEST ( test_RenderCurve_heads )
{
    fail_unless( ! C->isSetStartHead() );
    fail_unless( ! C->isSetEndHead() );
    fail_unless( C->getStartHead() == "" );
    fail_unless( C->getEndHead() == "" );
    C->setStartHead("SimpleArrow");
    fail_unless(C->isSetStartHead());
    fail_unless(C->getStartHead() == "SimpleArrow");
    C->setEndHead("SimpleCircle");
    fail_unless(C->isSetEndHead());
    fail_unless(C->getEndHead() == "SimpleCircle");
    C->setStartHead("none");
    fail_unless(! C->isSetStartHead());
    fail_unless( C->getStartHead() == "none");
    C->setEndHead("none");
    fail_unless(! C->isSetEndHead());
    fail_unless( C->getEndHead() == "none");
}
END_TEST

START_TEST ( test_RenderCurve_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" >\n"
                  "  <listOfElements>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
                  "  </listOfElements>\n"
                  "</curve>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // no attributes
  RenderCurve c(*pNode);
  fail_unless(c.getNumElements() == 4);
  fail_unless(!c.isSetMatrix());
  fail_unless(!c.isSetStroke());
  fail_unless(!c.isSetStrokeWidth());
  fail_unless(!c.isSetDashArray());

  delete pNode;
  delete pStream;

  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</curve>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  c = RenderCurve(*pNode);
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

  delete pNode;
  delete pStream;

  // 1D attributes (stroke, stroke_width, stroke-dasharray  

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</curve>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  c = RenderCurve(*pNode);
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

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_RenderCurve_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" >\n"
                  "  <listOfElements>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
                  "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
                  "  </listOfElements>\n"
                  "</curve>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  RenderCurve* pC = new RenderCurve(*pNode1);
  fail_unless(pC != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pC->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pC;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</curve>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pC = new RenderCurve(*pNode1);
  fail_unless(pC != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pC->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pC;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\">\n"
      "  <listOfElements>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"100%\" y=\"50%\"/>\n"
      "    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"100%\"/>\n"
      "  </listOfElements>\n"
      "</curve>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pC = new RenderCurve(*pNode1);
  fail_unless(pC != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pC->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pC;

}
END_TEST

START_TEST ( test_RenderCurve_read_old_style )
{
  // we need to pack the curve in a group because the code to read "old style"
  // curves is in the Group class.
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<g xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
                  "  <curve>\n"
                  "    <listOfCurveSegments>\n"
                  "      <curveSegment xsi:type=\"LineSegment\">\n"
                  "        <start x=\"0\" y=\"0\" />\n"
                  "        <end x=\"10\" y=\"3\" />\n"
                  "      </curveSegment>\n"
                  "      <curveSegment xsi:type=\"LineSegment\">\n"
                  "        <start x=\"10\" y=\"3\" />\n"
                  "        <end x=\"0\" y=\"6\" />\n"
                  "      </curveSegment>\n"
                  "    </listOfCurveSegments>\n"
                  "  </curve>\n"
                  "</g>\n"
                ;
      
  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  RenderGroup g = RenderGroup(*pNode);
  fail_unless(g.getNumElements() == 1);
  const Transformation2D* pElement=g.getElement(0);
  fail_unless(pElement != NULL);
  if (pElement == NULL) return;
  fail_unless(pElement->getTypeCode() == SBML_RENDER_CURVE);
  const RenderCurve* pC=dynamic_cast<const RenderCurve*>(pElement);
  fail_unless(!pC->isSetMatrix());
  fail_unless(!pC->isSetStroke());
  fail_unless(!pC->isSetStrokeWidth());
  fail_unless(!pC->isSetDashArray());
  fail_unless(pC->getNumElements() == 3);
  const RenderPoint* pP=pC->getElement(0);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( pP->x().getAbsoluteValue() < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( pP->y().getAbsoluteValue() < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(1);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 10.0 ) / 10.0) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(2);
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
      "<g xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      " <curve xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
      "    <listOfCurveSegments>\n"
      "      <curveSegment xsi:type=\"CubicBezier\">\n"
      "        <start x=\"2.0\" y=\"7.71428\"/>\n"
      "        <basePoint1 x=\"10.21428\" y=\"12.0\"/>\n"
      "        <basePoint2 x=\"15.21428\" y=\"17.04464\"/>\n"
      "        <end x=\"24.05357\" y=\"15.83928\"/>\n"
      "      </curveSegment>\n"
      "      <curveSegment xsi:type=\"CubicBezier\">\n"
      "        <start x=\"24.05357\" y=\"15.83928\"/>\n"
      "        <basePoint1 x=\"32.89285\" y=\"14.63392\"/>\n"
      "        <basePoint2 x=\"45.57142\" y=\"7.17856\"/>\n"
      "        <end x=\"45.57142\" y=\"7.17856\"/>\n"
      "      </curveSegment>\n"
      "      <curveSegment xsi:type=\"LineSegment\">\n"
      "        <start x=\"45.57142\" y=\"7.17856\"/>\n"
      "        <end x=\"41.46427\" y=\"2.0\"/>\n"
      "      </curveSegment>\n"
      "      <curveSegment xsi:type=\"CubicBezier\">\n"
      "        <start x=\"41.46427\" y=\"2.0\"/>\n"
      "        <basePoint1 x=\"41.46427\" y=\"2.0\"/>\n"
      "        <basePoint2 x=\"31.9107\" y=\"9.14285\"/>\n"
      "        <end x=\"23.42856\" y=\"9.32142\"/>\n"
      "      </curveSegment>\n"
      "      <curveSegment xsi:type=\"CubicBezier\">\n"
      "        <start x=\"23.42856\" y=\"9.32142\"/>\n"
      "        <basePoint1 x=\"14.94642\" y=\"9.49999\"/>\n"
      "        <basePoint2 x=\"7.5357\" y=\"2.71428\"/>\n"
      "        <end x=\"7.5357\" y=\"2.71428\"/>\n"
      "      </curveSegment>\n"
      "      <curveSegment xsi:type=\"LineSegment\">\n"
      "        <start x=\"7.5357\" y=\"2.71428\"/>\n"
      "        <end x=\"2.0\" y=\"7.71428\"/>\n"
      "      </curveSegment>\n"
      "    </listOfCurveSegments>\n"
      "  </curve>\n"
      "</g>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);
  
  g = RenderGroup(*pNode);
  fail_unless(g.getNumElements() == 1);
  pElement=g.getElement(0);
  fail_unless(pElement != NULL);
  fail_unless(pElement->getTypeCode() == SBML_RENDER_CURVE);
  pC=dynamic_cast<const RenderCurve*>(pElement);
  fail_unless(!pC->isSetMatrix());
  fail_unless(!pC->isSetStroke());
  fail_unless(!pC->isSetStrokeWidth());
  fail_unless(!pC->isSetDashArray());
  fail_unless(pC->getNumElements() == 7);
  pP=pC->getElement(0);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 7.71428) / 7.71428) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(1);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_CUBICBEZIER );
  const RenderCubicBezier* pCB = dynamic_cast<const RenderCubicBezier*>(pP);
  fail_unless( pCB != NULL);
  fail_unless( fabs((pCB->basePoint1_X().getAbsoluteValue() - 10.21428) / 10.21428) < 1e-9);
  fail_unless( pCB->basePoint1_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint1_Y().getAbsoluteValue() - 12.0) / 12.0) < 1e-9);
  fail_unless( pCB->basePoint1_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_X().getAbsoluteValue() - 15.21428) / 15.21428) < 1e-9);
  fail_unless( pCB->basePoint2_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_Y().getAbsoluteValue() - 17.04464) / 17.04464) < 1e-9);
  fail_unless( pCB->basePoint2_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->x().getAbsoluteValue() - 24.05357) / 24.05357) < 1e-9);
  fail_unless( pCB->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->y().getAbsoluteValue() - 15.83928) / 15.83928) < 1e-9);
  fail_unless( pCB->y().getRelativeValue() < 1e-9);
  fail_unless( pCB->z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(2);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_CUBICBEZIER );
  pCB = dynamic_cast<const RenderCubicBezier*>(pP);
  fail_unless( pCB != NULL);
  fail_unless( fabs((pCB->basePoint1_X().getAbsoluteValue() - 32.89285) / 32.89285) < 1e-9);
  fail_unless( pCB->basePoint1_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint1_Y().getAbsoluteValue() - 14.63392) / 14.63392) < 1e-9);
  fail_unless( pCB->basePoint1_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_X().getAbsoluteValue() - 45.57142) / 45.57142) < 1e-9);
  fail_unless( pCB->basePoint2_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_Y().getAbsoluteValue() - 7.17856) / 7.17856) < 1e-9);
  fail_unless( pCB->basePoint2_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->x().getAbsoluteValue() - 45.57142) / 45.57142) < 1e-9);
  fail_unless( pCB->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->y().getAbsoluteValue() - 7.17856) / 7.17856) < 1e-9);
  fail_unless( pCB->y().getRelativeValue() < 1e-9);
  fail_unless( pCB->z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(3);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_POINT );
  fail_unless( dynamic_cast<const RenderCubicBezier*>(pP) == NULL);
  fail_unless( fabs((pP->x().getAbsoluteValue() - 41.46427) / 41.46427) < 1e-9);
  fail_unless( pP->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pP->y().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
  fail_unless( pP->y().getRelativeValue() < 1e-9);
  fail_unless( pP->z().getAbsoluteValue() < 1e-9);
  fail_unless( pP->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(4);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_CUBICBEZIER );
  pCB = dynamic_cast<const RenderCubicBezier*>(pP);
  fail_unless( pCB != NULL);
  fail_unless( fabs((pCB->basePoint1_X().getAbsoluteValue() - 41.46427) / 41.46427) < 1e-9);
  fail_unless( pCB->basePoint1_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint1_Y().getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
  fail_unless( pCB->basePoint1_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_X().getAbsoluteValue() - 31.9107) / 31.9107) < 1e-9);
  fail_unless( pCB->basePoint2_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_Y().getAbsoluteValue() - 9.14285) / 9.14285) < 1e-9);
  fail_unless( pCB->basePoint2_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->x().getAbsoluteValue() - 23.42856) / 23.42856) < 1e-9);
  fail_unless( pCB->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->y().getAbsoluteValue() - 9.32142) / 9.32142) < 1e-9);
  fail_unless( pCB->y().getRelativeValue() < 1e-9);
  fail_unless( pCB->z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(5);
  fail_unless( pP != NULL );
  fail_unless( pP->getTypeCode() == SBML_RENDER_CUBICBEZIER );
  pCB = dynamic_cast<const RenderCubicBezier*>(pP);
  fail_unless( pCB != NULL);
  fail_unless( fabs((pCB->basePoint1_X().getAbsoluteValue() - 14.94642) / 14.94642) < 1e-9);
  fail_unless( pCB->basePoint1_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint1_Y().getAbsoluteValue() - 9.49999) / 9.49999) < 1e-9);
  fail_unless( pCB->basePoint1_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint1_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_X().getAbsoluteValue() - 7.5357) / 7.5357) < 1e-9);
  fail_unless( pCB->basePoint2_X().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->basePoint2_Y().getAbsoluteValue() - 2.71428) / 2.71428) < 1e-9);
  fail_unless( pCB->basePoint2_Y().getRelativeValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->basePoint2_Z().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->x().getAbsoluteValue() - 7.5357) / 7.5357) < 1e-9);
  fail_unless( pCB->x().getRelativeValue() < 1e-9);
  fail_unless( fabs((pCB->y().getAbsoluteValue() - 2.71428) / 2.71428) < 1e-9);
  fail_unless( pCB->y().getRelativeValue() < 1e-9);
  fail_unless( pCB->z().getAbsoluteValue() < 1e-9);
  fail_unless( pCB->z().getRelativeValue() < 1e-9);
  pP=pC->getElement(6);
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


Suite *
create_suite_RenderCurve (void)
{
  Suite *suite = suite_create("RenderCurve");
  TCase *tcase = tcase_create("RenderCurve");


  tcase_add_checked_fixture( tcase,
                             RenderCurveTest_setup,
                             RenderCurveTest_teardown );

  tcase_add_test( tcase, test_RenderCurve_createMethods      );
  tcase_add_test( tcase, test_RenderCurve_addElement         );
  tcase_add_test( tcase, test_RenderCurve_heads              );
  tcase_add_test( tcase, test_RenderCurve_read               );
  tcase_add_test( tcase, test_RenderCurve_read_old_style     );
  tcase_add_test( tcase, test_RenderCurve_write              );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
