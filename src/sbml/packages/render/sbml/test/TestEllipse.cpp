//
// Filename    : TestEllipse.cpp
// Description : Tests for the Ellipse class
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

#include <Ellipse.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Ellipse *E;
static RenderPkgNamespaces *renderns;

void
EllipseTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    E = new (std::nothrow) Ellipse(renderns);

    if (E == NULL)
    {
        fail("new(std::nothrow)Ellipse(renderns) returned a NULL pointer.");
    }

}

void 
EllipseTest_teardown (void)
{
    delete E;
    delete renderns;
}

START_TEST (test_Ellipse_setters )
{
    fail_unless(E->getCX().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCX().getRelativeValue() < 1e-9);
    fail_unless(E->getCY().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCY().getRelativeValue() < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCZ().getRelativeValue() < 1e-9);
    fail_unless(E->getRX().getAbsoluteValue() < 1e-9);
    fail_unless(E->getRX().getRelativeValue() < 1e-9);
    fail_unless(E->getRY().getAbsoluteValue() < 1e-9);
    fail_unless(E->getRY().getRelativeValue() < 1e-9);
    // setRadii
    E->setRadii(RelAbsVector(3.3,5.5),RelAbsVector(7.7,9.9));
    fail_unless(E->getCX().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCX().getRelativeValue() < 1e-9);
    fail_unless(E->getCY().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCY().getRelativeValue() < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -7.7) / 7.7) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -9.9) / 9.9) < 1e-9);
    // setRX
    E->setRX(RelAbsVector(2.2,4.4));
    fail_unless(E->getCX().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCX().getRelativeValue() < 1e-9);
    fail_unless(E->getCY().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCY().getRelativeValue() < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -7.7) / 7.7) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -9.9) / 9.9) < 1e-9);
    // setRY
    E->setRY(RelAbsVector(6.6,8.8));
    fail_unless(E->getCX().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCX().getRelativeValue() < 1e-9);
    fail_unless(E->getCY().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCY().getRelativeValue() < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -8.8) / 8.8) < 1e-9);
    // setCX
    // setCY
    // setCZ
    E->setCX(RelAbsVector(200.0,300.0));
    fail_unless(fabs((E->getCX().getAbsoluteValue() -200.0) / 200.0) < 1e-9);
    fail_unless(fabs((E->getCX().getRelativeValue() -300.0) / 300.0) < 1e-9);
    fail_unless(E->getCY().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCY().getRelativeValue() < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -8.8) / 8.8) < 1e-9);
    E->setCY(RelAbsVector(400.0,500.0));
    fail_unless(fabs((E->getCX().getAbsoluteValue() -200.0) / 200.0) < 1e-9);
    fail_unless(fabs((E->getCX().getRelativeValue() -300.0) / 300.0) < 1e-9);
    fail_unless(fabs((E->getCY().getAbsoluteValue() -400.0) / 400.0) < 1e-9);
    fail_unless(fabs((E->getCY().getRelativeValue() -500.0) / 500.0) < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(E->getCZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -8.8) / 8.8) < 1e-9);
    E->setCZ(RelAbsVector(600.0,700.0));
    fail_unless(fabs((E->getCX().getAbsoluteValue() -200.0) / 200.0) < 1e-9);
    fail_unless(fabs((E->getCX().getRelativeValue() -300.0) / 300.0) < 1e-9);
    fail_unless(fabs((E->getCY().getAbsoluteValue() -400.0) / 400.0) < 1e-9);
    fail_unless(fabs((E->getCY().getRelativeValue() -500.0) / 500.0) < 1e-9);
    fail_unless(fabs((E->getCZ().getAbsoluteValue() -600.0) / 600.0) < 1e-9);
    fail_unless(fabs((E->getCZ().getRelativeValue() -700.0) / 700.0) < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -8.8) / 8.8) < 1e-9);
    // setCenter3D
    E->setCenter3D(RelAbsVector(51.51,52.52),RelAbsVector(53.53,54.54),RelAbsVector(55.55,56.56));
    fail_unless(fabs((E->getCX().getAbsoluteValue() -51.51) / 51.51) < 1e-9);
    fail_unless(fabs((E->getCX().getRelativeValue() -52.52) / 52.52) < 1e-9);
    fail_unless(fabs((E->getCY().getAbsoluteValue() -53.53) / 53.53) < 1e-9);
    fail_unless(fabs((E->getCY().getRelativeValue() -54.54) / 54.54) < 1e-9);
    fail_unless(fabs((E->getCZ().getAbsoluteValue() -55.55) / 55.55) < 1e-9);
    fail_unless(fabs((E->getCZ().getRelativeValue() -56.56) / 56.56) < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -8.8) / 8.8) < 1e-9);
    // setCenter2D
    E->setCenter2D(RelAbsVector(53.53,54.54),RelAbsVector(55.55,56.56));
    fail_unless(fabs((E->getCX().getAbsoluteValue() -53.53) / 53.53) < 1e-9);
    fail_unless(fabs((E->getCX().getRelativeValue() -54.54) / 54.54) < 1e-9);
    fail_unless(fabs((E->getCY().getAbsoluteValue() -55.55) / 55.55) < 1e-9);
    fail_unless(fabs((E->getCY().getRelativeValue() -56.56) / 56.56) < 1e-9);
    fail_unless(fabs((E->getRX().getAbsoluteValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((E->getRX().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((E->getRY().getAbsoluteValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((E->getRY().getRelativeValue() -8.8) / 8.8) < 1e-9);
    fail_unless(E->getCZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((E->getCZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);

    fail_unless(E->isSetRatio() == false);
    fail_unless(util_isNaN(E->getRatio()));
    fail_unless(E->setRatio(1.0) == LIBSBML_OPERATION_SUCCESS);
    fail_unless(E->isSetRatio() == true);
    fail_unless(E->getRatio() == 1.0);
    fail_unless(E->unsetRatio() == LIBSBML_OPERATION_SUCCESS);
    fail_unless(E->isSetRatio() == false);
    fail_unless(util_isNaN(E->getRatio()));
}
END_TEST 


START_TEST ( test_Ellipse_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<ellipse cx=\"5.0\" cy=\"20.0\" rx=\"17.3\" >\n"
                  "</ellipse>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  Ellipse e(*pNode);
  fail_unless(!e.isSetMatrix());
  fail_unless(!e.isSetStroke());
  fail_unless(!e.isSetStrokeWidth());
  fail_unless(!e.isSetDashArray());
  fail_unless(!e.isSetFillColor());
  fail_unless(!e.isSetFillRule());
  fail_unless(fabs((e.getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e.getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e.getCY().getRelativeValue()  < 1e-9);
  fail_unless(e.getCZ().getAbsoluteValue()  < 1e-9);
  fail_unless(e.getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e.getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRY().getAbsoluteValue() - e.getRX().getAbsoluteValue()) / e.getRX().getAbsoluteValue()) < 1e-9);
  fail_unless(e.getRY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<ellipse cx=\"5.0\" cy=\"20.0\" cz=\"12.4\"\n"
                  "         rx=\"17.3\" ry=\"3.6\">\n"
                  "</ellipse>\n"
                ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  e = Ellipse(*pNode);
  fail_unless(!e.isSetMatrix());
  fail_unless(!e.isSetStroke());
  fail_unless(!e.isSetStrokeWidth());
  fail_unless(!e.isSetDashArray());
  fail_unless(!e.isSetFillColor());
  fail_unless(!e.isSetFillRule());
  fail_unless(fabs((e.getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e.getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e.getCY().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getCZ().getAbsoluteValue() - 12.4) / 12.4) < 1e-9);
  fail_unless(e.getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e.getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRY().getAbsoluteValue() - 3.6) / 3.6) < 1e-9);
  fail_unless(e.getRY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;

  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<ellipse transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "         cx=\"5.0\" cy=\"20.0\" rx=\"17.3\">\n" 
      "</ellipse>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  e = Ellipse(*pNode);
  fail_unless(e.isSetMatrix());
  const double* pMatrix=e.getMatrix2D();
  fail_unless(pMatrix != NULL);
  fail_unless(fabs((pMatrix[0] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(fabs((pMatrix[1] - 0.173648) / 0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[2] - -0.173648) / -0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[3] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(pMatrix[4] < 1e-9);
  fail_unless(pMatrix[5] < 1e-9);
  fail_unless(!e.isSetStroke());
  fail_unless(!e.isSetStrokeWidth());
  fail_unless(!e.isSetDashArray());
  fail_unless(!e.isSetFillColor());
  fail_unless(!e.isSetFillRule());
  fail_unless(fabs((e.getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e.getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e.getCY().getRelativeValue()  < 1e-9);
  fail_unless(e.getCZ().getAbsoluteValue()  < 1e-9);
  fail_unless(e.getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e.getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRY().getAbsoluteValue() - e.getRX().getAbsoluteValue()) / e.getRX().getAbsoluteValue()) < 1e-9);

  delete pNode;
  delete pStream;

  // 1D attributes (stroke, stroke_width, stroke-dasharray  

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<ellipse stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\"\n"
      "         cx=\"5.0\" cy=\"20.0\" rx=\"17.3\">\n" 
      "</ellipse>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  e = Ellipse(*pNode);
  fail_unless(!e.isSetMatrix());
  fail_unless(e.isSetStroke());
  fail_unless(e.getStroke() == "#00FF00");
  fail_unless(e.isSetStrokeWidth());
  fail_unless(fabs((e.getStrokeWidth() - 3.0) / 3.0) < 1e-9);
  fail_unless(e.isSetDashArray());
  const std::vector<unsigned int>& array = e.getDashArray();
  fail_unless(array.size() == 2);
  fail_unless(array[0] == 32);
  fail_unless(array[1] == 20);
  fail_unless(!e.isSetFillColor());
  fail_unless(!e.isSetFillRule());
  fail_unless(fabs((e.getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e.getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e.getCY().getRelativeValue()  < 1e-9);
  fail_unless(e.getCZ().getAbsoluteValue()  < 1e-9);
  fail_unless(e.getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e.getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRY().getAbsoluteValue() - e.getRX().getAbsoluteValue()) / e.getRX().getAbsoluteValue()) < 1e-9);
  fail_unless(e.getRY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;

  // 2d attributes (fill, fill-rule)

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<ellipse fill=\"#DDFF00\" fill-rule=\"evenodd\"\n"
      "         cx=\"5.0\" cy=\"20.0\" rx=\"17.3\">\n" 
      "</ellipse>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  e = Ellipse(*pNode);
  fail_unless(!e.isSetMatrix());
  fail_unless(!e.isSetStroke());
  fail_unless(!e.isSetStrokeWidth());
  fail_unless(!e.isSetDashArray());
  fail_unless(e.isSetFillColor());
  fail_unless(e.getFillColor() == "#DDFF00");
  fail_unless(e.isSetFillRule());
  fail_unless(e.getFillRule() == GraphicalPrimitive2D::EVENODD);
  fail_unless(fabs((e.getCX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(e.getCX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getCY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(e.getCY().getRelativeValue()  < 1e-9);
  fail_unless(e.getCZ().getAbsoluteValue()  < 1e-9);
  fail_unless(e.getCZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRX().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(e.getRX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((e.getRY().getAbsoluteValue() - e.getRX().getAbsoluteValue()) / e.getRX().getAbsoluteValue()) < 1e-9);
  fail_unless(e.getRY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_Ellipse_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<ellipse cx=\"5\" cy=\"20\" rx=\"17.3\" />\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  Ellipse* pE = new Ellipse(*pNode1);
  fail_unless(pE != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pE;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<ellipse cx=\"5\" cy=\"20\" cz=\"12.4\"\n"
                  "         rx=\"17.3\" ry=\"3.6\" />\n"
                ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pE = new Ellipse(*pNode1);
  fail_unless(pE != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pE;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<ellipse transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "         cx=\"5\" cy=\"20\" rx=\"17.3\" />\n" 
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pE = new Ellipse(*pNode1);
  fail_unless(pE != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pE;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<ellipse stroke=\"#00ff00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\"\n"
      "         cx=\"5\" cy=\"20\" rx=\"17.3\" />\n" 
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pE = new Ellipse(*pNode1);
  fail_unless(pE != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pE;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<ellipse fill=\"#ddff00\" fill-rule=\"evenodd\"\n"
      "         cx=\"5\" cy=\"20\" rx=\"17.3\" />\n" 
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pE = new Ellipse(*pNode1);
  fail_unless(pE != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pE->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pE;

}
END_TEST

Suite *
create_suite_Ellipse (void)
{
  Suite *suite = suite_create("Ellipse");
  TCase *tcase = tcase_create("Ellipse");


  tcase_add_checked_fixture( tcase,
                             EllipseTest_setup,
                             EllipseTest_teardown );

  tcase_add_test( tcase, test_Ellipse_setters );
  tcase_add_test( tcase, test_Ellipse_read    );
  tcase_add_test( tcase, test_Ellipse_write   );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
