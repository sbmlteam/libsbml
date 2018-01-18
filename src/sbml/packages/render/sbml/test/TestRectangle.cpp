//
// Filename    : TestRectangle.cpp
// Description : Tests for the Rectangle class
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

#include <Rectangle.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Rectangle *R;
static RenderPkgNamespaces *renderns;

void
RectangleTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    R = new (std::nothrow) Rectangle(renderns);

    if (R == NULL)
    {
        fail("new(std::nothrow)Rectangle(renderns) returned a NULL pointer.");
    }

}

void 
RectangleTest_teardown (void)
{
    delete R;
    delete renderns;
}

START_TEST (test_Rectangle_setters )
{
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(R->getRadiusX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getRadiusX().getRelativeValue() < 1e-9);
    fail_unless(R->getRadiusY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getRadiusY().getRelativeValue() < 1e-9);
    fail_unless(R->getWidth().getAbsoluteValue() < 1e-9);
    fail_unless(R->getWidth().getRelativeValue() < 1e-9);
    fail_unless(R->getHeight().getAbsoluteValue() < 1e-9);
    fail_unless(R->getHeight().getRelativeValue() < 1e-9);
    // setRadii
    R->setRadii(RelAbsVector(3.3,5.5),RelAbsVector(7.7,9.9));
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 7.7) / 7.7) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(R->getWidth().getAbsoluteValue() < 1e-9);
    fail_unless(R->getWidth().getRelativeValue() < 1e-9);
    fail_unless(R->getHeight().getAbsoluteValue() < 1e-9);
    fail_unless(R->getHeight().getRelativeValue() < 1e-9);
    // setRadiusX
    R->setRadiusX(RelAbsVector(2.2,4.4));
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 7.7) / 7.7) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(R->getWidth().getAbsoluteValue() < 1e-9);
    fail_unless(R->getWidth().getRelativeValue() < 1e-9);
    fail_unless(R->getHeight().getAbsoluteValue() < 1e-9);
    fail_unless(R->getHeight().getRelativeValue() < 1e-9);
    // setRadiusY
    R->setRadiusY(RelAbsVector(6.6,8.8));
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(R->getWidth().getAbsoluteValue() < 1e-9);
    fail_unless(R->getWidth().getRelativeValue() < 1e-9);
    fail_unless(R->getHeight().getAbsoluteValue() < 1e-9);
    fail_unless(R->getHeight().getRelativeValue() < 1e-9);
    // setSize
    R->setSize(RelAbsVector(1.1,3.3),RelAbsVector(5.5,7.7));
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 7.7) / 7.7) < 1e-9);
    // setWidth
    R->setWidth(RelAbsVector(2.2,9.9));
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 7.7) / 7.7) < 1e-9);
    // setHeight
    R->setHeight(RelAbsVector(22.22,99.99));
    fail_unless(R->getX().getAbsoluteValue() < 1e-9);
    fail_unless(R->getX().getRelativeValue() < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    // setX
    // setY
    // setZ
    R->setX(RelAbsVector(200.0,300.0));
    fail_unless(fabs((R->getX().getAbsoluteValue() - 200.0) / 200.0) < 1e-9);
    fail_unless(fabs((R->getX().getRelativeValue() - 300.0) / 300.0) < 1e-9);
    fail_unless(R->getY().getAbsoluteValue() < 1e-9);
    fail_unless(R->getY().getRelativeValue() < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    R->setY(RelAbsVector(400.0,500.0));
    fail_unless(fabs((R->getX().getAbsoluteValue() - 200.0) / 200.0) < 1e-9);
    fail_unless(fabs((R->getX().getRelativeValue() - 300.0) / 300.0) < 1e-9);
    fail_unless(fabs((R->getY().getAbsoluteValue() - 400.0) / 400.0) < 1e-9);
    fail_unless(fabs((R->getY().getRelativeValue() - 500.0) / 500.0) < 1e-9);
    fail_unless(R->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(R->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    R->setZ(RelAbsVector(600.0,700.0));
    fail_unless(fabs((R->getX().getAbsoluteValue() - 200.0) / 200.0) < 1e-9);
    fail_unless(fabs((R->getX().getRelativeValue() - 300.0) / 300.0) < 1e-9);
    fail_unless(fabs((R->getY().getAbsoluteValue() - 400.0) / 400.0) < 1e-9);
    fail_unless(fabs((R->getY().getRelativeValue() - 500.0) / 500.0) < 1e-9);
    fail_unless(fabs((R->getZ().getAbsoluteValue() - 600.0) / 600.0) < 1e-9);
    fail_unless(fabs((R->getZ().getRelativeValue() - 700.0) / 700.0) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    // setCoordinates
    R->setCoordinates(RelAbsVector(51.51,52.52),RelAbsVector(53.53,54.54),RelAbsVector(55.55,56.56));
    fail_unless(fabs((R->getX().getAbsoluteValue() - 51.51) / 51.51) < 1e-9);
    fail_unless(fabs((R->getX().getRelativeValue() - 52.52) / 52.52) < 1e-9);
    fail_unless(fabs((R->getY().getAbsoluteValue() - 53.53) / 53.53) < 1e-9);
    fail_unless(fabs((R->getY().getRelativeValue() - 54.54) / 54.54) < 1e-9);
    fail_unless(fabs((R->getZ().getAbsoluteValue() - 55.55) / 55.55) < 1e-9);
    fail_unless(fabs((R->getZ().getRelativeValue() - 56.56) / 56.56) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);

    // setCoordinatesAndSize
    R->setCoordinatesAndSize(RelAbsVector(111.111,222.222),RelAbsVector(333.333,444.444),RelAbsVector(555.555,666.666)
                            ,RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12)
                     );
    fail_unless(fabs((R->getX().getAbsoluteValue() - 111.111) / 111.111) < 1e-9);
    fail_unless(fabs((R->getX().getRelativeValue() - 222.222) / 222.222) < 1e-9);
    fail_unless(fabs((R->getY().getAbsoluteValue() - 333.333) / 333.333) < 1e-9);
    fail_unless(fabs((R->getY().getRelativeValue() - 444.444) / 444.444) < 1e-9);
    fail_unless(fabs((R->getZ().getAbsoluteValue() - 555.555) / 555.555) < 1e-9);
    fail_unless(fabs((R->getZ().getRelativeValue() - 666.666) / 666.666) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((R->getRadiusX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getAbsoluteValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((R->getRadiusY().getRelativeValue() - 8.8) / 8.8) < 1e-9);
    fail_unless(fabs((R->getWidth().getAbsoluteValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((R->getWidth().getRelativeValue() - 10.10) / 10.10) < 1e-9);
    fail_unless(fabs((R->getHeight().getAbsoluteValue() - 11.11) / 11.11) < 1e-9);
    fail_unless(fabs((R->getHeight().getRelativeValue() - 12.12) / 12.12) < 1e-9);

    fail_unless(R->isSetRatio() == false);
    fail_unless(util_isNaN(R->getRatio()));
    fail_unless(R->setRatio(1.0) == LIBSBML_OPERATION_SUCCESS);
    fail_unless(R->isSetRatio() == true);
    fail_unless(R->getRatio() == 1.0);
    fail_unless(R->unsetRatio() == LIBSBML_OPERATION_SUCCESS);
    fail_unless(R->isSetRatio() == false);
    fail_unless(util_isNaN(R->getRatio()));

}
END_TEST 


START_TEST ( test_Rectangle_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<rectangle x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\">\n"
                  "</rectangle>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  Rectangle r(*pNode);
  fail_unless(!r.isSetMatrix());
  fail_unless(!r.isSetStroke());
  fail_unless(!r.isSetStrokeWidth());
  fail_unless(!r.isSetDashArray());
  fail_unless(!r.isSetFillColor());
  fail_unless(!r.isSetFillRule());
  fail_unless(fabs((r.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getY().getRelativeValue()  < 1e-9);
  fail_unless(r.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r.getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r.getHeight().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusX().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusY().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle x=\"5.0\" y=\"20.0\" z=\"42.1\" width=\"17.3\" height=\"4.2\" rx=\"2.0\" ry=\"3.0\">\n"
      "</rectangle>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  r = Rectangle(*pNode);
  fail_unless(!r.isSetMatrix());
  fail_unless(!r.isSetStroke());
  fail_unless(!r.isSetStrokeWidth());
  fail_unless(!r.isSetDashArray());
  fail_unless(!r.isSetFillColor());
  fail_unless(!r.isSetFillRule());
  fail_unless(fabs((r.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getY().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getZ().getAbsoluteValue() -42.1) / 42.1) < 1e-9);
  fail_unless(r.getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r.getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r.getHeight().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getRadiusX().getAbsoluteValue() -2.0) / 2.0) < 1e-9);
  fail_unless(r.getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getRadiusY().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
  fail_unless(r.getRadiusY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;

  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "           x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\">\n"
      "</rectangle>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  r = Rectangle(*pNode);
  fail_unless(r.isSetMatrix());
  const double* pMatrix=r.getMatrix2D();
  fail_unless(pMatrix != NULL);
  fail_unless(fabs((pMatrix[0] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(fabs((pMatrix[1] - 0.173648) / 0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[2] - -0.173648) / -0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[3] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(pMatrix[4] < 1e-9);
  fail_unless(pMatrix[5] < 1e-9);
  fail_unless(!r.isSetStroke());
  fail_unless(!r.isSetStrokeWidth());
  fail_unless(!r.isSetDashArray());
  fail_unless(!r.isSetFillColor());
  fail_unless(!r.isSetFillRule());
  fail_unless(fabs((r.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getY().getRelativeValue()  < 1e-9);
  fail_unless(r.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r.getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r.getHeight().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusX().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusY().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;

  // 1D attributes (stroke, stroke_width, stroke-dasharray  

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\"\n"
      "           x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\">\n"
      "</rectangle>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  r = Rectangle(*pNode);
  fail_unless(!r.isSetMatrix());
  fail_unless(r.isSetStroke());
  fail_unless(r.getStroke() == "#00FF00");
  fail_unless(r.isSetStrokeWidth());
  fail_unless(fabs((r.getStrokeWidth() - 3.0) / 3.0) < 1e-9);
  fail_unless(r.isSetDashArray());
  const std::vector<unsigned int>& array = r.getDashArray();
  fail_unless(array.size() == 2);
  fail_unless(array[0] == 32);
  fail_unless(array[1] == 20);
  fail_unless(!r.isSetFillColor());
  fail_unless(!r.isSetFillRule());
  fail_unless(fabs((r.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getY().getRelativeValue()  < 1e-9);
  fail_unless(r.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r.getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r.getHeight().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusX().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusY().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;

  // 2d attributes (fill, fill-rule)

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle fill=\"#DDFF00\" fill-rule=\"evenodd\"\n"
      "           x=\"5.0\" y=\"20.0\" width=\"17.3\" height=\"4.2\">\n"
      "</rectangle>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  r = Rectangle(*pNode);
  fail_unless(!r.isSetMatrix());
  fail_unless(!r.isSetStroke());
  fail_unless(!r.isSetStrokeWidth());
  fail_unless(!r.isSetDashArray());
  fail_unless(r.isSetFillColor());
  fail_unless(r.getFillColor() == "#DDFF00");
  fail_unless(r.isSetFillRule());
  fail_unless(r.getFillRule() == GraphicalPrimitive2D::EVENODD);
  fail_unless(fabs((r.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getY().getRelativeValue()  < 1e-9);
  fail_unless(r.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getZ().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getWidth().getAbsoluteValue() - 17.3) / 17.3) < 1e-9);
  fail_unless(r.getWidth().getRelativeValue()  < 1e-9);
  fail_unless(fabs((r.getHeight().getAbsoluteValue() - 4.2) / 4.2) < 1e-9);
  fail_unless(r.getHeight().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusX().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusX().getRelativeValue()  < 1e-9);
  fail_unless(r.getRadiusY().getAbsoluteValue()  < 1e-9);
  fail_unless(r.getRadiusY().getRelativeValue()  < 1e-9);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_Rectangle_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<rectangle x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  Rectangle* pR = new Rectangle(*pNode1);
  fail_unless(pR != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pR->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pR;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle x=\"5\" y=\"20\" z=\"42.1\" width=\"17.3\" height=\"4.2\" rx=\"2\" ry=\"3\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pR = new Rectangle(*pNode1);
  fail_unless(pR != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pR->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pR;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "           x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pR = new Rectangle(*pNode1);
  fail_unless(pR != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pR->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pR;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle stroke=\"#00ff00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\"\n"
      "           x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pR = new Rectangle(*pNode1);
  fail_unless(pR != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pR->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pR;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<rectangle fill=\"#ddff00\" fill-rule=\"evenodd\"\n"
      "           x=\"5\" y=\"20\" width=\"17.3\" height=\"4.2\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pR = new Rectangle(*pNode1);
  fail_unless(pR != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pR->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pR;

}
END_TEST

Suite *
create_suite_Rectangle (void)
{
  Suite *suite = suite_create("Rectangle");
  TCase *tcase = tcase_create("Rectangle");


  tcase_add_checked_fixture( tcase,
                             RectangleTest_setup,
                             RectangleTest_teardown );

  tcase_add_test( tcase, test_Rectangle_setters );
  tcase_add_test( tcase, test_Rectangle_read    );
  tcase_add_test( tcase, test_Rectangle_write   );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
