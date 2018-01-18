//
// Filename    : TestRadialGradient.cpp
// Description : Tests for the RadialGradient class
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

#include <RadialGradient.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static RadialGradient *G;
static RenderPkgNamespaces *renderns;

void
RadialGradientTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    G = new (std::nothrow) RadialGradient(renderns);

    if (G == NULL)
    {
        fail("new(std::nothrow)RadialGradient(renderns) returned a NULL pointer.");
    }

}

void 
RadialGradientTest_teardown (void)
{
    delete G;
    delete renderns;
}

START_TEST (test_RadialGradient_setters )
{
    fail_unless(G->getCenterX().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(G->getCenterY().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(G->getCenterZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getCenterZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(G->getRadius().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(G->getFocalPointX().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - G->getCenterX().getRelativeValue()) / G->getCenterX().getRelativeValue()) < 1e-9);
    fail_unless(G->getFocalPointY().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() - G->getCenterY().getRelativeValue()) / G->getCenterY().getRelativeValue()) < 1e-9);
    fail_unless(G->getFocalPointZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getRelativeValue() - G->getCenterZ().getRelativeValue()) / G->getCenterZ().getRelativeValue()) < 1e-9);
    G->setCoordinates(RelAbsVector(1.1,2.2),RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6)
                     ,RelAbsVector(13.13,14.14)
                     ,RelAbsVector(7.7,8.8),RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12)
                     );
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() -1.1) / 1.1) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() -2.2) / 2.2) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() -3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() -4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterZ().getAbsoluteValue() -5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterZ().getRelativeValue() -6.6) / 6.6) < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() -13.13) / 13.13) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() -14.14) / 14.14) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() -7.7) / 7.7) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() -8.8) / 8.8) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() -9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() -10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getAbsoluteValue() -11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getRelativeValue() -12.12) / 12.12) < 1e-9);
    G->setCoordinates(RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6)
                     ,RelAbsVector(17.17,18.18)
                     ,RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12)
                     );
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(G->getCenterZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getCenterZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() - 17.17) / 17.17) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 18.18) / 18.18) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - 10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() - 11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() - 12.12) / 12.12) < 1e-9);
    fail_unless(G->getFocalPointZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    G->setCenter(RelAbsVector(1.1,2.2),RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6));
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterZ().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterZ().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() - 17.17) / 17.17) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 18.18) / 18.18) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - 10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() - 11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() -12.12) / 12.12) < 1e-9);
    fail_unless(G->getFocalPointZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    G->setCenter(RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6));
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(G->getCenterZ().getAbsoluteValue() < 1e-9);
    fail_unless(G->getCenterZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() - 17.17) / 17.17) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 18.18) / 18.18) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - 10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() - 11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() - 12.12) / 12.12) < 1e-9);
    fail_unless(G->getFocalPointZ().getAbsoluteValue() < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
    G->setFocalPoint(RelAbsVector(1.1,2.2),RelAbsVector(3.3,4.4),RelAbsVector(5.5,6.6));
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(G->getCenterZ().getAbsoluteValue() < 1e-9);
    fail_unless(G->getCenterZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() - 17.17) / 17.17) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 18.18) / 18.18) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getFocalPointZ().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    G->setFocalPoint(RelAbsVector(9.9,10.10),RelAbsVector(11.11,12.12));
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(G->getCenterZ().getAbsoluteValue() < 1e-9);
    fail_unless(G->getCenterZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() - 17.17) / 17.17) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 18.18) / 18.18) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - 10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() - 11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() - 12.12) / 12.12) < 1e-9);
    fail_unless(G->getFocalPointZ().getAbsoluteValue() < 1e-9);
    fail_unless(G->getFocalPointZ().getRelativeValue() < 1e-9);
    G->setRadius(RelAbsVector(20.20,21.21));
    fail_unless(fabs((G->getCenterX().getAbsoluteValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((G->getCenterX().getRelativeValue() - 4.4) / 4.4) < 1e-9);
    fail_unless(fabs((G->getCenterY().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((G->getCenterY().getRelativeValue() - 6.6) / 6.6) < 1e-9);
    fail_unless(G->getCenterZ().getAbsoluteValue() < 1e-9);
    fail_unless(G->getCenterZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((G->getRadius().getAbsoluteValue() - 20.20) / 20.20) < 1e-9);
    fail_unless(fabs((G->getRadius().getRelativeValue() - 21.21) / 21.21) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getAbsoluteValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((G->getFocalPointX().getRelativeValue() - 10.10) / 10.10) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getAbsoluteValue() - 11.11) / 11.11) < 1e-9);
    fail_unless(fabs((G->getFocalPointY().getRelativeValue() - 12.12) / 12.12) < 1e-9);
    fail_unless(G->getFocalPointZ().getAbsoluteValue() < 1e-9);
    fail_unless(G->getFocalPointZ().getRelativeValue() < 1e-9);
}
END_TEST 


START_TEST ( test_RadialGradient_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<radialGradient id=\"gradient\">\n"
                  "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
                  "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
                  "</radialGradient>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  RadialGradient r(*pNode, 4);
  fail_unless(r.isSetId());
  fail_unless(r.getId() == "gradient");
  fail_unless(r.getSpreadMethod() == GradientBase::PAD);
  fail_unless(r.getNumGradientStops() == 2);
  fail_unless(r.getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(r.getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(r.getGradientStop(1)->getStopColor() == "#0000FF");
  // radial gradient attributes 
  fail_unless(r.getCenterX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getCenterX().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(r.getCenterY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getCenterY().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(r.getCenterZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getCenterZ().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(r.getFocalPointX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointX().getRelativeValue() - r.getCenterX().getRelativeValue()) / r.getCenterX().getRelativeValue()) < 1e-9);
  fail_unless(r.getFocalPointY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointY().getRelativeValue() - r.getCenterY().getRelativeValue()) / r.getCenterY().getRelativeValue()) < 1e-9);
  fail_unless(r.getFocalPointZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointZ().getRelativeValue() - r.getCenterZ().getRelativeValue()) / r.getCenterZ().getRelativeValue()) < 1e-9);
  fail_unless(r.getRadius().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getRadius().getRelativeValue() - 50.0) / 50.0) < 1e-9);

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<radialGradient id=\"gradient\" spreadMethod=\"reflect\"\n"
      "                cx=\"3.0\" cy=\"4.0\" cz=\"5.0\"\n"
      "                fx=\"6.0\" fy=\"7.0\" fz=\"8.0\" r=\"9.0\">\n"
      "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
      "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
      "</radialGradient>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  r = RadialGradient(*pNode, 4);
  fail_unless(r.isSetId());
  fail_unless(r.getId() == "gradient");
  fail_unless(r.getSpreadMethod() == GradientBase::REFLECT);
  fail_unless(r.getNumGradientStops() == 2);
  fail_unless(r.getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(r.getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(r.getGradientStop(1)->getStopColor() == "#0000FF");
  // radial gradient attributes 
  fail_unless(fabs((r.getCenterX().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
  fail_unless(r.getCenterX().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getCenterY().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
  fail_unless(r.getCenterY().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getCenterZ().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getCenterZ().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointX().getAbsoluteValue() - 6.0) / 6.0) < 1e-9);
  fail_unless(r.getFocalPointX().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointY().getAbsoluteValue() - 7.0) / 7.0) < 1e-9);
  fail_unless(r.getFocalPointY().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointZ().getAbsoluteValue() - 8.0) / 8.0) < 1e-9);
  fail_unless(r.getFocalPointZ().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getRadius().getAbsoluteValue() - 9.0) / 9.0) < 1e-9);
  fail_unless(r.getRadius().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<radialGradient id=\"gradient\" spreadMethod=\"reflect\"\n"
      "                cx=\"3.0\" cy=\"4.0\" cz=\"5.0\" r=\"9.0\">\n"
      "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
      "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
      "</radialGradient>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional focal point attributes
  r = RadialGradient(*pNode, 4);
  fail_unless(r.isSetId());
  fail_unless(r.getId() == "gradient");
  fail_unless(r.getSpreadMethod() == GradientBase::REFLECT);
  fail_unless(r.getNumGradientStops() == 2);
  fail_unless(r.getGradientStop(0)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getGradientStop(0)->getOffset().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(r.getGradientStop(0)->getStopColor() == "#FF0000");
  fail_unless(r.getGradientStop(1)->getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((r.getGradientStop(1)->getOffset().getRelativeValue() - 80.0) / 80.0) < 1e-9);
  fail_unless(r.getGradientStop(1)->getStopColor() == "#0000FF");
  // radial gradient attributes 
  fail_unless(fabs((r.getCenterX().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
  fail_unless(r.getCenterX().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getCenterY().getAbsoluteValue() - 4.0) / 4.0) < 1e-9);
  fail_unless(r.getCenterY().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getCenterZ().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(r.getCenterZ().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointX().getAbsoluteValue() - r.getCenterX().getAbsoluteValue()) / r.getCenterX().getAbsoluteValue()) < 1e-9);
  fail_unless(r.getFocalPointX().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointY().getAbsoluteValue() - r.getCenterY().getAbsoluteValue()) / r.getCenterY().getAbsoluteValue()) < 1e-9);
  fail_unless(r.getFocalPointY().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getFocalPointZ().getAbsoluteValue() - r.getCenterZ().getAbsoluteValue()) / r.getCenterZ().getAbsoluteValue()) < 1e-9);
  fail_unless(r.getFocalPointZ().getRelativeValue() < 1e-9);
  fail_unless(fabs((r.getRadius().getAbsoluteValue() - 9.0) / 9.0) < 1e-9);
  fail_unless(r.getRadius().getRelativeValue() < 1e-9);

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_RadialGradient_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<radialGradient id=\"gradient\">\n"
                  "  <stop offset=\"20%\" stop-color=\"#FF0000\"/>\n"
                  "  <stop offset=\"80%\" stop-color=\"#0000FF\"/>\n"
                  "</radialGradient>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  RadialGradient* pRG = new RadialGradient(*pNode1, 4);
  fail_unless(pRG != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pRG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pRG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<radialGradient id=\"gradient\" spreadMethod=\"reflect\"\n"
      "                cx=\"3\" cy=\"4\" cz=\"5\"\n"
      "                fx=\"6\" fy=\"7\" fz=\"8\" r=\"9\">\n"
      "  <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
      "  <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
      "</radialGradient>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pRG = new RadialGradient(*pNode1, 4);
  fail_unless(pRG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pRG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pRG;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<radialGradient id=\"gradient\" spreadMethod=\"reflect\"\n"
      "                cx=\"3\" cy=\"4\" cz=\"5\" r=\"9\">\n"
      "  <stop offset=\"20%\" stop-color=\"#ff0000\"/>\n"
      "  <stop offset=\"80%\" stop-color=\"#0000ff\"/>\n"
      "</radialGradient>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pRG = new RadialGradient(*pNode1, 4);
  fail_unless(pRG != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pRG->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pRG;

}
END_TEST

Suite *
create_suite_RadialGradient (void)
{
  Suite *suite = suite_create("RadialGradient");
  TCase *tcase = tcase_create("RadialGradient");


  tcase_add_checked_fixture( tcase,
                             RadialGradientTest_setup,
                             RadialGradientTest_teardown );

  tcase_add_test( tcase, test_RadialGradient_setters );
  tcase_add_test( tcase, test_RadialGradient_read    );
  tcase_add_test( tcase, test_RadialGradient_write   );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
