//
// Filename    : TestImage.cpp
// Description : Tests for the Image class
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

#include <Image.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Image *I;
static RenderPkgNamespaces *renderns;

void
ImageTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    I = new (std::nothrow) Image(renderns);

    if (I == NULL)
    {
        fail("new(std::nothrow)Image(renderns) returned a NULL pointer.");
    }

}

void 
ImageTest_teardown (void)
{
    delete I;
    delete renderns;
}

START_TEST (test_Image_setters )
{
    fail_unless(I->getX().getAbsoluteValue() < 1e-9);
    fail_unless(I->getX().getRelativeValue() < 1e-9);
    fail_unless(I->getY().getAbsoluteValue() < 1e-9);
    fail_unless(I->getY().getRelativeValue() < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(I->getWidth().getAbsoluteValue() < 1e-9);
    fail_unless(I->getWidth().getRelativeValue() < 1e-9);
    fail_unless(I->getHeight().getAbsoluteValue() < 1e-9);
    fail_unless(I->getHeight().getRelativeValue() < 1e-9);
    // setDimensions
    I->setDimensions(RelAbsVector(1.1,3.3),RelAbsVector(5.5,7.7));
    fail_unless(I->getX().getAbsoluteValue() < 1e-9);
    fail_unless(I->getX().getRelativeValue() < 1e-9);
    fail_unless(I->getY().getAbsoluteValue() < 1e-9);
    fail_unless(I->getY().getRelativeValue() < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 1.1) / 1.1) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 3.3) / 3.3) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 7.7) / 7.7) < 1e-9);
    // setWidth
    I->setWidth(RelAbsVector(2.2,9.9));
    fail_unless(I->getX().getAbsoluteValue() < 1e-9);
    fail_unless(I->getX().getRelativeValue() < 1e-9);
    fail_unless(I->getY().getAbsoluteValue() < 1e-9);
    fail_unless(I->getY().getRelativeValue() < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 5.5) / 5.5) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 7.7) / 7.7) < 1e-9);
    // setHeight
    I->setHeight(RelAbsVector(22.22,99.99));
    fail_unless(I->getX().getAbsoluteValue() < 1e-9);
    fail_unless(I->getX().getRelativeValue() < 1e-9);
    fail_unless(I->getY().getAbsoluteValue() < 1e-9);
    fail_unless(I->getY().getRelativeValue() < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    // setX
    // setY
    // setZ
    I->setX(RelAbsVector(200.0,300.0));
    fail_unless(fabs((I->getX().getAbsoluteValue() - 200.0) / 200.0) < 1e-9);
    fail_unless(fabs((I->getX().getRelativeValue() - 300.0) / 300.0) < 1e-9);
    fail_unless(I->getY().getAbsoluteValue() < 1e-9);
    fail_unless(I->getY().getRelativeValue() < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    I->setY(RelAbsVector(400.0,500.0));
    fail_unless(fabs((I->getX().getAbsoluteValue() - 200.0) / 200.0) < 1e-9);
    fail_unless(fabs((I->getX().getRelativeValue() - 300.0) / 300.0) < 1e-9);
    fail_unless(fabs((I->getY().getAbsoluteValue() - 400.0) / 400.0) < 1e-9);
    fail_unless(fabs((I->getY().getRelativeValue() - 500.0) / 500.0) < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    I->setZ(RelAbsVector(600.0,700.0));
    fail_unless(fabs((I->getX().getAbsoluteValue() - 200.0) / 200.0) < 1e-9);
    fail_unless(fabs((I->getX().getRelativeValue() - 300.0) / 300.0) < 1e-9);
    fail_unless(fabs((I->getY().getAbsoluteValue() - 400.0) / 400.0) < 1e-9);
    fail_unless(fabs((I->getY().getRelativeValue() - 500.0) / 500.0) < 1e-9);
    fail_unless(fabs((I->getZ().getAbsoluteValue() - 600.0) / 600.0) < 1e-9);
    fail_unless(fabs((I->getZ().getRelativeValue() - 700.0) / 700.0) < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
    // setCoordinates
    I->setCoordinates(RelAbsVector(51.51,52.52),RelAbsVector(53.53,54.54),RelAbsVector(55.55,56.56));
    fail_unless(fabs((I->getX().getAbsoluteValue() - 51.51) / 51.51) < 1e-9);
    fail_unless(fabs((I->getX().getRelativeValue() - 52.52) / 52.52) < 1e-9);
    fail_unless(fabs((I->getY().getAbsoluteValue() - 53.53) / 53.53) < 1e-9);
    fail_unless(fabs((I->getY().getRelativeValue() - 54.54) / 54.54) < 1e-9);
    fail_unless(fabs((I->getZ().getAbsoluteValue() - 55.55) / 55.55) < 1e-9);
    fail_unless(fabs((I->getZ().getRelativeValue() - 56.56) / 56.56) < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);

    // setCoordinates (2 values)
    I->setCoordinates(RelAbsVector(111.111,222.222),RelAbsVector(333.333,444.444));
    fail_unless(fabs((I->getX().getAbsoluteValue() - 111.111) / 111.111) < 1e-9);
    fail_unless(fabs((I->getX().getRelativeValue() - 222.222) / 222.222) < 1e-9);
    fail_unless(fabs((I->getY().getAbsoluteValue() - 333.333) / 333.333) < 1e-9);
    fail_unless(fabs((I->getY().getRelativeValue() - 444.444) / 444.444) < 1e-9);
    fail_unless(I->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(I->getZ().getRelativeValue() < 1e-9);
    fail_unless(fabs((I->getWidth().getAbsoluteValue() - 2.2) / 2.2) < 1e-9);
    fail_unless(fabs((I->getWidth().getRelativeValue() - 9.9) / 9.9) < 1e-9);
    fail_unless(fabs((I->getHeight().getAbsoluteValue() - 22.22) / 22.22) < 1e-9);
    fail_unless(fabs((I->getHeight().getRelativeValue() - 99.99) / 99.99) < 1e-9);
}
END_TEST 

START_TEST (test_Image_id )
{
    fail_unless(!I->isSetId());
    fail_unless(I->getId() == "");
    I->setId("MyImage");
    fail_unless(I->isSetId());
    fail_unless(I->getId() == "MyImage");
    I->unsetId();
    fail_unless(!I->isSetId());
    fail_unless(I->getId() == "");
}
END_TEST

START_TEST (test_Image_href )
{
    fail_unless(!I->isSetImageReference());
    fail_unless(I->getImageReference() == "");
    I->setImageReference("/home/myhome/test.jpg");
    fail_unless(I->isSetImageReference());
    fail_unless(I->getImageReference() == "/home/myhome/test.jpg");
    I->setImageReference("");
    fail_unless(!I->isSetImageReference());
    fail_unless(I->getImageReference() == "");
}
END_TEST

START_TEST ( test_Image_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<image x=\"20%\" y=\"30%\" width=\"60%\" height=\"50%\" href=\"bird_small.png\">\n"
                  "</image>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // no transformation
  Image i(*pNode);
  fail_unless(!i.isSetMatrix());
  fail_unless(i.getX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getX().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(i.getY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getY().getRelativeValue() - 30.0) / 30.0) < 1e-9);
  fail_unless(i.getZ().getAbsoluteValue() < 1e-9);
  fail_unless(i.getZ().getRelativeValue() < 1e-9);
  fail_unless(i.getWidth().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getWidth().getRelativeValue() - 60.0) / 60.0) < 1e-9);
  fail_unless(i.getHeight().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getHeight().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(i.isSetImageReference());
  fail_unless(i.getImageReference() == "bird_small.png");

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<image x=\"20%\" y=\"30%\" z=\"40%\" width=\"60%\" height=\"50%\" href=\"bird_small.png\">\n"
                  "</image>\n"
                ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // no transformation
  i= Image(*pNode);
  fail_unless(!i.isSetMatrix());
  fail_unless(i.getX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getX().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(i.getY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getY().getRelativeValue() - 30.0) / 30.0) < 1e-9);
  fail_unless(i.getZ().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getZ().getRelativeValue() - 40.0) / 40.0) < 1e-9);
  fail_unless(i.getWidth().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getWidth().getRelativeValue() - 60.0) / 60.0) < 1e-9);
  fail_unless(i.getHeight().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getHeight().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(i.isSetImageReference());
  fail_unless(i.getImageReference() == "bird_small.png");

  delete pNode;
  delete pStream;


  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<image transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "       x=\"20%\" y=\"30%\" width=\"60%\" height=\"50%\" href=\"bird_small.png\">\n"
      "</image>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  i = Image(*pNode);
  fail_unless(i.isSetMatrix());
  const double* pMatrix=i.getMatrix2D();
  fail_unless(pMatrix != NULL);
  fail_unless(fabs((pMatrix[0] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(fabs((pMatrix[1] - 0.173648) / 0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[2] - -0.173648) / -0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[3] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(pMatrix[4] < 1e-9);
  fail_unless(pMatrix[5] < 1e-9);
  fail_unless(i.getX().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getX().getRelativeValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(i.getY().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getY().getRelativeValue() - 30.0) / 30.0) < 1e-9);
  fail_unless(i.getZ().getAbsoluteValue() < 1e-9);
  fail_unless(i.getZ().getRelativeValue() < 1e-9);
  fail_unless(i.getWidth().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getWidth().getRelativeValue() - 60.0) / 60.0) < 1e-9);
  fail_unless(i.getHeight().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((i.getHeight().getRelativeValue() - 50.0) / 50.0) < 1e-9);
  fail_unless(i.isSetImageReference());
  fail_unless(i.getImageReference() == "bird_small.png");

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_Image_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<image x=\"20%\" y=\"30%\" width=\"60%\" height=\"50%\" href=\"bird_small.png\" />\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  Image* pI = new Image(*pNode1);
  fail_unless(pI != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pI->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pI;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<image x=\"20%\" y=\"30%\" z=\"40%\" width=\"60%\" height=\"50%\" href=\"bird_small.png\" />\n"
                ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pI = new Image(*pNode1);
  fail_unless(pI != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pI->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pI;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<image transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "       x=\"20%\" y=\"30%\" width=\"60%\" height=\"50%\" href=\"bird_small.png\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pI = new Image(*pNode1);
  fail_unless(pI != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pI->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pI;

}
END_TEST

Suite *
create_suite_Image (void)
{
  Suite *suite = suite_create("Image");
  TCase *tcase = tcase_create("Image");


  tcase_add_checked_fixture( tcase,
                             ImageTest_setup,
                             ImageTest_teardown );

  tcase_add_test( tcase, test_Image_setters );
  tcase_add_test( tcase, test_Image_id      );
  tcase_add_test( tcase, test_Image_href    );
  tcase_add_test( tcase, test_Image_read    );
  tcase_add_test( tcase, test_Image_write   );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
