//
// Filename    : TestColorDefinition
// Description : Tests for the ColorDefinition class
// Organization: University of Heidelberg
// Created     : 2008-07-02
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

#include <sbml/packages/render/sbml/ColorDefinition.h>
#include <sbml/packages/render/sbml/ListOfColorDefinitions.h>

#include <check.h>
#include <limits>
#include <string>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static ColorDefinition *C;
static RenderPkgNamespaces *renderns;

void
ColorDefinitionTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    C = new (std::nothrow) ColorDefinition(renderns);

    if (C == NULL)
    {
        fail("new(std::nothrow)ColorDefinition(renderns) returned a NULL pointer.");
    }

}

void 
ColorDefinitionTest_teardown (void)
{
    delete C;
    delete renderns;
}


START_TEST(test_ColorDefinition_id)
{
    fail_unless(!C->isSetId());
    fail_unless(C->getId() == "");
    C->setId("black");
    fail_unless(C->isSetId());
    fail_unless(C->getId() == "black");
    C->unsetId();
    fail_unless(!C->isSetId());
    fail_unless(C->getId() == "");
}
END_TEST

START_TEST(test_ColorDefinition_setters)
{
    fail_unless(C->getRed()==0);
    fail_unless(C->getGreen()==0);
    fail_unless(C->getBlue()==0);
    fail_unless(C->getAlpha()==255);
    C->setRed(234);
    fail_unless(C->getRed() == 234);
    fail_unless(C->getGreen()==0);
    fail_unless(C->getBlue()==0);
    fail_unless(C->getAlpha()==255);
    C->setGreen(145);
    fail_unless(C->getRed() == 234);
    fail_unless(C->getGreen()== 145);
    fail_unless(C->getBlue()==0);
    fail_unless(C->getAlpha()==255);
    C->setBlue(25);
    fail_unless(C->getRed() == 234);
    fail_unless(C->getGreen()== 145);
    fail_unless(C->getBlue()==25);
    fail_unless(C->getAlpha()==255);
    C->setAlpha(5);
    fail_unless(C->getRed() == 234);
    fail_unless(C->getGreen()== 145);
    fail_unless(C->getBlue()==25);
    fail_unless(C->getAlpha()== 5);
}
END_TEST 

START_TEST (test_ColorDefinition_setColorValue)
{
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  // valid color definitions
  fail_unless(C->setColorValue("#FFFFFFFF")==true);
  fail_unless(C->getRed()==255);
  fail_unless(C->getGreen()==255);
  fail_unless(C->getBlue()==255);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#00000000")==true);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==0);
  fail_unless(C->setColorValue("#11335577")==true);
  fail_unless(C->getRed()==17);
  fail_unless(C->getGreen()==51);
  fail_unless(C->getBlue()==85);
  fail_unless(C->getAlpha()==119);
  fail_unless(C->setColorValue("#F34A2803")==true);
  fail_unless(C->getRed()==243);
  fail_unless(C->getGreen()==74);
  fail_unless(C->getBlue()==40);
  fail_unless(C->getAlpha()==3);
  fail_unless(C->setColorValue("#892E36B1")==true);
  fail_unless(C->getRed()==137);
  fail_unless(C->getGreen()==46);
  fail_unless(C->getBlue()==54);
  fail_unless(C->getAlpha()==177);
  fail_unless(C->setColorValue("#FFFFFF")==true);
  fail_unless(C->getRed()==255);
  fail_unless(C->getGreen()==255);
  fail_unless(C->getBlue()==255);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#000000")==true);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#113355")==true);
  fail_unless(C->getRed()==17);
  fail_unless(C->getGreen()==51);
  fail_unless(C->getBlue()==85);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#F34A28")==true);
  fail_unless(C->getRed()==243);
  fail_unless(C->getGreen()==74);
  fail_unless(C->getBlue()==40);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#892E36")==true);
  fail_unless(C->getRed()==137);
  fail_unless(C->getGreen()==46);
  fail_unless(C->getBlue()==54);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#ffffffff")==true);
  fail_unless(C->getRed()==255);
  fail_unless(C->getGreen()==255);
  fail_unless(C->getBlue()==255);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#f34a2803")==true);
  fail_unless(C->getRed()==243);
  fail_unless(C->getGreen()==74);
  fail_unless(C->getBlue()==40);
  fail_unless(C->getAlpha()==3);
  fail_unless(C->setColorValue("#892e36b1")==true);
  fail_unless(C->getRed()==137);
  fail_unless(C->getGreen()==46);
  fail_unless(C->getBlue()==54);
  fail_unless(C->getAlpha()==177);
  fail_unless(C->setColorValue("#ffffff")==true);
  fail_unless(C->getRed()==255);
  fail_unless(C->getGreen()==255);
  fail_unless(C->getBlue()==255);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#f34a28")==true);
  fail_unless(C->getRed()==243);
  fail_unless(C->getGreen()==74);
  fail_unless(C->getBlue()==40);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#892e36")==true);
  fail_unless(C->getRed()==137);
  fail_unless(C->getGreen()==46);
  fail_unless(C->getBlue()==54);
  fail_unless(C->getAlpha()==255);

  // invalid color definitions
  fail_unless(C->setColorValue("#")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#2")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25D")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25DF")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25DFC")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25DFC")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25DFCL")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25DFC21")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("#25DFC2143")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("25DFC214")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
  fail_unless(C->setColorValue("§25DFC214")==false);
  fail_unless(C->getRed()==0);
  fail_unless(C->getGreen()==0);
  fail_unless(C->getBlue()==0);
  fail_unless(C->getAlpha()==255);
}
END_TEST 

START_TEST (test_ColorDefinition_setRGBA)
{
    C->setRGBA(89,3,45,231);
    fail_unless(C->getRed()==89);
    fail_unless(C->getGreen()==3);
    fail_unless(C->getBlue()==45);
    fail_unless(C->getAlpha()==231);
    C->setRGBA(34,157,201,49);
    fail_unless(C->getRed()==34);
    fail_unless(C->getGreen()==157);
    fail_unless(C->getBlue()==201);
    fail_unless(C->getAlpha()==49);
    C->setRGBA(21,155,21);
    fail_unless(C->getRed()==21);
    fail_unless(C->getGreen()==155);
    fail_unless(C->getBlue()==21);
    fail_unless(C->getAlpha()==255);
    C->setRGBA(253,92,177);
    fail_unless(C->getRed()==253);
    fail_unless(C->getGreen()==92);
    fail_unless(C->getBlue()==177);
    fail_unless(C->getAlpha()==255);
}
END_TEST

START_TEST (test_ColorDefinition_createValueString)
{
    C->setRGBA(89,3,45,231);
    fail_unless(C->createValueString()=="#59032de7");
    C->setRGBA(89,3,45,255);
    fail_unless(C->createValueString()=="#59032d");
    C->setRGBA(34,157,201,49);
    fail_unless(C->createValueString()=="#229dc931");
    C->setRGBA(21,155,21);
    fail_unless(C->createValueString()=="#159b15");
    C->setRGBA(253,92,177);
    fail_unless(C->createValueString()=="#fd5cb1");
}
END_TEST

START_TEST ( test_ColorDefinition_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<colorDefinition id=\"gray\" value=\"#C9C8C7C6\" />\n"
  ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  ColorDefinition cd(*pNode);
  fail_unless(cd.isSetId());
  fail_unless(cd.getId() == "gray");
  fail_unless(cd.getRed() == 201 );
  fail_unless(cd.getGreen() == 200 );
  fail_unless(cd.getBlue() == 199 );
  fail_unless(cd.getAlpha() == 198 );

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<colorDefinition id=\"gray\" value=\"#C9C8C7\" />\n"
     ; 

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  cd=ColorDefinition(*pNode);
  fail_unless(cd.isSetId());
  fail_unless(cd.getId() == "gray");
  fail_unless(cd.getRed() == 201 );
  fail_unless(cd.getGreen() == 200 );
  fail_unless(cd.getBlue() == 199 );
  fail_unless(cd.getAlpha() == 255 );

  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<listOfColorDefinitions>\n"
      "  <colorDefinition id=\"red\" value=\"#C90000\" />\n"
      "  <colorDefinition id=\"black\" value=\"#000000\" />\n"
      "  <colorDefinition id=\"white\" value=\"#FFFFFF\" />\n"
      "</listOfColorDefinitions>\n"
     ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  ListOfColorDefinitions l(*pNode);
  fail_unless(l.size() == 3);
  ColorDefinition* pCD = l.get(0);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "red");
  fail_unless(pCD->getRed() == 201 );
  fail_unless(pCD->getGreen() == 0 );
  fail_unless(pCD->getBlue() == 0 );
  fail_unless(pCD->getAlpha() == 255 );
  pCD = l.get(1);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "black");
  fail_unless(pCD->getRed() == 0 );
  fail_unless(pCD->getGreen() == 0 );
  fail_unless(pCD->getBlue() == 0 );
  fail_unless(pCD->getAlpha() == 255 );
  pCD = l.get(2);
  fail_unless( pCD != NULL );
  fail_unless(pCD->isSetId());
  fail_unless(pCD->getId() == "white");
  fail_unless(pCD->getRed() == 255 );
  fail_unless(pCD->getGreen() == 255 );
  fail_unless(pCD->getBlue() == 255 );
  fail_unless(pCD->getAlpha() == 255 );

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_ColorDefinition_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<colorDefinition id=\"gray\" value=\"#c9c8c7c6\" />\n"
  ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  ColorDefinition* pCD = new ColorDefinition(*pNode1);
  fail_unless(pCD != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pCD->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pCD;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<colorDefinition id=\"gray\" value=\"#c9c8c7\" />\n"
     ; 

  pStream= new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pCD = new ColorDefinition(*pNode1);
  fail_unless(pCD != NULL);
  pNode2 = new XMLNode(pCD->toXML());
  fail_unless(pNode2 != NULL);
  // compare the two nodes 
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pCD;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<listOfColorDefinitions>\n"
      "  <colorDefinition id=\"red\" value=\"#c90000\" />\n"
      "  <colorDefinition id=\"black\" value=\"#000000\" />\n"
      "  <colorDefinition id=\"white\" value=\"#ffffff\" />\n"
      "</listOfColorDefinitions>\n"
     ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  ListOfColorDefinitions* pL = new ListOfColorDefinitions(*pNode1);
  fail_unless(pL != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pL->toXML());
  fail_unless(pNode2 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pL;


}
END_TEST


Suite *
create_suite_ColorDefinition (void)
{
  Suite *suite = suite_create("ColorDefinition");
  TCase *tcase = tcase_create("ColorDefinition");


  tcase_add_checked_fixture( tcase,
                             ColorDefinitionTest_setup,
                             ColorDefinitionTest_teardown );

  tcase_add_test( tcase, test_ColorDefinition_setColorValue                );
  tcase_add_test( tcase, test_ColorDefinition_setRGBA                      );
  tcase_add_test( tcase, test_ColorDefinition_createValueString            );
  tcase_add_test( tcase, test_ColorDefinition_setters                      );
  tcase_add_test( tcase, test_ColorDefinition_id                           );
  tcase_add_test( tcase, test_ColorDefinition_read                         );
  tcase_add_test( tcase, test_ColorDefinition_write                        );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
