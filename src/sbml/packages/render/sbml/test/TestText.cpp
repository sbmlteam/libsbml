//
// Filename    : TestText.cpp
// Description : Tests for the Text class
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

#include <Text.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Text *T;
static RenderPkgNamespaces *renderns;

void
TextTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    T = new (std::nothrow) Text(renderns);

    if (T == NULL)
    {
        fail("new(std::nothrow)Text(renderns) returned a NULL pointer.");
    }

}

void 
TextTest_teardown (void)
{
    delete T;
    delete renderns;
}

START_TEST (test_Text_position )
{
    fail_unless(T->getX().getAbsoluteValue() < 1e-9);
    fail_unless(T->getX().getRelativeValue() < 1e-9);
    fail_unless(T->getY().getAbsoluteValue() < 1e-9);
    fail_unless(T->getY().getRelativeValue() < 1e-9);
    fail_unless(T->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(T->getZ().getRelativeValue() < 1e-9);
    // setX
    // setY
    // setZ
    T->setX(RelAbsVector(200.0,300.0));
    fail_unless(fabs((T->getX().getAbsoluteValue() -200.0) / 200.0) < 1e-9);
    fail_unless(fabs((T->getX().getRelativeValue() -300.0) / 300.0) < 1e-9);
    fail_unless(T->getY().getAbsoluteValue() < 1e-9);
    fail_unless(T->getY().getRelativeValue() < 1e-9);
    fail_unless(T->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(T->getZ().getRelativeValue() < 1e-9);
    T->setY(RelAbsVector(400.0,500.0));
    fail_unless(fabs((T->getX().getAbsoluteValue() -200.0) / 200.0) < 1e-9);
    fail_unless(fabs((T->getX().getRelativeValue() -300.0) / 300.0) < 1e-9);
    fail_unless(fabs((T->getY().getAbsoluteValue() -400.0) / 400.0) < 1e-9);
    fail_unless(fabs((T->getY().getRelativeValue() -500.0) / 500.0) < 1e-9);
    fail_unless(T->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(T->getZ().getRelativeValue() < 1e-9);
    T->setZ(RelAbsVector(600.0,700.0));
    fail_unless(fabs((T->getX().getAbsoluteValue() -200.0) / 200.0) < 1e-9);
    fail_unless(fabs((T->getX().getRelativeValue() -300.0) / 300.0) < 1e-9);
    fail_unless(fabs((T->getY().getAbsoluteValue() -400.0) / 400.0) < 1e-9);
    fail_unless(fabs((T->getY().getRelativeValue() -500.0) / 500.0) < 1e-9);
    fail_unless(fabs((T->getZ().getAbsoluteValue() -600.0) / 600.0) < 1e-9);
    fail_unless(fabs((T->getZ().getRelativeValue() -700.0) / 700.0) < 1e-9);
    // setCoordinates
    T->setCoordinates(RelAbsVector(51.51,52.52),RelAbsVector(53.53,54.54),RelAbsVector(55.55,56.56));
    fail_unless(fabs((T->getX().getAbsoluteValue() -51.51) / 51.51) < 1e-9);
    fail_unless(fabs((T->getX().getRelativeValue() -52.52) / 52.52) < 1e-9);
    fail_unless(fabs((T->getY().getAbsoluteValue() -53.53) / 53.53) < 1e-9);
    fail_unless(fabs((T->getY().getRelativeValue() -54.54) / 54.54) < 1e-9);
    fail_unless(fabs((T->getZ().getAbsoluteValue() -55.55) / 55.55) < 1e-9);
    fail_unless(fabs((T->getZ().getRelativeValue() -56.56) / 56.56) < 1e-9);
    // setCoordinates
    T->setCoordinates(RelAbsVector(61.61,62.62),RelAbsVector(63.63,64.64));
    fail_unless(fabs((T->getX().getAbsoluteValue() -61.61) / 61.61) < 1e-9);
    fail_unless(fabs((T->getX().getRelativeValue() -62.62) / 62.62) < 1e-9);
    fail_unless(fabs((T->getY().getAbsoluteValue() -63.63) / 63.63) < 1e-9);
    fail_unless(fabs((T->getY().getRelativeValue() -64.64) / 64.64) < 1e-9);
    fail_unless(T->getZ().getAbsoluteValue() < 1e-9);
    fail_unless(T->getZ().getRelativeValue() < 1e-9);
}
END_TEST 

START_TEST ( test_Text_FontFamily )
{
    fail_unless(! T->isSetFontFamily() );
    fail_unless(T->getFontFamily() == "");
    T->setFontFamily("Helvetica");
    fail_unless( T->isSetFontFamily() );
    fail_unless(T->getFontFamily() == "Helvetica");
    T->setFontFamily("");
    fail_unless(! T->isSetFontFamily() );
    fail_unless(T->getFontFamily() == "");
}
END_TEST


START_TEST ( test_Text_FontSize )
{
    fail_unless(!T->isSetFontSize());
    T->setFontSize(RelAbsVector(18.0,0.0));
    fail_unless(T->isSetFontSize());
    fail_unless(T->getFontSize() == RelAbsVector(18.0,0.0));
    T->setFontSize(RelAbsVector(0.0,50.0));
    fail_unless(T->isSetFontSize());
    fail_unless(T->getFontSize() == RelAbsVector(0.0,50.0));
    T->setFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN()));
    fail_unless(!T->isSetFontSize());
    T->setFontSize(RelAbsVector(23.0,std::numeric_limits<double>::quiet_NaN()));
    fail_unless(!T->isSetFontSize());
    T->setFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),75.0));
    fail_unless(!T->isSetFontSize());
}
END_TEST

START_TEST( test_Text_FontWeight )
{
    fail_unless( !T->isSetFontWeight() );
    fail_unless( T->getFontWeight() == Text::WEIGHT_UNSET );
    T->setFontWeight(Text::WEIGHT_NORMAL);
    fail_unless(T->isSetFontWeight());
    fail_unless(T->getFontWeight() == Text::WEIGHT_NORMAL);
    T->setFontWeight(Text::WEIGHT_BOLD);
    fail_unless(T->isSetFontWeight());
    fail_unless(T->getFontWeight() == Text::WEIGHT_BOLD);
    T->setFontWeight(Text::WEIGHT_UNSET);
    fail_unless( !T->isSetFontWeight());
    fail_unless(T->getFontWeight() == Text::WEIGHT_UNSET);
}
END_TEST

START_TEST( test_Text_FontStyle )
{
    fail_unless( !T->isSetFontStyle() );
    fail_unless( T->getFontStyle() == Text::STYLE_UNSET );
    T->setFontStyle(Text::STYLE_NORMAL);
    fail_unless(T->isSetFontStyle());
    fail_unless(T->getFontStyle() == Text::STYLE_NORMAL);
    T->setFontStyle(Text::STYLE_ITALIC);
    fail_unless(T->isSetFontStyle());
    fail_unless(T->getFontStyle() == Text::STYLE_ITALIC);
    T->setFontStyle(Text::STYLE_UNSET);
    fail_unless( !T->isSetFontStyle());
    fail_unless(T->getFontStyle() == Text::STYLE_UNSET);
}
END_TEST

START_TEST( test_Text_TextAnchor )
{
    fail_unless( !T->isSetTextAnchor() );
    fail_unless( T->getTextAnchor() == Text::ANCHOR_UNSET );
    T->setTextAnchor(Text::ANCHOR_START);
    fail_unless(T->isSetTextAnchor());
    fail_unless(T->getTextAnchor() == Text::ANCHOR_START);
    T->setTextAnchor(Text::ANCHOR_MIDDLE);
    fail_unless(T->isSetTextAnchor());
    fail_unless(T->getTextAnchor() == Text::ANCHOR_MIDDLE);
    T->setTextAnchor(Text::ANCHOR_END);
    fail_unless(T->isSetTextAnchor());
    fail_unless(T->getTextAnchor() == Text::ANCHOR_END);
    T->setTextAnchor(Text::ANCHOR_BASELINE);
    fail_unless( !T->isSetTextAnchor());
    fail_unless(T->getTextAnchor() == Text::ANCHOR_UNSET);
    T->setTextAnchor(Text::ANCHOR_UNSET);
    fail_unless( !T->isSetTextAnchor());
    fail_unless(T->getTextAnchor() == Text::ANCHOR_UNSET);
}
END_TEST

START_TEST( test_Text_VTextAnchor )
{
    fail_unless( !T->isSetVTextAnchor() );
    fail_unless( T->getVTextAnchor() == Text::ANCHOR_UNSET );
    T->setVTextAnchor(Text::ANCHOR_TOP);
    fail_unless(T->isSetVTextAnchor());
    fail_unless(T->getVTextAnchor() == Text::ANCHOR_TOP);
    T->setVTextAnchor(Text::ANCHOR_MIDDLE);
    fail_unless(T->isSetVTextAnchor());
    fail_unless(T->getVTextAnchor() == Text::ANCHOR_MIDDLE);
    T->setVTextAnchor(Text::ANCHOR_BOTTOM);
    fail_unless(T->isSetVTextAnchor());
    fail_unless(T->getVTextAnchor() == Text::ANCHOR_BOTTOM);
    T->setVTextAnchor(Text::ANCHOR_BASELINE);
    fail_unless(T->isSetVTextAnchor());
    fail_unless(T->getVTextAnchor() == Text::ANCHOR_BASELINE);
    T->setVTextAnchor(Text::ANCHOR_UNSET);
    fail_unless( !T->isSetVTextAnchor());
    fail_unless(T->getVTextAnchor() == Text::ANCHOR_UNSET);
}
END_TEST

START_TEST ( test_Text_Text )
{
    fail_unless(! T->isSetText() );
    fail_unless( T->getText() == "" );
    T->setText("Some Text");
    fail_unless( T->isSetText() );
    fail_unless( T->getText() == "Some Text" );
    T->setText("");
    fail_unless(! T->isSetText() );
    fail_unless( T->getText() == "" );

}
END_TEST

START_TEST ( test_Text_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<text x=\"5.0\" y=\"20.0\">Test</text>\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  // required attributes
  Text t(*pNode);
  fail_unless(!t.isSetMatrix());
  fail_unless(!t.isSetStroke());
  fail_unless(!t.isSetStrokeWidth());
  fail_unless(!t.isSetDashArray());
  fail_unless(fabs((t.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(t.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((t.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(t.getY().getRelativeValue()  < 1e-9);
  fail_unless(t.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(t.getZ().getRelativeValue()  < 1e-9);
  fail_unless(!t.isSetFontFamily());
  fail_unless(t.getFontFamily() == "");
  fail_unless(!t.isSetFontSize());
  fail_unless(!t.isSetFontWeight());
  fail_unless(!t.isSetFontStyle());
  fail_unless(!t.isSetTextAnchor());
  fail_unless(!t.isSetVTextAnchor());
  fail_unless(t.isSetText());
  fail_unless(t.getText() == "Test");

  delete pNode;
  delete pStream;


  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<text x=\"5.0\" y=\"20.0\" z=\"42.1\"\n"
      "      font-family=\"Courier\" font-size=\"36.0\"\n"
      "      font-weight=\"bold\" font-style=\"italic\"\n"
      "      text-anchor=\"end\" vtext-anchor=\"bottom\">Test</text>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  // optional attributes
  t = Text(*pNode);
  fail_unless(!t.isSetMatrix());
  fail_unless(!t.isSetStroke());
  fail_unless(!t.isSetStrokeWidth());
  fail_unless(!t.isSetDashArray());
  fail_unless(fabs((t.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(t.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((t.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(t.getY().getRelativeValue()  < 1e-9);
  fail_unless(fabs((t.getZ().getAbsoluteValue() - 42.1) / 42.1) < 1e-9);
  fail_unless(t.getZ().getRelativeValue()  < 1e-9);
  fail_unless(t.isSetFontFamily());
  fail_unless(t.getFontFamily() == "Courier");
  fail_unless(t.isSetFontSize());
  fail_unless(fabs((t.getFontSize().getAbsoluteValue() - 36.0) / 36.0) < 1e-9);
  fail_unless(t.getFontSize().getRelativeValue()  < 1e-9);
  fail_unless(t.isSetFontWeight());
  fail_unless(t.getFontWeight() == Text::WEIGHT_BOLD);
  fail_unless(t.isSetFontStyle());
  fail_unless(t.getFontStyle() == Text::STYLE_ITALIC);
  fail_unless(t.isSetTextAnchor());
  fail_unless(t.getTextAnchor() == Text::ANCHOR_END);
  fail_unless(t.isSetVTextAnchor());
  fail_unless(t.getVTextAnchor() == Text::ANCHOR_BOTTOM);
  fail_unless(t.isSetText());
  fail_unless(t.getText() == "Test");

  delete pNode;
  delete pStream;

  // 2D transformation attributes  
  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<text transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "      x=\"5.0\" y=\"20.0\">Test</text>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  t = Text(*pNode);
  fail_unless(t.isSetMatrix());
  const double* pMatrix=t.getMatrix2D();
  fail_unless(pMatrix != NULL);
  fail_unless(fabs((pMatrix[0] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(fabs((pMatrix[1] - 0.173648) / 0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[2] - -0.173648) / -0.173648) < 1e-9);   
  fail_unless(fabs((pMatrix[3] - 0.984808) / 0.984808) < 1e-9);   
  fail_unless(pMatrix[4] < 1e-9);
  fail_unless(pMatrix[5] < 1e-9);
  fail_unless(!t.isSetStroke());
  fail_unless(!t.isSetStrokeWidth());
  fail_unless(!t.isSetDashArray());
  fail_unless(fabs((t.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(t.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((t.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(t.getY().getRelativeValue()  < 1e-9);
  fail_unless(t.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(t.getZ().getRelativeValue()  < 1e-9);
  fail_unless(!t.isSetFontFamily());
  fail_unless(t.getFontFamily() == "");
  fail_unless(!t.isSetFontSize());
  fail_unless(!t.isSetFontWeight());
  fail_unless(!t.isSetFontStyle());
  fail_unless(!t.isSetTextAnchor());
  fail_unless(!t.isSetVTextAnchor());
  fail_unless(t.isSetText());
  fail_unless(t.getText() == "Test");

  delete pNode;
  delete pStream;

  // 1D attributes (stroke, stroke_width, stroke-dasharray  

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<text stroke=\"#00FF00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\"\n"
      "      x=\"5.0\" y=\"20.0\">Test</text>\n"
    ;

  pStream= new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  t = Text(*pNode);
  fail_unless(!t.isSetMatrix());
  fail_unless(t.isSetStroke());
  fail_unless(t.getStroke() == "#00FF00");
  fail_unless(t.isSetStrokeWidth());
  fail_unless(fabs((t.getStrokeWidth() - 3.0) / 3.0) < 1e-9);
  fail_unless(t.isSetDashArray());
  const std::vector<unsigned int>& array = t.getDashArray();
  fail_unless(array.size() == 2);
  fail_unless(array[0] == 32);
  fail_unless(array[1] == 20);
  fail_unless(fabs((t.getX().getAbsoluteValue() - 5.0) / 5.0) < 1e-9);
  fail_unless(t.getX().getRelativeValue()  < 1e-9);
  fail_unless(fabs((t.getY().getAbsoluteValue() - 20.0) / 20.0) < 1e-9);
  fail_unless(t.getY().getRelativeValue()  < 1e-9);
  fail_unless(t.getZ().getAbsoluteValue()  < 1e-9);
  fail_unless(t.getZ().getRelativeValue()  < 1e-9);
  fail_unless(!t.isSetFontFamily());
  fail_unless(t.getFontFamily() == "");
  fail_unless(!t.isSetFontSize());
  fail_unless(!t.isSetFontWeight());
  fail_unless(!t.isSetFontStyle());
  fail_unless(!t.isSetTextAnchor());
  fail_unless(!t.isSetVTextAnchor());
  fail_unless(t.isSetText());
  fail_unless(t.getText() == "Test");

  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_Text_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<text x=\"5\" y=\"20\">Test</text>\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  Text* pT = new Text(*pNode1);
  fail_unless(pT != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pT->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pT;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<text x=\"5\" y=\"20\" z=\"42.1\"\n"
      "      font-family=\"Courier\" font-size=\"36\"\n"
      "      font-weight=\"bold\" font-style=\"italic\"\n"
      "      text-anchor=\"end\" vtext-anchor=\"bottom\">Test</text>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pT = new Text(*pNode1);
  fail_unless(pT != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pT->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pT;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<text transform=\"0.984808,0.173648,-0.173648,0.984808,0,0\"\n"
      "      x=\"5\" y=\"20\">Test</text>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pT = new Text(*pNode1);
  fail_unless(pT != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pT->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pT;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<text stroke=\"#00Ff00\" stroke-width=\"3\" stroke-dasharray=\"32 , 20\"\n"
      "      x=\"5\" y=\"20\">Test</text>\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pT = new Text(*pNode1);
  fail_unless(pT != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pT->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pT;

}
END_TEST

Suite *
create_suite_Text (void)
{
  Suite *suite = suite_create("Text");
  TCase *tcase = tcase_create("Text");


  tcase_add_checked_fixture( tcase,
                             TextTest_setup,
                             TextTest_teardown );

  tcase_add_test( tcase, test_Text_position    );
  tcase_add_test( tcase, test_Text_FontFamily  );
  tcase_add_test( tcase, test_Text_FontSize    );
  tcase_add_test( tcase, test_Text_FontWeight  );
  tcase_add_test( tcase, test_Text_FontStyle   );
  tcase_add_test( tcase, test_Text_TextAnchor  );
  tcase_add_test( tcase, test_Text_VTextAnchor );
  tcase_add_test( tcase, test_Text_Text        );
  tcase_add_test( tcase, test_Text_read        );
  tcase_add_test( tcase, test_Text_write       );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
