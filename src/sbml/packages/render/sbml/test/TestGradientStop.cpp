//
// Filename    : TestGradientStop
// Description : Tests for the GradientStop class
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

#include <GradientStop.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GradientStop *S;
static RenderPkgNamespaces *renderns;

void
GradientStop_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    S = new (std::nothrow) GradientStop(renderns);

    if (S == NULL)
    {
        fail("new(std::nothrow)GradientStop(renderns) returned a NULL pointer.");
    }

}

void 
GradientStop_teardown (void)
{
    delete S;
    delete renderns;
}

START_TEST (test_GradientStop_setOffset )
{

    fail_unless(S->getOffset().getAbsoluteValue() < 1e-9);
    fail_unless(S->getOffset().getRelativeValue() < 1e-9);
    S->setOffset(3.0,5.0);
    fail_unless(fabs((S->getOffset().getAbsoluteValue() - 3.0) / 3.0) < 1e-9);
    fail_unless(fabs((S->getOffset().getRelativeValue() - 5.0) / 5.0) < 1e-9);
    RelAbsVector v(-27.0,45.6);
    S->setOffset(v);
    fail_unless(fabs((S->getOffset().getAbsoluteValue() - -27.0) / -27.0) < 1e-9);
    fail_unless(fabs((S->getOffset().getRelativeValue() - 45.6) / 45.6) < 1e-9);
    S->setOffset(123.456,-34.5);
    fail_unless(fabs((S->getOffset().getAbsoluteValue() - 123.456) / 123.456) < 1e-9);
    fail_unless(fabs((S->getOffset().getRelativeValue() - -34.5) / -34.5) < 1e-9);
}
END_TEST 

START_TEST ( test_GradientStop_setStopColor )
{
    fail_unless(S->getStopColor() == "");
    S->setStopColor("black");
    fail_unless(S->getStopColor() == "black");
    S->setStopColor("#22334455");
    fail_unless(S->getStopColor() == "#22334455");
}
END_TEST

START_TEST ( test_GradientStop_read )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<stop offset=\"0%\" stop-color=\"white\" />\n"
                ;

  XMLInputStream* pStream= new XMLInputStream(s.c_str(),false);
  XMLNode* pNode = new XMLNode(*pStream);

  GradientStop gs(*pNode);
  fail_unless(gs.getStopColor() ==  "white");
  fail_unless(gs.getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(gs.getOffset().getRelativeValue() < 1e-9);
  delete pNode;
  delete pStream;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<stop offset=\"100%\" stop-color=\"lightBlue\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode = new XMLNode(*pStream);

  gs=GradientStop(*pNode);
  fail_unless(gs.getStopColor() ==  "lightBlue");
  fail_unless(gs.getOffset().getAbsoluteValue() < 1e-9);
  fail_unless(fabs((gs.getOffset().getRelativeValue() - 100.0) / 100.0) < 1e-9);
  delete pNode;
  delete pStream;
}
END_TEST

START_TEST ( test_GradientStop_write )
{
  std::string s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<stop offset=\"0\" stop-color=\"white\" />\n"
                ;

  XMLInputStream* pStream = new XMLInputStream(s.c_str(),false);
  XMLNode* pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  GradientStop* pStop = new GradientStop(*pNode1);
  fail_unless(pStop != NULL);
  // create the XMLNode from the object
  XMLNode* pNode2 = new XMLNode(pStop->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pStop;

  s = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<stop offset=\"100%\" stop-color=\"lightBlue\" />\n"
    ;

  pStream = new XMLInputStream(s.c_str(),false);
  pNode1 = new XMLNode(*pStream);
  fail_unless(pNode1 != NULL);
  pStop = new GradientStop(*pNode1);
  fail_unless(pStop != NULL);
  // create the XMLNode from the object
  pNode2 = new XMLNode(pStop->toXML());
  fail_unless(pNode1 != NULL);
  // compare the two nodes
  fail_unless(pNode1->equals(*pNode2, true));
  delete pNode1;
  delete pNode2;
  delete pStream;
  delete pStop;


}
END_TEST

Suite *
create_suite_GradientStop (void)
{
  Suite *suite = suite_create("GradientStop");
  TCase *tcase = tcase_create("GradientStop");


  tcase_add_checked_fixture( tcase,
                             GradientStop_setup,
                             GradientStop_teardown );

  tcase_add_test( tcase, test_GradientStop_setOffset    );
  tcase_add_test( tcase, test_GradientStop_setStopColor );
  tcase_add_test( tcase, test_GradientStop_read         );
  tcase_add_test( tcase, test_GradientStop_write        );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
