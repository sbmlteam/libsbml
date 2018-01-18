//
// Filename    : TestGraphicalPrimitive1D
// Description : Tests for the GraphicalPrimitive1D class
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

#include "GraphicalPrimitive1D.h"
#include "Text.h"

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GraphicalPrimitive1D *P;
static RenderPkgNamespaces *renderns;

void
GraphicalPrimitive1DTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    P = new (std::nothrow) Text(renderns);

    if (P == NULL)
    {
        fail("new(std::nothrow)Text(renderns) returned a NULL pointer.");
    }

}

void 
GraphicalPrimitive1DTest_teardown (void)
{
    delete P;
    delete renderns;
}

START_TEST (test_GraphicalPrimitive1D_setStrokeDashArray)
{
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().empty());
   std::vector<unsigned int> tmpV;
   tmpV.push_back(3);
   tmpV.push_back(7);
   tmpV.push_back(2);
   tmpV.push_back(9);
   P->setDashArray(tmpV);
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==4);
   fail_unless(P->getDashArray()[0]==3); 
   fail_unless(P->getDashArray()[1]==7); 
   fail_unless(P->getDashArray()[2]==2); 
   fail_unless(P->getDashArray()[3]==9);
   tmpV.clear(); 
   tmpV.push_back(12);
   tmpV.push_back(5);
   tmpV.push_back(8);
   tmpV.push_back(3);
   tmpV.push_back(7);
   P->setDashArray(tmpV);
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==5);
   fail_unless(P->getDashArray()[0]==12); 
   fail_unless(P->getDashArray()[1]==5); 
   fail_unless(P->getDashArray()[2]==8); 
   fail_unless(P->getDashArray()[3]==3);
   fail_unless(P->getDashArray()[4]==7);
   tmpV.clear();
   P->setDashArray(tmpV);
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().empty());
   // valid dash array strings

   fail_unless(P->setDashArray("2,2")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==2);
   fail_unless(P->getDashArray()[1]==2);

   fail_unless(P->setDashArray(" 3,4")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==3);
   fail_unless(P->getDashArray()[1]==4);
 
   fail_unless(P->setDashArray("5,6 ")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==5);
   fail_unless(P->getDashArray()[1]==6);
 
   fail_unless(P->setDashArray(" 7,8 ")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==7);
   fail_unless(P->getDashArray()[1]==8);
 
   fail_unless(P->setDashArray("9, 10")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==9);
   fail_unless(P->getDashArray()[1]==10);
 
   fail_unless(P->setDashArray("1 ,   8")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==1);
   fail_unless(P->getDashArray()[1]==8);
 
   fail_unless(P->setDashArray("  6  ,          78 ")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==2);
   fail_unless(P->getDashArray()[0]==6);
   fail_unless(P->getDashArray()[1]==78);
 
   fail_unless(P->setDashArray("9, 8,7,  5,4 ")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==5);
   fail_unless(P->getDashArray()[0]==9);
   fail_unless(P->getDashArray()[1]==8);
   fail_unless(P->getDashArray()[2]==7);
   fail_unless(P->getDashArray()[3]==5);
   fail_unless(P->getDashArray()[4]==4);
 
   fail_unless(P->setDashArray("5")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==1);
   fail_unless(P->getDashArray()[0]==5);
 
   fail_unless(P->setDashArray(" 7  ")==true); 
   fail_unless(P->isSetDashArray()==true);
   fail_unless(P->getDashArray().size()==1);
   fail_unless(P->getDashArray()[0]==7);
 
   fail_unless(P->setDashArray("")==true); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);

   // some invalid array strings
 
   fail_unless(P->setDashArray(",")==false); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);
    
   fail_unless(P->setDashArray("a,4")==false); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);
    
   fail_unless(P->setDashArray("1,,4")==false); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);

   fail_unless(P->setDashArray("2,3,")==false); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);
 
   fail_unless(P->setDashArray(",2,5")==false); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);
 
   fail_unless(P->setDashArray("2,4,5,,3")==false); 
   fail_unless(P->isSetDashArray()==false);
   fail_unless(P->getDashArray().size()==0);
 }

END_TEST 

START_TEST (test_GraphicalPrimitive1D_setStroke)
{
    fail_unless(P->isSetStroke()==false);
    fail_unless(P->getStroke()=="");
    P->setStroke("#223344");
    fail_unless(P->getStroke()=="#223344");
    P->setStroke("#55667788");
    fail_unless(P->getStroke()=="#55667788");
    P->setStroke("white");
    fail_unless(P->getStroke()=="white");
}
END_TEST

START_TEST (test_GraphicalPrimitive1D_setStrokeWidth)
{
    fail_unless(P->isSetStrokeWidth()==false);
    fail_unless(P->getStrokeWidth()!=P->getStrokeWidth());
    P->setStrokeWidth(2.4);
    fail_unless(P->getStrokeWidth()==2.4);
}
END_TEST

START_TEST(test_GraphicalPrimitive1D_id)
{
    fail_unless(!P->isSetId());
    fail_unless(P->getId() == "");
    P->setId("black");
    fail_unless(P->isSetId());
    fail_unless(P->getId() == "black");
    P->unsetId();
    fail_unless(!P->isSetId());
    fail_unless(P->getId() == "");
}
END_TEST


Suite *
create_suite_GraphicalPrimitive1D (void)
{
  Suite *suite = suite_create("GraphicalPrimitive1D");
  TCase *tcase = tcase_create("GraphicalPrimitive1D");


  tcase_add_checked_fixture( tcase,
                             GraphicalPrimitive1DTest_setup,
                             GraphicalPrimitive1DTest_teardown );

  tcase_add_test( tcase, test_GraphicalPrimitive1D_setStrokeDashArray );
  tcase_add_test( tcase, test_GraphicalPrimitive1D_setStroke          );
  tcase_add_test( tcase, test_GraphicalPrimitive1D_setStrokeWidth     );
  tcase_add_test( tcase, test_GraphicalPrimitive1D_id                 );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
