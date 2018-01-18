//
// Filename    : TestGradientBase
// Description : Tests for the GradientBase class
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

#include "GradientBase.h"
#include "LinearGradient.h"

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GradientBase *B;
static RenderPkgNamespaces *renderns;

void
GradientBaseTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    B = new (std::nothrow) LinearGradient(renderns);

    if (B == NULL)
    {
        fail("new(std::nothrow)LinearGradient(renderns) returned a NULL pointer.");
    }

}

void 
GradientBaseTest_teardown (void)
{
    delete B;
    delete renderns;
}

START_TEST (test_GradientBase_setId )
{
    fail_unless(!B->isSetId());
    fail_unless(B->getId() == "");
    B->setId("RedBlack_LinearGradient");
    fail_unless(B->isSetId());
    fail_unless(B->getId() == "RedBlack_LinearGradient");
    B->unsetId();
    fail_unless(!B->isSetId());
    fail_unless(B->getId() == "");
}
END_TEST 

START_TEST ( test_GradientBase_setSpreadMethod )
{
    fail_unless(B->getSpreadMethod() == GradientBase::PAD);
    B->setSpreadMethod(GradientBase::REFLECT);
    fail_unless(B->getSpreadMethod() == GradientBase::REFLECT);
    B->setSpreadMethod(GradientBase::REPEAT);
    fail_unless(B->getSpreadMethod() == GradientBase::REPEAT);
    B->setSpreadMethod(GradientBase::PAD);
    fail_unless(B->getSpreadMethod() == GradientBase::PAD);
}
END_TEST

START_TEST (test_GradientBase_GradientStops)
{
    fail_unless(B->getNumGradientStops() == 0);
    GradientStop* pStop=B->createGradientStop();
    fail_unless(pStop != NULL);
    fail_unless(B->getNumGradientStops() == 1);
    pStop = new GradientStop(renderns);
    fail_unless( B->addGradientStop(pStop) == LIBSBML_INVALID_OBJECT);
    fail_unless(B->getNumGradientStops() == 1);
    pStop->setOffset(RelAbsVector(0.0,50.0));
    fail_unless( B->addGradientStop(pStop) == LIBSBML_INVALID_OBJECT);
    fail_unless(B->getNumGradientStops() == 1);
    pStop->setStopColor("#FF0000");
    fail_unless( B->addGradientStop(pStop) == LIBSBML_OPERATION_SUCCESS);
    fail_unless(B->getNumGradientStops() == 2);
    delete pStop;
    pStop = new GradientStop(2,1);
    pStop->setOffset(RelAbsVector(0.0,50.0));
    pStop->setStopColor("#FF0000");
    fail_unless(B->addGradientStop(pStop) == LIBSBML_LEVEL_MISMATCH);
    delete pStop;
    fail_unless(B->getNumGradientStops() == 2);
}
END_TEST

Suite *
create_suite_GradientBase (void)
{
  Suite *suite = suite_create("GradientBaseTest");
  TCase *tcase = tcase_create("GradientBaseTest");


  tcase_add_checked_fixture( tcase,
                             GradientBaseTest_setup,
                             GradientBaseTest_teardown );

  tcase_add_test( tcase, test_GradientBase_setId    );
  tcase_add_test( tcase, test_GradientBase_setSpreadMethod );
  tcase_add_test( tcase, test_GradientBase_GradientStops );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
