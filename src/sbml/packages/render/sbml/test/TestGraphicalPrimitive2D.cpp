//
// Filename    : TestGraphicalPrimitive2D
// Description : Tests for the GraphicalPrimitive2D class
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

#include "GraphicalPrimitive2D.h"
#include "Rectangle.h"

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static GraphicalPrimitive2D *P;
static RenderPkgNamespaces *renderns;

void
GraphicalPrimitive2DTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    P = new (std::nothrow) Rectangle(renderns);

    if (P == NULL)
    {
        fail("new(std::nothrow)Rectangle(renderns) returned a NULL pointer.");
    }

}

void 
GraphicalPrimitive2DTest_teardown (void)
{
    delete P;
    delete renderns;
}

START_TEST (test_GraphicalPrimitive2D_fill )
{
    fail_unless( !P->isSetFillColor());
    fail_unless( P->getFillColor() == "");
    P->setFillColor("black");
    fail_unless( P->isSetFillColor());
    fail_unless( P->getFillColor() == "black");
    P->setFillColor("#FF6655");
    fail_unless( P->isSetFillColor());
    fail_unless( P->getFillColor() == "#FF6655");
    P->setFillColor("");
    fail_unless( !P->isSetFillColor());
    fail_unless( P->getFillColor() == "");
}
END_TEST 

START_TEST (test_GraphicalPrimitive2D_fillRule )
{
    fail_unless( !P->isSetFillRule() );
    fail_unless( P->getFillRule() == GraphicalPrimitive2D::UNSET);
    P->setFillRule(GraphicalPrimitive2D::NONZERO);
    fail_unless( P->isSetFillRule() );
    fail_unless( P->getFillRule() == GraphicalPrimitive2D::NONZERO);
    P->setFillRule(GraphicalPrimitive2D::EVENODD);
    fail_unless( P->isSetFillRule() );
    fail_unless( P->getFillRule() == GraphicalPrimitive2D::EVENODD);
    P->setFillRule(GraphicalPrimitive2D::INHERIT);
    fail_unless( P->isSetFillRule() );
    fail_unless( P->getFillRule() == GraphicalPrimitive2D::INHERIT);
    P->setFillRule(GraphicalPrimitive2D::UNSET);
    fail_unless( !P->isSetFillRule() );
    fail_unless( P->getFillRule() == GraphicalPrimitive2D::UNSET);
} 
END_TEST 



Suite *
create_suite_GraphicalPrimitive2D (void)
{
  Suite *suite = suite_create("GraphicalPrimitive2D");
  TCase *tcase = tcase_create("GraphicalPrimitive2D");


  tcase_add_checked_fixture( tcase,
                             GraphicalPrimitive2DTest_setup,
                             GraphicalPrimitive2DTest_teardown );

  tcase_add_test( tcase, test_GraphicalPrimitive2D_fill );
  tcase_add_test( tcase, test_GraphicalPrimitive2D_fillRule );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
