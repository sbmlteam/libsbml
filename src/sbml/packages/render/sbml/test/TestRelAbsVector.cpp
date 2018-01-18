//
// Filename    : TestRelAbsVector
// Description : Tests for the RelAbsVector class
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


#include <sbml/packages/render/sbml/RelAbsVector.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static RelAbsVector *V;
//static RenderPkgNamespaces *renderns;
 
void
RelAbsVectorTest_setup (void)
{
    V = new (std::nothrow) RelAbsVector();

    if (V == NULL)
    {
        fail("new(std::nothrow)RelAbsVector() returned a NULL pointer.");
    }

}

void 
RelAbsVectorTest_teardown (void)
{
    delete V;
}

START_TEST (test_RelAbsVector_setCoordinates_string)
{
   fail_unless(V->getAbsoluteValue()==0.0);
   fail_unless(V->getRelativeValue()==0.0);
   // should match (one match)
   
   // absolute values
   V->setCoordinate("2.45");
   fail_unless(fabs((V->getAbsoluteValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate(".45");
   fail_unless(fabs((V->getAbsoluteValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2.");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45");
   fail_unless(fabs((V->getAbsoluteValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45");
   fail_unless(fabs((V->getAbsoluteValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45");
   fail_unless(fabs((V->getAbsoluteValue()+2.45)/2.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45");
   fail_unless(fabs((V->getAbsoluteValue()+0.45)/0.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.");
   fail_unless(fabs((V->getAbsoluteValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45e2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate(".45e2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2e2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2.e2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45e2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45e2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2e2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.e2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45e2");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45e2");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2e2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.e2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2E2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E2");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E2");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E+2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2E+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E+2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E+2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E+2");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E+2");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E+2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E+2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());


   V->setCoordinate("2.45E-2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E-2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2E-2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E-2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E-2");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E-2");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E-2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E-2");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E-2");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E-2");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E-2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E-2");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs(V->getRelativeValue())<=std::numeric_limits<double>::min());

   // relative values
   V->setCoordinate("2.45%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45)/2.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45)/0.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0)/2.0)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.e2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E+2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E-2%");
   fail_unless(fabs(V->getAbsoluteValue())<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());

   // varying absolute values + const relative value
   V->setCoordinate("2.45+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.45)/2.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45+45%");
   fail_unless(fabs((V->getAbsoluteValue()+0.45)/0.45)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.e2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E+2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());

   V->setCoordinate("2.45E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate(".45E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("2.E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.45E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+.45E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("+2.E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.45E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-.45E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("-2.E-2+45%");
   fail_unless(fabs((V->getAbsoluteValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-45.0)/45.0)<=std::numeric_limits<double>::min());


   // const absolute + varying relative values
   V->setCoordinate("23+2.45%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45)/2.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+.45%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45)/0.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2.%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2.45e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+.45e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2.e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("23+2.45E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+.45E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2.E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("23+2.45E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+.45E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2.E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("23+2.45E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+.45E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23+2.E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()-2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());

   // subtraction of the relative value
   V->setCoordinate("23-2.45%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45)/2.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-.45%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45)/0.45)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2.%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0)/2.0)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2.45e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-.45e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2.e2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("23-2.45E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-.45E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2.E2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("23-2.45E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e2)/2.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-.45E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e2)/0.45e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2.E+2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e2)/2.0e2)<=std::numeric_limits<double>::min());

   V->setCoordinate("23-2.45E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.45e-2)/2.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-.45E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+0.45e-2)/0.45e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());
   V->setCoordinate("23-2.E-2%");
   fail_unless(fabs((V->getAbsoluteValue()-23.0)/23.0)<=std::numeric_limits<double>::min());
   fail_unless(fabs((V->getRelativeValue()+2.0e-2)/2.0e-2)<=std::numeric_limits<double>::min());

   // should not match
   V->setCoordinate("2.45.34");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate(".45.34");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate(".");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("a.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("4.A");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("andsfd");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("e24");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2e2.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2e-2.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2e+2.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("E24");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2E2.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2E-2.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2E+2.4");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());

   V->setCoordinate("2.45.34%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate(".45.34%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate(".%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("a.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("4.A%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("andsfd%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("e24%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2e2.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2e-2.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2e+2.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("E24%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2E2.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2E-2.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("2E+2.4%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());

   V->setCoordinate("23++2.45%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++.45%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2.%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.45%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-.45%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());

   V->setCoordinate("23++2.45E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++.45E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2.E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.45E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-.45E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.E-2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());

   V->setCoordinate("23++2.45E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++.45E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2.E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.45E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-.45E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.E+2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());

   V->setCoordinate("23++2.45E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++.45E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2.E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.45E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-.45E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.E2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());

   V->setCoordinate("23++2.45e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++.45e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23++2.e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.45e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-.45e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
   V->setCoordinate("23+-2.e2%");
   fail_unless(V->getAbsoluteValue()!=V->getAbsoluteValue());
   fail_unless(V->getRelativeValue()!=V->getRelativeValue());
}
END_TEST

START_TEST( test_RelAbsVector_setters )
{
   fail_unless(V->getAbsoluteValue()==0.0);
   fail_unless(V->getRelativeValue()==0.0);
   V->setCoordinate(3.4,23.6);
   fail_unless(fabs((V->getAbsoluteValue() - 3.4) / 3.4) < 1e-9);
   fail_unless(fabs((V->getRelativeValue() - 23.6) / 23.6) < 1e-9);
   V->setAbsoluteValue(-35.2);
   fail_unless(fabs((V->getAbsoluteValue() - -35.2) / -35.2) < 1e-9);
   fail_unless(fabs((V->getRelativeValue() - 23.6) / 23.6) < 1e-9);
   V->setRelativeValue(200.0);
   fail_unless(fabs((V->getAbsoluteValue() - -35.2) / -35.2) < 1e-9);
   fail_unless(fabs((V->getRelativeValue() - 200.0) / 200.0) < 1e-9);

}
END_TEST

START_TEST( test_RelAbsVector_constructors)
{
    // constructor with two default arguments
    RelAbsVector v1;
    fail_unless(v1.empty() == true);
    fail_unless(v1.getRelativeValue() < 1e-9);
    fail_unless(v1.getAbsoluteValue() < 1e-9);

    RelAbsVector v2(13.0);
    fail_unless(v2.empty() == false);
    fail_unless(v2.getRelativeValue() < 1e-9);
    fail_unless(fabs((v2.getAbsoluteValue() - 13.0) / 13.0) < 1e-9);

    v2=RelAbsVector(23.0,-5.3);
    fail_unless(v2.empty() == false);
    fail_unless(fabs((v2.getAbsoluteValue() - 23.0) / 23.0) < 1e-9);
    fail_unless(fabs((v2.getRelativeValue() - -5.3) / -5.3) < 1e-9);

    // constructor with  value string
    RelAbsVector v3("23-2.e2%");
    fail_unless(v3.empty() == false);
    fail_unless(fabs((v3.getAbsoluteValue() - 23.0) / 23.0) < 1e-9);
    fail_unless(fabs((v3.getRelativeValue() - -2e2) / -2e2) < 1e-9);
    v3.erase();
    fail_unless(v3.empty() == true);
    fail_unless(v3.getRelativeValue() < 1e-9);
    fail_unless(v3.getAbsoluteValue() < 1e-9);


}
END_TEST

START_TEST( test_RelAbsVector_operators )
{
    RelAbsVector v1(4.0,-2.0);
    RelAbsVector v2(3.0,50.0);
    // addition
    RelAbsVector v3=v1 + v2;
    fail_unless(fabs((v3.getAbsoluteValue() - 7.0) / 7.0) < 1e-9);
    fail_unless(fabs((v3.getRelativeValue() - 48.0) / 48.0) < 1e-9);
    // division by number
    RelAbsVector v4 = v1 / 2.0;
    fail_unless(fabs((v4.getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
    fail_unless(fabs((v4.getRelativeValue() - -1.0) / -1.0) < 1e-9);
    RelAbsVector v5;
    fail_unless(v5.getRelativeValue() < 1e-9);
    fail_unless(v5.getAbsoluteValue() < 1e-9);
    // assignment operator
    v5 = v4;
    fail_unless(fabs((v4.getAbsoluteValue() - 2.0) / 2.0) < 1e-9);
    fail_unless(fabs((v4.getRelativeValue() - -1.0) / -1.0) < 1e-9);
    // equal , unequal
    fail_unless(v1 != v2);
    fail_unless(v5 == v4);
}
END_TEST

Suite *
create_suite_RelAbsVector (void)
{
  Suite *suite = suite_create("RelAbsVector");
  TCase *tcase = tcase_create("RelAbsVector");


  tcase_add_checked_fixture( tcase,
                             RelAbsVectorTest_setup,
                             RelAbsVectorTest_teardown );

  tcase_add_test( tcase, test_RelAbsVector_setCoordinates_string                );
  tcase_add_test( tcase, test_RelAbsVector_setters                              );
  tcase_add_test( tcase, test_RelAbsVector_constructors                         );
  tcase_add_test( tcase, test_RelAbsVector_operators                            );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
