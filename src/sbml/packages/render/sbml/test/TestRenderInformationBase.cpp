//
// Filename    : TestRenderInformationBase.cpp
// Description : Tests for the RenderInformationBase class
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

#include <RenderInformationBase.h>
#include <GlobalRenderInformation.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static RenderInformationBase *R;
static RenderPkgNamespaces *renderns;

void
RenderInformationBaseTest_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
    R = new (std::nothrow) GlobalRenderInformation(renderns);

    if (R == NULL)
    {
        fail("new(std::nothrow)GlobalRenderInformation(renderns) returned a NULL pointer.");
    }

}

void 
RenderInformationBaseTest_teardown (void)
{
    delete R;
    delete renderns;
}

START_TEST (test_RenderInformationBase_programname )
{
    fail_unless(R->getProgramName() == "");
    R->setProgramName("SomeProgram");
    fail_unless(R->getProgramName() == "SomeProgram");
    R->setProgramName("");
    fail_unless(R->getProgramName() == "");
}
END_TEST 

START_TEST (test_RenderInformationBase_programversion )
{
    fail_unless(R->getProgramVersion() == "");
    R->setProgramVersion("1.0.0");
    fail_unless(R->getProgramVersion() == "1.0.0");
    R->setProgramVersion("");
    fail_unless(R->getProgramVersion() == "");
}
END_TEST 

START_TEST (test_RenderInformationBase_referencerenderinformation )
{
    fail_unless(R->getReferenceRenderInformationId() == "");
    R->setReferenceRenderInformationId("Other_Render_Information");
    fail_unless(R->getReferenceRenderInformationId() == "Other_Render_Information");
    R->setReferenceRenderInformationId("");
    fail_unless(R->getReferenceRenderInformationId() == "");
}
END_TEST 

START_TEST (test_RenderInformationBase_backgroundcolor )
{
    R->setBackgroundColor("");
    fail_unless(R->getBackgroundColor() == "");
    R->setBackgroundColor("#FF3478");
    fail_unless(R->getBackgroundColor() == "#FF3478");
    R->setBackgroundColor("");
    fail_unless(R->getBackgroundColor() == "");
}
END_TEST 

START_TEST (test_RenderInformationBase_id )
{
    fail_unless( !R->isSetId() );
    fail_unless( R->getId() == "");
    R->setId("render_info");
    fail_unless( R->isSetId() );
    fail_unless( R->getId() == "render_info");
    R->unsetId();
    fail_unless( !R->isSetId() );
    fail_unless( R->getId() == "");
}
END_TEST 

START_TEST (test_RenderInformationBase_name )
{
    fail_unless( !R->isSetName() );
    fail_unless( R->getName() == "");
    R->setName("render_info");
    fail_unless( R->isSetName() );
    fail_unless( R->getName() == "render_info");
    R->unsetName();
    fail_unless( !R->isSetName() );
    fail_unless( R->getName() == "");
}
END_TEST 

START_TEST ( test_RenderInformationBase_createMethods )
{
    // create color definition
    ColorDefinition* pCD=R->createColorDefinition();
    fail_unless ( pCD != NULL );
    fail_unless ( R->getNumColorDefinitions() == 1 );
    ColorDefinition* pCD2 = R->removeColorDefinition(0);
    fail_unless( pCD2 != NULL );
    fail_unless( pCD == pCD2 );
    fail_unless ( R->getNumColorDefinitions() == 0 );
    delete pCD2;
    // create linear gradient definition
    LinearGradient* pLG=R->createLinearGradientDefinition();
    fail_unless ( pLG != NULL );
    fail_unless ( R->getNumGradientDefinitions() == 1 );
    GradientBase* pG2 = R->removeGradientDefinition(0);
    fail_unless( pG2 != NULL );
    fail_unless( pLG == pG2 );
    fail_unless ( R->getNumGradientDefinitions() == 0 );
    delete pG2;
    // create radial gradient definition
    RadialGradient* pRG=R->createRadialGradientDefinition();
    fail_unless ( pRG != NULL );
    fail_unless ( R->getNumGradientDefinitions() == 1 );
    pG2 = R->removeGradientDefinition(0);
    fail_unless( pG2 != NULL );
    fail_unless( pRG == pG2 );
    fail_unless ( R->getNumGradientDefinitions() == 0 );
    delete pG2;
    // create line ending
    LineEnding* pLE=R->createLineEnding();
    fail_unless ( pLE != NULL );
    fail_unless ( R->getNumLineEndings() == 1 );
    LineEnding* pLE2 = R->removeLineEnding(0);
    fail_unless( pLE2 != NULL );
    fail_unless( pLE == pLE2 );
    fail_unless ( R->getNumLineEndings() == 0 );
    delete pLE2;
}
END_TEST

START_TEST ( test_RenderInformationBase_addMethods )
{
    fail_unless( R->getNumColorDefinitions() == 0 );
    fail_unless( R->getNumGradientDefinitions() == 0 );
    fail_unless( R->getNumLineEndings() == 0 );
    // add color definition
    ColorDefinition* pCD=new ColorDefinition(renderns);
    fail_unless ( pCD != NULL );
    pCD->setId("ColorDefinition_1");
    fail_unless ( R->addColorDefinition( pCD ) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( R->getNumColorDefinitions() == 1 );
    fail_unless( pCD != R->getColorDefinition(0));
    delete pCD;
    pCD = new ColorDefinition(renderns);
    fail_unless ( R->addColorDefinition( pCD ) == LIBSBML_INVALID_OBJECT );
    fail_unless( R->getNumColorDefinitions() == 1 );
    delete pCD;
    pCD = new ColorDefinition(2,1);
    fail_unless ( pCD != NULL );
    pCD->setId("ColorDefinition_2");
    fail_unless( R->addColorDefinition( pCD ) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( R->getNumColorDefinitions() == 1 );
    delete pCD;
    // add gradient definition
    LinearGradient* pLG=new LinearGradient(renderns);
    fail_unless ( pLG != NULL );
    fail_unless ( R->addGradientDefinition( pLG ) == LIBSBML_INVALID_OBJECT );
    fail_unless( R->getNumGradientDefinitions() == 0 );
    pLG->setId("lineargradient_1");
    fail_unless ( R->addGradientDefinition( pLG ) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( R->getNumGradientDefinitions() == 1 );
    fail_unless( pLG != R->getGradientDefinition(0));
    delete pLG;
    pLG = new LinearGradient(renderns);
    fail_unless ( R->addGradientDefinition( pLG ) == LIBSBML_INVALID_OBJECT );
    fail_unless( R->getNumGradientDefinitions() == 1 );
    delete pLG;
    pLG = new LinearGradient(2,1);
    pLG->setId("lineargradient_5");
    fail_unless ( pLG != NULL );
    fail_unless( R->addGradientDefinition( pLG ) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( R->getNumGradientDefinitions() == 1 );
    delete pLG;
    RadialGradient* pRG=new RadialGradient(renderns);
    fail_unless ( pRG != NULL );
    pRG->setId("radial_gradient_2");
    fail_unless ( R->addGradientDefinition( pRG ) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( R->getNumGradientDefinitions() == 2 );
    delete pRG;
    pRG=new RadialGradient(renderns);
    fail_unless ( pRG != NULL );
    fail_unless ( R->addGradientDefinition( pRG ) == LIBSBML_INVALID_OBJECT );
    fail_unless( R->getNumGradientDefinitions() == 2 );
    fail_unless( pRG != R->getGradientDefinition(1));
    delete pRG;
    pRG = new RadialGradient(2,1);
    fail_unless ( pRG != NULL );
    pRG->setId("radial_gradient_5");
    fail_unless( R->addGradientDefinition( pRG ) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( R->getNumGradientDefinitions() == 2 );
    delete pRG;
    // add line ending
    LineEnding* pLE=new LineEnding(renderns);
    fail_unless ( pLE != NULL );
    pLE->setId("lineending_1");
    fail_unless ( R->addLineEnding( pLE ) == LIBSBML_OPERATION_SUCCESS );
    fail_unless( R->getNumLineEndings() == 1 );
    fail_unless( pLE != R->getLineEnding(0));
    delete pLE;
    pLE=new LineEnding(renderns);
    fail_unless ( pLE != NULL );
    fail_unless ( R->addLineEnding( pLE ) == LIBSBML_INVALID_OBJECT );
    fail_unless( R->getNumLineEndings() == 1 );
    delete pLE;
    pLE = new LineEnding(2,1);
    fail_unless ( pLE != NULL );
    pLE->setId("lineending_5");
    fail_unless( R->addLineEnding( pLE ) == LIBSBML_LEVEL_MISMATCH );
    fail_unless( R->getNumLineEndings() == 1 );
    delete pLE;
}
END_TEST


Suite *
create_suite_RenderInformationBase (void)
{
  Suite *suite = suite_create("RenderInformationBase");
  TCase *tcase = tcase_create("RenderInformationBase");


  tcase_add_checked_fixture( tcase,
                             RenderInformationBaseTest_setup,
                             RenderInformationBaseTest_teardown );

  tcase_add_test( tcase, test_RenderInformationBase_id                         );
  tcase_add_test( tcase, test_RenderInformationBase_name                       );
  tcase_add_test( tcase, test_RenderInformationBase_programname                );
  tcase_add_test( tcase, test_RenderInformationBase_programversion             );
  tcase_add_test( tcase, test_RenderInformationBase_referencerenderinformation );
  tcase_add_test( tcase, test_RenderInformationBase_backgroundcolor            );
  tcase_add_test( tcase, test_RenderInformationBase_createMethods              );
  tcase_add_test( tcase, test_RenderInformationBase_addMethods                 );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
