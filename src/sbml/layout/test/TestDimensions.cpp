/**
 * Filename    : TestDimensions.cpp
 * Description : Unit tests for Dimensions
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2005-05-03
 *
 * Copyright 2005 European Media Laboratories Research gGmbH
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */


#include <common/common.h>
#include <common/extern.h>

#include "Dimensions.h"

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static Dimensions *D;

void
DimensionsTest_setup (void)
{
    D = new (std::nothrow) Dimensions();

    if (D == NULL)
    {
        fail("new(std::nothrow)Dimensions() returned a NULL pointer.");
    }

}

void 
DimensionsTest_teardown (void)
{
    delete D;
}

START_TEST (test_Dimensions_create)
{
    fail_unless( D->getTypeCode   () == SBML_LAYOUT_DIMENSIONS );
    fail_unless( D->getMetaId     () == "" );
//    fail_unless( D->getNotes      () == "" );
//    fail_unless( D->getAnnotation () == "" );
    fail_unless( D->getWidth () == 0.0 );
    fail_unless( D->getHeight() == 0.0 );
    fail_unless( D->getDepth () == 0.0 );
}
END_TEST


START_TEST ( test_Dimensions_new_WithNamespace )
{
   SBMLNamespaces* ns=new SBMLNamespaces;
   Dimensions *dim=new Dimensions(ns); 
   fail_unless( dim->getTypeCode() == SBML_LAYOUT_DIMENSIONS );
   fail_unless( dim->getMetaId()   == "" );

   fail_unless(dim->getLevel() == SBML_DEFAULT_LEVEL);
   fail_unless(dim->getVersion() == SBML_DEFAULT_VERSION);

   fail_unless( dim->isSetId() == false );
   fail_unless( dim->getWidth () == 0.0 );
   fail_unless( dim->getHeight() == 0.0 );
   fail_unless( dim->getDepth () == 0.0 );
   
   delete dim;
   delete ns;

   ns = new SBMLNamespaces(2,3);
   dim=new Dimensions(ns); 
   fail_unless( dim->getTypeCode() == SBML_LAYOUT_DIMENSIONS );
   fail_unless( dim->getMetaId()   == "" );

   fail_unless(dim->getLevel() == 2);
   fail_unless(dim->getVersion() == 3);

   fail_unless( dim->isSetId() == false );
   fail_unless( dim->getWidth () == 0.0 );
   fail_unless( dim->getHeight() == 0.0 );
   fail_unless( dim->getDepth () == 0.0 );
   
   delete dim;
   delete ns;
}
END_TEST


START_TEST (test_Dimensions_createWithSize)
{
    Dimensions* d = new(std::nothrow) Dimensions( 1.2 , 0.4 , 3.1415 );
    fail_unless( d->getTypeCode   () == SBML_LAYOUT_DIMENSIONS );
    fail_unless( d->getMetaId     () == "" );
//    fail_unless( d->getNotes      () == "" );
//    fail_unless( d->getAnnotation () == "" );
    fail_unless( d->getWidth () == 1.2 );
    fail_unless( d->getHeight() == 0.4 );
    fail_unless( d->getDepth () == 3.1415 );

    delete d;
}
END_TEST

START_TEST ( test_Dimensions_free_NULL)
{
    Dimensions_free(NULL);
}
END_TEST

START_TEST ( test_Dimensions_setBounds)
{
    D->setBounds(1.1 , -2.2 , 3.3);
   
    fail_unless( D->getWidth () ==  1.1 );
    fail_unless( D->getHeight() == -2.2 );
    fail_unless( D->getDepth () ==  3.3 );

}
END_TEST

START_TEST ( test_Dimensions_initDefaults)
{
    D->setBounds(-1.1 , 2.2 , -3.3);
    D->initDefaults();

    fail_unless( D->getWidth () == -1.1 );
    fail_unless( D->getHeight() ==  2.2 );
    fail_unless( D->getDepth () ==  0.0 );

}
END_TEST

START_TEST ( test_Dimensions_setWidth)
{
    D->setBounds( 1.1 , 2.2 , 3.3);
    D->setWidth(8.8);

    fail_unless(D->getWidth () == 8.8);
    fail_unless(D->getHeight() == 2.2);
    fail_unless(D->getDepth () == 3.3);

}
END_TEST

START_TEST ( test_Dimensions_setHeight)
{
    D->setBounds(1.1 , 2.2 , 3.3);
    D->setHeight(8.8);

    fail_unless(D->getWidth () == 1.1);
    fail_unless(D->getHeight() == 8.8);
    fail_unless(D->getDepth () == 3.3);

}
END_TEST

START_TEST ( test_Dimensions_setDepth)
{
    D->setBounds(1.1 , 2.2 , 3.3);
    D->setDepth(8.8);

    fail_unless(D->getWidth () == 1.1);
    fail_unless(D->getHeight() == 2.2);
    fail_unless(D->getDepth () == 8.8);

}
END_TEST


START_TEST ( test_Dimensions_copyConstructor )
{
    Dimensions* d1=new Dimensions();
    XMLNode* notes=new XMLNode();
    d1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    d1->setAnnotation(annotation);
    Dimensions* d2=new Dimensions(*d1);
    delete d2;
    delete d1;
}
END_TEST

START_TEST ( test_Dimensions_assignmentOperator )
{
    Dimensions* d1=new Dimensions();
    XMLNode* notes=new XMLNode();
    d1->setNotes(notes);
    XMLNode* annotation=new XMLNode();
    d1->setAnnotation(annotation);
    Dimensions* d2=new Dimensions();
    (*d2)=(*d1);
    delete d2;
    delete d1;
}
END_TEST

Suite *
create_suite_Dimensions (void)
{
  Suite *suite = suite_create("Dimensions");
  TCase *tcase = tcase_create("Dimensions");


  tcase_add_checked_fixture( tcase,
                             DimensionsTest_setup,
                             DimensionsTest_teardown );

  tcase_add_test( tcase, test_Dimensions_create                            );
  tcase_add_test( tcase, test_Dimensions_new_WithNamespace                 );
  tcase_add_test( tcase, test_Dimensions_createWithSize                    );
  tcase_add_test( tcase, test_Dimensions_free_NULL                         );
  tcase_add_test( tcase, test_Dimensions_setBounds                         );
  tcase_add_test( tcase, test_Dimensions_initDefaults                      );
  tcase_add_test( tcase, test_Dimensions_setWidth                          );
  tcase_add_test( tcase, test_Dimensions_setHeight                         );
  tcase_add_test( tcase, test_Dimensions_setDepth                          );
  tcase_add_test( tcase, test_Dimensions_copyConstructor                   );
  tcase_add_test( tcase, test_Dimensions_assignmentOperator                );

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
