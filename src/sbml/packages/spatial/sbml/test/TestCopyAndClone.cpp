/**
 * \file    TestCopyAndClone.cpp
 * \brief   Copy SBML unit tests
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 * 
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>


#include <check.h>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


CK_CPPSTART

static SpatialPkgNamespaces* GNS = NULL;


void
SpatialCopyAndClone_setup (void)
{
  GNS = new SpatialPkgNamespaces();
}

void 
SpatialCopyAndClone_teardown (void)
{
  delete GNS;
}


START_TEST ( test_ParametricObject_copyConstructor )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");

  ParametricObject* o2=new ParametricObject(*o1);

  fail_unless(o2->getId() == "s");

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o1;
  delete o2;
}
END_TEST


START_TEST ( test_ParametricObject_assignmentOperator )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");
 

  ParametricObject* o2 = new ParametricObject(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getId() == "s");

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_ParametricObject_clone )
{
  ParametricObject* o1=new ParametricObject(GNS);
  o1->setId("s");
  
  fail_unless(o1->getId() == "s");

  ParametricObject* o2=o1->clone();
 
  fail_unless(o2->getId() == "s");

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_SampledField_copyConstructor )
{
  SampledField* o1=new SampledField(GNS);
  int points [] = {1,2,3};
  o1->setSamples(points, 3);
  o1->setSamplesLength(3);
  
  fail_unless(o1->getSamplesLength() == 3);

  SampledField* o2=new SampledField(*o1);

  fail_unless(o2->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  o2->getSamples(samplesRet);
  
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_SampledField_assignmentOperator )
{
  SampledField* o1=new SampledField(GNS);
  int points [] = {1,2,3};
  o1->setSamples(points, 3);
  o1->setSamplesLength(3);

  fail_unless(o1->getSamplesLength() == 3);
  
  SampledField* o2 = new SampledField(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  o2->getSamples(samplesRet);
  
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_SampledField_clone )
{
  SampledField* o1=new SampledField(GNS);
  int points [] = {1,2,3};
  o1->setSamples(points, 3);
  o1->setSamplesLength(3);

  fail_unless(o1->getSamplesLength() == 3);

  SampledField* o2=o1->clone();
 
  fail_unless(o2->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  o2->getSamples(samplesRet);
  
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] == 3);

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_TransformationComponent_copyConstructor )
{
  TransformationComponent* o1=new TransformationComponent(GNS);
  double points [] = {1,2,3};
  o1->setComponents(points, 3);
  
  fail_unless(o1->getActualComponentsLength() == 3);

  TransformationComponent* o2=new TransformationComponent(*o1);

  fail_unless(o2->getActualComponentsLength() == 3);
  
  double componentsRet [] = {0, 0, 0};
  o2->getComponents(componentsRet);
  
  fail_unless(util_isEqual(componentsRet[0], 1));
  fail_unless(util_isEqual(componentsRet[1], 2));
  fail_unless(util_isEqual(componentsRet[2], 3));

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_TransformationComponent_assignmentOperator )
{
  TransformationComponent* o1=new TransformationComponent(GNS);
  double points [] = {1,2,3};
  o1->setComponents(points, 3);
  
  fail_unless(o1->getActualComponentsLength() == 3);
  
  TransformationComponent* o2 = new TransformationComponent(GNS);;
  (*o2)=*o1;

  fail_unless(o2->getActualComponentsLength() == 3);

  double componentsRet [] = {0, 0, 0};
  o2->getComponents(componentsRet);
  
  fail_unless(util_isEqual(componentsRet[0], 1));
  fail_unless(util_isEqual(componentsRet[1], 2));
  fail_unless(util_isEqual(componentsRet[2], 3));

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


START_TEST ( test_TransformationComponent_clone )
{
  TransformationComponent* o1=new TransformationComponent(GNS);
  double points [] = {1,2,3};
  o1->setComponents(points, 3);
  
  fail_unless(o1->getActualComponentsLength() == 3);

  TransformationComponent* o2=o1->clone();
 
  fail_unless(o2->getActualComponentsLength() == 3);

  double componentsRet [] = {0, 0, 0};
  o2->getComponents(componentsRet);
  
  fail_unless(util_isEqual(componentsRet[0], 1));
  fail_unless(util_isEqual(componentsRet[1], 2));
  fail_unless(util_isEqual(componentsRet[2], 3));

  fail_unless(o2->getParentSBMLObject() == o1->getParentSBMLObject());

  delete o2;
  delete o1;
}
END_TEST


Suite *
create_suite_CopyAndClone (void)
{
  Suite *suite = suite_create("CopyAndClone");
  TCase *tcase = tcase_create("CopyAndClone");

   tcase_add_checked_fixture( tcase,
                             SpatialCopyAndClone_setup,
                             SpatialCopyAndClone_teardown );

  tcase_add_test( tcase, test_ParametricObject_copyConstructor );
  tcase_add_test( tcase, test_ParametricObject_assignmentOperator );
  tcase_add_test( tcase, test_ParametricObject_clone );
  tcase_add_test( tcase, test_SampledField_copyConstructor );
  tcase_add_test( tcase, test_SampledField_assignmentOperator );
  tcase_add_test( tcase, test_SampledField_clone );
  tcase_add_test( tcase, test_TransformationComponent_copyConstructor );
  tcase_add_test( tcase, test_TransformationComponent_assignmentOperator );
  tcase_add_test( tcase, test_TransformationComponent_clone );

  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND

