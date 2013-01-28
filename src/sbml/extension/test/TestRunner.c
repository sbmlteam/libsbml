/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the extension module
 * \author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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

#include <check.h>
#include <stdlib.h>
#include <string.h>

#include <sbml/util/memory.h>
#include <sbml/common/extern.h>


/**
 * Test suite creation function prototypes.
 *
 * These functions are needed only for calls in main() below.  Therefore a
 * separate header file is not necessary and only adds a maintenance burden
 * to keep the two files synchronized.
 */
BEGIN_C_DECLS

Suite *create_suite_SBaseExtensionPoint      (void);
Suite *create_suite_SBasePlugin              (void);
Suite *create_suite_SBasePluginCreator       (void);
Suite *create_suite_SBasePluginCreatorBase   (void);
Suite *create_suite_SBMLDocumentPlugin       (void);
Suite *create_suite_SBMLExtension		     (void);
Suite *create_suite_SBMLExtensionNamespaces  (void);
Suite *create_suite_SBMLExtensionRegistry    (void);

END_C_DECLS



int
main (void) 
{ 
  int num_failed;
  SRunner *runner;

  runner = srunner_create( create_suite_SBaseExtensionPoint() );
  srunner_add_suite( runner, create_suite_SBasePlugin () );
  srunner_add_suite( runner, create_suite_SBasePluginCreator () );
  srunner_add_suite( runner, create_suite_SBasePluginCreatorBase () );
  srunner_add_suite( runner, create_suite_SBMLDocumentPlugin () );
  srunner_add_suite( runner, create_suite_SBMLExtension () );
  srunner_add_suite( runner, create_suite_SBMLExtensionNamespaces () );
  srunner_add_suite( runner, create_suite_SBMLExtensionRegistry () );

  /* srunner_set_fork_status(runner, CK_NOFORK); */

  srunner_run_all(runner, CK_NORMAL);
  num_failed = srunner_ntests_failed(runner);

  srunner_free(runner);

  return num_failed;
}
