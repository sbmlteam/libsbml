/* src/common/libsbml-namespace.h.  Generated from libsbml-namespace.h.in by configure.  */
/**
 * @file    libsbml-namespace.h
 * @brief   Defines C++ namespace of libSBML
 * @author  Akiya Jouraku
 *
 * $Id:$
 * $HeadURL:$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 */

#ifndef LIBSBML_NAMESPACE_H
#define LIBSBML_NAMESPACE_H 1

/*
 *
 * The idea of the following marcors are borrowed from 
 * Xerces-C++ XML Parser (http://xerces.apache.org/xerces-c/).
 *
 */

/* Define to enable libSBML C++ namespace */
/* #undef LIBSBML_USE_CPP_NAMESPACE */


#if defined(__cplusplus) && defined(LIBSBML_USE_CPP_NAMESPACE) && !defined(SWIG)
  /* C++ namespace of libSBML */
  #define LIBSBML_CPP_NAMESPACE            libsbml
  #define LIBSBML_CPP_NAMESPACE_BEGIN      namespace LIBSBML_CPP_NAMESPACE {
  #define LIBSBML_CPP_NAMESPACE_END        }
  #define LIBSBML_CPP_NAMESPACE_USE        using namespace LIBSBML_CPP_NAMESPACE;
  #define LIBSBML_CPP_NAMESPACE_QUALIFIER  LIBSBML_CPP_NAMESPACE::

  namespace LIBSBML_CPP_NAMESPACE {}
#else
  #define LIBSBML_CPP_NAMESPACE 
  #define LIBSBML_CPP_NAMESPACE_BEGIN
  #define LIBSBML_CPP_NAMESPACE_END  
  #define LIBSBML_CPP_NAMESPACE_USE 
  #define LIBSBML_CPP_NAMESPACE_QUALIFIER 
#endif


#endif  /* LIBSBML_NAMESPACE_H */
