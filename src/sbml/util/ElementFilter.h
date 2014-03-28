/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ElementFilter.h
 * @brief   Base class of all element filters as used by getAllElements
 * @author  Frank T. Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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
 * ---------------------------------------------------------------------- -->
 *
 * @class ElementFilter
 * @sbmlbrief{core}
 */

#ifndef ElementFilter_h
#define ElementFilter_h


#ifdef __cplusplus

#include <sbml/common/extern.h>
#include <sbml/common/libsbml-namespace.h>

LIBSBML_CPP_NAMESPACE_BEGIN

#define ADD_FILTERED_LIST(pResult,pSublist,list,pFilter)\
{\
  if (list.size() > 0) {\
    if (pFilter == NULL || pFilter->filter(&list))\
    pResult->add(&list);\
    pSublist = list.getAllElements(pFilter);\
    pResult->transferFrom(pSublist);\
    delete pSublist;\
  }\
}

#define ADD_FILTERED_PLIST(pResult,pSublist,pList,pFilter)\
{\
  if (pList != NULL && pList->size() > 0) {\
    if (pFilter == NULL || pFilter->filter(pList))\
    pResult->add(pList);\
    pSublist = pList->getAllElements(pFilter);\
    pResult->transferFrom(pSublist);\
    delete pSublist;\
  }\
}

#define ADD_FILTERED_POINTER(pResult,pSublist,pElement,pFilter)\
{\
  if (pElement != NULL) {\
    if (pFilter == NULL || pFilter->filter(pElement))\
    pResult->add(pElement);\
    pSublist = pElement->getAllElements(pFilter);\
    pResult->transferFrom(pSublist);\
    delete pSublist;\
  }\
}

#define ADD_FILTERED_ELEMENT(pResult,pSublist,element,pFilter)\
{\
  if (&element != NULL) {\
    if (pFilter == NULL || pFilter->filter(&element))\
    pResult->add(&element);\
    pSublist = element.getAllElements(pFilter);\
    pResult->transferFrom(pSublist);\
    delete pSublist;\
  }\
}

#define ADD_FILTERED_FROM_PLUGIN(pResult,pSublist,pFilter)\
{\
    pSublist = getAllElementsFromPlugins(pFilter);\
    pResult->transferFrom(pSublist);\
    delete pSublist;\
}

class SBase;

class LIBSBML_EXTERN ElementFilter
{
public:

  ElementFilter();
  virtual ~ElementFilter();
  virtual bool filter(const SBase* element);
  
  #ifndef SWIG
  void* getUserData();
  void setUserData(void* userData);
  #endif
  
private:
  void* mUserData;
};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* ElementFilter_h */

/** @endcond */
