/**
 * @cond doxygen-libsbml-internal
 *
 * @file    MetaMetaIdFilterFilter.h
 * @brief   Filter to return only elements with a metaid set
 * @author  Sarah Keating
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

#ifndef MetaIdFilter_h
#define MetaIdFilter_h


#ifdef __cplusplus

#include <sbml/util/ElementFilter.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class MetaIdFilter : public ElementFilter
{
public:
	MetaIdFilter() : ElementFilter()
	{
	}

	virtual bool filter(const SBase* element)
	{
		// return in case we don't have a valid element with an id
    if (element == NULL || element->isSetMetaId() == false)
    {
        return false;
    }

    return true;			
	}

};

LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */
#endif  /* MetaIdFilter_h */

/** @endcond */
