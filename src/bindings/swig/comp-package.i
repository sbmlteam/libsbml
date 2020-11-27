/**
 * Filename    : comp-package.i
 * Description : SBML comp swig file for bindings.
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
 * ---------------------------------------------------------------------- -->
 */

#ifdef USE_COMP

%newobject removeSubmodel;
%newobject removePort;
%newobject removeReplacedElement;
%newobject removeModelDefinition;
%newobject removeExternalModelDefinition;
%newobject removeDeletion;


%feature("director") SBMLResolver;  

%ignore Submodel::getAllInstantiatedElements;

%extend Submodel
{
	ListWrapper<SBase> getListOfAllInstantiatedElements()
	{
		List* list = $self->getAllInstantiatedElements();
		return ListWrapper<SBase>(list);
	}
}

/* 
 * Currently we have to ignore methods that take std::list and vector.
 */
%ignore SBMLFileResolver::setAdditionalDirs(const std::vector<std::string>& dirs);


%include <sbml/packages/comp/extension/CompExtension.h>
%include <sbml/packages/comp/extension/CompSBasePlugin.h>
%include <sbml/packages/comp/extension/CompModelPlugin.h>
%include <sbml/packages/comp/extension/CompSBMLDocumentPlugin.h>

%include sbml/packages/comp/util/SBMLUri.h
%include sbml/packages/comp/util/SBMLResolver.h
%include sbml/packages/comp/util/SBMLFileResolver.h
%include sbml/packages/comp/util/SBMLResolverRegistry.h
%include <sbml/packages/comp/util/CompFlatteningConverter.h>

%include sbml/packages/comp/sbml/CompBase.h
%include sbml/packages/comp/sbml/SBaseRef.h
%include sbml/packages/comp/sbml/Replacing.h
%include sbml/packages/comp/sbml/Deletion.h
%include sbml/packages/comp/sbml/ExternalModelDefinition.h
%include sbml/packages/comp/sbml/ListOfDeletions.h
%include sbml/packages/comp/sbml/ListOfExternalModelDefinitions.h
%include sbml/packages/comp/sbml/ListOfModelDefinitions.h
%include sbml/packages/comp/sbml/ListOfPorts.h
%include sbml/packages/comp/sbml/ListOfReplacedElements.h
%include sbml/packages/comp/sbml/ListOfSubmodels.h
%include sbml/packages/comp/sbml/ModelDefinition.h
%include sbml/packages/comp/sbml/Port.h
%include sbml/packages/comp/sbml/ReplacedBy.h
%include sbml/packages/comp/sbml/ReplacedElement.h
%include sbml/packages/comp/sbml/Submodel.h
%include sbml/packages/comp/sbml/Replacing.h

%include sbml/packages/comp/validator/CompSBMLError.h

%template(CompPkgNamespaces) SBMLExtensionNamespaces<CompExtension>;


#endif /* USE_COMP */
